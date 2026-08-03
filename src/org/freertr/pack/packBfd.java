package org.freertr.pack;

import org.freertr.util.bits;

/**
 * bidirectional forwarding detection protocol (rfc5880) packet
 *
 * @author matecsaba
 */
public class packBfd {

    /**
     * create instance
     */
    public packBfd() {
    }

    /**
     * size of header
     */
    public final static int size = 24;

    /**
     * port for one hop communication
     */
    public final static int portLoc = 3784;

    /**
     * port for multi hop communication
     */
    public final static int portRem = 4784;

    /**
     * admin down
     */
    public final static int stShut = 0;

    /**
     * down
     */
    public final static int stDown = 1;

    /**
     * init
     */
    public final static int stInit = 2;

    /**
     * up
     */
    public final static int stUp = 3;

    /**
     * poll
     */
    public final static int flgPoll = 0x20;

    /**
     * final
     */
    public final static int flgFinal = 0x10;

    /**
     * control plane independent
     */
    public final static int flgIndep = 0x08;

    /**
     * authentication
     */
    public final static int flgAuthen = 0x04;

    /**
     * demand mode
     */
    public final static int flgDemand = 0x02;

    /**
     * multipoint
     */
    public final static int flgMulti = 0x01;

    /**
     * status code
     */
    public int status;

    /**
     * diagnostic code
     */
    public int diagnostic;

    /**
     * flags value
     */
    public int flags;

    /**
     * detect multiplier
     */
    public int multiplier;

    /**
     * my discriminator
     */
    public int discrLoc;

    /**
     * your discriminator
     */
    public int discrRem;

    /**
     * minimum rx time in usec
     */
    public int timeRx;

    /**
     * minimum tx time in us
     */
    public int timeTx;

    /**
     * minimum echo time in us
     */
    public int timeEcho;

    /**
     * key id
     */
    public int keyId;

    /**
     * password
     */
    public String password;

    /**
     * parse header
     *
     * @param pck packet to use
     * @return false if successful, true if error happened
     */
    public boolean parseHeader(packHolder pck) {
        diagnostic = pck.getByte(0); // diagnostic + version
        if ((diagnostic & 0xe0) != 0x20) {
            return true;
        }
        diagnostic &= 0x1f;
        flags = pck.getByte(1); // state + flags
        status = flags >>> 6;
        flags &= 0x3f;
        multiplier = pck.getByte(2); // detect multiplier
        int i = pck.getByte(3); // size of packet
        if (i < size) {
            return true;
        }
        if (i > pck.dataSize()) {
            return true;
        }
        discrLoc = pck.msbGetD(4); // my discriminator
        discrRem = pck.msbGetD(8); // your discriminator
        timeTx = pck.msbGetD(12); // min tx time
        timeRx = pck.msbGetD(16); // min rx time
        timeEcho = pck.msbGetD(20); // min echo time
        pck.getSkip(size);
        if (pck.dataSize() < 3) {
            return false;
        }
        if (pck.getByte(0) != 1) { // auth type
            return true;
        }
        keyId = pck.getByte(2);
        password = pck.getAsciiZ(3, pck.getByte(1), -1);
        return false;
    }

    /**
     * create header
     *
     * @param pck packet to use
     */
    public void createHeader(packHolder pck) {
        pck.putByte(0, 0x20 | (diagnostic & 0x1f)); // version + diagnostic
        pck.putByte(1, (status << 6) + (flags & 0x3f)); // status + flags
        pck.putByte(2, multiplier); // multiplier
        pck.putByte(3, size); // message length
        pck.msbPutD(4, discrLoc); // my discriminator
        pck.msbPutD(8, discrRem); // your discriminator
        pck.msbPutD(12, timeTx); // min tx time
        pck.msbPutD(16, timeRx); // min rx time
        pck.msbPutD(20, timeEcho); // min echo time
        pck.putSkip(size);
        if (password == null) {
            return;
        }
        int len = password.length();
        pck.putByte(0, 1); // auth type
        pck.putByte(1, len); // auth len
        pck.putByte(2, keyId); // auth type
        pck.putAsciiZ(3, len, password, 0);
        pck.putSkip(len + 3);
    }

    /**
     * convert status to string
     *
     * @param i status
     * @return string
     */
    public static String state2string(int i) {
        switch (i) {
            case stUp:
                return "up";
            case stDown:
                return "down";
            case stShut:
                return "shut";
            case stInit:
                return "init";
            default:
                return "unknown=" + i;
        }
    }

    public String toString() {
        return "state=" + state2string(status) + " flag=" + bits.bit2str(flags, flgPoll, "p")
                + bits.bit2str(flags, flgFinal, "f") + bits.bit2str(flags, flgIndep, "c") + bits.bit2str(flags, flgAuthen, "a")
                + bits.bit2str(flags, flgDemand, "d") + bits.bit2str(flags, flgMulti, "m") + " loc=" + discrLoc + " rem="
                + discrRem + " rx=" + timeRx + " tx=" + timeTx + " echo=" + timeEcho + " multi=" + multiplier;
    }

    /**
     * get tx interval
     *
     * @param peer peer state
     * @param needed needed interval
     * @return tx interval in ms
     */
    public int getTxInt(packBfd peer, int needed) {
        if (peer == null) {
            return 1000;
        }
        int i = peer.timeRx / 1000;
        if (needed > i) {
            i = needed;
        }
        return i;
    }

    /**
     * get rx interval
     *
     * @param peer peer state
     * @param needed needed interval
     * @return rx interval in ms
     */
    public static long getRxInt(packBfd peer, int needed) {
        if (peer == null) {
            return 10000;
        }
        int i = peer.timeTx / 1000;
        if (needed > i) {
            i = needed;
        }
        return i * peer.multiplier;
    }

}
