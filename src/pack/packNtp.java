package pack;

import addr.addrIPv4;
import util.bits;

/**
 * network time protocol (rfc5905) packet
 *
 * @author matecsaba
 */
public class packNtp {

    /**
     * port number to use
     */
    public static final int port = 123;

    /**
     * size of packet
     */
    public static final int size = 48;

    /**
     * time difference
     */
    public static final long timDif = 2208988800000L;

    /**
     * leap indicator
     */
    public int leap;

    /**
     * version number
     */
    public int ver;

    /**
     * mode
     */
    public int mode;

    /**
     * stratum
     */
    public int stratum;

    /**
     * poll
     */
    public int poll;

    /**
     * precision
     */
    public int precise;

    /**
     * root delay
     */
    public int rotDel;

    /**
     * root dispersion
     */
    public int rotDis;

    /**
     * reference id
     */
    public addrIPv4 refId = new addrIPv4();

    /**
     * reference time
     */
    public long refTime;

    /**
     * origination time
     */
    public long origTime;

    /**
     * receive time
     */
    public long recvTime;

    /**
     * transmit time
     */
    public long sendTime;

    /**
     * symmetric active
     */
    public static final int modSymAct = 1;

    /**
     * symmetric passive
     */
    public static final int modSymPsv = 2;

    /**
     * client
     */
    public static final int modClnt = 3;

    /**
     * server
     */
    public static final int modServ = 4;

    /**
     * broadcast
     */
    public static final int modBcst = 5;

    /**
     * convert mode to string
     *
     * @param i mode
     * @return string
     */
    public static String mode2str(int i) {
        switch (i) {
            case modSymAct:
                return "symmetricActive";
            case modSymPsv:
                return "symmetricPassive";
            case modClnt:
                return "client";
            case modServ:
                return "server";
            case modBcst:
                return "broadcast";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * decode ntp time
     *
     * @param tim time
     * @return decoded
     */
    public static long decode(long tim) {
        return ((tim >>> 32) * 1000) - timDif;
    }

    /**
     * encode ntp time
     *
     * @param tim time
     * @return encoded
     */
    public static long encode(long tim) {
        return ((tim + timDif) / 1000) << 32;
    }

    public String toString() {
        return "ver=" + ver + " mode=" + mode2str(mode) + " str=" + stratum + " ref=" + refId + " ref=" + bits.time2str(null, decode(refTime), 3) + " orig=" + bits.time2str(null, decode(origTime), 3) + " rx=" + bits.time2str(null, decode(recvTime), 3) + " tx=" + bits.time2str(null, decode(sendTime), 3);
    }

    private long getTime(packHolder pck) {
        long tim = pck.msbGetQ(0);
        pck.getSkip(8);
        return tim;
    }

    private void putTime(packHolder pck, long tim) {
        pck.msbPutQ(0, tim);
        pck.putSkip(8);
    }

    /**
     * parse one packet
     *
     * @param pck packet to read
     * @return false on success, true on error
     */
    public boolean parsePacket(packHolder pck) {
        if (pck.dataSize() < size) {
            return true;
        }
        mode = pck.getByte(0);
        leap = mode >>> 6;
        ver = (mode >>> 3) & 7;
        mode &= 7;
        stratum = pck.getByte(1);
        poll = pck.getByte(2);
        precise = pck.getByte(3);
        rotDel = pck.msbGetD(4);
        rotDis = pck.msbGetD(8);
        pck.getAddr(refId, 12);
        pck.getSkip(16);
        refTime = getTime(pck);
        origTime = getTime(pck);
        recvTime = getTime(pck);
        sendTime = getTime(pck);
        return false;
    }

    /**
     * create one packet
     *
     * @param pck packet to write
     */
    public void createPacket(packHolder pck) {
        pck.putByte(0, (leap << 6) | (ver << 3) | mode);
        pck.putByte(1, stratum);
        pck.putByte(2, poll);
        pck.putByte(3, precise);
        pck.msbPutD(4, rotDel);
        pck.msbPutD(8, rotDis);
        pck.putAddr(12, refId);
        pck.putSkip(16);
        putTime(pck, refTime);
        putTime(pck, origTime);
        putTime(pck, recvTime);
        putTime(pck, sendTime);
        pck.merge2beg();
    }

}
