package pack;

import pipe.pipeSide;
import util.bits;

/**
 * block serial tunneling
 *
 * @author matecsaba
 */
public class packBstun {

    private pipeSide pipe;

    private final int group;

    public String toString() {
        return "stun";
    }

    /**
     * create one handler
     *
     * @param conn connection to use
     * @param grp group to use
     */
    public packBstun(pipeSide conn, int grp) {
        pipe = conn;
        group = grp;
    }

    /**
     * open the connection
     */
    public void setOpening() {
        byte[] buf = new byte[0x1b];
        sender(2, buf.length, buf);
    }

    /**
     * close the handler
     */
    public void setClose() {
        pipe.setClose();
    }

    /**
     * send one packet
     *
     * @param len bytes to send
     * @param buf bytes to send
     */
    public void sendPack(int len, byte[] buf) {
        sender(0, len, buf);
    }

    /**
     * receive one packet
     *
     * @return received bytes, null if nothing
     */
    public byte[] recvPack() {
        for (;;) {
            byte[] buf = new byte[11];
            if (pipe.moreGet(buf, 0, buf.length) != buf.length) {
                return null;
            }
            // int typ = bits.msbGetW(buf, 0); // type
            int cmd = bits.msbGetW(buf, 2); // command
            int len = bits.msbGetW(buf, 4) - 3; // size
            int grp = bits.getByte(buf, 6); // group
            buf = new byte[len];
            if (pipe.moreGet(buf, 0, buf.length) != buf.length) {
                return null;
            }
            if (cmd != 0) {
                continue;
            }
            if (grp != group) {
                continue;
            }
            return buf;
        }
    }

    private void sender(int cmd, int len, byte[] buf) {
        byte[] hdr = new byte[11];
        bits.byteFill(hdr, 0, hdr.length, 0);
        bits.msbPutW(hdr, 0, 0xebd); // type
        bits.msbPutW(hdr, 2, cmd); // command
        bits.msbPutW(hdr, 4, len + 3); // size
        bits.putByte(hdr, 6, group); // group number
        if (cmd == 0) {
            bits.putByte(hdr, 9, 0x21);
        }
        pipe.morePut(hdr, 0, hdr.length);
        pipe.morePut(buf, 0, len);
    }

}
