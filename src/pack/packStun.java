package pack;

import pipe.pipeSide;
import util.bits;

/**
 * serial tunneling
 *
 * @author matecsaba
 */
public class packStun {

    /**
     * port number
     */
    public final static int port = 1994;

    private pipeSide pipe;

    private final int group;

    /**
     * create one handler
     *
     * @param conn connection to use
     * @param grp group to use
     */
    public packStun(pipeSide conn, int grp) {
        pipe = conn;
        group = grp;
    }

    /**
     * open the connection
     */
    public void setOpening() {
        packHolder pck = new packHolder(true, true);
        pck.putFill(0, 0x1e, 0);
        pck.putSkip(0x1e);
        txPkt(2, pck);
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
     * @param pck packet to send
     */
    public void sendPack(packHolder pck) {
        txPkt(0, pck);
    }

    /**
     * receive one packet
     *
     * @return received packet, null if nothing
     */
    public packHolder recvPack() {
        for (;;) {
            byte[] buf = new byte[7];
            if (pipe.moreGet(buf, 0, buf.length) != buf.length) {
                return null;
            }
            // int typ = bits.msbGetW(buf, 0); // type
            int cmd = bits.msbGetW(buf, 2); // command
            int len = bits.msbGetW(buf, 4); // size
            // int grp = bits.getByte(buf, 6); // group
            packHolder pck = new packHolder(true, true);
            pck.pipeRecv(pipe, 0, len, 144);
            if (cmd != 0) {
                continue;
            }
            return pck;
        }
    }

    private void txPkt(int cmd, packHolder pck) {
        pck.merge2beg();
        pck.msbPutW(0, 0x831); // type
        pck.msbPutW(2, cmd); // command
        pck.msbPutW(4, pck.dataSize()); // size
        pck.putByte(6, group); // group
        pck.putSkip(7);
        pck.merge2beg();
        pipe.morePut(pck.getCopy(), 0, pck.dataSize());
    }

}
