package org.freertr.pack;

import org.freertr.pipe.pipeSide;
import org.freertr.util.bits;

/**
 * x25 over tcp (rfc1613) packet
 *
 * @author matecsaba
 */
public class packXotPad {

    /**
     * port number
     */
    public final static int port = 1998;

    /**
     * max data size
     */
    public final static int datMax = 128;

    private final pipeSide conn;

    private int lci;

    private int seqTx;

    private int seqRx;

    private boolean canTx;

    /**
     * create instance
     *
     * @param pipe pipeline to use
     */
    public packXotPad(pipeSide pipe) {
        conn = pipe;
    }

    /**
     * receive one packet
     *
     * @return packet, null if error
     */
    public packHolder recvPack() {
        byte[] buf = new byte[4];
        if (conn.moreGet(buf, 0, buf.length) != buf.length) {
            return null;
        }
        if (bits.msbGetW(buf, 0) != 0) { // version
            return null;
        }
        int len = bits.msbGetW(buf, 2);
        packHolder pck = new packHolder(true, true);
        if (pck.pipeRecv(conn, 0, len, 144) != len) {
            return null;
        }
        return pck;
    }

    /**
     * send one packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        pck.merge2end();
        pck.msbPutW(0, 0); // version
        pck.msbPutW(2, pck.dataSize());
        pck.putSkip(4);
        pck.merge2beg();
        pck.pipeSend(conn, 0, pck.dataSize(), 2);
    }

    /**
     * close connection
     */
    public void setClose() {
        conn.setClose();
        canTx = true;
    }

    /**
     * parse call request
     *
     * @param pck packet to read
     * @return true on error, false on success
     */
    public boolean parseCallReq(packHolder pck) {
        if (pck == null) {
            return true;
        }
        if (pck.getByte(0) != 0x10) { // info
            return true;
        }
        lci = pck.getByte(1);
        if (pck.getByte(2) != 0x0b) { // call request
            return true;
        }
        return false;
    }

    /**
     * create call request
     *
     * @param called called number
     * @param calling calling number
     * @return packet
     */
    public packHolder createCallReq(String called, String calling) {
        lci = bits.randomB();
        packHolder pck = new packHolder(true, true);
        pck.putByte(0, 0x10); // info
        pck.putByte(1, lci);
        pck.putByte(2, 0x0b); // call request
        pck.putByte(3, called.length() | (calling.length() << 4));
        pck.putSkip(4);
        called = called + calling;
        if ((called.length() & 1) != 0) {
            called += "0";
        }
        for (; called.length() > 0;) {
            calling = called.substring(0, 2);
            called = called.substring(2, called.length());
            pck.putByte(0, bits.fromHex(calling));
            pck.putSkip(1);
        }
        pck.putByte(0, 0x00); // facility length
        pck.putSkip(1);
        return pck;
    }

    /**
     * parse call accept
     *
     * @param pck packet
     * @return true on error, false on success
     */
    public boolean parseCallAcc(packHolder pck) {
        if (pck == null) {
            return true;
        }
        if (pck.getByte(0) != 0x10) { // info
            return true;
        }
        if (lci != pck.getByte(1)) {
            return true;
        }
        if (pck.getByte(2) != 0x0f) { // call accept
            return true;
        }
        canTx = true;
        return false;
    }

    /**
     * create call accept
     *
     * @return packet
     */
    public packHolder createCallAcc() {
        packHolder pck = new packHolder(true, true);
        pck.putByte(0, 0x10); // info
        pck.putByte(1, lci);
        pck.putByte(2, 0x0f); // call connected
        pck.putByte(3, 0x00); // address length
        pck.putByte(4, 0x00); // facility length
        pck.putSkip(5);
        canTx = true;
        return pck;
    }

    /**
     * parse data packet
     *
     * @param pck packet
     * @return null on error, bytes received
     */
    public byte[] parseData(packHolder pck) {
        if (pck == null) {
            return null;
        }
        if (pck.getByte(0) != 0x10) { // info
            return null;
        }
        if (lci != pck.getByte(1)) {
            return null;
        }
        int typ = pck.getByte(2);
        if ((typ & 0x1f) == 1) { // receiver ready
            canTx = true;
            return new byte[0];
        }
        if ((typ & 1) != 0) { // data
            return new byte[0];
        }
        pck.getSkip(3);
        byte[] res = pck.getCopy();
        pck.clear();
        pck.putByte(0, 0x10); // info
        pck.putByte(1, lci);
        pck.putByte(2, (typ << 4) | 1); // receiver ready
        pck.putSkip(3);
        sendPack(pck);
        seqRx = (typ >>> 1) & 7;
        return res;
    }

    /**
     * create data packet
     *
     * @param buf bytes to send
     * @return packet
     */
    public packHolder createData(byte[] buf) {
        packHolder pck = new packHolder(true, true);
        pck.putByte(0, 0x10); // info
        pck.putByte(1, lci);
        pck.putByte(2, (seqTx << 1) | (seqRx << 7)); // data
        pck.putSkip(3);
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        seqTx = (seqTx + 1) & 7;
        canTx = false;
        return pck;
    }

    /**
     * do receive work
     *
     * @param psx pipe side
     */
    public void doerRx(pipeSide psx) {
        for (;;) {
            byte[] buf = parseData(recvPack());
            if (buf == null) {
                return;
            }
            psx.morePut(buf, 0, buf.length);
        }
    }

    /**
     * do transmit work
     *
     * @param psx pipe side
     */
    public void doerTx(pipeSide psx) {
        for (;;) {
            if (!canTx) {
                bits.sleep(100);
                continue;
            }
            int len = psx.ready2rx();
            if (len > 128) {
                len = 128;
            }
            if (len < 1) {
                len = 1;
            }
            byte[] buf = new byte[len];
            if (psx.moreGet(buf, 0, buf.length) != buf.length) {
                return;
            }
            sendPack(createData(buf));
        }
    }

}
