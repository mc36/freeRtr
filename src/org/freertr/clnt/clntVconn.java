package org.freertr.clnt;

import org.freertr.enc.encCallOne;
import org.freertr.enc.encCodec;
import org.freertr.pack.packHolder;
import org.freertr.util.logger;

/**
 * connect sound sources
 *
 * @author matecsaba
 */
public class clntVconn {

    private final encCallOne side1;

    private final encCallOne side2;

    /**
     * create connection
     *
     * @param s1 side one
     * @param s2 side two
     * @param c1 codec one
     * @param c2 codec two
     */
    public clntVconn(encCallOne s1, encCallOne s2, encCodec c1, encCodec c2) {
        side1 = s1;
        side2 = s2;
        if (c1.getRTPtype() == c2.getRTPtype()) {
            new clntVconnSmpl(s1, s2, c1);
            new clntVconnSmpl(s2, s1, c2);
        } else {
            new clntVconnTrns(s1, s2, c1, c2);
            new clntVconnTrns(s2, s1, c2, c1);
        }
    }

    /**
     * close connection
     */
    public void setClose() {
        side1.setClose();
        side2.setClose();
    }

    /**
     * test if the any side was closed
     *
     * @return status
     */
    public int isClosed() {
        return side1.isClosed() | side2.isClosed();
    }

}

class clntVconnSmpl implements Runnable {

    private encCallOne rx;

    private encCallOne tx;

    private encCodec codec;

    public clntVconnSmpl(encCallOne s1, encCallOne s2, encCodec c) {
        rx = s1;
        tx = s2;
        codec = c;
        logger.startThread(this);
    }

    public void doer() {
        packHolder pck = new packHolder(true, true);
        for (;;) {
            if (rx.isClosed() != 0) {
                break;
            }
            if (tx.isClosed() != 0) {
                break;
            }
            pck.clear();
            if (rx.recvPack(pck, true, true) < 1) {
                continue;
            }
            tx.sendPack(pck);
        }
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
        rx.setClose();
        tx.setClose();
    }

}

class clntVconnTrns implements Runnable {

    private encCallOne rxS;

    private encCallOne txS;

    private encCodec rxC;

    private encCodec txC;

    public clntVconnTrns(encCallOne s1, encCallOne s2, encCodec c1, encCodec c2) {
        rxS = s1;
        txS = s2;
        rxC = c1;
        txC = c2;
        logger.startThread(this);
    }

    public void doer() {
        packHolder pck = new packHolder(true, true);
        for (;;) {
            if (rxS.isClosed() != 0) {
                break;
            }
            if (txS.isClosed() != 0) {
                break;
            }
            pck.clear();
            if (rxS.recvPack(pck, true, true) < 1) {
                continue;
            }
            byte[] buf = txC.encodeBuf(rxC.decodeBuf(pck.getCopy()));
            pck.clear();
            pck.putCopy(buf, 0, 0, buf.length);
            pck.putSkip(buf.length);
            pck.merge2end();
            txS.sendPack(pck);
        }
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
        rxS.setClose();
        txS.setClose();
    }

}
