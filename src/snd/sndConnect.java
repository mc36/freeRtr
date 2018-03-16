package snd;

import pack.packHolder;
import pack.packRtp;
import util.bits;
import util.logger;

/**
 * connect sound sources
 *
 * @author matecsaba
 */
public class sndConnect {

    private final packRtp side1;

    private final packRtp side2;

    /**
     * create connection
     *
     * @param s1 side one
     * @param s2 side two
     * @param c1 codec one
     * @param c2 codec two
     */
    public sndConnect(packRtp s1, packRtp s2, sndCodec c1, sndCodec c2) {
        side1 = s1;
        side2 = s2;
        if (c1.getRTPtype() == c2.getRTPtype()) {
            new sndConnectSmpl(s1, s2, c1);
            new sndConnectSmpl(s2, s1, c2);
        } else {
            new sndConnectTrns(s1, s2, c1, c2);
            new sndConnectTrns(s2, s1, c2, c1);
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

class sndConnectSmpl implements Runnable {

    private packRtp rx;

    private packRtp tx;

    private sndCodec codec;

    public sndConnectSmpl(packRtp s1, packRtp s2, sndCodec c) {
        rx = s1;
        tx = s2;
        codec = c;
        new Thread(this).start();
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
            if (rx.recvPack(pck, true) < 1) {
                break;
            }
            if (pck.RTPtyp != codec.getRTPtype()) {
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

class sndConnectTrns implements Runnable {

    private packRtp rxS;

    private packRtp txS;

    private sndCodec rxC;

    private sndCodec txC;

    private int syncSrc;

    public sndConnectTrns(packRtp s1, packRtp s2, sndCodec c1, sndCodec c2) {
        rxS = s1;
        txS = s2;
        rxC = c1;
        txC = c2;
        syncSrc = bits.randomD();
        new Thread(this).start();
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
            if (rxS.recvPack(pck, true) < 1) {
                break;
            }
            if (pck.RTPtyp != rxC.getRTPtype()) {
                continue;
            }
            byte[] buf = txC.encodeBuf(rxC.decodeBuf(pck.getCopy()));
            pck.clear();
            pck.putCopy(buf, 0, 0, buf.length);
            pck.putSkip(buf.length);
            pck.merge2end();
            pck.RTPtyp = txC.getRTPtype();
            pck.RTPsrc = syncSrc;
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
