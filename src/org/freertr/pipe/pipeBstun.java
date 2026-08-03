package org.freertr.pipe;

import org.freertr.pack.packBstun;
import org.freertr.util.logger;

/**
 * one bstun line handler
 *
 * @author matecsaba
 */
public class pipeBstun {

    /**
     * port number
     */
    public final static int port = 1976;

    private pipeSide lower;

    private pipeSide upper;

    /**
     * create one bstun handler
     *
     * @param net where to send bstun packets
     * @param group group number to use
     */
    public pipeBstun(pipeSide net, int group) {
        lower = net;
        pipeLine pipC = new pipeLine(65536, false);
        pipeSide pipA = pipC.getSide();
        upper = pipC.getSide();
        pipA.setReady();
        upper.setReady();
        new pipeBstunTx(net, pipA, group);
        new pipeBstunRx(net, pipA, group);
    }

    /**
     * close handler
     */
    public void setClose() {
        lower.setClose();
        upper.setClose();
    }

    /**
     * get client side
     *
     * @return pipeline of client
     */
    public pipeSide getPipe() {
        return upper;
    }

}

class pipeBstunRx implements Runnable {

    private packBstun pck;

    private pipeSide upper;

    public pipeBstunRx(pipeSide net, pipeSide usr, int grp) {
        pck = new packBstun(net, grp);
        upper = usr;
        logger.startThread(this);
    }

    public void run() {
        try {
            for (;;) {
                byte[] buf = pck.recvPack();
                if (buf == null) {
                    break;
                }
                if (upper.morePut(buf, 0, buf.length) != buf.length) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pck.setClose();
        upper.setClose();
    }

}

class pipeBstunTx implements Runnable {

    private packBstun pck;

    private pipeSide upper;

    public pipeBstunTx(pipeSide net, pipeSide usr, int grp) {
        pck = new packBstun(net, grp);
        upper = usr;
        logger.startThread(this);
    }

    public void run() {
        pck.setOpening();
        try {
            for (;;) {
                byte[] buf = new byte[1024];
                if (upper.moreGet(buf, 0, 1) != 1) {
                    break;
                }
                int i = upper.nonBlockGet(buf, 1, buf.length - 1);
                if (i < 0) {
                    i = 1;
                } else {
                    i += 1;
                }
                pck.sendPack(i, buf);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pck.setClose();
        upper.setClose();
    }

}
