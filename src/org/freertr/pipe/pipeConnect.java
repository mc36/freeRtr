package org.freertr.pipe;

import org.freertr.util.bits;
import org.freertr.util.logger;
import org.freertr.util.syncInt;

/**
 * pipeline connector
 *
 * @author matecsaba
 */
public class pipeConnect {

    /**
     * connect two pipelines into each other
     *
     * @param side1 one side
     * @param side2 another side
     * @param close true to close both sides after, false to not
     */
    public static void connect(pipeSide side1, pipeSide side2, boolean close) {
        syncInt cls = null;
        if (!close) {
            cls = new syncInt(0);
        }
        new pipeConnectDoer(side1, side2, cls, 0);
        new pipeConnectDoer(side2, side1, cls, 0);
    }

    /**
     * loop back one pipeline
     *
     * @param side pipeline side to loop back
     * @param delay delay in ms
     */
    public static void loopback(pipeSide side, int delay) {
        new pipeConnectDoer(side, side, null, delay);
    }

    /**
     * redirect between pipes
     *
     * @param rx receiver side
     * @param tx sender side
     * @return true on error
     */
    public static boolean redirect(pipeSide rx, pipeSide tx) {
        int siz = tx.ready2tx();
        if (siz < 16) {
            return true;
        }
        byte[] buf = new byte[siz - 16];
        siz = rx.nonBlockGet(buf, 0, buf.length);
        if (siz < 1) {
            return false;
        }
        tx.nonBlockPut(buf, 0, siz);
        return false;
    }

    private pipeConnect() {
    }

}

class pipeConnectDoer implements Runnable {

    private pipeSide rx;

    private pipeSide tx;

    private int siz;

    private syncInt cls;

    private int del;

    public pipeConnectDoer(pipeSide recv, pipeSide send, syncInt close, int delay) {
        rx = recv;
        tx = send;
        cls = close;
        siz = tx.getBufSize();
        siz = siz - (siz / 16);
        del = delay;
        logger.startThread(this);
    }

    public void run() {
        try {
            rx.setReady();
            for (;;) {
                if (cls != null) {
                    if (cls.get() != 0) {
                        break;
                    }
                }
                byte[] buf = new byte[siz];
                int i = rx.blockingGet(buf, 0, buf.length);
                if (i < 0) {
                    break;
                }
                if (del > 0) {
                    bits.sleep(del);
                }
                tx.blockingPut(buf, 0, i);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        if (cls == null) {
            rx.setClose();
            tx.setClose();
        } else {
            cls.set(1);
        }
    }

}
