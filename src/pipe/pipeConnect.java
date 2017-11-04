package pipe;

import util.logger;

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
        new pipeConnectDoer(side1, side2, close);
        new pipeConnectDoer(side2, side1, close);
    }

    /**
     * loop back one pipeline
     *
     * @param side pipeline side to loop back
     */
    public static void loopback(pipeSide side) {
        new pipeConnectDoer(side, side, true);
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

    private boolean cls;

    public pipeConnectDoer(pipeSide recv, pipeSide send, boolean close) {
        rx = recv;
        tx = send;
        cls = close;
        siz = tx.getBufSize();
        siz = siz - (siz / 16);
        new Thread(this).start();
    }

    public void run() {
        try {
            for (;;) {
                byte buf[] = new byte[siz];
                int i = rx.blockingGet(buf, 0, buf.length);
                if (i < 0) {
                    break;
                }
                tx.blockingPut(buf, 0, i);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        if (cls) {
            rx.setClose();
            tx.setClose();
        }
    }

}
