package net.freertr.pipe;

import net.freertr.util.logger;

/**
 * pipeline discarder
 *
 * @author matecsaba
 */
public class pipeDiscard {

    /**
     * discard one pipeline
     *
     * @param side pipeline side to discard
     */
    public static void discard(pipeSide side) {
        new pipeDiscardDoer(side);
    }

    /**
     * need any pipeline
     *
     * @param side side to check
     * @return if not exists then get a discarder, otherwise get the mine
     */
    public static pipeSide needAny(pipeSide side) {
        if (side != null) {
            return side;
        }
        pipeLine pip = new pipeLine(32768, false);
        pipeDiscard.discard(pip.getSide());
        pipeSide res = pip.getSide();
        pip.setClose();
        return res;
    }

    /**
     * flush pipe side
     *
     * @param pipe pipeline
     */
    public static void flush(pipeSide pipe) {
        for (;;) {
            byte[] buf = new byte[1024];
            if (pipe.nonBlockGet(buf, 0, buf.length) < buf.length) {
                break;
            }
        }
    }

    /**
     * read a line from pipe
     *
     * @param pipe pipeline
     * @return line read, null if nothing
     */
    public static String readLine(pipeSide pipe) {
        if (pipe.ready2rx() < 2) {
            return null;
        }
        return pipe.lineGet(1);
    }

    /**
     * log pipe side
     *
     * @param pre prepend
     * @param pipe pipeline
     */
    public static void logLines(String pre, pipeSide pipe) {
        for (;;) {
            String a = readLine(pipe);
            if (a == null) {
                break;
            }
            if (a.length() < 1) {
                break;
            }
            logger.info(pre + a);
        }
    }

    private pipeDiscard() {
    }

}

class pipeDiscardDoer implements Runnable {

    private pipeSide rx;

    private int siz;

    public pipeDiscardDoer(pipeSide side) {
        rx = side;
        siz = rx.getBufSize();
        new Thread(this).start();
    }

    public void run() {
        try {
            for (;;) {
                byte[] buf = new byte[siz];
                if (rx.blockingGet(buf, 0, buf.length) < 0) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        rx.setClose();
    }

}
