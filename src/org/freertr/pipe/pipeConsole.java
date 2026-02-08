package org.freertr.pipe;

import org.freertr.util.logger;

/**
 * convert console to pipeline
 *
 * @author matecsaba
 */
public class pipeConsole implements Runnable {

    private pipeSide pipe;

    /**
     * attach console to a pipeline
     *
     * @param pip where to attach
     */
    public pipeConsole(pipeSide pip) {
        pipe = pip;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.setTime(0);
        new pipeConsoleDisp(pipe);
        logger.startThread(this);
    }

    /**
     * create a pipeline attached to the console
     *
     * @return the new pipeline
     */
    public static pipeSide create() {
        pipeLine p = new pipeLine(65536, false);
        new pipeConsole(p.getSide());
        pipeSide ps = p.getSide();
        ps.lineTx = pipeSide.modTyp.modeCRLF;
        ps.lineRx = pipeSide.modTyp.modeCRorLF;
        ps.setTime(0);
        return ps;
    }

    public void run() {
        try {
            for (;;) {
                if (pipe.isClosed() != 0) {
                    break;
                }
                byte[] buf = new byte[1];
                int i = System.in.read(buf);
                if (i < 0) {
                    break;
                }
                if (pipe.blockingPut(buf, 0, i) != i) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

}

class pipeConsoleDisp implements Runnable {

    private pipeSide pipe;

    public pipeConsoleDisp(pipeSide pip) {
        pipe = pip;
        logger.startThread(this);
    }

    public void run() {
        try {
            pipe.setReady();
            for (;;) {
                byte[] buf = new byte[1024];
                int siz = pipe.blockingGet(buf, 0, buf.length);
                if (siz < 0) {
                    break;
                }
                String a = "";
                for (int i = 0; i < siz; i++) {
                    a += (char) (buf[i] & 0xff);
                }
                System.out.print(a);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        pipe.setClose();
    }

}
