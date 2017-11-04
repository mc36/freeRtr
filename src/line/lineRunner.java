package line;

import pipe.pipeSide;
import user.userLine;
import util.bits;
import util.debugger;
import util.logger;

/**
 * run one line infinitely
 *
 * @author matecsaba
 */
public class lineRunner implements Runnable {

    /**
     * hangup script
     */
    public lineScript scrptHangup;

    /**
     * init script
     */
    public lineScript scrptInit;

    /**
     * activate script
     */
    public lineScript scrptActv;

    /**
     * always monitors
     */
    private boolean monitor = false;

    /**
     * need activation character
     */
    private boolean dedicated = false;

    private boolean needNewLine = true;

    private lineThread thread;

    private userLine line;

    private String name;

    private pipeSide pipe;

    /**
     * set always monitor state
     *
     * @param mon monitoring state
     */
    public void setMon(boolean mon) {
        monitor = mon;
        if (monitor) {
            logger.pipeStart(pipe);
        } else {
            logger.pipeStop(pipe);
        }
    }

    /**
     * get always monitor state
     *
     * @return monitoring state
     */
    public boolean getMon() {
        return monitor;
    }

    /**
     * set dedicated state
     *
     * @param ded dedicated state
     */
    public void setDedi(boolean ded) {
        dedicated = ded;
        if (pipe == null) {
            return;
        }
        pipe.setClose();
    }

    /**
     * get dedicated state
     *
     * @return dedicated state
     */
    public boolean getDedi() {
        return dedicated;
    }

    /**
     * create one runner
     *
     * @param thrd thread to use
     * @param lin line to use
     * @param nam name to use
     */
    public lineRunner(lineThread thrd, userLine lin, String nam) {
        thread = thrd;
        line = lin;
        name = nam;
        pipe = thread.getPipe();
        new Thread(this).start();
    }

    public void run() {
        if (debugger.lineRunnerEvnt) {
            logger.debug("start watcher");
        }
        for (;;) {
            try {
                doWork();
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
    }

    private void doWork() {
        bits.sleep(1000);
        if (pipe.isClosed() != 0) {
            if (debugger.lineRunnerEvnt) {
                logger.debug("restart pipe");
            }
            if (monitor) {
                logger.pipeStop(pipe);
            }
            pipe = thread.getPipe();
            needNewLine = true;
            doScript(scrptHangup);
            if (doScript(scrptInit)) {
                pipe.setClose();
                return;
            }
            if (monitor) {
                logger.pipeStart(pipe);
            }
        }
        if (!needNewLine) {
            return;
        }
        if (!dedicated) {
            byte[] buf = new byte[1];
            if (pipe.nonBlockGet(buf, 0, buf.length) < 1) {
                return;
            }
            if (buf[0] != line.promptActivate) {
                return;
            }
        }
        doScript(scrptActv);
        line.createHandler(pipe, name, true);
        if (debugger.lineRunnerEvnt) {
            logger.debug("restart line");
        }
        needNewLine = false;
    }

    private boolean doScript(lineScript scr) {
        if (scr == null) {
            return false;
        }
        return scr.doScript(pipe);
    }

    /**
     * send one line
     *
     * @param s string to send
     */
    public void sendLine(String s) {
        byte[] buf = s.getBytes();
        pipe.blockingPut(buf, 0, buf.length);
        buf = pipeSide.getEnding(pipeSide.modTyp.modeCRLF);
        pipe.blockingPut(buf, 0, buf.length);
    }

    /**
     * do attach work
     *
     * @return pipeline to use
     */
    public pipeSide doAttach() {
        if (debugger.lineRunnerEvnt) {
            logger.debug("terminal to line");
        }
        needNewLine = false;
        pipe = thread.getPipe();
        needNewLine = false;
        return pipe;
    }

}
