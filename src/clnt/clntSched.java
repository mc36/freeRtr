package clnt;

import cfg.cfgAll;
import cfg.cfgTime;
import java.util.Timer;
import java.util.TimerTask;
import pipe.pipeDiscard;
import pipe.pipeLine;
import pipe.pipeSide;
import user.userExec;
import user.userReader;
import util.bits;
import util.cmds;
import util.logger;

/**
 * scheduler worker
 *
 * @author matecsaba
 */
public class clntSched {

    /**
     * time between runs
     */
    public int interval;

    /**
     * initial delay
     */
    public int initial;

    /**
     * exec command
     */
    public String command = cmds.finish;

    /**
     * hide commands
     */
    public boolean hidden = false;

    /**
     * action logging
     */
    public boolean logging = false;

    /**
     * status, false=stopped, true=running
     */
    public boolean working = false;

    /**
     * time range when allowed
     */
    public cfgTime time;

    private Timer keepTimer;

    public String toString() {
        return command;
    }

    /**
     * stop running
     */
    public synchronized void stopNow() {
        try {
            keepTimer.cancel();
        } catch (Exception e) {
        }
        keepTimer = null;
        working = false;
    }

    /**
     * start running
     */
    public synchronized void startNow() {
        if (working) {
            return;
        }
        if (interval < 1) {
            return;
        }
        working = true;
        keepTimer = new Timer();
        clntSchedTimer task = new clntSchedTimer(this);
        keepTimer.schedule(task, initial, interval);
    }

    /**
     * do one timer round
     */
    public void doRound() {
        if (time != null) {
            if (time.matches(bits.getTime() + cfgAll.timeServerOffset)) {
                return;
            }
        }
        if (logging) {
            logger.info("starting action " + this);
        }
        pipeLine pipe = new pipeLine(32768, false);
        pipeDiscard.discard(pipe.getSide());
        pipeSide pip = pipe.getSide();
        userReader rdr = new userReader(pip, 1023);
        rdr.height = 0;
        userExec exe = new userExec(pip, rdr);
        exe.privileged = true;
        pip.timeout = 120000;
        String s = exe.repairCommand(command);
        exe.executeCommand(s);
        pipe.setClose();
        if (logging) {
            logger.info("stopped action");
        }
    }

}

class clntSchedTimer extends TimerTask {

    private clntSched lower;

    public clntSchedTimer(clntSched parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.doRound();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
