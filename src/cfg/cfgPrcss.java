package cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pipe.pipeConnect;
import pipe.pipeDiscard;
import pipe.pipeLine;
import pipe.pipeShell;
import pipe.pipeSide;
import user.userFilter;
import user.userHelping;
import util.cmds;
import util.bits;
import util.logger;
import tab.tabGen;

/**
 * one process configuration
 *
 * @author matecsaba
 */
public class cfgPrcss implements Comparator<cfgPrcss>, Runnable, cfgGeneric {

    /**
     * name of this process
     */
    public String name;

    /**
     * description of this process
     */
    public String description = "";

    /**
     * respawn on termination
     */
    public boolean respawn = true;

    /**
     * execute this binary
     */
    public String execName = null;

    /**
     * parameters to give
     */
    public String execArgs = null;

    /**
     * time between runs
     */
    protected int interval = 5000;

    /**
     * initial delay
     */
    protected int initial = 1000;

    /**
     * console pipeline
     */
    public pipeSide con;

    /**
     * restart count
     */
    public int restartC;

    /**
     * restart time
     */
    public long restartT;

    private pipeShell proc;

    private pipeSide pipe;

    private boolean need2run;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "process definition .*! no description",
        "process definition .*! respawn",
        "process definition .*! exec null",
        "process definition .*! args null",
        "process definition .*! time 5000",
        "process definition .*! delay 1000",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public int compare(cfgPrcss o1, cfgPrcss o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String toString() {
        return "process " + name;
    }

    /**
     * create new process
     *
     * @param nam name of process
     */
    public cfgPrcss(String nam) {
        name = nam.trim();
    }

    /**
     * restart this process
     */
    public synchronized void restartNow() {
        try {
            pipe.setClose();
        } catch (Exception e) {
        }
        try {
            proc.kill(0);
        } catch (Exception e) {
        }
    }

    /**
     * destroy this process
     */
    public synchronized void stopNow() {
        need2run = false;
        restartNow();
    }

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        l.add("1 2,. description        description of this process");
        l.add("2 2,.   [text]           text describing this process");
        l.add("1 .  respawn             restart on termination");
        l.add("1 2  rename              rename this process");
        l.add("2 .    <name>            set new name of process");
        l.add("1 2  exec                set external binary to use");
        l.add("2 2,.  <name>            name of image");
        l.add("1 2  args                set arguments to use");
        l.add("2 2,.  <name>            name of image");
        l.add("1 2  time                specify time between runs");
        l.add("2 .    <num>             milliseconds between runs");
        l.add("1 2  delay               specify initial delay");
        l.add("2 .    <num>             milliseconds between start");
        l.add("1 .  stop                stop working");
        l.add("1 .  start               start working");
        l.add("1 .  runnow              run one round now");
        return l;
    }

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("process definition " + name);
        cmds.cfgLine(l, description.length() < 1, cmds.tabulator, "description", description);
        cmds.cfgLine(l, !respawn, cmds.tabulator, "respawn", "");
        l.add(cmds.tabulator + "exec " + execName);
        l.add(cmds.tabulator + "args " + execArgs);
        l.add(cmds.tabulator + "delay " + initial);
        l.add(cmds.tabulator + "time " + interval);
        if (need2run) {
            l.add(cmds.tabulator + "start");
        } else {
            l.add(cmds.tabulator + "stop");
        }
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if (!filter) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("rename")) {
            a = cmd.word();
            cfgPrcss v = cfgAll.prcFind(a, false);
            if (v != null) {
                cmd.error("process already exists");
                return;
            }
            name = a;
            return;
        }
        if (a.equals("respawn")) {
            respawn = true;
            return;
        }
        if (a.equals("description")) {
            description = cmd.getRemaining();
            return;
        }
        if (a.equals("exec")) {
            execName = cmd.getRemaining();
            return;
        }
        if (a.equals("args")) {
            execArgs = cmd.getRemaining();
            return;
        }
        if (a.equals("delay")) {
            initial = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("time")) {
            interval = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("stop")) {
            stopNow();
            return;
        }
        if (a.equals("start")) {
            if (need2run) {
                return;
            }
            need2run = true;
            new Thread(this).start();
            return;
        }
        if (a.equals("runnow")) {
            doRound();
            return;
        }
        if (!a.equals("no")) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        if (a.equals("start")) {
            stopNow();
            return;
        }
        if (a.equals("respawn")) {
            respawn = false;
            return;
        }
        if (a.equals("description")) {
            description = "";
            return;
        }
        if (a.equals("exec")) {
            execName = null;
            return;
        }
        if (a.equals("args")) {
            execArgs = null;
            return;
        }
        cmd.badCmd();
    }

    public String getPrompt() {
        return "prc";
    }

    public void run() {
        bits.sleep(initial);
        for (;;) {
            try {
                if (respawn) {
                    logger.info("restarting process " + name);
                    restartT = bits.getTime();
                    doRound();
                    restartC++;
                }
                if (!need2run) {
                    break;
                }
                bits.sleep(interval);
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
        logger.info("stopped process " + name);
    }

    private void doRound() {
        pipeLine pl = new pipeLine(65536, false);
        pipe = pl.getSide();
        String a = "" + execName;
        if (execArgs != null) {
            a += " " + execArgs;
        }
        proc = pipeShell.exec(pl.getSide(), a, null, true, true);
        if (proc == null) {
            return;
        }
        for (;;) {
            if (!proc.isRunning()) {
                break;
            }
            if (con == null) {
                pipeDiscard.flush(pipe);
                bits.sleep(1000);
                continue;
            }
            boolean b = pipeConnect.redirect(pipe, con);
            b |= pipeConnect.redirect(con, pipe);
            if (b) {
                con.setClose();
                con = null;
            }
            bits.sleep(100);
        }
    }

}
