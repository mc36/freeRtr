package org.freertr.cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import org.freertr.pipe.pipeConnect;
import org.freertr.pipe.pipeDiscard;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeShell;
import org.freertr.pipe.pipeSide;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelping;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logBuf;
import org.freertr.util.logger;

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
     * hidden process
     */
    protected boolean hidden = false;

    /**
     * respawn on termination
     */
    public boolean respawn = true;

    /**
     * kill children on termination
     */
    public boolean children = true;

    /**
     * action logging
     */
    public boolean logAct = false;

    /**
     * console logging
     */
    public boolean logCon = false;

    /**
     * console collector
     */
    public logBuf logCol;

    /**
     * execute this binary
     */
    public String execName = null;

    /**
     * final long parameter
     */
    public String execFinal = null;

    /**
     * user to use
     */
    public String userValue = null;

    /**
     * cpu pinning
     */
    public String cpuPinning = null;

    /**
     * time range when allowed
     */
    public cfgTime time;

    /**
     * time between runs
     */
    protected int interval = 1000;

    /**
     * initial delay
     */
    protected int initial = 1000;

    /**
     * random time between runs
     */
    public int randInt;

    /**
     * random initial delay
     */
    public int randIni;

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
    public final static String[] defaultL = {
        "process definition .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "description",
        "process definition .*!" + cmds.tabulator + "respawn",
        "process definition .*!" + cmds.tabulator + "children",
        "process definition .*!" + cmds.tabulator + "pinning null",
        "process definition .*!" + cmds.tabulator + "user null",
        "process definition .*!" + cmds.tabulator + "exec null",
        "process definition .*!" + cmds.tabulator + "final null",
        "process definition .*!" + cmds.tabulator + "time 1000",
        "process definition .*!" + cmds.tabulator + "delay 1000",
        "process definition .*!" + cmds.tabulator + "random-time 0",
        "process definition .*!" + cmds.tabulator + "random-delay 0",
        "process definition .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "log-actions",
        "process definition .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "log-console",
        "process definition .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "log-collect",
        "process definition .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "range"
    };

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
    public void restartNow() {
        try {
            proc.kill();
        } catch (Exception e) {
        }
        try {
            pipe.setClose();
        } catch (Exception e) {
        }
    }

    /**
     * destroy this process
     */
    public void stopNow() {
        need2run = false;
        restartNow();
    }

    /**
     * start this process
     */
    public void startNow() {
        if (need2run) {
            return;
        }
        need2run = true;
        new Thread(this).start();
    }

    public void getHelp(userHelping l) {
        l.add(null, "1  2,.    description                description of this process");
        l.add(null, "2  2,.      [text]                   text describing this process");
        l.add(null, "1  .      respawn                    restart on termination");
        l.add(null, "1  .      children                   kill children on termination");
        l.add(null, "1  2      rename                     rename this process");
        l.add(null, "2  .        <str>                    set new name of process");
        l.add(null, "1  2      exec                       set external binary to use");
        l.add(null, "2  2,.      <str>                    name of image");
        l.add(null, "1  2      program                    set external binary to use");
        l.add(null, "2  .        <str>                    name of image");
        l.add(null, "1  2      parameter                  set parameters to use on the binary");
        l.add(null, "2  2,.      <str>                    name of image");
        l.add(null, "1  2      final                      set final long parameter");
        l.add(null, "2  2,.      <str>                    parameter");
        l.add(null, "1  2      user                       set user to use");
        l.add(null, "2  .        <str>                    user value");
        l.add(null, "1  2      pinning                    set pinning mask");
        l.add(null, "2  .        <str>                    cpu mask in hex");
        l.add(null, "1  2      time                       specify time between runs");
        l.add(null, "2  .        <num>                    milliseconds between runs");
        l.add(null, "1  2      delay                      specify initial delay");
        l.add(null, "2  .        <num>                    milliseconds before start");
        l.add(null, "1  2      random-time                specify random time between runs");
        l.add(null, "2  .        <num>                    milliseconds between runs");
        l.add(null, "1  2      random-delay               specify random initial delay");
        l.add(null, "2  .        <num>                    milliseconds before start");
        l.add(null, "1  2      range                      specify time range");
        l.add(null, "2  .        <name:tm>                name of time map");
        l.add(null, "1  .      log-actions                log actions");
        l.add(null, "1  .      log-console                log console activity");
        l.add(null, "1  2      log-collect                collect console activity");
        l.add(null, "2  .        <num>                    lines to store");
        l.add(null, "1  .      stop                       stop working");
        l.add(null, "1  .      start                      start working");
        l.add(null, "1  .      runnow                     run one round now");
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        if (hidden) {
            return l;
        }
        l.add("process definition " + name);
        cmds.cfgLine(l, description.length() < 1, cmds.tabulator, "description", description);
        cmds.cfgLine(l, !respawn, cmds.tabulator, "respawn", "");
        cmds.cfgLine(l, !children, cmds.tabulator, "children", "");
        l.add(cmds.tabulator + "exec " + execName);
        l.add(cmds.tabulator + "final " + execFinal);
        l.add(cmds.tabulator + "user " + userValue);
        l.add(cmds.tabulator + "pinning " + cpuPinning);
        l.add(cmds.tabulator + "delay " + initial);
        l.add(cmds.tabulator + "time " + interval);
        l.add(cmds.tabulator + "random-time " + randInt);
        l.add(cmds.tabulator + "random-delay " + randIni);
        cmds.cfgLine(l, time == null, cmds.tabulator, "range", "" + time);
        cmds.cfgLine(l, !logAct, cmds.tabulator, "log-actions", "");
        cmds.cfgLine(l, !logCon, cmds.tabulator, "log-console", "");
        cmds.cfgLine(l, logCol == null, cmds.tabulator, "log-collect", "" + logBuf.getSize(logCol));
        if (need2run) {
            l.add(cmds.tabulator + "start");
        } else {
            l.add(cmds.tabulator + "stop");
        }
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
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
        if (a.equals("children")) {
            children = true;
            return;
        }
        if (a.equals("description")) {
            description = cmd.getRemaining();
            return;
        }
        if (a.equals("parameter")) {
            if (execName == null) {
                cmd.error("no exec specified");
                return;
            }
            a = cmd.getRemaining();
            cmd = new cmds("old", execName);
            execName = (cmd.word() + " " + a).trim();
            return;
        }
        if (a.equals("program")) {
            if (execName == null) {
                cmd.error("no exec specified");
                return;
            }
            a = cmd.word();
            cmd = new cmds("old", execName);
            cmd.word();
            execName = (a + " " + cmd.getRemaining()).trim();
            return;
        }
        if (a.equals("exec")) {
            execName = cmd.getRemaining();
            return;
        }
        if (a.equals("final")) {
            execFinal = cmd.getRemaining();
            return;
        }
        if (a.equals("user")) {
            userValue = cmd.word();
            return;
        }
        if (a.equals("pinning")) {
            cpuPinning = cmd.word();
            return;
        }
        if (a.equals("range")) {
            time = cfgAll.timeFind(cmd.word(), false);
            return;
        }
        if (a.equals("random-time")) {
            randInt = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("random-delay")) {
            randIni = bits.str2num(cmd.word());
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
        if (a.equals("log-actions")) {
            logAct = true;
            return;
        }
        if (a.equals("log-collect")) {
            logCol = new logBuf(bits.str2num(cmd.word()));
            return;
        }
        if (a.equals("log-console")) {
            logCon = true;
            return;
        }
        if (a.equals("stop")) {
            stopNow();
            return;
        }
        if (a.equals("start")) {
            startNow();
            return;
        }
        if (a.equals("runnow")) {
            doRound();
            return;
        }
        if (!a.equals(cmds.negated)) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        if (a.equals("start")) {
            stopNow();
            return;
        }
        if (a.equals("log-actions")) {
            logAct = false;
            return;
        }
        if (a.equals("log-collect")) {
            logCol = null;
            return;
        }
        if (a.equals("log-console")) {
            logCon = false;
            return;
        }
        if (a.equals("range")) {
            time = null;
            return;
        }
        if (a.equals("random-time")) {
            randInt = 0;
            return;
        }
        if (a.equals("random-delay")) {
            randIni = 0;
            return;
        }
        if (a.equals("respawn")) {
            respawn = false;
            return;
        }
        if (a.equals("children")) {
            children = false;
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
        if (a.equals("final")) {
            execFinal = null;
            return;
        }
        if (a.equals("user")) {
            userValue = null;
            return;
        }
        if (a.equals("pinning")) {
            cpuPinning = null;
            return;
        }
        if (a.equals("stop")) {
            startNow();
            return;
        }
        if (a.equals("start")) {
            stopNow();
            return;
        }
        cmd.badCmd();
    }

    public String getPrompt() {
        return "prc";
    }

    public void run() {
        for (;;) {
            if (!cfgInit.booting) {
                break;
            }
            bits.sleep(1000);
        }
        int del = initial;
        if (randIni > 0) {
            del += bits.random(1, randIni);
        }
        if (del > 0) {
            bits.sleep(del);
        }
        try {
            doRound();
        } catch (Exception e) {
            logger.traceback(e);
        }
        for (;;) {
            bits.sleep(interval);
            if (!need2run) {
                break;
            }
            if (!respawn) {
                continue;
            }
            try {
                doRound();
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
        if (logAct) {
            logger.info("stopped process " + name);
        }
    }

    private synchronized void doRound() {
        if (time != null) {
            if (time.matches(bits.getTime() + cfgAll.timeServerOffset)) {
                return;
            }
        }
        if (execName == null) {
            return;
        }
        if (logAct) {
            logger.info("restarting process " + name);
        }
        if (randInt > 0) {
            bits.sleep(bits.random(1, randInt));
        }
        restartT = bits.getTime();
        restartC++;
        pipeLine pl = new pipeLine(65536, false);
        pipe = pl.getSide();
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        String cmd = execName;
        if (cpuPinning != null) {
            cmd = "taskset " + cpuPinning + " " + cmd;
        }
        if (userValue != null) {
            cmd = "sudo -u " + userValue + " " + cmd;
        }
        proc = pipeShell.exec(pl.getSide(), cmd, execFinal, true, true, false, children);
        if (proc == null) {
            return;
        }
        for (;;) {
            if (!proc.isRunning()) {
                break;
            }
            if (con == null) {
                pipeDiscard.logLines("process " + name + " said ", pipe, logCon, logCol);
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

    /**
     * get info
     *
     * @return info
     */
    public String getShow() {
        return name + "|" + restartC + "|" + pipeShell.info(proc) + "|" + bits.timePast(restartT) + "|" + bits.time2str(cfgAll.timeZoneName, restartT + cfgAll.timeServerOffset, 3);
    }

}
