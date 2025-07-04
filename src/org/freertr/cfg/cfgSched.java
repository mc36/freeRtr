package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.auth.authLocal;
import org.freertr.pipe.pipeConnect;
import org.freertr.pipe.pipeDiscard;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSetting;
import org.freertr.pipe.pipeSide;
import org.freertr.tab.tabGen;
import org.freertr.user.userExec;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.user.userRead;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logBuf;
import org.freertr.util.logger;
import org.freertr.util.notifier;

/**
 * scheduler configuration
 *
 * @author matecsaba
 */
public class cfgSched implements Comparable<cfgSched>, cfgGeneric {

    /**
     * create instance
     *
     * @param s name
     */
    public cfgSched(String s) {
        name = s;
    }

    /**
     * name of scheduler
     */
    public String name;

    /**
     * description of this scheduler
     */
    public String description = "";

    /**
     * respawn on termination
     */
    public boolean respawn = true;

    /**
     * time between runs
     */
    public int interval;

    /**
     * initial delay
     */
    public int initial;

    /**
     * random time between runs
     */
    public int randInt;

    /**
     * random initial delay
     */
    public int randIni;

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
     * time range when allowed
     */
    public cfgTime time;

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

    private pipeSide loc;

    /**
     * status, false=stopped, true=running
     */
    protected boolean working = false;

    /**
     * notifier in use
     */
    protected final notifier notif = new notifier();

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("scheduler .*", cmds.tabulator + cmds.negated + cmds.tabulator + "description", null),
        new userFilter("scheduler .*", cmds.tabulator + "respawn", null),
        new userFilter("scheduler .*", cmds.tabulator + "time 0", null),
        new userFilter("scheduler .*", cmds.tabulator + "delay 0", null),
        new userFilter("scheduler .*", cmds.tabulator + "random-time 0", null),
        new userFilter("scheduler .*", cmds.tabulator + "random-delay 0", null),
        new userFilter("scheduler .*", cmds.tabulator + "command exit", null),
        new userFilter("scheduler .*", cmds.tabulator + cmds.negated + cmds.tabulator + "hidden", null),
        new userFilter("scheduler .*", cmds.tabulator + cmds.negated + cmds.tabulator + "log-actions", null),
        new userFilter("scheduler .*", cmds.tabulator + cmds.negated + cmds.tabulator + "log-console", null),
        new userFilter("scheduler .*", cmds.tabulator + cmds.negated + cmds.tabulator + "log-collect", null),
        new userFilter("scheduler .*", cmds.tabulator + cmds.negated + cmds.tabulator + "range", null)
    };

    public int compareTo(cfgSched o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
    }

    public String toString() {
        return "scheduler " + name;
    }

    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2, -1}, "description", "description of this scheduler");
        l.add(null, false, 2, new int[]{2, -1}, "[text]", "text describing this scheduler");
        l.add(null, false, 1, new int[]{2}, "rename", "rename this scheduler");
        l.add(null, false, 2, new int[]{-1}, "<str>", "set new name");
        l.add(null, false, 1, new int[]{-1}, "respawn", "restart on termination");
        l.add(null, false, 1, new int[]{2}, "command", "specify command to run");
        l.add(null, false, 2, new int[]{2, -1}, "<cmd>", "exec command to run");
        l.add(null, false, 1, new int[]{2}, "time", "specify time between runs");
        l.add(null, false, 2, new int[]{-1}, "<num>", "milliseconds between runs");
        l.add(null, false, 1, new int[]{2}, "delay", "specify initial delay");
        l.add(null, false, 2, new int[]{-1}, "<num>", "milliseconds before start");
        l.add(null, false, 1, new int[]{2}, "random-time", "specify random time between runs");
        l.add(null, false, 2, new int[]{-1}, "<num>", "milliseconds between runs");
        l.add(null, false, 1, new int[]{2}, "random-delay", "specify random initial delay");
        l.add(null, false, 2, new int[]{-1}, "<num>", "milliseconds before start");
        l.add(null, false, 1, new int[]{2}, "range", "specify time range");
        l.add(null, false, 2, new int[]{-1}, "<name:tm>", "name of time map");
        l.add(null, false, 1, new int[]{-1}, "log-actions", "log actions");
        l.add(null, false, 1, new int[]{-1}, "log-console", "log console activity");
        l.add(null, false, 1, new int[]{2}, "log-collect", "collect console activity");
        l.add(null, false, 2, new int[]{-1}, "<num>", "lines to store");
        l.add(null, false, 1, new int[]{-1}, "stop", "stop working");
        l.add(null, false, 1, new int[]{-1}, "start", "start working");
        l.add(null, false, 1, new int[]{-1}, "runnow", "run one round now");
        l.add(null, false, 1, new int[]{-1}, "hidden", "hide command");
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("scheduler " + name);
        cmds.cfgLine(l, description.length() < 1, cmds.tabulator, "description", description);
        cmds.cfgLine(l, !hidden, cmds.tabulator, "hidden", "");
        cmds.cfgLine(l, !respawn, cmds.tabulator, "respawn", "");
        l.add(cmds.tabulator + "time " + interval);
        l.add(cmds.tabulator + "delay " + initial);
        l.add(cmds.tabulator + "random-time " + randInt);
        l.add(cmds.tabulator + "random-delay " + randIni);
        if (hidden) {
            l.add(cmds.tabulator + "command " + authLocal.passwdEncode(command, (filter & 2) != 0));
        } else {
            l.add(cmds.tabulator + "command " + command);
        }
        cmds.cfgLine(l, time == null, cmds.tabulator, "range", "" + time);
        cmds.cfgLine(l, !logAct, cmds.tabulator, "log-actions", "");
        cmds.cfgLine(l, !logCon, cmds.tabulator, "log-console", "");
        cmds.cfgLine(l, logCol == null, cmds.tabulator, "log-collect", "" + logBuf.getSize(logCol));
        if (working) {
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
        if (a.equals("description")) {
            description = cmd.getRemaining();
            return;
        }
        if (a.equals("rename")) {
            a = cmd.word();
            cfgSched v = cfgAll.schedFind(a, false);
            if (v != null) {
                cmd.error("already exists");
                return;
            }
            name = a;
            return;
        }
        if (a.equals("respawn")) {
            respawn = true;
            return;
        }
        if (a.equals("hidden")) {
            hidden = true;
            return;
        }
        if (a.equals("time")) {
            interval = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("delay")) {
            initial = bits.str2num(cmd.word());
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
        if (a.equals("command")) {
            command = authLocal.passwdDecode(cmd.getRemaining());
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
        if (a.equals("range")) {
            time = cfgAll.timeFind(cmd.word(), false);
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
        if (a.equals("description")) {
            description = "";
            return;
        }
        if (a.equals("time")) {
            interval = 0;
            return;
        }
        if (a.equals("delay")) {
            initial = 0;
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
        if (a.equals("hidden")) {
            hidden = false;
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
        return "sched";
    }

    /**
     * stop running
     */
    public void stopNow() {
        working = false;
        notif.wakeup();
    }

    /**
     * start running
     */
    public void startNow() {
        if (working) {
            return;
        }
        if (interval < 1) {
            return;
        }
        working = true;
        new Thread(new cfgSchedWork(this)).start();
    }

    /**
     * do one timer round
     */
    public synchronized void doRound() {
        if (cfgInit.booting) {
            return;
        }
        if (time != null) {
            if (time.matches(bits.getTime() + cfgAll.timeServerOffset)) {
                return;
            }
        }
        if (logAct) {
            logger.info("starting " + name);
        }
        restartC++;
        restartT = bits.getTime();
        pipeLine pipe = new pipeLine(32768, false);
        loc = pipe.getSide();
        new Thread(new cfgSchedRead(this)).start();
        pipeSide pip = pipe.getSide();
        userRead rdr = new userRead(pip, null);
        pip.settingsPut(pipeSetting.height, 0);
        userExec exe = new userExec(pip, rdr);
        exe.privileged = true;
        pip.setTime(120000);
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        String s = exe.repairCommand(command);
        exe.executeCommand(s);
        pipe.setClose();
        if (logAct) {
            logger.info("stopped " + name);
        }
    }

    /**
     * do read rounds
     */
    protected void doReader() {
        pipeSide pipe = loc;
        for (;;) {
            if (pipe.isClosed() != 0) {
                break;
            }
            if (con == null) {
                pipeDiscard.logLines("scheduler " + name + " said ", pipe, logCon, logCol);
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
        return name + "|" + restartC + "|" + bits.timePast(restartT) + "|" + bits.time2str(cfgAll.timeZoneName, restartT + cfgAll.timeServerOffset, 3);
    }

}

class cfgSchedRead implements Runnable {

    private cfgSched lower;

    public cfgSchedRead(cfgSched parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.doReader();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class cfgSchedWork implements Runnable {

    private cfgSched lower;

    public cfgSchedWork(cfgSched parent) {
        lower = parent;
    }

    public void run() {
        int del = lower.initial;
        if (lower.randIni > 0) {
            del += bits.random(1, lower.randIni);
        }
        if (del > 0) {
            lower.notif.sleep(del);
        }
        if (!lower.respawn) {
            lower.doRound();
            return;
        }
        for (;;) {
            if (!lower.working) {
                break;
            }
            try {
                lower.doRound();
            } catch (Exception e) {
                logger.traceback(e);
            }
            del = lower.interval;
            if (lower.randInt > 0) {
                del += bits.random(1, lower.randInt);
            }
            lower.notif.sleep(del);
        }
    }

}
