package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.auth.authLocal;
import org.freertr.pipe.pipeConnect;
import org.freertr.pipe.pipeDiscard;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabScrptN;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.user.userScript;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logBuf;
import org.freertr.util.logger;
import org.freertr.util.notifier;

/**
 * one script configuration
 *
 * @author matecsaba
 */
public class cfgScrpt implements Comparable<cfgScrpt>, cfgGeneric {

    /**
     * create instance
     *
     * @param s name
     */
    public cfgScrpt(String s) {
        name = s;
    }

    /**
     * name of this script
     */
    public String name;

    /**
     * text of this script
     */
    public tabListing<tabScrptN, addrIP> script = new tabListing<tabScrptN, addrIP>();

    /**
     * description of this script
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
        new userFilter("script .*", cmds.tabulator + cmds.negated + cmds.tabulator + "description", null),
        new userFilter("script .*", cmds.tabulator + "respawn", null),
        new userFilter("script .*", cmds.tabulator + "time 0", null),
        new userFilter("script .*", cmds.tabulator + "delay 0", null),
        new userFilter("script .*", cmds.tabulator + "random-time 0", null),
        new userFilter("script .*", cmds.tabulator + "random-delay 0", null),
        new userFilter("script .*", cmds.tabulator + cmds.negated + cmds.tabulator + "hidden", null),
        new userFilter("script .*", cmds.tabulator + cmds.negated + cmds.tabulator + "log-actions", null),
        new userFilter("script .*", cmds.tabulator + cmds.negated + cmds.tabulator + "log-console", null),
        new userFilter("script .*", cmds.tabulator + cmds.negated + cmds.tabulator + "log-collect", null),
        new userFilter("script .*", cmds.tabulator + cmds.negated + cmds.tabulator + "range", null)
    };

    public int compareTo(cfgScrpt o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
    }

    public String toString() {
        return "script " + name;
    }

    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2, -1}, "description", "description of this script");
        l.add(null, false, 2, new int[]{2, -1}, "[text]", "text describing this script");
        l.add(null, false, 1, new int[]{2}, "rename", "rename this script");
        l.add(null, false, 2, new int[]{-1}, "<str>", "set new name");
        l.add(null, false, 1, new int[]{-1}, "respawn", "restart on termination");
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
        l.add(null, false, 1, new int[]{2}, "sequence", "sequence number of an entry");
        l.add(null, false, 2, new int[]{3, -1}, "<num>", "sequence number");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "tcl commands");
        l.add(null, false, 1, new int[]{2, -1}, "reindex", "reindex route map");
        l.add(null, false, 2, new int[]{3, -1}, "[num]", "initial number to start with");
        l.add(null, false, 3, new int[]{-1}, "[num]", "increment number");
        l.add(null, false, 1, new int[]{-1}, "stop", "stop working");
        l.add(null, false, 1, new int[]{-1}, "start", "start working");
        l.add(null, false, 1, new int[]{-1}, "hidden", "hide command");
        l.add(null, false, 1, new int[]{-1}, "runnow", "run one round now");
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("script " + name);
        cmds.cfgLine(l, description.length() < 1, cmds.tabulator, "description", description);
        cmds.cfgLine(l, !hidden, cmds.tabulator, "hidden", "");
        cmds.cfgLine(l, !respawn, cmds.tabulator, "respawn", "");
        l.add(cmds.tabulator + "time " + interval);
        l.add(cmds.tabulator + "delay " + initial);
        l.add(cmds.tabulator + "random-time " + randInt);
        l.add(cmds.tabulator + "random-delay " + randIni);
        cmds.cfgLine(l, time == null, cmds.tabulator, "range", "" + time);
        cmds.cfgLine(l, !logAct, cmds.tabulator, "log-actions", "");
        cmds.cfgLine(l, !logCon, cmds.tabulator, "log-console", "");
        cmds.cfgLine(l, logCol == null, cmds.tabulator, "log-collect", "" + logBuf.getSize(logCol));
        if (hidden) {
            l.addAll(script.dump(cmds.tabulator, 0x10000 | filter));
        } else {
            l.addAll(script.dump(cmds.tabulator, filter));
        }
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
            cfgScrpt v = cfgAll.scrptFind(a, false);
            if (v != null) {
                cmd.error("already exists");
                return;
            }
            name = a;
            return;
        }
        if (a.equals("hidden")) {
            hidden = true;
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
        if (a.equals("time")) {
            interval = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("delay")) {
            initial = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("respawn")) {
            respawn = true;
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
            doRound(null);
            return;
        }
        if (a.equals("sequence")) {
            tabScrptN ntry = new tabScrptN();
            ntry.sequence = bits.str2num(cmd.word());
            ntry.lin = authLocal.passwdDecode(cmd.getRemaining());
            script.del(ntry);
            script.add(ntry);
            return;
        }
        if (a.equals("reindex")) {
            int i = bits.str2num(cmd.word());
            script.reindex(i, bits.str2num(cmd.word()));
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
        if (a.equals("hidden")) {
            hidden = false;
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
        if (a.equals("sequence")) {
            tabScrptN ntry = new tabScrptN();
            ntry.sequence = bits.str2num(cmd.word());
            script.del(ntry);
            return;
        }
        cmd.badCmd();
    }

    public String getPrompt() {
        return "scrpt";
    }

    /**
     * get script text
     *
     * @return the script
     */
    public List<String> getText() {
        List<String> l = new ArrayList<String>();
        for (int i = 0; i < script.size(); i++) {
            l.add(script.get(i).lin);
        }
        return l;
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
        new Thread(new cfgScrptWork(this)).start();
    }

    /**
     * do one timer round
     *
     * @param beg extra commands
     */
    public synchronized void doRound(List<String> beg) {
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
        pipeLine pl = new pipeLine(32768, false);
        loc = pl.getSide();
        new Thread(new cfgScrptRead(this)).start();
        pipeSide pip = pl.getSide();
        pip.setTime(120000);
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        userScript s = new userScript(pip, "");
        s.allowExec = true;
        s.allowConfig = true;
        if (beg != null) {
            s.addLines(beg);
        }
        s.addLines(getText());
        s.cmdAll();
        pl.setClose();
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
                pipeDiscard.logLines("script " + name + " said ", pipe, logCon, logCol);
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

class cfgScrptRead implements Runnable {

    private cfgScrpt lower;

    public cfgScrptRead(cfgScrpt parent) {
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

class cfgScrptWork implements Runnable {

    private cfgScrpt lower;

    public cfgScrptWork(cfgScrpt parent) {
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
            lower.doRound(null);
            return;
        }
        for (;;) {
            if (!lower.working) {
                break;
            }
            try {
                lower.doRound(null);
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
