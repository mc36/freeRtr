package net.freertr.cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.auth.authLocal;
import net.freertr.pipe.pipeConnect;
import net.freertr.pipe.pipeDiscard;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabScrptN;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.user.userScript;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logBuf;
import net.freertr.util.logger;
import net.freertr.util.notifier;

/**
 * one script configuration
 *
 * @author matecsaba
 */
public class cfgScrpt implements Comparator<cfgScrpt>, cfgGeneric {

    /**
     * create instance
     */
    public cfgScrpt() {
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
    public final static String[] defaultL = {
        "script .*! no description",
        "script .*! respawn",
        "script .*! time 0",
        "script .*! delay 0",
        "script .*! random-time 0",
        "script .*! random-delay 0",
        "script .*! no hidden",
        "script .*! no log-actions",
        "script .*! no log-console",
        "script .*! no log-collect",
        "script .*! no range"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public int compare(cfgScrpt o1, cfgScrpt o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String toString() {
        return "script " + name;
    }

    public void getHelp(userHelping l) {
        l.add(null, "1  2,.    description                description of this script");
        l.add(null, "2  2,.      [text]                   text describing this script");
        l.add(null, "1  2      rename                     rename this script");
        l.add(null, "2  .        <str>                    set new name");
        l.add(null, "1  .      respawn                    restart on termination");
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
        l.add(null, "1  2      sequence                   sequence number of an entry");
        l.add(null, "2  3,.      <num>                    sequence number");
        l.add(null, "3  3,.        <str>                  tcl commands");
        l.add(null, "1  2,.    reindex                    reindex route map");
        l.add(null, "2  3,.      [num]                    initial number to start with");
        l.add(null, "3  4,.        [num]                  increment number");
        l.add(null, "1  .      stop                       stop working");
        l.add(null, "1  .      start                      start working");
        l.add(null, "1  .      hidden                     hide command");
        l.add(null, "1  .      runnow                     run one round now");
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
        if (!a.equals("no")) {
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
