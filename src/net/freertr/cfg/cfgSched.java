package net.freertr.cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import net.freertr.auth.authLocal;
import net.freertr.pipe.pipeConnect;
import net.freertr.pipe.pipeDiscard;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSetting;
import net.freertr.pipe.pipeSide;
import net.freertr.tab.tabGen;
import net.freertr.user.userExec;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.user.userReader;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logger;

/**
 * scheduler configuration
 *
 * @author matecsaba
 */
public class cfgSched implements Comparator<cfgSched>, Runnable, cfgGeneric {

    /**
     * create instance
     */
    public cfgSched() {
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
    public boolean logging = false;

    /**
     * status, false=stopped, true=running
     */
    public boolean working = false;

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

    private Timer keepTimer;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "scheduler .*! no description",
        "scheduler .*! respawn",
        "scheduler .*! time 0",
        "scheduler .*! delay 0",
        "scheduler .*! random-time 0",
        "scheduler .*! random-delay 0",
        "scheduler .*! command exit",
        "scheduler .*! no hidden",
        "scheduler .*! no log",
        "scheduler .*! no range"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public int compare(cfgSched o1, cfgSched o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String toString() {
        return "scheduler " + name;
    }

    public void getHelp(userHelping l) {
        l.add(null, "1  2,.    description                description of this scheduler");
        l.add(null, "2  2,.      [text]                   text describing this scheduler");
        l.add(null, "1  .      respawn                    restart on termination");
        l.add(null, "1  2      command                    specify command to run");
        l.add(null, "2  2,.      <cmd>                    exec command to run");
        l.add(null, "1  2      time                       specify time between runs");
        l.add(null, "2  .        <num>                    milliseconds between runs");
        l.add(null, "1  2      delay                      specify initial delay");
        l.add(null, "2  .        <num>                    milliseconds before start");
        l.add(null, "1  2      random-time                specify random time between runs");
        l.add(null, "2  .        <num>                    milliseconds between runs");
        l.add(null, "1  2      random-delay               specify random initial delay");
        l.add(null, "2  .        <num>                    milliseconds before start");
        l.add(null, "1  2      range                      specify time range");
        l.add(null, "2  .        <name>                   name of time map");
        l.add(null, "1  .      log                        log actions");
        l.add(null, "1  .      stop                       stop working");
        l.add(null, "1  .      start                      start working");
        l.add(null, "1  .      runnow                     run one round now");
        l.add(null, "1  .      hidden                     hide command");
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
        cmds.cfgLine(l, !logging, cmds.tabulator, "log", "");
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
        if (a.equals("log")) {
            logging = true;
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
        if (!a.equals("no")) {
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
        if (a.equals("log")) {
            logging = false;
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
    public void startNow() {
        if (working) {
            return;
        }
        if (interval < 1) {
            return;
        }
        working = true;
        keepTimer = new Timer();
        cfgSchedTimer task = new cfgSchedTimer(this);
        int del = initial;
        if (randIni > 0) {
            del += bits.random(1, randIni);
        }
        if (respawn) {
            keepTimer.schedule(task, del, interval);
        } else {
            keepTimer.schedule(task, del);
        }
    }

    /**
     * do one timer round
     */
    public synchronized void doRound() {
        if (time != null) {
            if (time.matches(bits.getTime() + cfgAll.timeServerOffset)) {
                return;
            }
        }
        if (logging) {
            logger.info("starting " + name);
        }
        if (randInt > 0) {
            bits.sleep(bits.random(1, randInt));
        }
        restartC++;
        restartT = bits.getTime();
        pipeLine pipe = new pipeLine(32768, false);
        loc = pipe.getSide();
        new Thread(this).start();
        pipeSide pip = pipe.getSide();
        userReader rdr = new userReader(pip, null);
        pip.settingsPut(pipeSetting.height, 0);
        userExec exe = new userExec(pip, rdr);
        exe.privileged = true;
        pip.setTime(120000);
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        String s = exe.repairCommand(command);
        exe.executeCommand(s);
        pipe.setClose();
        if (logging) {
            logger.info("stopped " + name);
        }
    }

    private void doReader(pipeSide pipe) {
        for (;;) {
            if (pipe.isClosed() != 0) {
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

    public void run() {
        try {
            doReader(loc);
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class cfgSchedTimer extends TimerTask {

    private cfgSched lower;

    public cfgSchedTimer(cfgSched parent) {
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
