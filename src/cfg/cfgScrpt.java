package cfg;

import addr.addrIP;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
import pipe.pipeConnect;
import pipe.pipeDiscard;
import pipe.pipeLine;
import pipe.pipeSide;
import tab.tabGen;
import tab.tabListing;
import tab.tabScrptN;
import user.userEditor;
import user.userFilter;
import user.userHelping;
import user.userScreen;
import user.userScript;
import util.bits;
import util.cmds;
import util.logger;

/**
 * one script configuration
 *
 * @author matecsaba
 */
public class cfgScrpt implements Comparator<cfgScrpt>, Runnable, cfgGeneric {

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
     * time between runs
     */
    public int interval;

    /**
     * initial delay
     */
    public int initial;

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
    public final static String defaultL[] = {
        "script .*! no description",
        "script .*! time 0",
        "script .*! delay 0",
        "script .*! no log",
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

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        l.add("1  2,.    description                description of this script");
        l.add("2  2,.      [text]                   text describing this script");
        l.add("1  2      time                       specify time between runs");
        l.add("2  .        <num>                    milliseconds between runs");
        l.add("1  2      delay                      specify initial delay");
        l.add("2  .        <num>                    milliseconds between start");
        l.add("1  2      range                      specify time range");
        l.add("2  .        <name>                   name of time map");
        l.add("1  .      log                        log actions");
        l.add("1  2      sequence                   sequence number of an entry");
        l.add("2  3,.      <num>                    sequence number");
        l.add("3  3,.        <str>                  tcl commands");
        l.add("1  2,.    reindex                    reindex route map");
        l.add("2  3,.      [num]                    initial number to start with");
        l.add("3  4,.        [num]                  increment number");
        l.add("1  .      stop                       stop working");
        l.add("1  .      start                      start working");
        l.add("1  .      runnow                     run one round now");
        l.add("1  .      editor                     edit this script");
        return l;
    }

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("script " + name);
        cmds.cfgLine(l, description.length() < 1, cmds.tabulator, "description", description);
        l.add(cmds.tabulator + "time " + interval);
        l.add(cmds.tabulator + "delay " + initial);
        cmds.cfgLine(l, time == null, cmds.tabulator, "range", "" + time);
        cmds.cfgLine(l, !logging, cmds.tabulator, "log", "");
        l.addAll(script.dump(cmds.tabulator));
        if (working) {
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
        if (a.equals("description")) {
            description = cmd.getRemaining();
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
        if (a.equals("sequence")) {
            tabScrptN ntry = new tabScrptN();
            ntry.sequence = bits.str2num(cmd.word());
            ntry.lin = cmd.getRemaining();
            script.del(ntry);
            script.add(ntry);
            return;
        }
        if (a.equals("reindex")) {
            int i = bits.str2num(cmd.word());
            script.reindex(i, bits.str2num(cmd.word()));
            return;
        }
        if (a.equals("editor")) {
            List<String> txt = getText();
            userEditor e = new userEditor(new userScreen(cmd.pipe, 80, 25), txt, "script", false);
            if (e.doEdit()) {
                return;
            }
            script.clear();
            for (int i = 0; i < txt.size(); i++) {
                tabScrptN ntry = new tabScrptN();
                ntry.sequence = (i + 1) * 10;
                ntry.lin = txt.get(i);
                script.add(ntry);
            }
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
        if (a.equals("log")) {
            logging = false;
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

    private List<String> getText() {
        List<String> l = new ArrayList<String>();
        for (int i = 0; i < script.size(); i++) {
            l.add(script.get(i).lin);
        }
        return l;
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
        cfgScrptTimer task = new cfgScrptTimer(this);
        keepTimer.schedule(task, initial, interval);
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
        restartC++;
        restartT = bits.getTime();
        pipeLine pl = new pipeLine(32768, false);
        loc = pl.getSide();
        new Thread(this).start();
        pipeSide pip = pl.getSide();
        pip.timeout = 120000;
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        userScript s = new userScript(pip, "");
        s.allowExec = true;
        s.allowConfig = true;
        s.addLines(getText());
        s.cmdAll();
        pl.setClose();
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

class cfgScrptTimer extends TimerTask {

    private cfgScrpt lower;

    public cfgScrptTimer(cfgScrpt parent) {
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
