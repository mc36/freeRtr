package cfg;

import auth.authLocal;
import clnt.clntSched;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * scheduler configuration
 *
 * @author matecsaba
 */
public class cfgSched implements Comparator<cfgSched>, cfgGeneric {

    /**
     * name of scheduler
     */
    public String name;

    /**
     * worker
     */
    public final clntSched worker = new clntSched();

    /**
     * time range
     */
    public cfgTime timerange;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "scheduler .*! time 0",
        "scheduler .*! delay 0",
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

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        l.add("1  2      command                    specify command to run");
        l.add("2  2,.      <cmd>                    exec command to run");
        l.add("1  2      time                       specify time between runs");
        l.add("2  .        <num>                    milliseconds between runs");
        l.add("1  2      delay                      specify initial delay");
        l.add("2  .        <num>                    milliseconds between start");
        l.add("1  2      range                      specify time range");
        l.add("2  .        <name>                   name of time map");
        l.add("1  .      log                        log actions");
        l.add("1  .      stop                       stop working");
        l.add("1  .      start                      start working");
        l.add("1  .      runnow                     run one round now");
        l.add("1  .      hidden                     hide command");
        return l;
    }

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("scheduler " + name);
        cmds.cfgLine(l, !worker.hidden, cmds.tabulator, "hidden", "");
        l.add(cmds.tabulator + "time " + worker.interval);
        l.add(cmds.tabulator + "delay " + worker.initial);
        if (worker.hidden) {
            l.add(cmds.tabulator + "command " + authLocal.passwdEncode(worker.command));
        } else {
            l.add(cmds.tabulator + "command " + worker.command);
        }
        cmds.cfgLine(l, worker.time == null, cmds.tabulator, "range", "" + worker.time);
        cmds.cfgLine(l, !worker.logging, cmds.tabulator, "log", "");
        if (worker.working) {
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
        if (a.equals("hidden")) {
            worker.hidden = true;
            return;
        }
        if (a.equals("time")) {
            worker.interval = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("delay")) {
            worker.initial = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("command")) {
            worker.command = authLocal.passwdDecode(cmd.getRemaining());
            return;
        }
        if (a.equals("log")) {
            worker.logging = true;
            return;
        }
        if (a.equals("stop")) {
            worker.stopNow();
            return;
        }
        if (a.equals("start")) {
            worker.startNow();
            return;
        }
        if (a.equals("range")) {
            worker.time = cfgAll.timeFind(cmd.word(), false);
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
        if (a.equals("hidden")) {
            worker.hidden = false;
            return;
        }
        if (a.equals("log")) {
            worker.logging = false;
            return;
        }
        if (a.equals("range")) {
            worker.time = null;
            return;
        }
        cmd.badCmd();
    }

    public String getPrompt() {
        return "sched";
    }

    /**
     * do one round
     */
    public void doRound() {
        worker.doRound();
    }

}
