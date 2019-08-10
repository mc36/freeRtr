package cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import line.lineRunner;
import line.lineThread;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import user.userLine;
import util.cmds;

/**
 * one line configuration
 *
 * @author matecsaba
 */
public class cfgLin implements Comparator<cfgLin>, cfgGeneric {

    /**
     * name of this line
     */
    public String name = "";

    /**
     * byte thread
     */
    public lineThread thread;

    /**
     * runner thread
     */
    public lineRunner runner;

    /**
     * line handler
     */
    public userLine line = new userLine();

    /**
     * line defaults text
     */
    public final static String linedefL[] = {
        ".*! no exec interface",
        ".*! exec timeout 300000",
        ".*! exec width 79",
        ".*! exec height 24",
        ".*! no exec timestamp",
        ".*! no exec colorized",
        ".*! exec tablemode normal",
        ".*! exec welcome welcome",
        ".*! exec ready line ready",
        ".*! exec bye see you later",
        ".*! no exec logging",
        ".*! exec privilege 15",
        ".*! exec autocommand ",
        ".*! no exec autohangup",
        ".*! login timeout 60000",
        ".*! login retry 3",
        ".*! login delay 3000",
        ".*! login user username:",
        ".*! login pass password:",
        ".*! login fail authentication failed",
        ".*! login activate 13",
        ".*! login deactivate 255",
        ".*! no login logging"
    };

    /**
     * line defaults filter
     */
    public static tabGen<userFilter> linedefF;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "line .*! no script init",
        "line .*! no script activate",
        "line .*! no script hangup",
        "line .*! no dedicated",
        "line .*! no disabled",
        "line .*! log-monitor"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public int compare(cfgLin o1, cfgLin o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String toString() {
        return "lin " + name;
    }

    /**
     * start this line
     */
    public void setup2run() {
        runner = new lineRunner(thread, line, name);
    }

    /**
     * send one line
     *
     * @param s string to send
     */
    public void sendLine(String s) {
        runner.sendLine(s);
    }

    /**
     * create new interface
     *
     * @param nam name of interface
     */
    public cfgLin(String nam) {
        name = nam.trim();
    }

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("line " + name);
        line.getShRun(cmds.tabulator, l);
        if (runner.scrptInit == null) {
            l.add(cmds.tabulator + "no script init");
        } else {
            l.add(cmds.tabulator + "script init " + runner.scrptInit.scrName);
        }
        if (runner.scrptActv == null) {
            l.add(cmds.tabulator + "no script activate");
        } else {
            l.add(cmds.tabulator + "script activate " + runner.scrptActv.scrName);
        }
        if (runner.scrptHangup == null) {
            l.add(cmds.tabulator + "no script hangup");
        } else {
            l.add(cmds.tabulator + "script hangup " + runner.scrptHangup.scrName);
        }
        cmds.cfgLine(l, !runner.getDedi(), cmds.tabulator, "dedicated", "");
        cmds.cfgLine(l, !runner.getDisa(), cmds.tabulator, "disabled", "");
        cmds.cfgLine(l, !runner.getMon(), cmds.tabulator, "log-monitor", "");
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if (!filter) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        line.getHelp(l);
        l.add("1 .  log-monitor                    set as monitoring line");
        l.add("1 .  dedicated                      set as dedicated line");
        l.add("1 .  disabled                       set as disabled line");
        l.add("1 2  script                         set scripts to use");
        l.add("2 3    init                         script to run on before activation");
        l.add("3 .      <name>                     name of script");
        l.add("2 3    activate                     script to run on after activation");
        l.add("3 .      <name>                     name of script");
        l.add("2 3    hangup                       script to run on after session");
        l.add("3 .      <name>                     name of script");
        return l;
    }

    public synchronized void doCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("dedicated")) {
            runner.setDedi(true);
            return;
        }
        if (s.equals("disabled")) {
            runner.setDisa(true);
            return;
        }
        if (s.equals("log-monitor")) {
            runner.setMon(true);
            return;
        }
        if (s.equals("script")) {
            s = cmd.word();
            if (s.equals("init")) {
                cfgChat scrpt = cfgAll.chatFind(cmd.word(), false);
                if (scrpt == null) {
                    cmd.error("no such chat script");
                    return;
                }
                runner.scrptInit = scrpt.script;
                return;
            }
            if (s.equals("activate")) {
                cfgChat scrpt = cfgAll.chatFind(cmd.word(), false);
                if (scrpt == null) {
                    cmd.error("no such chat script");
                    return;
                }
                runner.scrptActv = scrpt.script;
                return;
            }
            if (s.equals("hangup")) {
                cfgChat scrpt = cfgAll.chatFind(cmd.word(), false);
                if (scrpt == null) {
                    cmd.error("no such chat script");
                    return;
                }
                runner.scrptHangup = scrpt.script;
                return;
            }
        }
        if (s.equals("no")) {
            s = cmd.word();
            if (s.equals("dedicated")) {
                runner.setDedi(false);
                return;
            }
            if (s.equals("disabled")) {
                runner.setDisa(false);
                return;
            }
            if (s.equals("log-monitor")) {
                runner.setMon(false);
                return;
            }
            if (s.equals("script")) {
                s = cmd.word();
                if (s.equals("init")) {
                    runner.scrptInit = null;
                    return;
                }
                if (s.equals("activate")) {
                    runner.scrptActv = null;
                    return;
                }
                if (s.equals("hangup")) {
                    runner.scrptHangup = null;
                    return;
                }
            }
        }
        if (line.doCfgStr(cmd.copyBytes(true))) {
            cmd.badCmd();
            return;
        }
    }

    public String getPrompt() {
        return "line";
    }

}
