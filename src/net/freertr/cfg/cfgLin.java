package net.freertr.cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.line.lineRunner;
import net.freertr.line.lineThread;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.user.userLine;
import net.freertr.util.cmds;

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
    public final static String[] linedefL = {
        ".*! no exec interface",
        ".*! exec timeout 300000",
        ".*! exec width 79",
        ".*! exec height 24",
        ".*! exec history 64",
        ".*! no exec timestamp",
        ".*! exec colorize normal",
        ".*! no exec spacetab",
        ".*! exec tablemode normal",
        ".*! exec welcome welcome",
        ".*! exec before before:",
        ".*! exec ready line ready",
        ".*! exec bye see you later",
        ".*! no exec logging",
        ".*! exec privilege 15",
        ".*! exec autocommand ",
        ".*! exec banner",
        ".*! no exec expirity",
        ".*! no exec monitor",
        ".*! no exec autohangup",
        ".*! login timeout 60000",
        ".*! login retry 3",
        ".*! login delay 3000",
        ".*! login user username:",
        ".*! login pass password:",
        ".*! login fail authentication failed",
        ".*! login activate 13",
        ".*! login deactivate 65536",
        ".*! login escape 3",
        ".*! no login logging",
        ".*! login last none",
    };

    /**
     * line defaults filter
     */
    public static tabGen<userFilter> linedefF;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
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

    public List<String> getShRun(int filter) {
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
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void getHelp(userHelping l) {
        line.getHelp(l);
        l.add(null, "1 .  log-monitor                    set as monitoring line");
        l.add(null, "1 .  dedicated                      set as dedicated line");
        l.add(null, "1 .  disabled                       set as disabled line");
        l.add(null, "1 2  script                         set scripts to use");
        l.add(null, "2 3    init                         script to run on before activation");
        l.add(null, "3 .      <name:scr>                 name of script");
        l.add(null, "2 3    activate                     script to run on after activation");
        l.add(null, "3 .      <name:scr>                 name of script");
        l.add(null, "2 3    hangup                       script to run on after session");
        l.add(null, "3 .      <name:scr>                 name of script");
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
