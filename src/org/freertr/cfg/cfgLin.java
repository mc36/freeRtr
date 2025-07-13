package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.line.lineRunner;
import org.freertr.line.lineThread;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.user.userLine;
import org.freertr.user.userRead;
import org.freertr.util.cmds;

/**
 * one line configuration
 *
 * @author matecsaba
 */
public class cfgLin implements Comparable<cfgLin>, cfgGeneric {

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
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("line .*", cmds.tabulator + cmds.negated + cmds.tabulator + "script init", null),
        new userFilter("line .*", cmds.tabulator + cmds.negated + cmds.tabulator + "script activate", null),
        new userFilter("line .*", cmds.tabulator + cmds.negated + cmds.tabulator + "script hangup", null),
        new userFilter("line .*", cmds.tabulator + cmds.negated + cmds.tabulator + "dedicated", null),
        new userFilter("line .*", cmds.tabulator + cmds.negated + cmds.tabulator + "disabled", null),
        new userFilter("line .*", cmds.tabulator + "log-monitor", null)
    };

    public int compareTo(cfgLin o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
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
        line.getShRun(cmds.tabulator, l, filter);
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

    public void getHelp(userHelp l) {
        line.getHelp(l);
        l.add(null, false, 1, new int[]{-1}, "log-monitor", "set as monitoring line");
        l.add(null, false, 1, new int[]{-1}, "dedicated", "set as dedicated line");
        l.add(null, false, 1, new int[]{-1}, "disabled", "set as disabled line");
        l.add(null, false, 1, new int[]{2}, "script", "set scripts to use");
        l.add(null, false, 2, new int[]{3}, "init", "script to run on before activation");
        l.add(null, false, 3, new int[]{-1}, "<name:scr>", "name of script");
        l.add(null, false, 2, new int[]{3}, "activate", "script to run on after activation");
        l.add(null, false, 3, new int[]{-1}, "<name:scr>", "name of script");
        l.add(null, false, 2, new int[]{3}, "hangup", "script to run on after session");
        l.add(null, false, 3, new int[]{-1}, "<name:scr>", "name of script");
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
        if (s.equals(cmds.negated)) {
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
