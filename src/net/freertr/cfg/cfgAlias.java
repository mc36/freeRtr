package net.freertr.cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.auth.authLocal;
import net.freertr.tab.tabGen;
import net.freertr.user.userExec;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.user.userReader;
import net.freertr.util.cmds;

/**
 * one alias configuration
 *
 * @author matecsaba
 */
public class cfgAlias implements Comparator<cfgAlias>, cfgGeneric {

    /**
     * create instance
     */
    public cfgAlias() {
    }

    /**
     * type of alias
     */
    public aliasType type = aliasType.exec;

    /**
     * name of this alias
     */
    public String name = "";

    /**
     * command to execute
     */
    public String command = "";

    /**
     * 2nd command to execute
     */
    public String cmd2nd = null;

    /**
     * 3rd command to execute
     */
    public String cmd3rd = null;

    /**
     * 4th command to execute
     */
    public String cmd4th = null;

    /**
     * hide commands
     */
    public boolean hidden = false;

    /**
     * error free execution
     */
    public boolean errorFree = false;

    /**
     * sticky parameter
     */
    public String sticky = null;

    /**
     * default parameter
     */
    public String defParam = null;

    /**
     * help description text
     */
    public String description = null;

    /**
     * parameter existence
     */
    public paraMode parameter = paraMode.allow;

    /**
     * 2nd parameter existence
     */
    public paraMode param2nd = paraMode.allow;

    /**
     * 3rd parameter existence
     */
    public paraMode param3rd = paraMode.allow;

    /**
     * 4th parameter existence
     */
    public paraMode param4th = paraMode.allow;

    /**
     * alias types
     */
    public enum aliasType {

        /**
         * exec alias
         */
        exec,
        /**
         * show alias
         */
        show,
        /**
         * clear alias
         */
        clear,
        /**
         * test alias
         */
        test,
        /**
         * packet alias
         */
        pckt,
        /**
         * flash alias
         */
        flsh,

    }

    /**
     * parameter type
     */
    public enum paraMode {

        /**
         * need parameters
         */
        always,
        /**
         * dont need parameters
         */
        never,
        /**
         * parameters allowed
         */
        allow

    }

    /**
     * convert type to string
     *
     * @param i type
     * @return string
     */
    public static String type2string(aliasType i) {
        switch (i) {
            case exec:
                return "exec";
            case show:
                return "show";
            case clear:
                return "clear";
            case test:
                return "test";
            case pckt:
                return "packet";
            case flsh:
                return "flash";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * convert string to type
     *
     * @param s string
     * @return type
     */
    public static aliasType string2type(String s) {
        if (s.equals("exec")) {
            return aliasType.exec;
        }
        if (s.equals("show")) {
            return aliasType.show;
        }
        if (s.equals("clear")) {
            return aliasType.clear;
        }
        if (s.equals("test")) {
            return aliasType.test;
        }
        if (s.equals("packet")) {
            return aliasType.pckt;
        }
        if (s.equals("flash")) {
            return aliasType.flsh;
        }
        return aliasType.exec;
    }

    /**
     * convert parameter to string
     *
     * @param i parameter
     * @return string
     */
    public static String param2string(paraMode i) {
        switch (i) {
            case allow:
                return "optional";
            case always:
                return "required";
            case never:
                return "forbidden";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * convert string to parameter
     *
     * @param s string
     * @return parameter
     */
    public static paraMode string2param(String s) {
        if (s.equals("optional")) {
            return paraMode.allow;
        }
        if (s.equals("required")) {
            return paraMode.always;
        }
        if (s.equals("forbidden")) {
            return paraMode.never;
        }
        return paraMode.allow;
    }

    public String toString() {
        return type2string(type) + "|" + name + "|" + command;
    }

    /**
     * defaults text
     */
    public final static String[] defaultL = {};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public int compare(cfgAlias o1, cfgAlias o2) {
        int i = o1.type.compareTo(o2.type);
        if (i != 0) {
            return i;
        }
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public void getHelp(userHelping l) {
    }

    public String getPrompt() {
        return "alias";
    }

    private void getShCmds(List<String> l, int filter, String a, String k, String m, String c, paraMode p) {
        if (c == null) {
            return;
        }
        if (hidden) {
            l.add(a + k + authLocal.passwdEncode(c, (filter & 2) != 0));
        } else {
            l.add(a + k + c);
        }
        if (p != paraMode.allow) {
            l.add(a + m + param2string(p));
        }
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        String a = "alias " + type2string(type) + " " + name;
        getShCmds(l, filter, a, " command ", " parameter ", command, parameter);
        getShCmds(l, filter, a, " cmd2nd ", " param2nd ", cmd2nd, param2nd);
        getShCmds(l, filter, a, " cmd3rd ", " param3rd ", cmd3rd, param3rd);
        getShCmds(l, filter, a, " cmd4th ", " param4th ", cmd4th, param4th);
        if (errorFree) {
            l.add(a + " error-free");
        }
        if (defParam != null) {
            l.add(a + " default-param " + defParam);
        }
        if (sticky != null) {
            l.add(a + " sticky-param " + sticky);
        }
        if (description != null) {
            l.add(a + " description " + description);
        }
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        l = userFilter.filterText(l, cfgAll.defaultF);
        return userFilter.filterText(l, defaultF);
    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        boolean neg = a.equals("no");
        if (neg) {
            a = cmd.word();
        }
        if (a.equals("command")) {
            if (neg) {
                command = "";
            } else {
                command = authLocal.passwdDecode(cmd.getRemaining());
            }
            return;
        }
        if (a.equals("cmd2nd")) {
            if (neg) {
                cmd2nd = null;
            } else {
                cmd2nd = authLocal.passwdDecode(cmd.getRemaining());
            }
            return;
        }
        if (a.equals("cmd3rd")) {
            if (neg) {
                cmd3rd = null;
            } else {
                cmd3rd = authLocal.passwdDecode(cmd.getRemaining());
            }
            return;
        }
        if (a.equals("cmd4th")) {
            if (neg) {
                cmd4th = null;
            } else {
                cmd4th = authLocal.passwdDecode(cmd.getRemaining());
            }
            return;
        }
        if (a.equals("description")) {
            if (neg) {
                description = null;
            } else {
                description = cmd.getRemaining();
            }
            return;
        }
        if (a.equals("parameter")) {
            if (neg) {
                parameter = paraMode.allow;
            } else {
                parameter = string2param(cmd.word());
            }
            return;
        }
        if (a.equals("param2nd")) {
            if (neg) {
                param2nd = paraMode.allow;
            } else {
                param2nd = string2param(cmd.word());
            }
            return;
        }
        if (a.equals("param3rd")) {
            if (neg) {
                param3rd = paraMode.allow;
            } else {
                param3rd = string2param(cmd.word());
            }
            return;
        }
        if (a.equals("param4th")) {
            if (neg) {
                param4th = paraMode.allow;
            } else {
                param4th = string2param(cmd.word());
            }
            return;
        }
        if (a.equals("hidden")) {
            hidden = !neg;
            return;
        }
        if (a.equals("error-free")) {
            errorFree = !neg;
            return;
        }
        if (a.equals("sticky-param")) {
            if (neg) {
                sticky = null;
                return;
            }
            sticky = cmd.getRemaining();
            userReader rdr = new userReader(cmd.pipe, null);
            userExec exe = new userExec(cmd.pipe, rdr);
            exe.privileged = true;
            doCommands(exe, cmd);
            return;
        }
        if (a.equals("default-param")) {
            if (neg) {
                defParam = null;
            } else {
                defParam = cmd.getRemaining();
            }
            return;
        }
        cmd.badCmd();
    }

    /**
     * get help text
     *
     * @param l help text to append
     * @param lev starting level
     */
    public void getLines(userHelping l, int lev) {
        String s = ".";
        switch (parameter) {
            case always:
                s = "" + (lev + 1);
                break;
            case allow:
                s = (lev + 1) + ",.";
                break;
            case never:
                s = ".";
                break;
        }
        String a = description;
        if (a == null) {
            a = "execute " + command;
        }
        a = lev + " " + s + " " + name + " " + a;
        l.add(null, a);
        if (parameter == paraMode.never) {
            return;
        }
        s = "" + (lev + 1);
        l.add(null, s + " " + s + ",. <text>   parameter");
    }

    private boolean doOneCmd(userExec exe, String a, cmds par, paraMode p) {
        if (a == null) {
            return false;
        }
        if (p != paraMode.never) {
            a += " " + par.getRemaining();
        }
        a = exe.repairCommand(a);
        exe.executeCommand(a);
        if (!errorFree) {
            return false;
        }
        return exe.cmd.barked != 0;
    }

    /**
     * execute commands
     *
     * @param exe environment
     * @param par parameters
     */
    public void doCommands(userExec exe, cmds par) {
        if ((defParam != null) && (par.getRemaining().length() < 1)) {
            par = new cmds("def", defParam);
        }
        if (sticky != null) {
            sticky = par.getRemaining();
        }
        if (doOneCmd(exe, command, par, parameter)) {
            return;
        }
        if (doOneCmd(exe, cmd2nd, par, param2nd)) {
            return;
        }
        if (doOneCmd(exe, cmd3rd, par, param3rd)) {
            return;
        }
        if (doOneCmd(exe, cmd4th, par, param4th)) {
            return;
        }
    }

}
