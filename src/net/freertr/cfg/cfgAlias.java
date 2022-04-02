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
     * hide commands
     */
    public boolean hidden = false;

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

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        String a = "alias " + type2string(type) + " " + name;
        if (hidden) {
            l.add(a + " command " + authLocal.passwdEncode(command, (filter & 2) != 0));
            if (cmd2nd != null) {
                l.add(a + " cmd2nd " + authLocal.passwdEncode(cmd2nd, (filter & 2) != 0));
            }
            l.add(a + " hidden");
        } else {
            l.add(a + " command " + command);
            if (cmd2nd != null) {
                l.add(a + " cmd2nd " + cmd2nd);
            }
        }
        if (parameter != paraMode.allow) {
            l.add(a + " parameter " + param2string(parameter));
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
        if (a.equals("hidden")) {
            hidden = !neg;
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
        String a = command;
        if (parameter != paraMode.never) {
            a += " " + par.getRemaining();
        }
        a = exe.repairCommand(a);
        exe.executeCommand(a);
        a = cmd2nd;
        if (a == null) {
            return;
        }
        if (parameter != paraMode.never) {
            a += " " + par.getRemaining();
        }
        a = exe.repairCommand(a);
        exe.executeCommand(a);
    }

}
