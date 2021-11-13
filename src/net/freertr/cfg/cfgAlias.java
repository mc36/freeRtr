package net.freertr.cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.auth.authLocal;
import net.freertr.user.userExec;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
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
     * help description text
     */
    public String description = "";

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
        if (description.length() > 0) {
            l.add(a + " description " + description);
        }
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, cfgAll.defaultF);
    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("command")) {
            command = authLocal.passwdDecode(cmd.getRemaining());
            return;
        }
        if (a.equals("cmd2nd")) {
            cmd2nd = authLocal.passwdDecode(cmd.getRemaining());
            return;
        }
        if (a.equals("description")) {
            description = cmd.getRemaining();
            return;
        }
        if (a.equals("parameter")) {
            parameter = string2param(cmd.word());
            return;
        }
        if (a.equals("hidden")) {
            hidden = true;
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
        String a = "" + description;
        if (a.length() < 1) {
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
        cmds orig = par.copyBytes(false);
        String a = getCommand(orig);
        a = exe.repairCommand(a);
        exe.executeCommand(a);
        a = getCmd2nd(orig);
        if (a == null) {
            return;
        }
        a = exe.repairCommand(a);
        exe.executeCommand(a);
    }

    /**
     * get command line
     *
     * @param cmd parameters
     * @return command line
     */
    public String getCommand(cmds cmd) {
        String s = command;
        if (parameter != paraMode.never) {
            s += " " + cmd.getRemaining();
        }
        return s;
    }

    /**
     * get 2nd command line
     *
     * @param cmd parameters
     * @return command line
     */
    public String getCmd2nd(cmds cmd) {
        String s = cmd2nd;
        if (s == null) {
            return null;
        }
        if (parameter != paraMode.never) {
            s += " " + cmd.getRemaining();
        }
        return s;
    }

}
