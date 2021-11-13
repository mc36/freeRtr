package net.freertr.cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.auth.authConstant;
import net.freertr.auth.authGeneric;
import net.freertr.auth.authList;
import net.freertr.auth.authLocal;
import net.freertr.auth.authRadius;
import net.freertr.auth.authTacacs;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.cmds;

/**
 * one user list configuration
 *
 * @author matecsaba
 */
public class cfgAuther implements Comparator<cfgAuther>, cfgGeneric {

    /**
     * name of this user list
     */
    public final String name;

    /**
     * method type
     */
    public enum methodType {

        /**
         * local userlist
         */
        local,
        /**
         * radius
         */
        radius,
        /**
         * tacacs
         */
        tacacs,
        /**
         * list of methods
         */
        listing,
        /**
         * never succeed
         */
        never,
        /**
         * always succeed
         */
        always

    }

    private authLocal loc;

    private authRadius rad;

    private authTacacs tac;

    private authList lst;

    private authConstant cnst;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "aaa userlist .*! username .* autocommand ",
        "aaa userlist .*! username .* privilege 15",
        "aaa radius .*! no secret",
        "aaa radius .*! privilege 15",
        "aaa tacacs .*! no secret",
        "aaa tacacs .*! privilege 15",
        "aaa .*! no log-error",
        "aaa .*! no log-failure",
        "aaa .*! no log-success",
        "aaa .*! no log-password"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public int compare(cfgAuther o1, cfgAuther o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String toString() {
        return "authenticator " + name;
    }

    /**
     * get authenticator
     *
     * @return authenticator to use
     */
    public authGeneric getAuther() {
        if (loc != null) {
            return loc;
        }
        if (rad != null) {
            return rad;
        }
        if (tac != null) {
            return tac;
        }
        if (lst != null) {
            return lst;
        }
        if (cnst != null) {
            return cnst;
        }
        return null;
    }

    /**
     * create new vrf
     *
     * @param nam name of vrf
     * @param typ type
     */
    public cfgAuther(String nam, methodType typ) {
        name = nam.trim();
        if (typ == null) {
            return;
        }
        switch (typ) {
            case local:
                loc = new authLocal();
                break;
            case radius:
                rad = new authRadius();
                break;
            case tacacs:
                tac = new authTacacs();
                break;
            case listing:
                lst = new authList();
                break;
            case never:
                cnst = new authConstant(false);
                break;
            case always:
                cnst = new authConstant(true);
                break;
        }
        authGeneric aut = getAuther();
        if (aut != null) {
            aut.autName = name;
        }
    }

    /**
     * convert string to type
     *
     * @param s string
     * @return type
     */
    public static methodType string2auther(String s) {
        if (s.equals("userlist")) {
            return methodType.local;
        }
        if (s.equals("radius")) {
            return methodType.radius;
        }
        if (s.equals("tacacs")) {
            return methodType.tacacs;
        }
        if (s.equals("list")) {
            return methodType.listing;
        }
        if (s.equals("never")) {
            return methodType.never;
        }
        if (s.equals("always")) {
            return methodType.always;
        }
        return null;
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        authGeneric aut = getAuther();
        if (aut == null) {
            return l;
        }
        l.add("aaa " + aut.getCfgName() + " " + name);
        cmds.cfgLine(l, !aut.logFail, cmds.tabulator, "log-failure", "");
        cmds.cfgLine(l, !aut.logErr, cmds.tabulator, "log-error", "");
        cmds.cfgLine(l, !aut.logOk, cmds.tabulator, "log-success", "");
        cmds.cfgLine(l, !aut.logPass, cmds.tabulator, "log-password", "");
        l.addAll(aut.getShRun(cmds.tabulator, filter));
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void getHelp(userHelping l) {
        l.add(null, "1 .    log-failure         log failure");
        l.add(null, "1 .    log-success         log success");
        l.add(null, "1 .    log-error           log error");
        l.add(null, "1 .    log-password        log password");
        authGeneric aut = getAuther();
        if (aut != null) {
            aut.getHelp(l);
        }
    }

    public synchronized void doCfgStr(cmds cmd) {
        boolean b = true;
        authGeneric aut = getAuther();
        cmds c = cmd.copyBytes(false);
        String a = c.word();
        boolean neg = a.equals("no");
        if (neg) {
            a = c.word();
        }
        if (aut == null) {
            cmd.badCmd();
            return;
        }
        if (a.equals("log-failure")) {
            aut.logFail = !neg;
            return;
        }
        if (a.equals("log-error")) {
            aut.logErr = !neg;
            return;
        }
        if (a.equals("log-success")) {
            aut.logOk = !neg;
            return;
        }
        if (a.equals("log-password")) {
            aut.logPass = !neg;
            return;
        }
        b = aut.fromString(cmd);
        if (b) {
            cmd.badCmd();
        }
    }

    public String getPrompt() {
        return "auther";
    }

}
