package cfg;

import auth.authLocal;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import sec.secTransform;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * ipsec profile configuration
 *
 * @author matecsaba
 */
public class cfgIpsec implements Comparator<cfgIpsec>, cfgGeneric {

    /**
     * role mode
     */
    public enum roleMode {

        /**
         * initiator role
         */
        initiator,
        /**
         * responder role
         */
        responder,
        /**
         * static role
         */
        staticKeys

    }

    /**
     * name of profile
     */
    public final String name;

    /**
     * transform to use
     */
    public final secTransform trans;

    /**
     * preshared key
     */
    public String preshared;

    /**
     * role in session
     */
    public roleMode role = roleMode.staticKeys;

    /**
     * isakmp version
     */
    public int ikeVer = 1;

    /**
     * replay window size
     */
    public int replay = 1024;

    /**
     * work for ipv6
     */
    public boolean ipv6 = false;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "crypto ipsec .*! no group",
        "crypto ipsec .*! no cipher",
        "crypto ipsec .*! no hash",
        "crypto ipsec .*! no seconds",
        "crypto ipsec .*! no bytes",
        "crypto ipsec .*! no key",
        "crypto ipsec .*! role static",
        "crypto ipsec .*! protected ipv4",
        "crypto ipsec .*! isakmp 1",
        "crypto ipsec .*! replay 1024"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public int compare(cfgIpsec o1, cfgIpsec o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String toString() {
        return "ipsec " + name;
    }

    /**
     * create new profile
     *
     * @param nam name of interface
     */
    public cfgIpsec(String nam) {
        name = nam.trim();
        trans = new secTransform();
    }

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("crypto ipsec " + name);
        trans.getShRun(cmds.tabulator, l);
        cmds.cfgLine(l, preshared == null, cmds.tabulator, "key", authLocal.passwdEncode(preshared));
        String s = "unknown";
        switch (role) {
            case initiator:
                s = "initiator";
                break;
            case responder:
                s = "responder";
                break;
            case staticKeys:
                s = "static";
                break;
        }
        l.add(cmds.tabulator + "role " + s);
        if (ipv6) {
            s = "ipv6";
        } else {
            s = "ipv4";
        }
        l.add(cmds.tabulator + "protected " + s);
        l.add(cmds.tabulator + "isakmp " + ikeVer);
        l.add(cmds.tabulator + "replay " + replay);
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if (!filter) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        trans.getHelp(l);
        l.add("1 2  key                 set preshared key");
        l.add("2 .    <text>            key");
        l.add("1 2  protected           set protected protocol");
        l.add("2 .    ipv4              ipv4");
        l.add("2 .    ipv6              ipv6");
        l.add("1 2  role                set role in session");
        l.add("2 .    initiator         initiate the session");
        l.add("2 .    responder         respond to the initiator");
        l.add("2 .    static            static tunnel");
        l.add("1 2  isakmp              set isakmp version to use");
        l.add("2 .    <num>             version");
        l.add("1 2  replay              set replay window size");
        l.add("2 .    <num>             size in packets");
        return l;
    }

    public void doCfgStr(cmds cmd) {
        if (!trans.doCfgStr(cmd.copyBytes(true))) {
            return;
        }
        String s = cmd.word();
        if (s.equals("key")) {
            preshared = authLocal.passwdDecode(cmd.word());
            trans.authAlg = 1;
            return;
        }
        if (s.equals("role")) {
            s = cmd.word();
            if (s.equals("initiator")) {
                role = roleMode.initiator;
            }
            if (s.equals("responder")) {
                role = roleMode.responder;
            }
            if (s.equals("static")) {
                role = roleMode.staticKeys;
            }
            return;
        }
        if (s.equals("protected")) {
            ipv6 = cmd.word().equals("ipv6");
            return;
        }
        if (s.equals("isakmp")) {
            ikeVer = ((bits.str2num(cmd.word()) - 1) & 1) + 1;
            return;
        }
        if (s.equals("replay")) {
            replay = bits.str2num(cmd.word());
            return;
        }
        if (!s.equals("no")) {
            cmd.badCmd();
            return;
        }
        s = cmd.word();
        if (s.equals("key")) {
            preshared = null;
            trans.authAlg = 0;
            return;
        }
        if (s.equals("role")) {
            role = roleMode.staticKeys;
            return;
        }
        cmd.badCmd();
    }

    public String getPrompt() {
        return "ipsec";
    }

}
