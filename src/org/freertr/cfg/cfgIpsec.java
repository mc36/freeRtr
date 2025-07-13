package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.auth.authLocal;
import org.freertr.sec.secTransform;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * ipsec profile configuration
 *
 * @author matecsaba
 */
public class cfgIpsec implements Comparable<cfgIpsec>, cfgGeneric {

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
    public String name;

    /**
     * description
     */
    public String description;

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
    public final static userFilter[] defaultF = {
        new userFilter("crypto ipsec .*", cmds.tabulator + cmds.negated + cmds.tabulator + "description", null),
        new userFilter("crypto ipsec .*", cmds.tabulator + cmds.negated + cmds.tabulator + "group", null),
        new userFilter("crypto ipsec .*", cmds.tabulator + cmds.negated + cmds.tabulator + "cipher", null),
        new userFilter("crypto ipsec .*", cmds.tabulator + cmds.negated + cmds.tabulator + "hash", null),
        new userFilter("crypto ipsec .*", cmds.tabulator + cmds.negated + cmds.tabulator + "prf", null),
        new userFilter("crypto ipsec .*", cmds.tabulator + cmds.negated + cmds.tabulator + "seconds", null),
        new userFilter("crypto ipsec .*", cmds.tabulator + cmds.negated + cmds.tabulator + "random", null),
        new userFilter("crypto ipsec .*", cmds.tabulator + cmds.negated + cmds.tabulator + "bytes", null),
        new userFilter("crypto ipsec .*", cmds.tabulator + cmds.negated + cmds.tabulator + "key", null),
        new userFilter("crypto ipsec .*", cmds.tabulator + "role static", null),
        new userFilter("crypto ipsec .*", cmds.tabulator + "protected ipv4", null),
        new userFilter("crypto ipsec .*", cmds.tabulator + "isakmp 1", null),
        new userFilter("crypto ipsec .*", cmds.tabulator + "replay 1024", null)
    };

    public int compareTo(cfgIpsec o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
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

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("crypto ipsec " + name);
        cmds.cfgLine(l, description == null, cmds.tabulator, "description", description);
        trans.getShRun(cmds.tabulator, l);
        cmds.cfgLine(l, preshared == null, cmds.tabulator, "key", authLocal.passwdEncode(preshared, (filter & 2) != 0));
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
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void getHelp(userHelp l) {
        trans.getHelp(l);
        l.add(null, false, 1, new int[]{3, -1}, "description", "specify description");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "text");
        l.add(null, false, 1, new int[]{2}, "rename", "rename this ipsec");
        l.add(null, false, 2, new int[]{-1}, "<str>", "set new name");
        l.add(null, false, 1, new int[]{2}, "key", "set preshared key");
        l.add(null, false, 2, new int[]{-1}, "<text>", "key");
        l.add(null, false, 1, new int[]{2}, "protected", "set protected protocol");
        l.add(null, false, 2, new int[]{-1}, "ipv4", "ipv4");
        l.add(null, false, 2, new int[]{-1}, "ipv6", "ipv6");
        l.add(null, false, 1, new int[]{2}, "role", "set role in session");
        l.add(null, false, 2, new int[]{-1}, "initiator", "initiate the session");
        l.add(null, false, 2, new int[]{-1}, "responder", "respond to the initiator");
        l.add(null, false, 2, new int[]{-1}, "static", "static tunnel");
        l.add(null, false, 1, new int[]{2}, "isakmp", "set isakmp version to use");
        l.add(null, false, 2, new int[]{-1}, "<num>", "version");
        l.add(null, false, 1, new int[]{2}, "replay", "set replay window size");
        l.add(null, false, 2, new int[]{-1}, "<num>", "size in packets");
    }

    public void doCfgStr(cmds cmd) {
        if (!trans.doCfgStr(cmd.copyBytes(true))) {
            return;
        }
        String s = cmd.word();
        if (s.equals("description")) {
            description = cmd.getRemaining();
            return;
        }
        if (s.equals("rename")) {
            s = cmd.word();
            cfgIpsec v = cfgAll.ipsecFind(s, false);
            if (v != null) {
                cmd.error("already exists");
                return;
            }
            name = s;
            return;
        }
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
        if (!s.equals(cmds.negated)) {
            cmd.badCmd();
            return;
        }
        s = cmd.word();
        if (s.equals("description")) {
            description = null;
            return;
        }
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
