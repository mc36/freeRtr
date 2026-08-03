package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabSession;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.cmds;

/**
 * stateful session configuration
 *
 * @author matecsaba
 */
public class cfgSessn implements Comparable<cfgSessn>, cfgGeneric {

    /**
     * name of session
     */
    public String name;

    /**
     * description of this dialpeer
     */
    public String description = null;

    /**
     * sessions
     */
    public tabSession connects = new tabSession(true, 180000);

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("session .*", cmds.tabulator + cmds.negated + cmds.tabulator + "description", null),
        new userFilter("session .*", cmds.tabulator + cmds.negated + cmds.tabulator + "timeout", null),
        new userFilter("session .*", cmds.tabulator + cmds.negated + cmds.tabulator + "sessions", null),
        new userFilter("session .*", cmds.tabulator + cmds.negated + cmds.tabulator + "sample", null),
        new userFilter("session .*", cmds.tabulator + cmds.negated + cmds.tabulator + "rate", null),
        new userFilter("session .*", cmds.tabulator + cmds.negated + cmds.tabulator + "mac", null),
        new userFilter("session .*", cmds.tabulator + cmds.negated + cmds.tabulator + "before", null),
        new userFilter("session .*", cmds.tabulator + cmds.negated + cmds.tabulator + "after", null),
        new userFilter("session .*", cmds.tabulator + cmds.negated + cmds.tabulator + "dropped", null),
        new userFilter("session .*", cmds.tabulator + cmds.negated + cmds.tabulator + "allow-routing", null),
        new userFilter("session .*", cmds.tabulator + cmds.negated + cmds.tabulator + "allow-sending", null),
        new userFilter("session .*", cmds.tabulator + cmds.negated + cmds.tabulator + "allow-linklocal", null),
        new userFilter("session .*", cmds.tabulator + cmds.negated + cmds.tabulator + "allow-multicast", null),
        new userFilter("session .*", cmds.tabulator + cmds.negated + cmds.tabulator + "allow-broadcast", null),
        new userFilter("session .*", cmds.tabulator + cmds.negated + cmds.tabulator + "allow-list", null),
        new userFilter("session .*", cmds.tabulator + cmds.negated + cmds.tabulator + "allow-url", null),
        new userFilter("session .*", cmds.tabulator + cmds.negated + cmds.tabulator + "drop-rx", null),
        new userFilter("session .*", cmds.tabulator + cmds.negated + cmds.tabulator + "drop-tx", null),
        new userFilter("session .*", cmds.tabulator + cmds.negated + cmds.tabulator + "drop-frg", null),
        new userFilter("session .*", cmds.tabulator + cmds.negated + cmds.tabulator + "member", null)
    };

    public int compareTo(cfgSessn o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
    }

    /**
     * create new certificate
     *
     * @param nam name of interface
     */
    public cfgSessn(String nam) {
        name = nam.trim();
        connects.name = name;
    }

    public String toString() {
        return name;
    }

    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "rename", "rename this session");
        l.add(null, false, 2, new int[]{-1}, "<str>", "set new name");
        l.add(null, false, 1, new int[]{2}, "description", "specify description");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "description");
        l.add(null, false, 1, new int[]{2}, "timeout", "set timeout");
        l.add(null, false, 2, new int[]{-1}, "<num>", "timeout in ms");
        l.add(null, false, 1, new int[]{2}, "sessions", "set session limit");
        l.add(null, false, 2, new int[]{-1}, "<num>", "number of sessions");
        l.add(null, false, 1, new int[]{2}, "sample", "set sample value");
        l.add(null, false, 2, new int[]{-1}, "<num>", "one of every n packet");
        l.add(null, false, 1, new int[]{2}, "rate", "specify translation rate");
        l.add(null, false, 2, new int[]{-1}, "<nam:pm>", "name of policy map");
        l.add(null, false, 1, new int[]{-1}, "mac", "with mac addresses");
        l.add(null, false, 1, new int[]{-1}, "before", "log on session start");
        l.add(null, false, 1, new int[]{-1}, "after", "log on session stop");
        l.add(null, false, 1, new int[]{-1}, "dropped", "log on session drop");
        l.add(null, false, 1, new int[]{-1}, "drop-rx", "drop sessions ingress");
        l.add(null, false, 1, new int[]{-1}, "drop-tx", "drop sessions egress");
        l.add(null, false, 1, new int[]{-1}, "drop-frg", "drop fragmented packets");
        l.add(null, false, 1, new int[]{-1}, "allow-routing", "allow control multicast traffic");
        l.add(null, false, 1, new int[]{-1}, "allow-sending", "allow self originated traffic");
        l.add(null, false, 1, new int[]{-1}, "allow-linklocal", "allow link local traffic");
        l.add(null, false, 1, new int[]{-1}, "allow-multicast", "allow multicast traffic");
        l.add(null, false, 1, new int[]{-1}, "allow-broadcast", "allow broadcast traffic");
        l.add(null, false, 1, new int[]{2}, "allow-list", "allow specific traffic");
        l.add(null, false, 2, new int[]{-1}, "<name:acl>", "name of access list");
        l.add(null, false, 1, new int[]{2}, "allow-url", "allow specific traffic");
        l.add(null, false, 2, new int[]{-1}, "<num>", "translation rule");
    }

    public String getPrompt() {
        return "session";
    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        boolean neg = a.equals(cmds.negated);
        if (neg) {
            a = cmd.word();
        }
        if (a.equals("rename")) {
            a = cmd.word();
            cfgSessn v = cfgAll.sessnFind(a, false);
            if (v != null) {
                cmd.error("already exists");
                return;
            }
            name = a;
            connects.name = a;
            return;
        }
        if (a.equals("description")) {
            description = cmd.getRemaining();
            if (neg) {
                description = null;
            }
            return;
        }
        connects.fromString(cmd.copyBytes(true));
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("session " + name);
        cmds.cfgLine(l, description == null, cmds.tabulator, "description", description);
        connects.getConfig(l, cmds.tabulator);
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

}
