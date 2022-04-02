package net.freertr.cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabSession;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.cmds;

/**
 * stateful session configuration
 *
 * @author matecsaba
 */
public class cfgSessn implements Comparator<cfgSessn>, cfgGeneric {

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
    public final static String[] defaultL = {
        "session .*! no description",
        "session .*! no timeout",
        "session .*! no sessions",
        "session .*! no rate",
        "session .*! no mac",
        "session .*! no before",
        "session .*! no after",
        "session .*! no dropped",
        "session .*! no allow-routing",
        "session .*! no allow-sending",
        "session .*! no allow-linklocal",
        "session .*! no allow-multicast",
        "session .*! no allow-broadcast",
        "session .*! no allow-list",
        "session .*! no allow-url",
        "session .*! no drop-rx",
        "session .*! no drop-tx",
        "session .*! no drop-frg",
        "session .*! no member"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public int compare(cfgSessn o1, cfgSessn o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    /**
     * create new certificate
     *
     * @param nam name of interface
     */
    public cfgSessn(String nam) {
        name = nam.trim();
    }

    public String toString() {
        return name;
    }

    public void getHelp(userHelping l) {
        l.add(null, "1 2  rename                       rename this session");
        l.add(null, "2 .    <str>                      set new name");
        l.add(null, "1 2  description                  specify description");
        l.add(null, "2 2,.  <str>                      description");
        l.add(null, "1 2  timeout                      set timeout");
        l.add(null, "2 .    <num>                      timeout in ms");
        l.add(null, "1 2  sessions                     set session limit");
        l.add(null, "2 .    <num>                      number of sessions");
        l.add(null, "1 2  rate                         specify translation rate");
        l.add(null, "2 .    <nam:pm>                   name of policy map");
        l.add(null, "1 .  mac                          with mac addresses");
        l.add(null, "1 .  before                       log on session start");
        l.add(null, "1 .  after                        log on session stop");
        l.add(null, "1 .  dropped                      log on session stop");
        l.add(null, "1 .  drop-rx                      drop sessions ingress");
        l.add(null, "1 .  drop-tx                      drop sessions egress");
        l.add(null, "1 .  drop-frg                     drop fragmented packets");
        l.add(null, "1 .  allow-routing                allow control multicast traffic");
        l.add(null, "1 .  allow-sending                allow self originated traffic");
        l.add(null, "1 .  allow-linklocal              allow link local traffic");
        l.add(null, "1 .  allow-multicast              allow multicast traffic");
        l.add(null, "1 .  allow-broadcast              allow broadcast traffic");
        l.add(null, "1 2  allow-list                   allow specific traffic");
        l.add(null, "2 .    <name:acl>                 name of access list");
        l.add(null, "1 2  allow-url                    allow specific traffic");
        l.add(null, "2 .    <num>                      translation rule");
    }

    public String getPrompt() {
        return "session";
    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        boolean neg = a.equals("no");
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
