package net.freertr.cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabListingEntry;
import net.freertr.tab.tabPrfxlstN;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * prefix list configuration
 *
 * @author matecsaba
 */
public class cfgPrfxlst implements Comparator<cfgPrfxlst>, cfgGeneric {

    /**
     * name of prefixlist
     */
    public String name;

    /**
     * description of access list
     */
    public String description;

    /**
     * list of prefixes
     */
    public tabListing<tabPrfxlstN, addrIP> prflst;

    /**
     * create new prefix list
     */
    public cfgPrfxlst() {
        prflst = new tabListing<tabPrfxlstN, addrIP>();
    }

    /**
     * defaults text
     */
    public final static String[] defaultL = {};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("prefix-list " + name);
        if (description != null) {
            l.add(cmds.tabulator + "description " + description);
        }
        l.addAll(prflst.dump(cmds.tabulator));
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void getHelp(userHelping l) {
        l.add(null, "1 2   sequence              sequence number of an entry");
        l.add(null, "2 1     <num>               sequence number");
        l.add(null, "1 3,. description           specify description");
        l.add(null, "3 3,.   <str>               text");
        l.add(null, "1 2   rename                rename this prefix list");
        l.add(null, "2 .     <name>              set new name");
        l.add(null, "1 3   evaluate              evaluate another list");
        l.add(null, "3 4     permit              specify list to allow");
        l.add(null, "3 4     deny                specify list to forbid");
        l.add(null, "4 .       <name:pl>         name of list");
        l.add(null, "1 3   permit                specify networks to allow");
        l.add(null, "1 3   deny                  specify networks to forbid");
        l.add(null, "3 4,.   <net/mask>          network in perfix/mask format");
        l.add(null, "4 5       ge                minimum prefix length to be matched");
        l.add(null, "5 4,.       <num>           minimum prefix length");
        l.add(null, "4 5       le                maximum prefix length to be matched");
        l.add(null, "5 4,.       <num>           maximum prefix length");
        l.add(null, "4 4,.     log               set logging on match");
        l.add(null, "1 2,. reindex               reindex prefix list");
        l.add(null, "2 3,.   [num]               initial number to start with");
        l.add(null, "3 4,.     [num]             increment number");
    }

    public synchronized void doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("no")) {
            a = cmd.word();
            if (a.equals("description")) {
                description = null;
                return;
            }
            if (a.equals("sequence")) {
                tabPrfxlstN ntry = new tabPrfxlstN();
                ntry.sequence = bits.str2num(cmd.word());
                if (prflst.del(ntry)) {
                    cmd.error("invalid sequence");
                    return;
                }
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("description")) {
            description = cmd.getRemaining();
            return;
        }
        if (a.equals("rename")) {
            a = cmd.word();
            cfgPrfxlst v = cfgAll.prfxFind(a, false);
            if (v != null) {
                cmd.error("already exists");
                return;
            }
            name = a;
            prflst.listName = a;
            return;
        }
        if (a.equals("reindex")) {
            int i = bits.str2num(cmd.word());
            prflst.reindex(i, bits.str2num(cmd.word()));
            return;
        }
        int seq = prflst.nextseq();
        if (a.equals("sequence")) {
            seq = bits.str2num(cmd.word());
            a = cmd.word();
        }
        tabPrfxlstN ntry = new tabPrfxlstN();
        ntry.sequence = seq;
        if (a.equals("evaluate")) {
            ntry.action = tabListingEntry.string2action(cmd.word());
            cfgPrfxlst res = cfgAll.prfxFind(cmd.word(), false);
            if (res == null) {
                cmd.error("no such list");
                return;
            }
            ntry.evaluate = res.prflst;
            prflst.add(ntry);
            return;
        }
        ntry.action = tabListingEntry.string2action(a);
        if (ntry.fromString(cmd.getRemaining())) {
            cmd.error("invalid network");
            return;
        }
        prflst.add(ntry);
    }

    public int compare(cfgPrfxlst o1, cfgPrfxlst o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String getPrompt() {
        return "prfx";
    }

    public String toString() {
        return name;
    }

}
