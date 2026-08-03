package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.tab.tabAceslstN;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabListingEntry;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * access list configuration
 *
 * @author matecsaba
 */
public class cfgAceslst implements Comparable<cfgAceslst>, cfgGeneric {

    /**
     * name of access list
     */
    public String name;

    /**
     * description of access list
     */
    public String description;

    /**
     * hidden list
     */
    public boolean hidden;

    /**
     * list of statements
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> aceslst;

    /**
     * create new access list
     *
     * @param s name
     */
    public cfgAceslst(String s) {
        aceslst = new tabListing<tabAceslstN<addrIP>, addrIP>();
        name = s;
        aceslst.listName = s;
    }

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {};

    public String toString() {
        return name;
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("access-list " + name);
        if (description != null) {
            l.add(cmds.tabulator + "description " + description);
        }
        if (hidden) {
            l.add(cmds.tabulator + "hidden");
        }
        if (!hidden) {
            l.addAll(aceslst.dump(cmds.tabulator, filter));
        }
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "sequence", "sequence number of an entry");
        l.add(null, false, 2, new int[]{1}, "<num>", "sequence number");
        l.add(null, false, 1, new int[]{3, -1}, "description", "specify description");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "text");
        l.add(null, false, 1, new int[]{2}, "rename", "rename this access list");
        l.add(null, false, 2, new int[]{-1}, "<str>", "set new name");
        l.add(null, false, 1, new int[]{-1}, "hidden", "hide the entries");
        l.add(null, false, 1, new int[]{3}, "evaluate", "evaluate another list");
        l.add(null, false, 3, new int[]{4}, "permit", "specify list to allow");
        l.add(null, false, 3, new int[]{4}, "deny", "specify list to forbid");
        l.add(null, false, 4, new int[]{-1}, "<name:acl>", "name of list");
        l.add(null, false, 1, new int[]{3}, "reflect", "create forward entry on match");
        l.add(null, false, 3, new int[]{4}, "<name:acl>", "name of forward list");
        l.add(null, false, 4, new int[]{5}, "<name:acl>", "name of reverse list");
        l.add(null, false, 5, new int[]{-1}, "<num>", "timeout");
        l.add(null, false, 1, new int[]{3}, "permit", "specify networks to allow");
        l.add(null, false, 1, new int[]{3}, "deny", "specify networks to forbid");
        l.add(null, false, 3, new int[]{4}, "all", "no protocol matching");
        l.add(null, false, 3, new int[]{4}, "<proto>", "protocol number");
        l.add(null, false, 4, new int[]{6}, "any", "no source address matching");
        l.add(null, false, 4, new int[]{5}, "obj", "object group source address matching");
        l.add(null, false, 4, new int[]{5}, "host", "host source address matching");
        l.add(null, false, 4, new int[]{5}, "<addr>", "address of source network");
        l.add(null, false, 5, new int[]{6}, "<mask>", "mask of source network");
        l.add(null, false, 6, new int[]{8}, "all", "no source port matching");
        l.add(null, false, 6, new int[]{8}, "<port>", "source port");
        l.add(null, false, 6, new int[]{7}, "obj", "object group source port matching");
        l.add(null, false, 7, new int[]{8}, "<str>", "name of object group");
        l.add(null, false, 8, new int[]{10}, "any", "no target address matching");
        l.add(null, false, 8, new int[]{9}, "obj", "object group target address matching");
        l.add(null, false, 8, new int[]{9}, "host", "host target address matching");
        l.add(null, false, 8, new int[]{9}, "<addr>", "address of target network");
        l.add(null, false, 9, new int[]{10}, "<mask>", "mask of target network");
        l.add(null, false, 10, new int[]{12, -1}, "all", "no target port matching");
        l.add(null, false, 10, new int[]{12, -1}, "<port>", "target port");
        l.add(null, false, 10, new int[]{11}, "obj", "object group target port matching");
        l.add(null, false, 11, new int[]{12, -1}, "<str>", "name of object group");
        l.add(null, false, 12, new int[]{12, -1}, "alrt", "alerted datagrams");
        l.add(null, false, 12, new int[]{12, -1}, "frag", "fragmented datagrams");
        l.add(null, false, 12, new int[]{13}, "flag", "tcp flags");
        l.add(null, false, 13, new int[]{12, -1}, "all", "no flag matching");
        l.add(null, false, 13, new int[]{12, -1}, "<num>", "flag value");
        l.add(null, false, 12, new int[]{13}, "tos", "type of service matching");
        l.add(null, false, 13, new int[]{12, -1}, "all", "no tos matching");
        l.add(null, false, 13, new int[]{12, -1}, "<num>", "tos value");
        l.add(null, false, 12, new int[]{13}, "flow", "flow label matching");
        l.add(null, false, 13, new int[]{12, -1}, "all", "no tos matching");
        l.add(null, false, 13, new int[]{12, -1}, "<num>", "tos value");
        l.add(null, false, 12, new int[]{13}, "dscp", "dscp matching");
        l.add(null, false, 13, new int[]{12, -1}, "all", "no dscp matching");
        l.add(null, false, 13, new int[]{12, -1}, "<num>", "tos value");
        l.add(null, false, 12, new int[]{13}, "prec", "precedence matching");
        l.add(null, false, 13, new int[]{12, -1}, "all", "no precedence matching");
        l.add(null, false, 13, new int[]{12, -1}, "<num>", "tos value");
        l.add(null, false, 12, new int[]{13}, "len", "length matching");
        l.add(null, false, 13, new int[]{12, -1}, "all", "no length matching");
        l.add(null, false, 13, new int[]{12, -1}, "<num>", "length value");
        l.add(null, false, 12, new int[]{13}, "ttl", "time to live matching");
        l.add(null, false, 13, new int[]{12, -1}, "all", "no ttl matching");
        l.add(null, false, 13, new int[]{12, -1}, "<num>", "ttl value");
        l.add(null, false, 12, new int[]{13}, "sgt", "security group tag matching");
        l.add(null, false, 13, new int[]{12, -1}, "all", "no sgt matching");
        l.add(null, false, 13, new int[]{12, -1}, "<num>", "sgt value");
        l.add(null, false, 12, new int[]{12, -1}, "log", "log on matching");
        l.add(null, false, 1, new int[]{2, -1}, "reindex", "reindex access list");
        l.add(null, false, 2, new int[]{3, -1}, "[num]", "initial number to start with");
        l.add(null, false, 3, new int[]{-1}, "[num]", "increment number");
    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals(cmds.negated)) {
            a = cmd.word();
            if (a.equals("description")) {
                description = null;
                return;
            }
            if (a.equals("hidden")) {
                hidden = false;
                return;
            }
            if (a.equals("sequence")) {
                tabAceslstN<addrIP> ntry = new tabAceslstN<addrIP>(new addrIP());
                ntry.sequence = bits.str2num(cmd.word());
                if (aceslst.del(ntry)) {
                    cmd.error("invalid sequence");
                    return;
                }
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("rename")) {
            a = cmd.word();
            cfgAceslst v = cfgAll.aclsFind(a, false);
            if (v != null) {
                cmd.error("already exists");
                return;
            }
            name = a;
            aceslst.listName = a;
            return;
        }
        if (a.equals("description")) {
            description = cmd.getRemaining();
            return;
        }
        if (a.equals("hidden")) {
            hidden = true;
            return;
        }
        if (a.equals("reindex")) {
            int i = bits.str2num(cmd.word());
            aceslst.reindex(i, bits.str2num(cmd.word()));
            return;
        }
        int seq = aceslst.nextseq();
        if (a.equals("sequence")) {
            seq = bits.str2num(cmd.word());
            a = cmd.word();
        }
        tabAceslstN<addrIP> ntry = new tabAceslstN<addrIP>(new addrIP());
        ntry.sequence = seq;
        if (a.equals("reflect")) {
            ntry = aceslst.find(ntry);
            if (ntry == null) {
                cmd.error("no such entry");
                return;
            }
            cfgAceslst res = cfgAll.aclsFind(cmd.word(), false);
            if (res == null) {
                cmd.error("no such list");
                return;
            }
            ntry.reflectFwd = res.aceslst;
            res = cfgAll.aclsFind(cmd.word(), false);
            if (res == null) {
                cmd.error("no such list");
                return;
            }
            ntry.reflectRev = res.aceslst;
            ntry.reflectTim = bits.str2num(cmd.word());
            return;
        }
        if (a.equals("evaluate")) {
            ntry.action = tabListingEntry.string2action(cmd.word());
            cfgAceslst res = cfgAll.aclsFind(cmd.word(), false);
            if (res == null) {
                cmd.error("no such list");
                return;
            }
            ntry.evaluate = res.aceslst;
            aceslst.add(ntry);
            return;
        }
        ntry.action = tabListingEntry.string2action(a);
        if (tabAceslstN.fromString(ntry, cmd)) {
            cmd.error("invalid network");
            return;
        }
        aceslst.add(ntry);
    }

    public int compareTo(cfgAceslst o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
    }

    public String getPrompt() {
        return "acl";
    }

}
