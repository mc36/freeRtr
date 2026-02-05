package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabListingEntry;
import org.freertr.tab.tabRtrmapN;
import org.freertr.user.userEditor;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.pipe.pipeScreen;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * route map configuration
 *
 * @author matecsaba
 */
public class cfgRoump implements Comparable<cfgRoump>, cfgGeneric {

    /**
     * name of routemap
     */
    public String name;

    /**
     * current sequence number
     */
    public int seq;

    /**
     * list of routemaps
     */
    public tabListing<tabRtrmapN, addrIP> roumap;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("route-map .*", cmds.tabulator + "sequence .* description ", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* tcldel", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match access-list", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match prefix-list", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match route-map", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match route-policy", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match rd", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match network", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match aspath", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match peerstd", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match peerlrg", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match stdcomm", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match extcomm", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match lrgcomm", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match privateas", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match entropy", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match tracker", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match interface", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match nexthop", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match recursive", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match protocol", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match peerasn all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match distance all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match locpref all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match aigp all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match validroa all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match validaspa all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match aggregator all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match customer all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match destpref all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match pathlen all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match unknowns all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match asend all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match asbeg all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match asmid all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match asany all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match bandwidth all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match origin all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match metric all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match tag all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match label-local all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match label-remote all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match segrout all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match bier all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match afi all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* match safi all", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match srv6", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match nostdcomm", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match noextcomm", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no match nolrgcomm", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no clear stdcomm", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no clear extcomm", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no clear lrgcomm", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no clear originator", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no clear clustlist", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no clear privateas", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no clear entropy", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no clear peeras", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no clear exactas", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no clear firstas", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no set rd", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no set route-map", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no set route-policy", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no set aspath", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no set asconfed", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no set stdcomm", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no set extcomm", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no set lrgcomm", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no set nexthop", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no set vrf", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* set distance leave", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* set locpref leave", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* set aigp leave", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* set validroa leave", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* set validaspa leave", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* set aggregator leave null", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* set connector null", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* set pathlimit leave leave", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* set customer leave", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* set destpref leave leave", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* set bandwidth leave", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* set origin leave", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* set metric leave", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* set tag leave", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* set label-local leave", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* set label-remote leave", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* set segrout leave", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* set bier leave leave", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no set srv6", null),
        new userFilter("route-map .*", cmds.tabulator + "sequence .* no log", null)
    };

    /**
     * create new route map
     *
     * @param s name
     */
    public cfgRoump(String s) {
        roumap = new tabListing<tabRtrmapN, addrIP>();
        seq = roumap.nextseq();
        name = s;
        roumap.listName = s;
    }

    /**
     * get current entry
     *
     * @return current entry
     */
    public synchronized tabRtrmapN getCurr() {
        tabRtrmapN ntry = new tabRtrmapN();
        ntry.sequence = seq;
        ntry = roumap.find(ntry);
        if (ntry != null) {
            return ntry;
        }
        ntry = new tabRtrmapN();
        ntry.sequence = seq;
        ntry.action = tabListingEntry.actionType.actPermit;
        roumap.add(ntry);
        return ntry;
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("route-map " + name);
        l.addAll(roumap.dump(cmds.tabulator, filter));
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "sequence", "sequence number of an entry");
        l.add(null, false, 2, new int[]{1, -1}, "<num>", "sequence number");
        l.add(null, false, 1, new int[]{2, -1}, "reindex", "reindex route map");
        l.add(null, false, 2, new int[]{3, -1}, "[num]", "initial number to start with");
        l.add(null, false, 3, new int[]{-1}, "[num]", "increment number");
        l.add(null, false, 1, new int[]{2}, "action", "set action to do");
        l.add(null, false, 2, new int[]{-1}, "deny", "specify to forbid");
        l.add(null, false, 2, new int[]{-1}, "permit", "specify to allow");
        l.add(null, false, 1, new int[]{2, -1}, "description", "description of this route map");
        l.add(null, false, 2, new int[]{2, -1}, "[text]", "text describing this route map");
        l.add(null, false, 1, new int[]{2}, "rename", "rename this route map");
        l.add(null, false, 2, new int[]{-1}, "<str>", "set new name");
        l.add(null, false, 1, new int[]{2}, "tcladd", "add tcl line");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "script");
        l.add(null, false, 1, new int[]{-1}, "tcldel", "delete tcl script");
        l.add(null, false, 1, new int[]{-1}, "tcledit", "edit tcl script");
        l.add(null, false, 1, new int[]{-1}, "log", "set logging on match");
        l.add(null, false, 1, new int[]{2}, "match", "match values from source routing protocol");
        l.add(null, false, 2, new int[]{3}, "aspath", "match as path");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "regexp against as path");
        l.add(null, false, 2, new int[]{3}, "peerstd", "match standard community based on peer asn");
        l.add(null, false, 3, new int[]{-1}, "<str>", "community");
        l.add(null, false, 2, new int[]{3}, "peerlrg", "match large community based on peer asn");
        l.add(null, false, 3, new int[]{-1}, "<str>", "community");
        l.add(null, false, 2, new int[]{3}, "stdcomm", "match standard community");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "community");
        l.add(null, false, 2, new int[]{3}, "extcomm", "match extended community");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "community");
        l.add(null, false, 2, new int[]{3}, "lrgcomm", "match large community");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "community");
        l.add(null, false, 2, new int[]{3}, "interface", "match interface");
        l.add(null, false, 3, new int[]{-1}, "<name:ifc>", "interface");
        l.add(null, false, 2, new int[]{3}, "nexthop", "match next hop");
        l.add(null, false, 3, new int[]{-1}, "<addr>", "address");
        l.add(null, false, 2, new int[]{3}, "recursive", "match old next hop");
        l.add(null, false, 3, new int[]{-1}, "<addr>", "address");
        l.add(null, false, 2, new int[]{3}, "protocol", "match source protocol");
        cfgRtr.getRouterList(l, 3, new int[]{-1}, "");
        cfgRtr.getRouterList(l, 1, "");
        l.add(null, false, 4, new int[]{-1}, "<num:rtr>", "process id");
        l.add(null, false, 2, new int[]{3}, "peerasn", "match peer asn");
        l.add(null, false, 3, new int[]{-1}, "<num>", "asn");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "distance", "match administrative distance");
        l.add(null, false, 3, new int[]{-1}, "<num>", "administrative distance");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "locpref", "match local preference");
        l.add(null, false, 3, new int[]{-1}, "<num>", "local preference");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "aigp", "match accumulated igp");
        l.add(null, false, 3, new int[]{-1}, "<num>", "aigp");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "validroa", "match roa validity status");
        l.add(null, false, 3, new int[]{-1}, "<num>", "validity");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "validaspa", "match aspa validity status");
        l.add(null, false, 3, new int[]{-1}, "<num>", "validity");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "aggregator", "match aggregator");
        l.add(null, false, 3, new int[]{-1}, "<num>", "asn");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "customer", "match customer");
        l.add(null, false, 3, new int[]{-1}, "<num>", "asn");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "destpref", "match destination preference");
        l.add(null, false, 3, new int[]{-1}, "<num>", "preference");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "pathlen", "match as path length");
        l.add(null, false, 3, new int[]{-1}, "<num>", "length");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "unknowns", "match number of unknown attributes");
        l.add(null, false, 3, new int[]{-1}, "<num>", "length");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "asend", "match as path ending");
        l.add(null, false, 3, new int[]{-1}, "<num>", "length");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "asbeg", "match as path beginning");
        l.add(null, false, 3, new int[]{-1}, "<num>", "length");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "asmid", "match as path middle");
        l.add(null, false, 3, new int[]{-1}, "<num>", "length");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "asany", "match as path anywhere");
        l.add(null, false, 3, new int[]{-1}, "<num>", "length");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "bandwidth", "match bandwidth");
        l.add(null, false, 3, new int[]{-1}, "<num>", "bandwidth");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "origin", "match origin type");
        l.add(null, false, 3, new int[]{-1}, "<num>", "origin");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "metric", "match metric");
        l.add(null, false, 3, new int[]{-1}, "<num>", "metric");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "tag", "match tag");
        l.add(null, false, 3, new int[]{-1}, "<num>", "tag");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "label-local", "match local label");
        l.add(null, false, 3, new int[]{-1}, "<num>", "label");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "label-remote", "match remote label");
        l.add(null, false, 3, new int[]{-1}, "<num>", "label");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "segrout", "match sr index");
        l.add(null, false, 3, new int[]{-1}, "<num>", "index");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "bier", "match bier index");
        l.add(null, false, 3, new int[]{-1}, "<num>", "index");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "srv6", "match srv6 prefix");
        l.add(null, false, 3, new int[]{-1}, "<addr>", "address");
        l.add(null, false, 2, new int[]{3}, "afi", "match afi");
        l.add(null, false, 3, new int[]{-1}, "<num>", "afi");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "safi", "match safi");
        l.add(null, false, 3, new int[]{-1}, "<num>", "safi");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "rd", "match route distinguisher");
        l.add(null, false, 3, new int[]{-1}, "<str>", "rd");
        l.add(null, false, 2, new int[]{3}, "network", "match network");
        l.add(null, false, 3, new int[]{4, -1}, "<net/mask>", "network in perfix/mask format");
        l.add(null, false, 4, new int[]{5}, "ge", "minimum prefix length to be matched");
        l.add(null, false, 5, new int[]{4, -1}, "<num>", "minimum prefix length");
        l.add(null, false, 4, new int[]{5}, "le", "maximum prefix length to be matched");
        l.add(null, false, 5, new int[]{4, -1}, "<num>", "maximum prefix length");
        l.add(null, false, 2, new int[]{-1}, "nostdcomm", "match empty standard community");
        l.add(null, false, 2, new int[]{-1}, "noextcomm", "match empty extended community");
        l.add(null, false, 2, new int[]{-1}, "nolrgcomm", "match empty large community");
        l.add(null, false, 2, new int[]{-1}, "privateas", "match private asn");
        l.add(null, false, 2, new int[]{-1}, "entropy", "match entropy label");
        l.add(null, false, 2, new int[]{3}, "tracker", "match tracker state");
        l.add(null, false, 3, new int[]{-1}, "<name:trk>", "name of tracker");
        l.add(null, false, 2, new int[]{3}, "access-list", "match access list");
        l.add(null, false, 3, new int[]{-1}, "<name:acl>", "name of access list");
        l.add(null, false, 2, new int[]{3}, "prefix-list", "match prefix list");
        l.add(null, false, 3, new int[]{-1}, "<name:pl>", "name of prefix list");
        l.add(null, false, 2, new int[]{3}, "route-map", "match route map");
        l.add(null, false, 3, new int[]{-1}, "<name:rm>", "name of route map");
        l.add(null, false, 2, new int[]{3}, "route-policy", "match route policy");
        l.add(null, false, 3, new int[]{-1}, "<name:rpl>", "name of route policy");
        l.add(null, false, 1, new int[]{2}, "clear", "clear values in destination routing protocol");
        l.add(null, false, 2, new int[]{3}, "stdcomm", "clear standard community");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "regexp to match");
        l.add(null, false, 2, new int[]{3}, "extcomm", "clear extended community");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "regexp to match");
        l.add(null, false, 2, new int[]{3}, "lrgcomm", "clear large community");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "regexp to match");
        l.add(null, false, 2, new int[]{-1}, "privateas", "clear private asn");
        l.add(null, false, 2, new int[]{-1}, "entropy", "clear entropy label");
        l.add(null, true, 2, new int[]{3}, "originator", "clear originator");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "regexp to match");
        l.add(null, true, 2, new int[]{3}, "clustlist", "clear cluster list");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "regexp to match");
        l.add(null, true, 2, new int[]{-1}, "peeras", "clear peer asn");
        l.add(null, true, 2, new int[]{3}, "exactas", "clear exact asn");
        l.add(null, true, 3, new int[]{-1}, "<num>", "as number to remove");
        l.add(null, true, 2, new int[]{-1}, "firstas", "clear first asn");
        l.add(null, false, 1, new int[]{2}, "set", "set values in destination routing protocol");
        l.add(null, false, 2, new int[]{3}, "rd", "set route distinguisher");
        l.add(null, false, 3, new int[]{-1}, "<str>", "rd");
        l.add(null, false, 2, new int[]{3}, "aspath", "prepend as path");
        l.add(null, false, 3, new int[]{3, -1}, "<num>", "as to prepend");
        l.add(null, false, 2, new int[]{3}, "asconfed", "prepend as path");
        l.add(null, false, 3, new int[]{3, -1}, "<num>", "as to prepend");
        l.add(null, false, 2, new int[]{3}, "stdcomm", "add standard community");
        l.add(null, false, 3, new int[]{3, -1}, "<num>", "community");
        l.add(null, false, 2, new int[]{3}, "extcomm", "add extended community");
        l.add(null, false, 3, new int[]{3, -1}, "<num>", "community");
        l.add(null, false, 2, new int[]{3}, "lrgcomm", "add large community");
        l.add(null, false, 3, new int[]{3, -1}, "<num>", "community");
        l.add(null, false, 2, new int[]{3}, "nexthop", "set next hop");
        l.add(null, false, 3, new int[]{-1}, "<addr>", "address");
        l.add(null, false, 2, new int[]{3}, "vrf", "set vrf");
        l.add(null, false, 3, new int[]{4}, "<name:vrf>", "name of vrf");
        l.add(null, false, 4, new int[]{-1}, "ipv4", "select ipv4");
        l.add(null, false, 4, new int[]{-1}, "ipv6", "select ipv6");
        l.add(null, false, 2, new int[]{3}, "distance", "set administrative distance");
        l.add(null, false, 3, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "locpref", "set local preference");
        l.add(null, false, 3, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "aigp", "set accumulated igp");
        l.add(null, false, 3, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "validroa", "set roa validity status");
        l.add(null, false, 3, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "validaspa", "set aspa validity status");
        l.add(null, false, 3, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "aggregator", "set aggregator");
        l.add(null, false, 3, new int[]{4}, "leave", "leave value unchanged");
        l.add(null, false, 4, new int[]{-1}, "<addr>", "address");
        l.add(null, false, 3, new int[]{4}, "<num>", "asn");
        l.add(null, false, 4, new int[]{-1}, "<addr>", "address");
        l.add(null, false, 2, new int[]{3}, "connector", "set connector");
        l.add(null, false, 3, new int[]{-1}, "<addr>", "address");
        l.add(null, false, 2, new int[]{3}, "pathlimit", "set as path limit");
        l.add(null, false, 3, new int[]{4}, "leave", "leave value unchanged");
        l.add(null, false, 4, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 4, new int[]{-1}, "<num>", "asn");
        l.add(null, false, 3, new int[]{4}, "<num>", "limit");
        l.add(null, false, 4, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 4, new int[]{-1}, "<num>", "asn");
        l.add(null, false, 2, new int[]{3}, "customer", "set customer");
        l.add(null, false, 3, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 3, new int[]{-1}, "<num>", "asn");
        l.add(null, false, 2, new int[]{3}, "destpref", "set as destination preference");
        l.add(null, false, 3, new int[]{4}, "leave", "leave value unchanged");
        l.add(null, false, 4, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 4, new int[]{-1}, "<num>", "asn");
        l.add(null, false, 3, new int[]{4}, "<num>", "preference");
        l.add(null, false, 4, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 4, new int[]{-1}, "<num>", "asn");
        l.add(null, false, 2, new int[]{3}, "bandwidth", "set bandwidth");
        l.add(null, false, 3, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "origin", "set origin");
        l.add(null, false, 3, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "metric", "set metric");
        l.add(null, false, 3, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "tag", "set tag");
        l.add(null, false, 3, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "label-local", "set local label");
        l.add(null, false, 3, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "label-remote", "set remote label");
        l.add(null, false, 3, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "segrout", "set sr index");
        l.add(null, false, 3, new int[]{-1}, "leave", "leave value unchanged");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 2, new int[]{3}, "bier", "set bier index");
        l.add(null, false, 3, new int[]{4}, "leave", "leave index unchanged");
        l.add(null, false, 4, new int[]{-1}, "leave", "leave subdomain unchanged");
        l.add(null, false, 4, new int[]{-1}, "<num>", "subdomain");
        l.add(null, false, 3, new int[]{4}, "<num>", "index");
        l.add(null, false, 4, new int[]{-1}, "leave", "leave subdomain unchanged");
        l.add(null, false, 4, new int[]{-1}, "<num>", "subdomain");
        l.add(null, false, 2, new int[]{3}, "srv6", "set srv6 prefix");
        l.add(null, false, 3, new int[]{-1}, "<addr>", "address");
        l.add(null, false, 2, new int[]{3}, "route-map", "set route map");
        l.add(null, false, 3, new int[]{-1}, "<name:rm>", "name of route map");
        l.add(null, false, 2, new int[]{3}, "route-policy", "set route policy");
        l.add(null, false, 3, new int[]{-1}, "<name:rpl>", "name of route policy");
    }

    public synchronized void doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("sequence")) {
            seq = bits.str2num(cmd.word());
            a = cmd.word();
            if (a.length() < 1) {
                return;
            }
        }
        if (a.equals("action")) {
            tabRtrmapN ntry = getCurr();
            ntry.action = tabListingEntry.string2action(cmd.word());
            return;
        }
        if (a.equals("description")) {
            tabRtrmapN ntry = getCurr();
            ntry.description = cmd.getRemaining();
            return;
        }
        if (a.equals("rename")) {
            a = cmd.word();
            cfgRoump v = cfgAll.rtmpFind(a, false);
            if (v != null) {
                cmd.error("already exists");
                return;
            }
            name = a;
            roumap.listName = a;
            return;
        }
        if (a.equals("tcldel")) {
            tabRtrmapN ntry = getCurr();
            ntry.script = null;
            return;
        }
        if (a.equals("tcladd")) {
            tabRtrmapN ntry = getCurr();
            if (ntry.script == null) {
                ntry.script = new ArrayList<String>();
            }
            ntry.script.add(cmd.getRemaining());
            return;
        }
        if (a.equals("tcledit")) {
            tabRtrmapN ntry = getCurr();
            List<String> txt = new ArrayList<String>();
            if (ntry.script != null) {
                txt.addAll(ntry.script);
            }
            userEditor e = new userEditor(new pipeScreen(cmd.pipe), txt, "route-map", false);
            if (e.doEdit()) {
                return;
            }
            ntry.script = txt;
            return;
        }
        if (a.equals("log")) {
            tabRtrmapN ntry = getCurr();
            ntry.logMatch = true;
            return;
        }
        if (a.equals("match")) {
            a = cmd.word();
            tabRtrmapN ntry = getCurr();
            if (ntry.cfgDoMatch(a, cmd)) {
                cmd.badCmd();
                return;
            }
            return;
        }
        if (a.equals("clear")) {
            a = cmd.word();
            tabRtrmapN ntry = getCurr();
            if (ntry.cfgDoClear(a, cmd)) {
                cmd.badCmd();
                return;
            }
            return;
        }
        if (a.equals("set")) {
            a = cmd.word();
            tabRtrmapN ntry = getCurr();
            if (ntry.cfgDoSet(a, cmd)) {
                cmd.badCmd();
                return;
            }
            return;
        }
        if (a.equals("reindex")) {
            int i = bits.str2num(cmd.word());
            roumap.reindex(i, bits.str2num(cmd.word()));
            return;
        }
        if (!a.equals(cmds.negated)) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        if (a.equals("description")) {
            tabRtrmapN ntry = getCurr();
            ntry.description = "";
            return;
        }
        if (a.equals("log")) {
            tabRtrmapN ntry = getCurr();
            ntry.logMatch = false;
            return;
        }
        if (a.equals("match")) {
            a = cmd.word();
            tabRtrmapN ntry = getCurr();
            ntry.cfgNoMatch(a, cmd);
            return;
        }
        if (a.equals("clear")) {
            a = cmd.word();
            tabRtrmapN ntry = getCurr();
            ntry.cfgNoClear(a, cmd);
            return;
        }
        if (a.equals("set")) {
            a = cmd.word();
            tabRtrmapN ntry = getCurr();
            ntry.cfgNoSet(a, cmd);
            return;
        }
        if (a.equals("sequence")) {
            tabRtrmapN ntry = new tabRtrmapN();
            ntry.sequence = bits.str2num(cmd.word());
            if (roumap.del(ntry)) {
                cmd.error("invalid sequence");
                return;
            }
            return;
        }
        cmd.badCmd();
    }

    public int compareTo(cfgRoump o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
    }

    public String getPrompt() {
        return "roump";
    }

    public String toString() {
        return name;
    }

}
