package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabListingEntry;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.tab.tabRouteUtil;
import org.freertr.tab.tabRtrplc;
import org.freertr.tab.tabRtrplcN;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * route policy configuration
 *
 * @author matecsaba
 */
public class cfgRouplc implements Comparable<cfgRouplc>, cfgGeneric {

    /**
     * name of routepolicy
     */
    public String name;

    /**
     * current sequence number
     */
    public int seq;

    /**
     * list of routepolicies
     */
    public tabListing<tabRtrplcN, addrIP> rouplc;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {};

    /**
     * create new route policy
     *
     * @param s name
     */
    public cfgRouplc(String s) {
        rouplc = new tabListing<tabRtrplcN, addrIP>();
        seq = rouplc.nextseq();
        name = s;
        rouplc.listName = s;
    }

    /**
     * get current entry
     *
     * @return current entry
     */
    public synchronized tabRtrplcN getCurr() {
        tabRtrplcN ntry = new tabRtrplcN();
        ntry.sequence = seq;
        ntry = rouplc.find(ntry);
        if (ntry != null) {
            seq = rouplc.nextseq();
            return ntry;
        }
        ntry = new tabRtrplcN();
        ntry.sequence = seq;
        ntry.action = tabListingEntry.actionType.actPermit;
        rouplc.add(ntry);
        seq = rouplc.nextseq();
        return ntry;
    }

    /**
     * indent table
     */
    public synchronized void indent() {
        tabRtrplc.indent(rouplc);
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("route-policy " + name);
        l.addAll(rouplc.dump(cmds.tabulator, filter));
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
        l.add(null, false, 1, new int[]{-1}, "next", "no operation");
        l.add(null, false, 1, new int[]{2, -1}, "description", "description of this route policy");
        l.add(null, false, 2, new int[]{2, -1}, "[text]", "text describing this route policy");
        l.add(null, false, 1, new int[]{2}, "rename", "rename this route policy");
        l.add(null, false, 2, new int[]{-1}, "<str>", "set new name");
        l.add(null, false, 1, new int[]{2}, "if", "match values from source routing protocol");
        l.add(null, false, 1, new int[]{2}, "elsif", "match values from source routing protocol");
        l.add(null, false, 2, new int[]{-1}, "always", "match always");
        l.add(null, false, 2, new int[]{-1}, "never", "match never");
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
        l.add(null, false, 2, new int[]{3}, "distance", "match administrative distance");
        l.add(null, false, 3, new int[]{-1}, "<num>", "administrative distance");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "peerasn", "match peer asn");
        l.add(null, false, 3, new int[]{-1}, "<num>", "asn");
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
        l.add(null, false, 1, new int[]{-1}, "enif", "end if statement");
        l.add(null, false, 1, new int[]{-1}, "else", "begin else statement");
        l.add(null, false, 1, new int[]{-1}, "pass", "permit the prefix");
        l.add(null, false, 1, new int[]{-1}, "drop", "deny the prefix");
        l.add(null, false, 1, new int[]{-1}, "log", "set logging on match");
        l.add(null, false, 1, new int[]{2}, "tcl", "add tcl line");
        l.add(null, false, 2, new int[]{2, -1}, "[text]", "script");
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
        l.add(null, false, 2, new int[]{3}, "vrf", "set vrf");
        l.add(null, false, 3, new int[]{4}, "<name:vrf>", "name of vrf");
        l.add(null, false, 4, new int[]{-1}, "ipv4", "select ipv4");
        l.add(null, false, 4, new int[]{-1}, "ipv6", "select ipv6");
        l.add(null, false, 2, new int[]{3}, "nexthop", "set next hop");
        l.add(null, false, 3, new int[]{-1}, "<addr>", "address");
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

    private void doIfStr(cmds cmd, tabRtrplcN ntry) {
        String a = cmd.word();
        if (a.equals("never")) {
            ntry.ifMode = tabRtrplcN.ifType.never;
            return;
        }
        if (a.equals("always")) {
            ntry.ifMode = tabRtrplcN.ifType.always;
            return;
        }
        if (a.equals("aspath")) {
            ntry.ifMode = tabRtrplcN.ifType.aspath;
            ntry.strVal = cmd.getRemaining();
            return;
        }
        if (a.equals("peerstd")) {
            ntry.ifMode = tabRtrplcN.ifType.peerstd;
            ntry.intLst = tabRouteUtil.string2stdComms(cmd.getRemaining());
            return;
        }
        if (a.equals("peerlrg")) {
            ntry.ifMode = tabRtrplcN.ifType.peerlrg;
            ntry.lrgLst = tabRouteUtil.string2lrgComms(cmd.getRemaining());
            return;
        }
        if (a.equals("stdcomm")) {
            ntry.ifMode = tabRtrplcN.ifType.stdcomm;
            ntry.intLst = tabRouteUtil.string2stdComms(cmd.getRemaining());
            return;
        }
        if (a.equals("extcomm")) {
            ntry.ifMode = tabRtrplcN.ifType.extcomm;
            ntry.lngLst = tabRouteUtil.string2extComms(cmd.getRemaining());
            return;
        }
        if (a.equals("lrgcomm")) {
            ntry.ifMode = tabRtrplcN.ifType.lrgcomm;
            ntry.lrgLst = tabRouteUtil.string2lrgComms(cmd.getRemaining());
            return;
        }
        if (a.equals("rd")) {
            ntry.ifMode = tabRtrplcN.ifType.roudst;
            ntry.longVal = tabRouteUtil.string2rd(cmd.word());
            return;
        }
        if (a.equals("network")) {
            ntry.ifMode = tabRtrplcN.ifType.network;
            ntry.networkMatch = new tabPrfxlstN();
            ntry.networkMatch.action = tabListingEntry.actionType.actPermit;
            if (ntry.networkMatch.fromString(cmd.getRemaining())) {
                ntry.networkMatch = null;
                cmd.error("invalid prefix");
                return;
            }
            return;
        }
        if (a.equals("nostdcomm")) {
            ntry.ifMode = tabRtrplcN.ifType.nostdcomm;
            return;
        }
        if (a.equals("noextcomm")) {
            ntry.ifMode = tabRtrplcN.ifType.noextcomm;
            return;
        }
        if (a.equals("nolrgcomm")) {
            ntry.ifMode = tabRtrplcN.ifType.nolrgcomm;
            return;
        }
        if (a.equals("privateas")) {
            ntry.ifMode = tabRtrplcN.ifType.privas;
            return;
        }
        if (a.equals("entropy")) {
            ntry.ifMode = tabRtrplcN.ifType.entropy;
            return;
        }
        if (a.equals("tracker")) {
            ntry.ifMode = tabRtrplcN.ifType.track;
            ntry.strVal = cmd.word();
            return;
        }
        if (a.equals("access-list")) {
            ntry.ifMode = tabRtrplcN.ifType.aceslst;
            cfgAceslst acl = cfgAll.aclsFind(cmd.word(), false);
            if (acl == null) {
                cmd.error("no such access list");
                return;
            }
            ntry.aceslst = acl.aceslst;
            return;
        }
        if (a.equals("prefix-list")) {
            ntry.ifMode = tabRtrplcN.ifType.prfxlst;
            cfgPrfxlst prfxlst = cfgAll.prfxFind(cmd.word(), false);
            if (prfxlst == null) {
                cmd.error("no such prefix list");
                return;
            }
            ntry.prfxlst = prfxlst.prflst;
            return;
        }
        if (a.equals("route-map")) {
            ntry.ifMode = tabRtrplcN.ifType.roumap;
            cfgRoump roumap = cfgAll.rtmpFind(cmd.word(), false);
            if (roumap == null) {
                cmd.error("no such route map");
                return;
            }
            ntry.roumap = roumap.roumap;
            return;
        }
        if (a.equals("route-policy")) {
            ntry.ifMode = tabRtrplcN.ifType.rouplc;
            cfgRouplc roumap = cfgAll.rtplFind(cmd.word(), false);
            if (roumap == null) {
                cmd.error("no such route policy");
                return;
            }
            ntry.rouplc = roumap.rouplc;
            return;
        }
        if (a.equals("peerasn")) {
            ntry.ifMode = tabRtrplcN.ifType.peerasn;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("distance")) {
            ntry.ifMode = tabRtrplcN.ifType.distance;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("metric")) {
            ntry.ifMode = tabRtrplcN.ifType.metric;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("origin")) {
            ntry.ifMode = tabRtrplcN.ifType.origin;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("locpref")) {
            ntry.ifMode = tabRtrplcN.ifType.locpref;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("aigp")) {
            ntry.ifMode = tabRtrplcN.ifType.accigp;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("validroa")) {
            ntry.ifMode = tabRtrplcN.ifType.validroa;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("validaspa")) {
            ntry.ifMode = tabRtrplcN.ifType.validaspa;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("aggregator")) {
            ntry.ifMode = tabRtrplcN.ifType.aggregator;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("customer")) {
            ntry.ifMode = tabRtrplcN.ifType.customer;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("destpref")) {
            ntry.ifMode = tabRtrplcN.ifType.destPref;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("pathlen")) {
            ntry.ifMode = tabRtrplcN.ifType.pathlen;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("unknowns")) {
            ntry.ifMode = tabRtrplcN.ifType.unknown;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("asend")) {
            ntry.ifMode = tabRtrplcN.ifType.asend;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("asbeg")) {
            ntry.ifMode = tabRtrplcN.ifType.asbeg;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("asmid")) {
            ntry.ifMode = tabRtrplcN.ifType.asmid;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("asany")) {
            ntry.ifMode = tabRtrplcN.ifType.asany;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("bandwidth")) {
            ntry.ifMode = tabRtrplcN.ifType.bandwidth;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("tag")) {
            ntry.ifMode = tabRtrplcN.ifType.tag;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("label-local")) {
            ntry.ifMode = tabRtrplcN.ifType.labloc;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("label-remote")) {
            ntry.ifMode = tabRtrplcN.ifType.labrem;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("segrout")) {
            ntry.ifMode = tabRtrplcN.ifType.segrou;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("bier")) {
            ntry.ifMode = tabRtrplcN.ifType.bier;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("srv6")) {
            ntry.ifMode = tabRtrplcN.ifType.srv6;
            ntry.addrSet = new addrIP();
            if (ntry.addrSet.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("afi")) {
            ntry.ifMode = tabRtrplcN.ifType.afi;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("safi")) {
            ntry.ifMode = tabRtrplcN.ifType.safi;
            if (ntry.intMatch.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("interface")) {
            ntry.ifMode = tabRtrplcN.ifType.iface;
            ntry.ifaceMatch = cfgAll.ifcFind(cmd.getRemaining(), 0);
            if (ntry.ifaceMatch == null) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("nexthop")) {
            ntry.ifMode = tabRtrplcN.ifType.nexthop;
            ntry.addrSet = new addrIP();
            if (ntry.addrSet.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("recursive")) {
            ntry.ifMode = tabRtrplcN.ifType.recursive;
            ntry.addrSet = new addrIP();
            if (ntry.addrSet.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
            }
            return;
        }
        if (a.equals("protocol")) {
            ntry.ifMode = tabRtrplcN.ifType.protocol;
            ntry.protoMatch = cfgRtr.name2num(cmd.word());
            if (ntry.protoMatch == null) {
                cmd.error("invalid protocol");
                return;
            }
            ntry.intVal = bits.str2num(cmd.word());
            if (!cfgRtr.num2proc(ntry.protoMatch)) {
                ntry.intVal = -1;
            }
            return;
        }
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
        if (a.equals("next")) {
            tabRtrplcN ntry = getCurr();
            ntry.doMode = tabRtrplcN.doType.next;
            indent();
            return;
        }
        if (a.equals("description")) {
            tabRtrplcN ntry = getCurr();
            ntry.doMode = tabRtrplcN.doType.description;
            indent();
            ntry.description = cmd.getRemaining();
            return;
        }
        if (a.equals("rename")) {
            a = cmd.word();
            cfgRouplc v = cfgAll.rtplFind(a, false);
            if (v != null) {
                cmd.error("already exists");
                return;
            }
            name = a;
            rouplc.listName = a;
            return;
        }
        if (a.equals("if")) {
            tabRtrplcN ntry = getCurr();
            ntry.doMode = tabRtrplcN.doType.iff;
            indent();
            doIfStr(cmd, ntry);
            return;
        }
        if (a.equals("elsif")) {
            tabRtrplcN ntry = getCurr();
            ntry.doMode = tabRtrplcN.doType.elsif;
            indent();
            doIfStr(cmd, ntry);
            return;
        }
        if (a.equals("else")) {
            tabRtrplcN ntry = getCurr();
            ntry.doMode = tabRtrplcN.doType.els;
            indent();
            doIfStr(cmd, ntry);
            return;
        }
        if (a.equals("enif")) {
            tabRtrplcN ntry = getCurr();
            ntry.doMode = tabRtrplcN.doType.enif;
            indent();
            doIfStr(cmd, ntry);
            return;
        }
        if (a.equals("pass")) {
            tabRtrplcN ntry = getCurr();
            ntry.doMode = tabRtrplcN.doType.pass;
            indent();
            doIfStr(cmd, ntry);
            return;
        }
        if (a.equals("drop")) {
            tabRtrplcN ntry = getCurr();
            ntry.doMode = tabRtrplcN.doType.drop;
            indent();
            doIfStr(cmd, ntry);
            return;
        }
        if (a.equals("log")) {
            tabRtrplcN ntry = getCurr();
            ntry.doMode = tabRtrplcN.doType.log;
            indent();
            doIfStr(cmd, ntry);
            return;
        }
        if (a.equals("tcl")) {
            tabRtrplcN ntry = getCurr();
            ntry.doMode = tabRtrplcN.doType.tcl;
            ntry.strVal = cmd.getRemaining();
            indent();
            doIfStr(cmd, ntry);
            return;
        }
        if (a.equals("clear")) {
            a = cmd.word();
            tabRtrplcN ntry = getCurr();
            indent();
            if (a.equals("stdcomm")) {
                ntry.doMode = tabRtrplcN.doType.clrStdcomm;
                ntry.strVal = cmd.getRemaining();
                return;
            }
            if (a.equals("extcomm")) {
                ntry.doMode = tabRtrplcN.doType.clrExtcomm;
                ntry.strVal = cmd.getRemaining();
                return;
            }
            if (a.equals("lrgcomm")) {
                ntry.doMode = tabRtrplcN.doType.clrLrgcomm;
                ntry.strVal = cmd.getRemaining();
                return;
            }
            if (a.equals("originator")) {
                ntry.doMode = tabRtrplcN.doType.clrOrgntr;
                ntry.strVal = cmd.getRemaining();
                return;
            }
            if (a.equals("clustlist")) {
                ntry.doMode = tabRtrplcN.doType.clrClstlst;
                ntry.strVal = cmd.getRemaining();
                return;
            }
            if (a.equals("privateas")) {
                ntry.doMode = tabRtrplcN.doType.clrPrivas;
                return;
            }
            if (a.equals("entropy")) {
                ntry.doMode = tabRtrplcN.doType.clrEntropy;
                return;
            }
            if (a.equals("peeras")) {
                ntry.doMode = tabRtrplcN.doType.clrPeeras;
                return;
            }
            if (a.equals("exactas")) {
                ntry.doMode = tabRtrplcN.doType.clrExactas;
                ntry.intVal = bits.str2num(cmd.word());
                return;
            }
            if (a.equals("firstas")) {
                ntry.doMode = tabRtrplcN.doType.clrFirstas;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("set")) {
            a = cmd.word();
            tabRtrplcN ntry = getCurr();
            indent();
            if (a.equals("rd")) {
                ntry.doMode = tabRtrplcN.doType.setRoudst;
                ntry.longVal = tabRouteUtil.string2rd(cmd.word());
                return;
            }
            if (a.equals("stdcomm")) {
                ntry.doMode = tabRtrplcN.doType.setStdcomm;
                ntry.intLst = tabRouteUtil.string2stdComms(cmd.getRemaining());
                return;
            }
            if (a.equals("extcomm")) {
                ntry.doMode = tabRtrplcN.doType.setExtcomm;
                ntry.lngLst = tabRouteUtil.string2extComms(cmd.getRemaining());
                return;
            }
            if (a.equals("lrgcomm")) {
                ntry.doMode = tabRtrplcN.doType.setLrgcomm;
                ntry.lrgLst = tabRouteUtil.string2lrgComms(cmd.getRemaining());
                return;
            }
            if (a.equals("vrf")) {
                ntry.doMode = tabRtrplcN.doType.setVrf;
                cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
                if (vrf == null) {
                    cmd.error("no such vrf");
                    return;
                }
                a = cmd.word();
                if (a.equals("ipv4")) {
                    ntry.vrfSetF = vrf.fwd4;
                    ntry.vrfSetT = true;
                } else {
                    ntry.vrfSetF = vrf.fwd6;
                    ntry.vrfSetT = false;
                }
                return;
            }
            if (a.equals("nexthop")) {
                ntry.doMode = tabRtrplcN.doType.setNexthop;
                ntry.addrSet = new addrIP();
                ntry.addrSet.fromString(cmd.getRemaining());
                return;
            }
            if (a.equals("aspath")) {
                ntry.doMode = tabRtrplcN.doType.setAspath;
                ntry.intLst = tabRouteUtil.string2intList(cmd.getRemaining());
                return;
            }
            if (a.equals("asconfed")) {
                ntry.doMode = tabRtrplcN.doType.setAsconf;
                ntry.intLst = tabRouteUtil.string2intList(cmd.getRemaining());
                return;
            }
            if (a.equals("distance")) {
                ntry.doMode = tabRtrplcN.doType.setDistance;
                if (ntry.intSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("metric")) {
                ntry.doMode = tabRtrplcN.doType.setMetric;
                if (ntry.intSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("origin")) {
                ntry.doMode = tabRtrplcN.doType.setOrigin;
                if (ntry.intSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("locpref")) {
                ntry.doMode = tabRtrplcN.doType.setLocPref;
                if (ntry.intSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("aigp")) {
                ntry.doMode = tabRtrplcN.doType.setAccIgp;
                if (ntry.intSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("validroa")) {
                ntry.doMode = tabRtrplcN.doType.setValidRoa;
                if (ntry.intSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("validaspa")) {
                ntry.doMode = tabRtrplcN.doType.setValidAspa;
                if (ntry.intSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("aggregator")) {
                ntry.doMode = tabRtrplcN.doType.setAggregator;
                a = cmd.word();
                if (ntry.intSet.fromString(a)) {
                    cmd.error("invalid action");
                    return;
                }
                ntry.addrSet = new addrIP();
                a = cmd.word();
                if (ntry.addrSet.fromString(a)) {
                    cmd.error("bad address");
                    return;
                }
                return;
            }
            if (a.equals("connector")) {
                ntry.doMode = tabRtrplcN.doType.setConnect;
                ntry.addrSet = new addrIP();
                a = cmd.word();
                if (ntry.addrSet.fromString(a)) {
                    cmd.error("bad address");
                    return;
                }
                return;
            }
            if (a.equals("pathlimit")) {
                ntry.doMode = tabRtrplcN.doType.setPathLimit;
                if (ntry.intSet.fromString(cmd.word())) {
                    cmd.error("invalid action");
                    return;
                }
                if (ntry.int2set.fromString(cmd.word())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("customer")) {
                ntry.doMode = tabRtrplcN.doType.setCustomer;
                if (ntry.intSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("destpref")) {
                ntry.doMode = tabRtrplcN.doType.setDestPref;
                if (ntry.intSet.fromString(cmd.word())) {
                    cmd.error("invalid action");
                    return;
                }
                if (ntry.int2set.fromString(cmd.word())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("bandwidth")) {
                ntry.doMode = tabRtrplcN.doType.setBandwidth;
                if (ntry.intSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("tag")) {
                ntry.doMode = tabRtrplcN.doType.setTag;
                if (ntry.intSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("label-local")) {
                ntry.doMode = tabRtrplcN.doType.setLabloc;
                if (ntry.intSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("label-remote")) {
                ntry.doMode = tabRtrplcN.doType.setLabrem;
                if (ntry.intSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("segrout")) {
                ntry.doMode = tabRtrplcN.doType.setSegrou;
                if (ntry.intSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("bier")) {
                ntry.doMode = tabRtrplcN.doType.setBier;
                if (ntry.intSet.fromString(cmd.word())) {
                    cmd.error("invalid action");
                    return;
                }
                if (ntry.int2set.fromString(cmd.word())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("srv6")) {
                ntry.doMode = tabRtrplcN.doType.setSrv6;
                ntry.addrSet = new addrIP();
                if (ntry.addrSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("route-map")) {
                ntry.doMode = tabRtrplcN.doType.setRoumap;
                cfgRoump roumap = cfgAll.rtmpFind(cmd.word(), false);
                if (roumap == null) {
                    cmd.error("no such route map");
                    return;
                }
                ntry.roumap = roumap.roumap;
                return;
            }
            if (a.equals("route-policy")) {
                ntry.doMode = tabRtrplcN.doType.setRouplc;
                cfgRouplc roumap = cfgAll.rtplFind(cmd.word(), false);
                if (roumap == null) {
                    cmd.error("no such route policy");
                    return;
                }
                ntry.rouplc = roumap.rouplc;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("reindex")) {
            int i = bits.str2num(cmd.word());
            rouplc.reindex(i, bits.str2num(cmd.word()));
            indent();
            return;
        }
        if (!a.equals(cmds.negated)) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        if (a.equals("sequence")) {
            tabRtrplcN ntry = new tabRtrplcN();
            ntry.sequence = bits.str2num(cmd.word());
            if (rouplc.del(ntry)) {
                cmd.error("invalid sequence");
                return;
            }
            indent();
            return;
        }
        cmd.badCmd();
    }

    public int compareTo(cfgRouplc o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
    }

    public String getPrompt() {
        return "rouplc";
    }

    public String toString() {
        return name;
    }

}
