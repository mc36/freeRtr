package org.freertr.cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabListingEntry;
import org.freertr.tab.tabRtrmapN;
import org.freertr.user.userEditor;
import org.freertr.user.userFilter;
import org.freertr.user.userHelping;
import org.freertr.user.userScreen;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * route map configuration
 *
 * @author matecsaba
 */
public class cfgRoump implements Comparator<cfgRoump>, cfgGeneric {

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
    public final static String[] defaultL = {
        "route-map .*! sequence .* description ",
        "route-map .*! sequence .* tcldel",
        "route-map .*! sequence .* no match access-list",
        "route-map .*! sequence .* no match prefix-list",
        "route-map .*! sequence .* no match route-map",
        "route-map .*! sequence .* no match route-policy",
        "route-map .*! sequence .* no match rd",
        "route-map .*! sequence .* no match network",
        "route-map .*! sequence .* no match aspath",
        "route-map .*! sequence .* no match peerstd",
        "route-map .*! sequence .* no match peerlrg",
        "route-map .*! sequence .* no match stdcomm",
        "route-map .*! sequence .* no match extcomm",
        "route-map .*! sequence .* no match lrgcomm",
        "route-map .*! sequence .* no match privateas",
        "route-map .*! sequence .* no match entropy",
        "route-map .*! sequence .* no match tracker",
        "route-map .*! sequence .* no match interface",
        "route-map .*! sequence .* no match nexthop",
        "route-map .*! sequence .* no match recursive",
        "route-map .*! sequence .* no match protocol",
        "route-map .*! sequence .* match peerasn all",
        "route-map .*! sequence .* match distance all",
        "route-map .*! sequence .* match locpref all",
        "route-map .*! sequence .* match aigp all",
        "route-map .*! sequence .* match validity all",
        "route-map .*! sequence .* match aggregator all",
        "route-map .*! sequence .* match customer all",
        "route-map .*! sequence .* match pathlen all",
        "route-map .*! sequence .* match unknowns all",
        "route-map .*! sequence .* match asend all",
        "route-map .*! sequence .* match asbeg all",
        "route-map .*! sequence .* match asmid all",
        "route-map .*! sequence .* match bandwidth all",
        "route-map .*! sequence .* match origin all",
        "route-map .*! sequence .* match metric all",
        "route-map .*! sequence .* match tag all",
        "route-map .*! sequence .* match label-local all",
        "route-map .*! sequence .* match label-remote all",
        "route-map .*! sequence .* match segrout all",
        "route-map .*! sequence .* match bier all",
        "route-map .*! sequence .* match afi all",
        "route-map .*! sequence .* match safi all",
        "route-map .*! sequence .* no match nostdcomm",
        "route-map .*! sequence .* no match noextcomm",
        "route-map .*! sequence .* no match nolrgcomm",
        "route-map .*! sequence .* no clear stdcomm",
        "route-map .*! sequence .* no clear extcomm",
        "route-map .*! sequence .* no clear lrgcomm",
        "route-map .*! sequence .* no clear originator",
        "route-map .*! sequence .* no clear clustlist",
        "route-map .*! sequence .* no clear privateas",
        "route-map .*! sequence .* no clear entropy",
        "route-map .*! sequence .* no clear peeras",
        "route-map .*! sequence .* no clear exactas",
        "route-map .*! sequence .* no clear firstas",
        "route-map .*! sequence .* no set rd",
        "route-map .*! sequence .* no set route-map",
        "route-map .*! sequence .* no set route-policy",
        "route-map .*! sequence .* no set aspath",
        "route-map .*! sequence .* no set asconfed",
        "route-map .*! sequence .* no set stdcomm",
        "route-map .*! sequence .* no set extcomm",
        "route-map .*! sequence .* no set lrgcomm",
        "route-map .*! sequence .* no set nexthop",
        "route-map .*! sequence .* no set vrf",
        "route-map .*! sequence .* set distance leave",
        "route-map .*! sequence .* set locpref leave",
        "route-map .*! sequence .* set aigp leave",
        "route-map .*! sequence .* set validity leave",
        "route-map .*! sequence .* set aggregator leave null",
        "route-map .*! sequence .* set connector null",
        "route-map .*! sequence .* set aslimit leave leave",
        "route-map .*! sequence .* set customer leave",
        "route-map .*! sequence .* set bandwidth leave",
        "route-map .*! sequence .* set origin leave",
        "route-map .*! sequence .* set metric leave",
        "route-map .*! sequence .* set tag leave",
        "route-map .*! sequence .* set label-local leave",
        "route-map .*! sequence .* set label-remote leave",
        "route-map .*! sequence .* set segrout leave",
        "route-map .*! sequence .* set bier leave leave",
        "route-map .*! sequence .* no log"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

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

    public void getHelp(userHelping l) {
        l.add(null, "1 2   sequence              sequence number of an entry");
        l.add(null, "2 1,.   <num>               sequence number");
        l.add(null, "1 2,. reindex               reindex route map");
        l.add(null, "2 3,.   [num]               initial number to start with");
        l.add(null, "3 4,.     [num]             increment number");
        l.add(null, "1 2   action                set action to do");
        l.add(null, "2 .     deny                specify to forbid");
        l.add(null, "2 .     permit              specify to allow");
        l.add(null, "1 2,. description           description of this route map");
        l.add(null, "2 2,.   [text]              text describing this route map");
        l.add(null, "1 2   rename                rename this route map");
        l.add(null, "2 .     <str>               set new name");
        l.add(null, "1 2   tcladd                add tcl line");
        l.add(null, "2 2,.   <str>               script");
        l.add(null, "1 .   tcldel                delete tcl script");
        l.add(null, "1 .   tcledit               edit tcl script");
        l.add(null, "1 .   log                   set logging on match");
        l.add(null, "1 2   match                 match values from source routing protocol");
        l.add(null, "2 3     aspath              match as path");
        l.add(null, "3 3,.     <str>             regexp against as path");
        l.add(null, "2 3     peerstd             match standard community based on peer asn");
        l.add(null, "3 .       <str>             community");
        l.add(null, "2 3     peerlrg             match large community based on peer asn");
        l.add(null, "3 .       <str>             community");
        l.add(null, "2 3     stdcomm             match standard community");
        l.add(null, "3 3,.     <str>             community");
        l.add(null, "2 3     extcomm             match extended community");
        l.add(null, "3 3,.     <str>             community");
        l.add(null, "2 3     lrgcomm             match large community");
        l.add(null, "3 3,.     <str>             community");
        l.add(null, "2 3     interface           match interface");
        l.add(null, "3 .       <name:ifc>        interface");
        l.add(null, "2 3     nexthop             match next hop");
        l.add(null, "3 .       <addr>            address");
        l.add(null, "2 3     recursive           match old next hop");
        l.add(null, "3 .       <addr>            address");
        l.add(null, "2 3     protocol            match source protocol");
        cfgRtr.getRouterList(l, "3 .", "");
        cfgRtr.getRouterList(l, 1, "");
        l.add(null, "4 .         <num:rtr>       process id");
        l.add(null, "2 3     peerasn             match peer asn");
        l.add(null, "3 .       <num>             asn");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     distance            match administrative distance");
        l.add(null, "3 .       <num>             administrative distance");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     locpref             match local preference");
        l.add(null, "3 .       <num>             local preference");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     aigp                match accumulated igp");
        l.add(null, "3 .       <num>             aigp");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     validity            match validity status");
        l.add(null, "3 .       <num>             validity");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     aggregator          match aggregator");
        l.add(null, "3 .       <num>             asn");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     customer            match customer");
        l.add(null, "3 .       <num>             asn");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     pathlen             match as path length");
        l.add(null, "3 .       <num>             length");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     unknowns            match number of unknown attributes");
        l.add(null, "3 .       <num>             length");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     asend               match as path ending");
        l.add(null, "3 .       <num>             length");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     asbeg               match as path beginning");
        l.add(null, "3 .       <num>             length");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     asmid               match as path middle");
        l.add(null, "3 .       <num>             length");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     bandwidth           match bandwidth");
        l.add(null, "3 .       <num>             bandwidth");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     origin              match origin type");
        l.add(null, "3 .       <num>             origin");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     metric              match metric");
        l.add(null, "3 .       <num>             metric");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     tag                 match tag");
        l.add(null, "3 .       <num>             tag");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     label-local         match local label");
        l.add(null, "3 .       <num>             label");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     label-remote        match remote label");
        l.add(null, "3 .       <num>             label");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     segrout             match sr index");
        l.add(null, "3 .       <num>             index");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     bier                match bier index");
        l.add(null, "3 .       <num>             index");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     afi                 match afi");
        l.add(null, "3 .       <num>             afi");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     safi                match safi");
        l.add(null, "3 .       <num>             safi");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     rd                  match route distinguisher");
        l.add(null, "3 .       <str>             rd");
        l.add(null, "2 3     network             match network");
        l.add(null, "3 4,.     <net/mask>        network in perfix/mask format");
        l.add(null, "4 5         ge              minimum prefix length to be matched");
        l.add(null, "5 4,.         <num>         minimum prefix length");
        l.add(null, "4 5         le              maximum prefix length to be matched");
        l.add(null, "5 4,.         <num>         maximum prefix length");
        l.add(null, "2 .     nostdcomm           match empty standard community");
        l.add(null, "2 .     noextcomm           match empty extended community");
        l.add(null, "2 .     nolrgcomm           match empty large community");
        l.add(null, "2 .     privateas           match private asn");
        l.add(null, "2 .     entropy             match entropy label");
        l.add(null, "2 3     tracker             match tracker state");
        l.add(null, "3 .       <name:trk>        name of tracker");
        l.add(null, "2 3     access-list         match access list");
        l.add(null, "3 .       <name:acl>        name of access list");
        l.add(null, "2 3     prefix-list         match prefix list");
        l.add(null, "3 .       <name:pl>         name of prefix list");
        l.add(null, "2 3     route-map           match route map");
        l.add(null, "3 .       <name:rm>         name of route map");
        l.add(null, "2 3     route-policy        match route policy");
        l.add(null, "3 .       <name:rpl>        name of route policy");
        l.add(null, "1 2   clear                 clear values in destination routing protocol");
        l.add(null, "2 3     stdcomm             clear standard community");
        l.add(null, "3 3,.     <str>             regexp to match");
        l.add(null, "2 3     extcomm             clear extended community");
        l.add(null, "3 3,.     <str>             regexp to match");
        l.add(null, "2 3     lrgcomm             clear large community");
        l.add(null, "3 3,.     <str>             regexp to match");
        l.add(null, "2 .     privateas           clear private asn");
        l.add(null, "2 .     entropy             clear entropy label");
        l.add(null, ".2 3    originator          clear originator");
        l.add(null, "3 3,.     <str>             regexp to match");
        l.add(null, ".2 3    clustlist           clear cluster list");
        l.add(null, "3 3,.     <str>             regexp to match");
        l.add(null, ".2 .    peeras              clear peer asn");
        l.add(null, ".2 3    exactas             clear exact asn");
        l.add(null, ".3 .      <num>             as number to remove");
        l.add(null, ".2 .    firstas             clear first asn");
        l.add(null, "1 2   set                   set values in destination routing protocol");
        l.add(null, "2 3     rd                  set route distinguisher");
        l.add(null, "3 .       <str>             rd");
        l.add(null, "2 3     aspath              prepend as path");
        l.add(null, "3 3,.     <num>             as to prepend");
        l.add(null, "2 3     asconfed            prepend as path");
        l.add(null, "3 3,.     <num>             as to prepend");
        l.add(null, "2 3     stdcomm             add standard community");
        l.add(null, "3 3,.     <num>             community");
        l.add(null, "2 3     extcomm             add extended community");
        l.add(null, "3 3,.     <num>             community");
        l.add(null, "2 3     lrgcomm             add large community");
        l.add(null, "3 3,.     <num>             community");
        l.add(null, "2 3     nexthop             set next hop");
        l.add(null, "3 .       <addr>            address");
        l.add(null, "2 3     vrf                 set vrf");
        l.add(null, "3 4       <name:vrf>        name of vrf");
        l.add(null, "4 .         ipv4            select ipv4");
        l.add(null, "4 .         ipv6            select ipv6");
        l.add(null, "2 3     distance            set administrative distance");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 .       <num>             value");
        l.add(null, "2 3     locpref             set local preference");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 .       <num>             value");
        l.add(null, "2 3     aigp                set accumulated igp");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 .       <num>             value");
        l.add(null, "2 3     validity            set validity status");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 .       <num>             value");
        l.add(null, "2 3     aggregator          set aggregator");
        l.add(null, "3 4       leave             leave value unchanged");
        l.add(null, "4 .         <addr>          address");
        l.add(null, "3 4       <num>             asn");
        l.add(null, "4 .         <addr>          address");
        l.add(null, "2 3     connector           set connector");
        l.add(null, "3 .       <addr>            address");
        l.add(null, "2 3     aslimit             set as path limit");
        l.add(null, "3 4       leave             leave value unchanged");
        l.add(null, "4 .         leave           leave value unchanged");
        l.add(null, "4 .         <num>           asn");
        l.add(null, "3 4       <num>             limit");
        l.add(null, "4 .         leave           leave value unchanged");
        l.add(null, "4 .         <num>           asn");
        l.add(null, "2 3     customer            set customer");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 .       <num>             asn");
        l.add(null, "2 3     bandwidth           set bandwidth");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 .       <num>             value");
        l.add(null, "2 3     origin              set origin");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 .       <num>             value");
        l.add(null, "2 3     metric              set metric");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 .       <num>             value");
        l.add(null, "2 3     tag                 set tag");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 .       <num>             value");
        l.add(null, "2 3     label-local         set local label");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 .       <num>             value");
        l.add(null, "2 3     label-remote        set remote label");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 .       <num>             value");
        l.add(null, "2 3     segrout             set sr index");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 .       <num>             value");
        l.add(null, "2 3     bier                set bier index");
        l.add(null, "3 4       leave             leave index unchanged");
        l.add(null, "4 .         leave           leave subdomain unchanged");
        l.add(null, "4 .         <num>           subdomain");
        l.add(null, "3 4       <num>             index");
        l.add(null, "4 .         leave           leave subdomain unchanged");
        l.add(null, "4 .         <num>           subdomain");
        l.add(null, "2 3     route-map           set route map");
        l.add(null, "3 .       <name:rm>         name of route map");
        l.add(null, "2 3     route-policy        set route policy");
        l.add(null, "3 .       <name:rpl>        name of route policy");
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
            userEditor e = new userEditor(new userScreen(cmd.pipe), txt, "route-map", false);
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

    public int compare(cfgRoump o1, cfgRoump o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String getPrompt() {
        return "roump";
    }

    public String toString() {
        return name;
    }

}
