package net.freertr.cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabLargeComm;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabListingEntry;
import net.freertr.tab.tabPrfxlstN;
import net.freertr.tab.tabRouteUtil;
import net.freertr.tab.tabRtrmapN;
import net.freertr.user.userEditor;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.user.userScreen;
import net.freertr.util.bits;
import net.freertr.util.cmds;

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
        "route-map .*! sequence .* match pathlen all",
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
        "route-map .*! sequence .* no clear clustlist",
        "route-map .*! sequence .* no clear privateas",
        "route-map .*! sequence .* no clear peeras",
        "route-map .*! sequence .* no clear exactas",
        "route-map .*! sequence .* no clear firstas",
        "route-map .*! sequence .* no set route-map",
        "route-map .*! sequence .* no set route-policy",
        "route-map .*! sequence .* no set aspath",
        "route-map .*! sequence .* no set asconfed",
        "route-map .*! sequence .* no set stdcomm",
        "route-map .*! sequence .* no set extcomm",
        "route-map .*! sequence .* no set lrgcomm",
        "route-map .*! sequence .* no set nexthop",
        "route-map .*! sequence .* set distance leave",
        "route-map .*! sequence .* set locpref leave",
        "route-map .*! sequence .* set aigp leave",
        "route-map .*! sequence .* set bandwidth leave",
        "route-map .*! sequence .* set origin leave",
        "route-map .*! sequence .* set metric leave",
        "route-map .*! sequence .* set tag leave",
        "route-map .*! sequence .* set label-local leave",
        "route-map .*! sequence .* set label-remote leave",
        "route-map .*! sequence .* set segrout leave",
        "route-map .*! sequence .* set bier leave",
        "route-map .*! sequence .* no log"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * create new route map
     */
    public cfgRoump() {
        roumap = new tabListing<tabRtrmapN, addrIP>();
        seq = roumap.nextseq();
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
        l.addAll(roumap.dump(cmds.tabulator));
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
        l.add(null, "4 .         <num>           process id");
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
        l.add(null, "2 3     pathlen             match as path length");
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
        l.add(null, ".2 3    clustlist           clear cluster list");
        l.add(null, "3 3,.     <str>             regexp to match");
        l.add(null, ".2 .    peeras              clear peer asn");
        l.add(null, ".2 3    exactas             clear exact asn");
        l.add(null, ".3 .      <num>             as number to remove");
        l.add(null, ".2 .    firstas             clear first asn");
        l.add(null, "1 2   set                   set values in destination routing protocol");
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
        l.add(null, "2 3     distance            set administrative distance");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 4       set               set value to a specific value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "3 4       add               add value to current value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "3 4       sub               substract value to current value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "2 3     locpref             set local preference");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 4       set               set value to a specific value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "3 4       add               add value to current value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "3 4       sub               substract value to current value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "2 3     aigp                set accumulated igp");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 4       set               set value to a specific value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "3 4       add               add value to current value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "3 4       sub               substract value to current value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "2 3     bandwidth           set bandwidth");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 4       set               set value to a specific value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "3 4       add               add value to current value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "3 4       sub               substract value to current value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "2 3     origin              set origin");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 4       set               set value to a specific value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "3 4       add               add value to current value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "3 4       sub               substract value to current value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "2 3     metric              set metric");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 4       set               set value to a specific value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "3 4       add               add value to current value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "3 4       sub               substract value to current value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "2 3     tag                 set tag");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 4       set               set value to a specific value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "3 4       add               add value to current value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "3 4       sub               substract value to current value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "2 3     label-local         set local label");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 4       set               set value to a specific value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "3 4       add               add value to current value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "3 4       sub               substract value to current value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "2 3     label-remote        set remote label");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 4       set               set value to a specific value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "3 4       add               add value to current value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "3 4       sub               substract value to current value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "2 3     segrout             set sr index");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 4       set               set value to a specific value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "3 4       add               add value to current value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "3 4       sub               substract value to current value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "2 3     bier                set bier index");
        l.add(null, "3 .       leave             leave value unchanged");
        l.add(null, "3 4       set               set value to a specific value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "3 4       add               add value to current value");
        l.add(null, "4 .         <num>           value");
        l.add(null, "3 4       sub               substract value to current value");
        l.add(null, "4 .         <num>           value");
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
            if (a.equals("interface")) {
                ntry.ifaceMatch = cfgAll.ifcFind(cmd.word(), 0);
                return;
            }
            if (a.equals("nexthop")) {
                ntry.nexthopMatch = new addrIP();
                ntry.nexthopMatch.fromString(cmd.getRemaining());
                return;
            }
            if (a.equals("recursive")) {
                ntry.oldhopMatch = new addrIP();
                ntry.oldhopMatch.fromString(cmd.getRemaining());
                return;
            }
            if (a.equals("protocol")) {
                ntry.protoTypMatch = cfgRtr.name2num(cmd.word());
                if (ntry.protoTypMatch == null) {
                    cmd.error("invalid protocol");
                    return;
                }
                ntry.protoNumMatch = bits.str2num(cmd.word());
                if (!cfgRtr.num2proc(ntry.protoTypMatch)) {
                    ntry.protoNumMatch = -1;
                }
                return;
            }
            if (a.equals("aspath")) {
                ntry.aspathMatch = cmd.getRemaining();
                return;
            }
            if (a.equals("peerstd")) {
                ntry.peerStdMatch = tabRouteUtil.string2stdComm(cmd.word());
                return;
            }
            if (a.equals("peerlrg")) {
                tabLargeComm d = new tabLargeComm();
                if (d.fromString(cmd.word())) {
                    return;
                }
                ntry.peerLrgMatch = d;
                return;
            }
            if (a.equals("stdcomm")) {
                ntry.stdCommMatch = tabRouteUtil.string2stdComms(cmd.getRemaining());
                return;
            }
            if (a.equals("extcomm")) {
                ntry.extCommMatch = tabRouteUtil.string2extComms(cmd.getRemaining());
                return;
            }
            if (a.equals("lrgcomm")) {
                ntry.lrgCommMatch = tabRouteUtil.string2lrgComms(cmd.getRemaining());
                return;
            }
            if (a.equals("rd")) {
                ntry.rouDstMatch = tabRouteUtil.string2rd(cmd.word());
                return;
            }
            if (a.equals("network")) {
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
                ntry.noStdComm = true;
                return;
            }
            if (a.equals("noextcomm")) {
                ntry.noExtComm = true;
                return;
            }
            if (a.equals("nolrgcomm")) {
                ntry.noLrgComm = true;
                return;
            }
            if (a.equals("privateas")) {
                ntry.privasMatch = true;
                return;
            }
            if (a.equals("tracker")) {
                ntry.trackMatch = cmd.word();
                return;
            }
            if (a.equals("access-list")) {
                cfgAceslst acl = cfgAll.aclsFind(cmd.word(), false);
                if (acl == null) {
                    cmd.error("no such access list");
                    return;
                }
                ntry.aceslstMatch = acl.aceslst;
                return;
            }
            if (a.equals("prefix-list")) {
                cfgPrfxlst prfxlst = cfgAll.prfxFind(cmd.word(), false);
                if (prfxlst == null) {
                    cmd.error("no such prefix list");
                    return;
                }
                ntry.prfxlstMatch = prfxlst.prflst;
                return;
            }
            if (a.equals("route-map")) {
                cfgRoump roumap = cfgAll.rtmpFind(cmd.word(), false);
                if (roumap == null) {
                    cmd.error("no such route map");
                    return;
                }
                ntry.roumapMatch = roumap.roumap;
                return;
            }
            if (a.equals("route-policy")) {
                cfgRouplc roumap = cfgAll.rtplFind(cmd.word(), false);
                if (roumap == null) {
                    cmd.error("no such route policy");
                    return;
                }
                ntry.rouplcMatch = roumap.rouplc;
                return;
            }
            if (a.equals("peerasn")) {
                if (ntry.peerasnMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("distance")) {
                if (ntry.distanceMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("metric")) {
                if (ntry.metricMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("origin")) {
                if (ntry.originMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("locpref")) {
                if (ntry.locPrefMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("aigp")) {
                if (ntry.accIgpMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("validity")) {
                if (ntry.validityMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("pathlen")) {
                if (ntry.pathlenMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("asend")) {
                if (ntry.asendMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("asbeg")) {
                if (ntry.asbegMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("asmid")) {
                if (ntry.asmidMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("bandwidth")) {
                if (ntry.bandwidthMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("tag")) {
                if (ntry.tagMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("label-local")) {
                if (ntry.lablocMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("label-remote")) {
                if (ntry.labremMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("segrout")) {
                if (ntry.segrouMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("bier")) {
                if (ntry.bierMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("afi")) {
                if (ntry.afiMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("safi")) {
                if (ntry.safiMatch.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("clear")) {
            a = cmd.word();
            tabRtrmapN ntry = getCurr();
            if (a.equals("stdcomm")) {
                ntry.stdCommClear = cmd.getRemaining();
                return;
            }
            if (a.equals("extcomm")) {
                ntry.extCommClear = cmd.getRemaining();
                return;
            }
            if (a.equals("lrgcomm")) {
                ntry.lrgCommClear = cmd.getRemaining();
                return;
            }
            if (a.equals("clustlist")) {
                ntry.clstLstClear = cmd.getRemaining();
                return;
            }
            if (a.equals("privateas")) {
                ntry.privasClear = true;
                return;
            }
            if (a.equals("peeras")) {
                ntry.peerasClear = true;
                return;
            }
            if (a.equals("exactas")) {
                ntry.exactasClear = bits.str2num(cmd.word());
                return;
            }
            if (a.equals("firstas")) {
                ntry.firstasClear = true;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("set")) {
            a = cmd.word();
            tabRtrmapN ntry = getCurr();
            if (a.equals("stdcomm")) {
                ntry.stdCommSet = tabRouteUtil.string2stdComms(cmd.getRemaining());
                return;
            }
            if (a.equals("extcomm")) {
                ntry.extCommSet = tabRouteUtil.string2extComms(cmd.getRemaining());
                return;
            }
            if (a.equals("lrgcomm")) {
                ntry.lrgCommSet = tabRouteUtil.string2lrgComms(cmd.getRemaining());
                return;
            }
            if (a.equals("nexthop")) {
                ntry.nexthopSet = new addrIP();
                ntry.nexthopSet.fromString(cmd.getRemaining());
                return;
            }
            if (a.equals("aspath")) {
                ntry.aspathSet = tabRouteUtil.string2intList(cmd.getRemaining());
                return;
            }
            if (a.equals("asconfed")) {
                ntry.aspathCnf = tabRouteUtil.string2intList(cmd.getRemaining());
                return;
            }
            if (a.equals("distance")) {
                if (ntry.distanceSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("metric")) {
                if (ntry.metricSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("origin")) {
                if (ntry.originSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("locpref")) {
                if (ntry.locPrefSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("aigp")) {
                if (ntry.accIgpSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("bandwidth")) {
                if (ntry.bandwidthSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("tag")) {
                if (ntry.tagSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("label-local")) {
                if (ntry.lablocSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("label-remote")) {
                if (ntry.labremSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("segrout")) {
                if (ntry.segrouSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("bier")) {
                if (ntry.bierSet.fromString(cmd.getRemaining())) {
                    cmd.error("invalid action");
                    return;
                }
                return;
            }
            if (a.equals("route-map")) {
                cfgRoump roumap = cfgAll.rtmpFind(cmd.word(), false);
                if (roumap == null) {
                    cmd.error("no such route map");
                    return;
                }
                ntry.roumapSet = roumap.roumap;
                return;
            }
            if (a.equals("route-policy")) {
                cfgRouplc roumap = cfgAll.rtplFind(cmd.word(), false);
                if (roumap == null) {
                    cmd.error("no such route policy");
                    return;
                }
                ntry.rouplcSet = roumap.rouplc;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("reindex")) {
            int i = bits.str2num(cmd.word());
            roumap.reindex(i, bits.str2num(cmd.word()));
            return;
        }
        if (!a.equals("no")) {
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
            if (a.equals("interface")) {
                ntry.ifaceMatch = null;
                return;
            }
            if (a.equals("nexthop")) {
                ntry.nexthopMatch = null;
                return;
            }
            if (a.equals("recursive")) {
                ntry.oldhopMatch = null;
                return;
            }
            if (a.equals("protocol")) {
                ntry.protoTypMatch = null;
                ntry.protoNumMatch = 0;
                return;
            }
            if (a.equals("aspath")) {
                ntry.aspathMatch = null;
                return;
            }
            if (a.equals("peerstd")) {
                ntry.peerStdMatch = 0;
                return;
            }
            if (a.equals("peerlrg")) {
                ntry.peerLrgMatch = null;
                return;
            }
            if (a.equals("stdcomm")) {
                ntry.stdCommMatch = null;
                return;
            }
            if (a.equals("extcomm")) {
                ntry.extCommMatch = null;
                return;
            }
            if (a.equals("lrgcomm")) {
                ntry.lrgCommMatch = null;
                return;
            }
            if (a.equals("rd")) {
                ntry.rouDstMatch = 0;
                return;
            }
            if (a.equals("network")) {
                ntry.networkMatch = null;
                return;
            }
            if (a.equals("access-list")) {
                ntry.aceslstMatch = null;
                return;
            }
            if (a.equals("prefix-list")) {
                ntry.prfxlstMatch = null;
                return;
            }
            if (a.equals("route-map")) {
                ntry.roumapMatch = null;
                return;
            }
            if (a.equals("route-policy")) {
                ntry.rouplcMatch = null;
                return;
            }
            if (a.equals("nostdcomm")) {
                ntry.noStdComm = false;
                return;
            }
            if (a.equals("noextcomm")) {
                ntry.noExtComm = false;
                return;
            }
            if (a.equals("nolrgcomm")) {
                ntry.noLrgComm = false;
                return;
            }
            if (a.equals("privateas")) {
                ntry.privasMatch = false;
                return;
            }
            if (a.equals("tracker")) {
                ntry.trackMatch = null;
                return;
            }
            if (a.equals("peerasn")) {
                ntry.peerasnMatch.set2always();
                return;
            }
            if (a.equals("distance")) {
                ntry.distanceMatch.set2always();
                return;
            }
            if (a.equals("metric")) {
                ntry.metricMatch.set2always();
                return;
            }
            if (a.equals("origin")) {
                ntry.originMatch.set2always();
                return;
            }
            if (a.equals("locpref")) {
                ntry.locPrefMatch.set2always();
                return;
            }
            if (a.equals("aigp")) {
                ntry.accIgpMatch.set2always();
                return;
            }
            if (a.equals("validity")) {
                ntry.validityMatch.set2always();
                return;
            }
            if (a.equals("pathlen")) {
                ntry.pathlenMatch.set2always();
                return;
            }
            if (a.equals("asend")) {
                ntry.asendMatch.set2always();
                return;
            }
            if (a.equals("asbeg")) {
                ntry.asbegMatch.set2always();
                return;
            }
            if (a.equals("asmid")) {
                ntry.asmidMatch.set2always();
                return;
            }
            if (a.equals("bandwidth")) {
                ntry.bandwidthMatch.set2always();
                return;
            }
            if (a.equals("tag")) {
                ntry.tagMatch.set2always();
                return;
            }
            if (a.equals("label-local")) {
                ntry.lablocMatch.set2always();
                return;
            }
            if (a.equals("label-remote")) {
                ntry.labremMatch.set2always();
                return;
            }
            if (a.equals("segrout")) {
                ntry.segrouMatch.set2always();
                return;
            }
            if (a.equals("bier")) {
                ntry.bierMatch.set2always();
                return;
            }
            if (a.equals("afi")) {
                ntry.afiMatch.set2always();
                return;
            }
            if (a.equals("safi")) {
                ntry.safiMatch.set2always();
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("clear")) {
            a = cmd.word();
            tabRtrmapN ntry = getCurr();
            if (a.equals("stdcomm")) {
                ntry.stdCommClear = null;
                return;
            }
            if (a.equals("extcomm")) {
                ntry.extCommClear = null;
                return;
            }
            if (a.equals("lrgcomm")) {
                ntry.lrgCommClear = null;
                return;
            }
            if (a.equals("clustlist")) {
                ntry.clstLstClear = null;
                return;
            }
            if (a.equals("privateas")) {
                ntry.privasClear = false;
                return;
            }
            if (a.equals("peeras")) {
                ntry.peerasClear = false;
                return;
            }
            if (a.equals("exactas")) {
                ntry.exactasClear = 0;
                return;
            }
            if (a.equals("firstas")) {
                ntry.firstasClear = false;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("set")) {
            a = cmd.word();
            tabRtrmapN ntry = getCurr();
            if (a.equals("stdcomm")) {
                ntry.stdCommSet = null;
                return;
            }
            if (a.equals("nexthop")) {
                ntry.nexthopSet = null;
                return;
            }
            if (a.equals("extcomm")) {
                ntry.extCommSet = null;
                return;
            }
            if (a.equals("lrgcomm")) {
                ntry.lrgCommSet = null;
                return;
            }
            if (a.equals("aspath")) {
                ntry.aspathSet = null;
                return;
            }
            if (a.equals("asconfed")) {
                ntry.aspathCnf = null;
                return;
            }
            if (a.equals("distance")) {
                ntry.distanceSet.set2unchange();
                return;
            }
            if (a.equals("metric")) {
                ntry.metricSet.set2unchange();
                return;
            }
            if (a.equals("origin")) {
                ntry.originSet.set2unchange();
                return;
            }
            if (a.equals("locpref")) {
                ntry.locPrefSet.set2unchange();
                return;
            }
            if (a.equals("aigp")) {
                ntry.accIgpSet.set2unchange();
                return;
            }
            if (a.equals("bandwidth")) {
                ntry.bandwidthSet.set2unchange();
                return;
            }
            if (a.equals("tag")) {
                ntry.tagSet.set2unchange();
                return;
            }
            if (a.equals("label-local")) {
                ntry.lablocSet.set2unchange();
                return;
            }
            if (a.equals("label-remote")) {
                ntry.labremSet.set2unchange();
                return;
            }
            if (a.equals("segrout")) {
                ntry.segrouSet.set2unchange();
                return;
            }
            if (a.equals("bier")) {
                ntry.bierSet.set2unchange();
                return;
            }
            if (a.equals("route-map")) {
                ntry.roumapSet = null;
                return;
            }
            if (a.equals("route-policy")) {
                ntry.rouplcSet = null;
                return;
            }
            cmd.badCmd();
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
