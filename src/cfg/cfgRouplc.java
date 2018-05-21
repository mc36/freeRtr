package cfg;

import addr.addrIP;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import tab.tabListing;
import tab.tabPlcmapN;
import tab.tabPrfxlstN;
import tab.tabRtrmapN;
import tab.tabRtrplcN;
import tab.tabRtrplc;
import user.userHelping;
import util.bits;
import util.cmds;

/**
 * route policy configuration
 *
 * @author matecsaba
 */
public class cfgRouplc implements Comparator<cfgRouplc>, cfgGeneric {

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
     * create new route policy
     */
    public cfgRouplc() {
        rouplc = new tabListing<tabRtrplcN, addrIP>();
        seq = rouplc.nextseq();
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
        ntry.action = tabPlcmapN.actionType.actPermit;
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

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("route-policy " + name);
        l.addAll(rouplc.dump(cmds.tabulator));
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        return l;
    }

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        l.add("1 2   sequence              sequence number of an entry");
        l.add("2 1,.   <num>               sequence number");
        l.add("1 2,. reindex               reindex route map");
        l.add("2 3,.   [num]               initial number to start with");
        l.add("3 4,.     [num]             increment number");
        l.add("1 .   next                  no operation");
        l.add("1 2,. description           description of this route policy");
        l.add("2 2,.   [text]              text describing this route policy");
        l.add("1 2   if                    match values from source routing protocol");
        l.add("1 2   elsif                 match values from source routing protocol");
        l.add("2 .     always              match always");
        l.add("2 .     never               match never");
        l.add("2 3     nexthop             match next hop");
        l.add("3 .       <addr>            address");
        l.add("2 3     aspath              match as path");
        l.add("3 3,.     <str>             regexp against as path");
        l.add("2 3     stdcomm             match standard community");
        l.add("3 3,.     <str>             community");
        l.add("2 3     extcomm             match extended community");
        l.add("3 3,.     <str>             community");
        l.add("2 3     lrgcomm             match large community");
        l.add("3 3,.     <str>             community");
        l.add("2 3     distance            match administrative distance");
        l.add("3 .       <num>             administrative distance");
        l.add("3 .       all               any value");
        l.add("2 3     locpref             match local preference");
        l.add("3 .       <num>             local preference");
        l.add("3 .       all               any value");
        l.add("2 3     aigp                match accumulated igp");
        l.add("3 .       <num>             aigp");
        l.add("3 .       all               any value");
        l.add("2 3     validity            match validity status");
        l.add("3 .       <num>             validity");
        l.add("3 .       all               any value");
        l.add("2 3     pathlen             match as path length");
        l.add("3 .       <num>             length");
        l.add("3 .       all               any value");
        l.add("2 3     bandwidth           match bandwidth");
        l.add("3 .       <num>             bandwidth");
        l.add("3 .       all               any value");
        l.add("2 3     origin              match origin type");
        l.add("3 .       <num>             origin");
        l.add("3 .       all               any value");
        l.add("2 3     metric              match metric");
        l.add("3 .       <num>             metric");
        l.add("3 .       all               any value");
        l.add("2 3     tag                 match tag");
        l.add("3 .       <num>             tag");
        l.add("3 .       all               any value");
        l.add("2 3     segrout             match sr index");
        l.add("3 .       <num>             index");
        l.add("3 .       all               any value");
        l.add("2 3     bier                match bier index");
        l.add("3 .       <num>             index");
        l.add("3 .       all               any value");
        l.add("2 3     afi                 match afi");
        l.add("3 .       <num>             afi");
        l.add("3 .       all               any value");
        l.add("2 3     safi                match safi");
        l.add("3 .       <num>             safi");
        l.add("3 .       all               any value");
        l.add("2 3     rd                  match route distinguisher");
        l.add("3 .       <str>             rd");
        l.add("2 3     network             match network");
        l.add("3 4,.     <net/mask>        network in perfix/mask format");
        l.add("4 5         ge              minimum prefix length to be matched");
        l.add("5 4,.         <num>         minimum prefix length");
        l.add("4 5         le              maximum prefix length to be matched");
        l.add("5 4,.         <num>         maximum prefix length");
        l.add("2 .     nostdcomm           match empty standard community");
        l.add("2 .     noextcomm           match empty extended community");
        l.add("2 .     nolrgcomm           match empty large community");
        l.add("2 3     prefix-list         match prefix list");
        l.add("3 .       <name>            name of prefix list");
        l.add("2 3     route-map           match route map");
        l.add("3 .       <name>            name of route map");
        l.add("2 3     route-policy        match route policy");
        l.add("3 .       <name>            name of route policy");
        l.add("1 .   enif                  end if statement");
        l.add("1 .   else                  begin else statement");
        l.add("1 .   pass                  permit the prefix");
        l.add("1 .   drop                  deny the prefix");
        l.add("1 .   log                   set logging on match");
        l.add("1 2   tcl                   add tcl line");
        l.add("2 2,.   [text]              script");
        l.add("1 2   clear                 clear values in destination routing protocol");
        l.add("2 .     stdcomm             clear standard community");
        l.add("2 .     extcomm             clear extended community");
        l.add("2 .     lrgcomm             clear large community");
        l.add("1 2   set                   set values in destination routing protocol");
        l.add("2 3     aspath              prepend as path");
        l.add("3 3,.     <num>             as to prepend");
        l.add("2 3     stdcomm             add standard community");
        l.add("3 3,.     <num>             community");
        l.add("2 3     extcomm             add extended community");
        l.add("3 3,.     <num>             community");
        l.add("2 3     lrgcomm             add large community");
        l.add("3 3,.     <num>             community");
        l.add("2 3     nexthop             set next hop");
        l.add("3 .       <addr>            address");
        l.add("2 3     distance            set administrative distance");
        l.add("3 .       leave             leave value unchanged");
        l.add("3 4       set               set value to a specific value");
        l.add("4 .         <num>           value");
        l.add("3 4       add               add value to current value");
        l.add("4 .         <num>           value");
        l.add("3 4       sub               substract value to current value");
        l.add("4 .         <num>           value");
        l.add("2 3     locpref             set local preference");
        l.add("3 .       leave             leave value unchanged");
        l.add("3 4       set               set value to a specific value");
        l.add("4 .         <num>           value");
        l.add("3 4       add               add value to current value");
        l.add("4 .         <num>           value");
        l.add("3 4       sub               substract value to current value");
        l.add("4 .         <num>           value");
        l.add("2 3     aigp                set accumulated igp");
        l.add("3 .       leave             leave value unchanged");
        l.add("3 4       set               set value to a specific value");
        l.add("4 .         <num>           value");
        l.add("3 4       add               add value to current value");
        l.add("4 .         <num>           value");
        l.add("3 4       sub               substract value to current value");
        l.add("4 .         <num>           value");
        l.add("2 3     bandwidth           set bandwidth");
        l.add("3 .       leave             leave value unchanged");
        l.add("3 4       set               set value to a specific value");
        l.add("4 .         <num>           value");
        l.add("3 4       add               add value to current value");
        l.add("4 .         <num>           value");
        l.add("3 4       sub               substract value to current value");
        l.add("4 .         <num>           value");
        l.add("2 3     origin              set origin");
        l.add("3 .       leave             leave value unchanged");
        l.add("3 4       set               set value to a specific value");
        l.add("4 .         <num>           value");
        l.add("3 4       add               add value to current value");
        l.add("4 .         <num>           value");
        l.add("3 4       sub               substract value to current value");
        l.add("4 .         <num>           value");
        l.add("2 3     metric              set metric");
        l.add("3 .       leave             leave value unchanged");
        l.add("3 4       set               set value to a specific value");
        l.add("4 .         <num>           value");
        l.add("3 4       add               add value to current value");
        l.add("4 .         <num>           value");
        l.add("3 4       sub               substract value to current value");
        l.add("4 .         <num>           value");
        l.add("2 3     tag                 set tag");
        l.add("3 .       leave             leave value unchanged");
        l.add("3 4       set               set value to a specific value");
        l.add("4 .         <num>           value");
        l.add("3 4       add               add value to current value");
        l.add("4 .         <num>           value");
        l.add("3 4       sub               substract value to current value");
        l.add("4 .         <num>           value");
        l.add("2 3     segrout             set sr index");
        l.add("3 .       leave             leave value unchanged");
        l.add("3 4       set               set value to a specific value");
        l.add("4 .         <num>           value");
        l.add("3 4       add               add value to current value");
        l.add("4 .         <num>           value");
        l.add("3 4       sub               substract value to current value");
        l.add("4 .         <num>           value");
        l.add("2 3     bier                set bier index");
        l.add("3 .       leave             leave value unchanged");
        l.add("3 4       set               set value to a specific value");
        l.add("4 .         <num>           value");
        l.add("3 4       add               add value to current value");
        l.add("4 .         <num>           value");
        l.add("3 4       sub               substract value to current value");
        l.add("4 .         <num>           value");
        l.add("2 3     route-map           set route map");
        l.add("3 .       <name>            name of route map");
        l.add("2 3     route-policy        set route policy");
        l.add("3 .       <name>            name of route policy");
        return l;
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
        if (a.equals("stdcomm")) {
            ntry.ifMode = tabRtrplcN.ifType.stdcomm;
            ntry.intLst = tabRtrmapN.string2stdComms(cmd.getRemaining());
            return;
        }
        if (a.equals("extcomm")) {
            ntry.ifMode = tabRtrplcN.ifType.extcomm;
            ntry.lngLst = tabRtrmapN.string2extComms(cmd.getRemaining());
            return;
        }
        if (a.equals("lrgcomm")) {
            ntry.ifMode = tabRtrplcN.ifType.lrgcomm;
            ntry.lrgLst = tabRtrmapN.string2lrgComms(cmd.getRemaining());
            return;
        }
        if (a.equals("rd")) {
            ntry.ifMode = tabRtrplcN.ifType.roudst;
            ntry.rouDstMatch = tabRtrmapN.string2rd(cmd.word());
            return;
        }
        if (a.equals("network")) {
            ntry.ifMode = tabRtrplcN.ifType.network;
            ntry.networkMatch = new tabPrfxlstN();
            ntry.networkMatch.action = tabPlcmapN.actionType.actPermit;
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
        if (a.equals("validity")) {
            ntry.ifMode = tabRtrplcN.ifType.validity;
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
        if (a.equals("nexthop")) {
            ntry.ifMode = tabRtrplcN.ifType.nexthop;
            ntry.nexthopSet = new addrIP();
            if (ntry.nexthopSet.fromString(cmd.getRemaining())) {
                cmd.error("invalid action");
                return;
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
                return;
            }
            if (a.equals("extcomm")) {
                ntry.doMode = tabRtrplcN.doType.clrExtcomm;
                return;
            }
            if (a.equals("lrgcomm")) {
                ntry.doMode = tabRtrplcN.doType.clrLrgcomm;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("set")) {
            a = cmd.word();
            tabRtrplcN ntry = getCurr();
            indent();
            if (a.equals("stdcomm")) {
                ntry.doMode = tabRtrplcN.doType.setStdcomm;
                ntry.intLst = tabRtrmapN.string2stdComms(cmd.getRemaining());
                return;
            }
            if (a.equals("extcomm")) {
                ntry.doMode = tabRtrplcN.doType.setExtcomm;
                ntry.lngLst = tabRtrmapN.string2extComms(cmd.getRemaining());
                return;
            }
            if (a.equals("lrgcomm")) {
                ntry.doMode = tabRtrplcN.doType.setLrgcomm;
                ntry.lrgLst = tabRtrmapN.string2lrgComms(cmd.getRemaining());
                return;
            }
            if (a.equals("nexthop")) {
                ntry.doMode = tabRtrplcN.doType.setNexthop;
                ntry.nexthopSet = new addrIP();
                ntry.nexthopSet.fromString(cmd.getRemaining());
                return;
            }
            if (a.equals("aspath")) {
                ntry.doMode = tabRtrplcN.doType.setAspath;
                ntry.intLst = tabRtrmapN.string2intList(cmd.getRemaining());
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
                if (ntry.intSet.fromString(cmd.getRemaining())) {
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
        if (!a.equals("no")) {
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

    public int compare(cfgRouplc o1, cfgRouplc o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String getPrompt() {
        return "rouplc";
    }

    public String toString() {
        return name;
    }

}
