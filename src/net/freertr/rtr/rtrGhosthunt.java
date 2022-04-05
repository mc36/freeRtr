package net.freertr.rtr;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgRoump;
import net.freertr.cfg.cfgRouplc;
import net.freertr.cfg.cfgTime;
import net.freertr.ip.ipCor4;
import net.freertr.ip.ipCor6;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipRtr;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabIndex;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRtrmapN;
import net.freertr.tab.tabRtrplc;
import net.freertr.tab.tabRtrplcN;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.differ;
import net.freertr.util.logger;
import net.freertr.util.notifier;

/**
 * ghost (zombie) route hunter
 *
 * @author matecsaba
 */
public class rtrGhosthunt extends ipRtr implements Runnable {

    /**
     * the forwarder protocol
     */
    public final ipFwd fwdCore;

    /**
     * route type
     */
    protected final tabRouteAttr.routeType rouTyp;

    /**
     * router number
     */
    protected final int rtrNum;

    /**
     * route afi
     */
    protected final boolean ipv6;

    /**
     * logging
     */
    protected boolean logging;

    /**
     * paused
     */
    protected boolean stopped;

    /**
     * originator mode
     */
    protected boolean originator;

    /**
     * address family
     */
    protected int afi;

    /**
     * distance to give
     */
    protected int distance;

    /**
     * lower half nexthop
     */
    protected addrIP nextHop;

    /**
     * prefix to use
     */
    protected addrPrefix<addrIP> prefix;

    /**
     * time map to use
     */
    protected cfgTime timap;

    /**
     * route map to use
     */
    protected tabListing<tabRtrmapN, addrIP> roumap;

    /**
     * route policy to use
     */
    protected tabListing<tabRtrplcN, addrIP> rouplc;

    /**
     * ignore attributes
     */
    protected int ignore;

    /**
     * advertise grace time
     */
    protected int graceAdv;

    /**
     * withdraw grace time
     */
    protected int graceWdr;

    /**
     * redistributed
     */
    protected boolean forwrdr;

    /**
     * current failed on existence
     */
    protected boolean curGhst;

    /**
     * current failed on attribute
     */
    protected int curAtrF;

    /**
     * times executed
     */
    protected int cntExec;

    /**
     * times failed on existence
     */
    protected int cntGhst;

    /**
     * times passed on existence
     */
    protected int cntPass;

    /**
     * times failed on attribute
     */
    protected int cntAtrF;

    /**
     * times passed on attribute
     */
    protected int cntAtrP;

    /**
     * time executed
     */
    protected long timExec;

    /**
     * time failed on existence
     */
    protected long timGhst;

    /**
     * time passed on existence
     */
    protected long timPass;

    /**
     * time failed on attribute
     */
    protected long timAtrF;

    /**
     * time passed on attribute
     */
    protected long timAtrP;

    /**
     * sent prefix
     */
    protected tabRouteEntry<addrIP> sent;

    /**
     * received prefix
     */
    protected tabRouteEntry<addrIP> rcvd;

    /**
     * recorded prefix
     */
    protected tabRouteEntry<addrIP> recd;

    private notifier notif = new notifier();

    private boolean need2run = true;

    /**
     * create unicast to multicast process
     *
     * @param forwarder forwarder to update
     * @param id process id
     */
    public rtrGhosthunt(ipFwd forwarder, int id) {
        fwdCore = forwarder;
        rtrNum = id;
        switch (fwdCore.ipVersion) {
            case ipCor4.protocolVersion:
                rouTyp = tabRouteAttr.routeType.ghosthunt4;
                ipv6 = false;
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteAttr.routeType.ghosthunt6;
                ipv6 = true;
                break;
            default:
                rouTyp = null;
                ipv6 = false;
                break;
        }
        afi = 1;
        distance = 10;
        originator = false;
        ignore = 0;
        graceAdv = 0;
        graceWdr = 0;
        forwrdr = true;
        routerComputedU = new tabRoute<addrIP>("rx");
        routerComputedM = new tabRoute<addrIP>("rx");
        routerComputedF = new tabRoute<addrIP>("rx");
        routerComputedI = new tabGen<tabIndex<addrIP>>();
        routerCreateComputed();
        fwdCore.routerAdd(this, rouTyp, id);
        new Thread(this).start();
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "ghosthunt on " + fwdCore;
    }

    /**
     * create computed
     */
    public synchronized void routerCreateComputed() {
        if (prefix == null) {
            return;
        }
        if (nextHop == null) {
            return;
        }
        if (timap == null) {
            return;
        }
        if (stopped) {
            return;
        }
        tabRoute<addrIP> tabU = new tabRoute<addrIP>("computed");
        tabRoute<addrIP> tabM = new tabRoute<addrIP>("computed");
        tabRoute<addrIP> tabF = new tabRoute<addrIP>("computed");
        sent = new tabRouteEntry<addrIP>();
        sent.prefix = prefix.copyBytes();
        sent.best.rouTyp = rouTyp;
        sent.best.protoNum = rtrNum;
        sent.best.distance = distance;
        sent.best.nextHop = nextHop.copyBytes();
        sent.best.rouTyp = routerProtoTyp;
        sent.best.protoNum = routerProcNum;
        if (afi == 3) {
            sent = rtrBgpFlow.advertNetwork(prefix, ipv6, 2, sent);
            if (sent == null) {
                return;
            }
        }
        if (roumap != null) {
            roumap.update(rtrBgpUtil.sfiUnicast, 0, sent, false);
        }
        if (rouplc != null) {
            tabRtrplc.doRpl(rtrBgpUtil.sfiUnicast, 0, sent, rouplc, false);
        }
        cntExec++;
        timExec = bits.getTime();
        boolean needed = !timap.matches(timExec);
        if (originator && needed) {
            switch (afi) {
                case 1:
                    tabU.add(tabRoute.addType.better, sent, true, true);
                    break;
                case 2:
                    tabM.add(tabRoute.addType.better, sent, true, true);
                    break;
                case 3:
                    tabF.add(tabRoute.addType.better, sent, true, true);
                    break;
            }
        }
        rcvd = null;
        if (forwrdr) {
            switch (afi) {
                case 1:
                    rcvd = fwdCore.actualU.find(sent);
                    break;
                case 2:
                    rcvd = fwdCore.actualM.find(sent);
                    break;
                case 3:
                    rcvd = fwdCore.actualF.find(sent);
                    break;
            }
        } else {
            switch (afi) {
                case 1:
                    rcvd = routerRedistedU.find(sent);
                    break;
                case 2:
                    rcvd = routerRedistedM.find(sent);
                    break;
                case 3:
                    rcvd = routerRedistedF.find(sent);
                    break;
            }
        }
        tabRouteEntry<addrIP> orig = rcvd;
        if (rcvd != null) {
            rcvd = rcvd.copyBytes(tabRoute.addType.notyet);
            tabRouteAttr.ignoreAttribs(rcvd.best, ignore);
            tabRouteAttr.ignoreAttribs(sent.best, ignore);
        }
        curGhst = needed != (rcvd != null);
        int period;
        if (needed) {
            period = graceAdv;
        } else {
            period = graceWdr;
        }
        if (needed == timap.matches(timExec - period)) {
            curGhst = false;
        }
        if (needed == timap.matches(timExec + period)) {
            curGhst = false;
        }
        if (curGhst) {
            cntGhst++;
            timGhst = timExec;
            if (orig != null) {
                recd = orig.copyBytes(tabRoute.addType.alters);
            }
            if (logging) {
                logger.info("ghosting " + rtrLogger.afi2str(afi) + " " + rtrLogger.prf2str(afi, sent.prefix));
            }
        } else {
            cntPass++;
            timPass = timExec;
        }
        if (rcvd != null) {
            curAtrF = sent.differs(tabRoute.addType.notyet, rcvd);
            if (curAtrF != 0) {
                cntAtrF++;
                timAtrF = timExec;
            } else {
                cntAtrP++;
                timAtrP = timExec;
            }
        }
        routerDoAggregates(rtrBgpUtil.sfiMulticast, tabU, tabU, fwdCore.commonLabel, null, 0);
        routerDoAggregates(rtrBgpUtil.sfiMulticast, tabM, tabM, fwdCore.commonLabel, null, 0);
        boolean same = tabU.preserveTime(routerComputedU);
        same &= tabM.preserveTime(routerComputedM);
        same &= tabF.preserveTime(routerComputedF);
        if (same) {
            return;
        }
        routerComputedU = tabU;
        routerComputedM = tabM;
        routerComputedF = tabF;
        routerComputedI = new tabGen<tabIndex<addrIP>>();
        fwdCore.routerChg(this);
    }

    /**
     * redistribution changed
     */
    public void routerRedistChanged() {
        notif.wakeup();
    }

    /**
     * others changed
     */
    public void routerOthersChanged() {
    }

    /**
     * get help
     *
     * @param l list
     */
    public void routerGetHelp(userHelping l) {
        l.add(null, "1 .   logging                     log events");
        l.add(null, "1 .   start                       start running");
        l.add(null, "1 .   stop                        stop running");
        l.add(null, "1 2   distance                    specify default distance");
        l.add(null, "2 .     <num>                     distance");
        l.add(null, "1 2   grace                       specify grace interval in ms");
        l.add(null, "2 3     <num>                     for advertisement");
        l.add(null, "3 .       <num>                   for withdrawal");
        l.add(null, "1 2   mode                        set mode");
        l.add(null, "2 .     originator                select originator");
        l.add(null, "2 .     observer                  select observer");
        l.add(null, "1 2   lookup                      set lookup");
        l.add(null, "2 .     vrf                       select vrf routes");
        l.add(null, "2 .     redist                    select redistribted");
        l.add(null, "1 2   afi                         set address family");
        l.add(null, "2 .     unicast                   select unicast");
        l.add(null, "2 .     multicast                 select multicast");
        l.add(null, "2 .     flowspec                  select flowspec");
        l.add(null, "1 2   prefix                      specify prefix to use");
        l.add(null, "2 .     <str>                     prefix");
        l.add(null, "1 2   nexthop                     specify nexthop to use");
        l.add(null, "2 .     <str>                     prefix");
        l.add(null, "1 2   time-map                    specify time map to use");
        l.add(null, "2 .     <nam:tm>                  time map");
        l.add(null, "1 2   route-map                   specify route map to use");
        l.add(null, "2 .     <nam:rm>                  route map");
        l.add(null, "1 2   route-policy                specify route policy to use");
        l.add(null, "2 .     <nam:rpl>                 route policy");
        l.add(null, "1 2   ignore                      specify attributes to ignore");
        tabRouteAttr.ignoreHelp(l, 2);
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void routerGetConfig(List<String> l, String beg, int filter) {
        cmds.cfgLine(l, !logging, beg, "logging", "");
        l.add(beg + "distance " + distance);
        if (prefix == null) {
            l.add(beg + "no prefix");
        } else {
            l.add(beg + "prefix " + addrPrefix.ip2str(prefix));
        }
        cmds.cfgLine(l, nextHop == null, beg, "nexthop", "" + nextHop);
        if (timap == null) {
            l.add(beg + "no time-map");
        } else {
            l.add(beg + "time-map " + timap.name);
        }
        l.add(beg + "afi " + rtrLogger.afi2str(afi));
        l.add(beg + "grace " + graceAdv + " " + graceWdr);
        String a;
        if (forwrdr) {
            a = "vrf";
        } else {
            a = "redist";
        }
        l.add(beg + "lookup " + a);
        cmds.cfgLine(l, roumap == null, beg, "route-map", "" + roumap);
        cmds.cfgLine(l, rouplc == null, beg, "route-policy", "" + rouplc);
        cmds.cfgLine(l, ignore == 0, beg, "ignore", tabRouteAttr.ignore2string(ignore));
        if (originator) {
            a = "originator";
        } else {
            a = "observer";
        }
        l.add(beg + "mode " + a);
        cmds.cfgLine(l, stopped, beg, "start", "");
    }

    /**
     * configure
     *
     * @param cmd command
     * @return false on success, true on error
     */
    public boolean routerConfigure(cmds cmd) {
        String s = cmd.word();
        boolean negated = false;
        if (s.equals("no")) {
            s = cmd.word();
            negated = true;
        }
        if (s.equals("logging")) {
            logging = !negated;
            return false;
        }
        if (s.equals("stop")) {
            stopped = !negated;
            return false;
        }
        if (s.equals("start")) {
            stopped = negated;
            return false;
        }
        if (s.equals("distance")) {
            distance = bits.str2num(cmd.word());
            notif.wakeup();
            return false;
        }
        if (s.equals("afi")) {
            if (negated) {
                afi = 1;
                return false;
            }
            afi = rtrLogger.str2afi(cmd.word());
            return false;
        }
        if (s.equals("grace")) {
            if (negated) {
                graceAdv = 0;
                graceWdr = 0;
                return false;
            }
            graceAdv = bits.str2num(cmd.word());
            graceWdr = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("lookup")) {
            if (negated) {
                forwrdr = true;
                return false;
            }
            forwrdr = cmd.word().equals("vrf");
            return false;
        }
        if (s.equals("mode")) {
            if (negated) {
                originator = false;
                return false;
            }
            originator = cmd.word().equals("originator");
            notif.wakeup();
            return false;
        }
        if (s.equals("prefix")) {
            if (negated) {
                prefix = null;
                notif.wakeup();
                return false;
            }
            prefix = addrPrefix.str2ip(cmd.word());
            if (prefix == null) {
                cmd.error("invalid prefix");
                return false;
            }
            notif.wakeup();
            return false;
        }
        if (s.equals("nexthop")) {
            if (negated) {
                nextHop = null;
                notif.wakeup();
                return false;
            }
            nextHop = new addrIP();
            if (nextHop.fromString(cmd.word())) {
                nextHop = null;
                cmd.error("invalid address");
                return false;
            }
            notif.wakeup();
            return false;
        }
        if (s.equals("time-map")) {
            if (negated) {
                timap = null;
                notif.wakeup();
                return false;
            }
            timap = cfgAll.timeFind(cmd.word(), false);
            if (timap == null) {
                cmd.error("no such time map");
                return false;
            }
            notif.wakeup();
            return false;
        }
        if (s.equals("route-map")) {
            if (negated) {
                roumap = null;
                notif.wakeup();
                return false;
            }
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return false;
            }
            roumap = ntry.roumap;
            notif.wakeup();
            return false;
        }
        if (s.equals("route-policy")) {
            if (negated) {
                rouplc = null;
                notif.wakeup();
                return false;
            }
            cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route policy");
                return false;
            }
            rouplc = ntry.rouplc;
            notif.wakeup();
            return false;
        }
        if (s.equals("ignore")) {
            ignore = 0;
            if (negated) {
                return false;
            }
            for (;;) {
                s = cmd.word();
                if (s.length() < 1) {
                    break;
                }
                ignore |= tabRouteAttr.string2ignore(s);
            }
            return false;
        }
        return true;
    }

    /**
     * stop work
     */
    public void routerCloseNow() {
        need2run = false;
    }

    /**
     * get neighbor count
     *
     * @return count
     */
    public int routerNeighCount() {
        return 0;
    }

    /**
     * list neighbors
     *
     * @param tab list
     */
    public void routerNeighList(tabRoute<addrIP> tab) {
    }

    /**
     * get interface count
     *
     * @return count
     */
    public int routerIfaceCount() {
        return 0;
    }

    /**
     * maximum recursion depth
     *
     * @return allowed number
     */
    public int routerRecursions() {
        return 1;
    }

    /**
     * get list of link states
     *
     * @param tab table to update
     * @param par parameter
     * @param asn asn
     * @param adv advertiser
     */
    public void routerLinkStates(tabRoute<addrIP> tab, int par, int asn, addrIPv4 adv) {
    }

    public void run() {
        for (;;) {
            notif.misleep(1000);
            if (!need2run) {
                return;
            }
            try {
                routerCreateComputed();
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
    }

    /**
     * get stats
     *
     * @return list of statistics
     */
    public userFormat getStats() {
        userFormat l = new userFormat("|", "category|curr|times|last|ago");
        l.add("executed|" + need2run + "|" + cntExec + "|" + bits.time2str(cfgAll.timeZoneName, timExec + cfgAll.timeServerOffset, 3) + "|" + bits.timePast(timExec));
        l.add("ghosted|" + curGhst + "|" + cntGhst + "|" + bits.time2str(cfgAll.timeZoneName, timGhst + cfgAll.timeServerOffset, 3) + "|" + bits.timePast(timGhst));
        l.add("passed|" + !curGhst + "|" + cntPass + "|" + bits.time2str(cfgAll.timeZoneName, timPass + cfgAll.timeServerOffset, 3) + "|" + bits.timePast(timPass));
        l.add("attrib err|" + curAtrF + "|" + cntAtrF + "|" + bits.time2str(cfgAll.timeZoneName, timAtrF + cfgAll.timeServerOffset, 3) + "|" + bits.timePast(timAtrF));
        l.add("attrib ok|" + curAtrF + "|" + cntAtrP + "|" + bits.time2str(cfgAll.timeZoneName, timAtrP + cfgAll.timeServerOffset, 3) + "|" + bits.timePast(timAtrP));
        return l;
    }

    /**
     * get diffs
     *
     * @param wid screen width
     * @return list of differences
     */
    public List<String> getDiffs(int wid) {
        if (rcvd == null) {
            return null;
        }
        if (curAtrF == 0) {
            return null;
        }
        List<String> dump1 = sent.fullDump(fwdCore).formatAll(userFormat.tableMode.normal);
        List<String> dump2 = rcvd.fullDump(fwdCore).formatAll(userFormat.tableMode.normal);
        differ df = new differ();
        df.calc(dump1, dump2);
        List<String> res = df.getText(wid, 0);
        res.add(0, "difference=" + curAtrF);
        return res;
    }

    /**
     * get recorded
     *
     * @return list of recorded
     */
    public List<String> getRecord() {
        if (recd == null) {
            return null;
        }
        return recd.fullDump(fwdCore).formatAll(userFormat.tableMode.normal);
    }

    /**
     * set paused
     *
     * @param need pause state
     */
    public void setPaused(boolean need) {
        stopped = need;
    }

}
