package net.freertr.rtr;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgInit;
import net.freertr.cfg.cfgRoump;
import net.freertr.cfg.cfgRouplc;
import net.freertr.cfg.cfgRtr;
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
     * f
     * route map to use
     */
    protected tabListing<tabRtrmapN, addrIP> sndMap;

    /**
     * route policy to use
     */
    protected tabListing<tabRtrplcN, addrIP> sndPlc;

    /**
     * f
     * route map to use
     */
    protected tabListing<tabRtrmapN, addrIP> rcvMap;

    /**
     * route policy to use
     */
    protected tabListing<tabRtrplcN, addrIP> rcvPlc;

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
     * lookup: 1=vrf, 2=redist, 3=router
     */
    protected int lookMod;

    /**
     * type of router
     */
    protected tabRouteAttr.routeType lookTyp;

    /**
     * number of router
     */
    protected int lookNum;

    /**
     * current failed on existence
     */
    protected boolean curGhst;

    /**
     * current advertise
     */
    protected boolean curAdv;

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
     * times advertised
     */
    protected int cntAdv;

    /**
     * times withdrawn
     */
    protected int cntWdr;

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
     * time advertised
     */
    protected long timAdv;

    /**
     * time withdrawn
     */
    protected long timWdr;

    /**
     * time failed on attribute
     */
    protected long timAtrF;

    /**
     * time passed on attribute
     */
    protected long timAtrP;

    /**
     * recorded found prefix
     */
    protected tabRouteEntry<addrIP> lastFond;

    /**
     * recorded ghost prefix
     */
    protected tabRouteEntry<addrIP> lastGhst;

    /**
     * recorded attrib prefix
     */
    protected tabRouteEntry<addrIP> lastAttr;

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
        stopped = true;
        ignore = 0;
        graceAdv = 0;
        graceWdr = 0;
        lookMod = 1;
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

    private tabRouteEntry<addrIP> createPrefix() {
        if (prefix == null) {
            return null;
        }
        if (nextHop == null) {
            return null;
        }
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = prefix.copyBytes();
        ntry.best.rouTyp = rouTyp;
        ntry.best.protoNum = rtrNum;
        ntry.best.distance = distance;
        ntry.best.nextHop = nextHop.copyBytes();
        ntry.best.rouTyp = routerProtoTyp;
        ntry.best.protoNum = routerProcNum;
        if (afi == 3) {
            ntry = rtrBgpFlow.advertNetwork(prefix, ipv6, 2, ntry);
            if (ntry == null) {
                return null;
            }
        }
        ntry = tabRoute.doUpdateEntry(rtrBgpUtil.sfiUnicast, 0, ntry, sndMap, sndPlc, null);
        return ntry;
    }

    /**
     * create computed
     */
    public synchronized void routerCreateComputed() {
        if (timap == null) {
            return;
        }
        if (stopped) {
            return;
        }
        if (cfgInit.booting) {
            return;
        }
        tabRouteEntry<addrIP> sent = createPrefix();
        if (sent == null) {
            return;
        }
        tabRoute<addrIP> tabU = new tabRoute<addrIP>("computed");
        tabRoute<addrIP> tabM = new tabRoute<addrIP>("computed");
        tabRoute<addrIP> tabF = new tabRoute<addrIP>("computed");
        cntExec++;
        timExec = bits.getTime();
        boolean needed = !timap.matches(timExec);
        boolean grace;
        if (needed) {
            curAdv = originator;
            cntAdv++;
            timAdv = timExec;
            grace = (timWdr + graceAdv) > timExec;
        } else {
            curAdv = false;
            cntWdr++;
            timWdr = timExec;
            grace = (timAdv + graceWdr) > timExec;
        }
        if (curAdv) {
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
        tabRouteEntry<addrIP> rcvd = null;
        switch (lookMod) {
            case 1:
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
                break;
            case 2:
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
                break;
            case 3:
                ipRtr rtr = fwdCore.routerFind(lookTyp, lookNum);
                if (rtr == null) {
                    return;
                }
                switch (afi) {
                    case 1:
                        rcvd = rtr.routerComputedU.find(sent);
                        break;
                    case 2:
                        rcvd = rtr.routerComputedM.find(sent);
                        break;
                    case 3:
                        rcvd = rtr.routerComputedF.find(sent);
                        break;
                }
                break;
        }
        if (rcvd != null) {
            lastFond = rcvd.copyBytes(tabRoute.addType.alters);
            rcvd = rcvd.copyBytes(tabRoute.addType.notyet);
            rcvd = tabRoute.doUpdateEntry(rtrBgpUtil.sfiUnicast, 0, rcvd, rcvMap, rcvPlc, null);
            if (rcvd != null) {
                tabRouteAttr.ignoreAttribs(rcvd.best, ignore);
            }
            tabRouteAttr.ignoreAttribs(sent.best, ignore);
        } else {
            lastFond = null;
        }
        curGhst = needed != (rcvd != null);
        if (grace) {
            curGhst = false;
        } else {
            if (curGhst) {
                cntGhst++;
                timGhst = timExec;
                if (lastFond != null) {
                    lastGhst = lastFond;
                }
                if (logging) {
                    logger.info("ghosting " + rtrLogger.afi2str(afi) + " " + rtrLogger.prf2str(afi, sent.prefix));
                }
            } else {
                cntPass++;
                timPass = timExec;
            }
        }
        if (rcvd != null) {
            curAtrF = sent.differs(tabRoute.addType.notyet, rcvd);
            if (curAtrF != 0) {
                cntAtrF++;
                timAtrF = timExec;
                if (lastFond != null) {
                    lastAttr = lastFond;
                }
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
        fwdCore.routerChg(this, false);
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
        l.add(null, "2 3     router                    select an other router");
        cfgRtr.getRouterList(l, 1, " to use");
        l.add(null, "4 .         <num>                 process id");
        l.add(null, "1 2   afi                         set address family");
        l.add(null, "2 .     unicast                   select unicast");
        l.add(null, "2 .     multicast                 select multicast");
        l.add(null, "2 .     flowspec                  select flowspec");
        l.add(null, "1 2   prefix                      specify prefix to use");
        l.add(null, "2 .     <str>                     prefix");
        l.add(null, "1 2   nexthop                     specify nexthop to use");
        l.add(null, "2 .     <str>                     prefix");
        l.add(null, "1 2   range                       specify time map to use");
        l.add(null, "2 .     <nam:tm>                  time map");
        l.add(null, "1 2   send-map                    specify route map for advertisement");
        l.add(null, "2 .     <nam:rm>                  route map");
        l.add(null, "1 2   send-policy                 specify route policy for advertisement");
        l.add(null, "2 .     <nam:rpl>                 route policy");
        l.add(null, "1 2   recv-map                    specify route map for comparison");
        l.add(null, "2 .     <nam:rm>                  route map");
        l.add(null, "1 2   recv-policy                 specify route policy for comparison");
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
            l.add(beg + "no range");
        } else {
            l.add(beg + "range " + timap.name);
        }
        l.add(beg + "afi " + rtrLogger.afi2str(afi));
        l.add(beg + "grace " + graceAdv + " " + graceWdr);
        String a;
        switch (lookMod) {
            case 1:
                a = "vrf";
                break;
            case 2:
                a = "redist";
                break;
            case 3:
                a = "router " + cfgRtr.num2name(lookTyp) + " " + lookNum;
                break;
            default:
                a = "unknown=" + lookMod;
                break;
        }
        l.add(beg + "lookup " + a);
        cmds.cfgLine(l, sndMap == null, beg, "send-map", "" + sndMap);
        cmds.cfgLine(l, sndPlc == null, beg, "send-policy", "" + sndPlc);
        cmds.cfgLine(l, rcvMap == null, beg, "recv-map", "" + rcvMap);
        cmds.cfgLine(l, rcvPlc == null, beg, "recv-policy", "" + rcvPlc);
        cmds.cfgLine(l, ignore == 0, beg, "ignore", tabRouteAttr.ignore2string(ignore));
        if (originator) {
            a = "originator";
        } else {
            a = "observer";
        }
        l.add(beg + "mode " + a);
        if (stopped) {
            l.add(cmds.tabulator + "stop");
        } else {
            l.add(cmds.tabulator + "start");
        }
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
                lookMod = 1;
                return false;
            }
            s = cmd.word();
            lookMod = 1;
            if (s.equals("vrf")) {
                lookMod = 1;
                return false;
            }
            if (s.equals("redist")) {
                lookMod = 2;
                return false;
            }
            if (!s.equals("router")) {
                return false;
            }
            lookTyp = cfgRtr.name2num(cmd.word());
            if (lookTyp == null) {
                cmd.error("invalid routing protocol");
                return true;
            }
            lookNum = bits.str2num(cmd.word());
            lookMod = 3;
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
        if (s.equals("range")) {
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
        if (s.equals("send-map")) {
            if (negated) {
                sndMap = null;
                notif.wakeup();
                return false;
            }
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return false;
            }
            sndMap = ntry.roumap;
            notif.wakeup();
            return false;
        }
        if (s.equals("send-policy")) {
            if (negated) {
                sndPlc = null;
                notif.wakeup();
                return false;
            }
            cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route policy");
                return false;
            }
            sndPlc = ntry.rouplc;
            notif.wakeup();
            return false;
        }
        if (s.equals("recv-map")) {
            if (negated) {
                rcvMap = null;
                return false;
            }
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return false;
            }
            rcvMap = ntry.roumap;
            return false;
        }
        if (s.equals("recv-policy")) {
            if (negated) {
                rcvPlc = null;
                return false;
            }
            cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route policy");
                return false;
            }
            rcvPlc = ntry.rouplc;
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
        userFormat l = new userFormat("|", "category|curr|times|ago|last");
        l.add("execute|" + need2run + "|" + cntExec + "|" + bits.timePast(timExec) + "|" + bits.time2str(cfgAll.timeZoneName, timExec + cfgAll.timeServerOffset, 3));
        l.add("advertise|" + curAdv + "|" + cntAdv + "|" + bits.timePast(timAdv) + "|" + bits.time2str(cfgAll.timeZoneName, timAdv + cfgAll.timeServerOffset, 3));
        l.add("withdraw|" + !curAdv + "|" + cntWdr + "|" + bits.timePast(timWdr) + "|" + bits.time2str(cfgAll.timeZoneName, timWdr + cfgAll.timeServerOffset, 3));
        l.add("ghost|" + curGhst + "|" + cntGhst + "|" + bits.timePast(timGhst) + "|" + bits.time2str(cfgAll.timeZoneName, timGhst + cfgAll.timeServerOffset, 3));
        l.add("pass|" + !curGhst + "|" + cntPass + "|" + bits.timePast(timPass) + "|" + bits.time2str(cfgAll.timeZoneName, timPass + cfgAll.timeServerOffset, 3));
        l.add("attrib err|" + curAtrF + "|" + cntAtrF + "|" + bits.timePast(timAtrF) + "|" + bits.time2str(cfgAll.timeZoneName, timAtrF + cfgAll.timeServerOffset, 3));
        l.add("attrib ok|" + curAtrF + "|" + cntAtrP + "|" + bits.timePast(timAtrP) + "|" + bits.time2str(cfgAll.timeZoneName, timAtrP + cfgAll.timeServerOffset, 3));
        return l;
    }

    /**
     * get attrib
     *
     * @param cmd attribs to ignore
     * @param wid screen width
     * @return list of differences
     */
    public List<String> getDiffer(cmds cmd, int wid) {
        if (lastFond == null) {
            return null;
        }
        tabRouteEntry<addrIP> ntry2 = lastFond.copyBytes(tabRoute.addType.notyet);
        tabRouteEntry<addrIP> ntry1 = createPrefix();
        if (ntry1 == null) {
            return null;
        }
        int ign = 0;
        for (;;) {
            String s = cmd.word();
            if (s.length() < 1) {
                break;
            }
            ign |= tabRouteAttr.string2ignore(s);
        }
        tabRouteAttr.ignoreAttribs(ntry1.best, ign);
        tabRouteAttr.ignoreAttribs(ntry2.best, ign);
        List<String> dump1 = ntry1.fullDump(fwdCore).formatAll(userFormat.tableMode.normal);
        List<String> dump2 = ntry2.fullDump(fwdCore).formatAll(userFormat.tableMode.normal);
        differ df = new differ();
        df.calc(dump1, dump2);
        List<String> res = df.getText(wid, 0);
        res.add(0, "difference=" + ntry1.differs(tabRoute.addType.notyet, ntry2));
        return res;
    }

    /**
     * get attrib
     *
     * @param wid screen width
     * @return list of differences
     */
    public List<String> getAttribed(int wid) {
        if (lastAttr == null) {
            return null;
        }
        tabRouteEntry<addrIP> ntry = createPrefix();
        if (ntry == null) {
            return null;
        }
        List<String> dump1 = ntry.fullDump(fwdCore).formatAll(userFormat.tableMode.normal);
        List<String> dump2 = lastAttr.fullDump(fwdCore).formatAll(userFormat.tableMode.normal);
        differ df = new differ();
        df.calc(dump1, dump2);
        List<String> res = df.getText(wid, 0);
        res.add(0, "difference=" + ntry.differs(tabRoute.addType.notyet, lastAttr));
        return res;
    }

    /**
     * get ghost
     *
     * @return list of recorded
     */
    public List<String> getGhosted() {
        if (lastGhst == null) {
            return null;
        }
        return lastGhst.fullDump(fwdCore).formatAll(userFormat.tableMode.normal);
    }

    /**
     * get found
     *
     * @return list of recorded
     */
    public List<String> getFound() {
        if (lastFond == null) {
            return null;
        }
        return lastFond.fullDump(fwdCore).formatAll(userFormat.tableMode.normal);
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
