package net.freertr.ip;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.addr.addrMac;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgRtr;
import net.freertr.cfg.cfgVrf;
import net.freertr.clnt.clntMplsTeP2p;
import net.freertr.ifc.ifcNull;
import net.freertr.pack.packHolder;
import net.freertr.pack.packRsvp;
import net.freertr.rtr.rtrBfdNeigh;
import net.freertr.rtr.rtrBgpUtil;
import net.freertr.rtr.rtrLdpNeigh;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabHop;
import net.freertr.tab.tabIndex;
import net.freertr.tab.tabLabel;
import net.freertr.tab.tabLabelEntry;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabNatCfgN;
import net.freertr.tab.tabNatTraN;
import net.freertr.tab.tabPrfxlstN;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRtrplc;
import net.freertr.user.userFormat;
import net.freertr.util.bits;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * calculates ip forwarding tables
 *
 * @author matecsaba
 */
public class ipFwdTab {

    private ipFwdTab() {
    }

    /**
     * find originating interface to address
     *
     * @param lower forwarder
     * @param adr address to look to
     * @return interface to use
     */
    public static ipFwdIface findSendingIface(ipFwd lower, addrIP adr) {
        tabRouteEntry<addrIP> prf = lower.actualU.route(adr);
        if (prf == null) {
            return null;
        }
        if (prf.best.rouTab != null) {
            return findStableIface(lower);
        }
        return (ipFwdIface) prf.best.iface;
    }

    /**
     * find connected interface to address
     *
     * @param lower forwarder
     * @param adr address to look to
     * @return interface to use
     */
    public static ipFwdIface findConnedIface(ipFwd lower, addrIP adr) {
        tabRouteEntry<addrIP> prf = lower.actualU.route(adr);
        if (prf == null) {
            return null;
        }
        switch (prf.best.rouTyp) {
            case conn:
            case remote:
            case defpref:
            case automesh:
                break;
            default:
                return null;
        }
        return (ipFwdIface) prf.best.iface;
    }

    /**
     * find stable interface
     *
     * @param lower forwarder
     * @return interface id, null if none
     */
    public static ipFwdIface findStableIface(ipFwd lower) {
        ipFwdIface best = new ipFwdIface(0, null);
        boolean seen = false;
        for (int i = lower.ifaces.size() - 1; i >= 0; i--) {
            ipFwdIface ifc = lower.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (!ifc.ready) {
                continue;
            }
            if (ifc.mask < best.mask) {
                continue;
            }
            best = ifc;
            seen = true;
        }
        if (!seen) {
            return null;
        }
        return best;
    }

    /**
     * list protocols
     *
     * @param lower forwarder
     * @param l list to update
     * @param b beginning to use
     */
    public static void listProtocols(ipFwd lower, userFormat l, String b) {
        lower.protos.dump(l, b);
    }

    /**
     * list ldp nulled prefix neighbors
     *
     * @param lower forwarder
     * @return list of neighbors
     */
    public static userFormat ldpNulledShow(ipFwd lower) {
        userFormat l = new userFormat("|", "learn|advert|nulled|uptime");
        for (int i = 0; i < lower.ldpNeighs.size(); i++) {
            rtrLdpNeigh ntry = lower.ldpNeighs.get(i);
            if (ntry == null) {
                continue;
            }
            l.add(ntry.getShNulled());
        }
        return l;
    }

    /**
     * list ldp neighbors
     *
     * @param lower forwarder
     * @return list of neighbors
     */
    public static userFormat ldpNeighShow(ipFwd lower) {
        userFormat l = new userFormat("|", "learn|advert|learn|advert|learn|advert|neighbor|uptime", "2prefix|2layer2|2p2mp|2");
        for (int i = 0; i < lower.ldpNeighs.size(); i++) {
            rtrLdpNeigh ntry = lower.ldpNeighs.get(i);
            if (ntry == null) {
                continue;
            }
            l.add(ntry.getShNeigh());
        }
        return l;
    }

    /**
     * list rsvp tunnels
     *
     * @param lower forwarder
     * @return list of te tunnels
     */
    public static userFormat rsvpTunnelShow(ipFwd lower) {
        userFormat l = new userFormat("|", "source|id|subgroup|id|target|id|description|");
        for (int i = 0; i < lower.trafEngs.size(); i++) {
            ipFwdTrfng ntry = lower.trafEngs.get(i);
            if (ntry == null) {
                continue;
            }
            l.add("" + ntry);
        }
        return l;
    }

    /**
     * find bfd neighbor
     *
     * @param lower forwarder
     * @param adr address
     * @return neighbor, null if not found
     */
    public static rtrBfdNeigh bfdFindNeigh(ipFwd lower, addrIP adr) {
        for (int i = 0; i < lower.ifaces.size(); i++) {
            ipFwdIface ntry = lower.ifaces.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.bfdCfg == null) {
                continue;
            }
            rtrBfdNeigh res = ntry.bfdCfg.clientFind(adr);
            if (res != null) {
                return res;
            }
        }
        return null;
    }

    /**
     * list bfd neighbors
     *
     * @param lower forwarder
     * @return list of neighbors
     */
    public static userFormat bfdNeighShow(ipFwd lower) {
        userFormat l = new userFormat("|", "interface|address|state|uptime|clients");
        for (int i = 0; i < lower.ifaces.size(); i++) {
            ipFwdIface ntry = lower.ifaces.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.bfdCfg == null) {
                continue;
            }
            ntry.bfdCfg.getShNeighs(l);
        }
        return l;
    }

    /**
     * list hsrp neighbors
     *
     * @param lower forwarder
     * @return list of neighbors
     */
    public static userFormat hsrpNeighShow(ipFwd lower) {
        userFormat l = new userFormat("|", "interface|address|state|priority|uptime");
        for (int i = 0; i < lower.ifaces.size(); i++) {
            ipFwdIface ntry = lower.ifaces.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.hsrpCfg == null) {
                continue;
            }
            ntry.hsrpCfg.getShNeighs(l);
        }
        return l;
    }

    /**
     * list vrrp neighbors
     *
     * @param lower forwarder
     * @return list of neighbors
     */
    public static userFormat vrrpNeighShow(ipFwd lower) {
        userFormat l = new userFormat("|", "interface|address|priority|uptime");
        for (int i = 0; i < lower.ifaces.size(); i++) {
            ipFwdIface ntry = lower.ifaces.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.vrrpCfg == null) {
                continue;
            }
            ntry.vrrpCfg.getShNeighs(l);
        }
        return l;
    }

    /**
     * list pim neighbors
     *
     * @param lower forwarder
     * @return list of neighbors
     */
    public static userFormat pimNeighShow(ipFwd lower) {
        userFormat l = new userFormat("|", "interface|address|priority|uptime");
        for (int i = 0; i < lower.ifaces.size(); i++) {
            ipFwdIface ntry = lower.ifaces.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.pimCfg == null) {
                continue;
            }
            ntry.pimCfg.getShNeighs(l);
        }
        return l;
    }

    /**
     * list pim interfaces
     *
     * @param lower forwarder
     * @return list of interfaces
     */
    public static userFormat pimIfaceShow(ipFwd lower) {
        userFormat l = new userFormat("|", "interface|neighbors");
        for (int i = 0; i < lower.ifaces.size(); i++) {
            ipFwdIface ntry = lower.ifaces.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.pimCfg == null) {
                continue;
            }
            l.add(ntry + "|" + ntry.pimCfg.neighCount());
        }
        return l;
    }

    /**
     * list routing protocols
     *
     * @param lower forwarder
     * @return list of protocols
     */
    public static userFormat routersShow(ipFwd lower) {
        userFormat res = new userFormat("|", "proto|id|ifc|nei|uni|mlt|flw|chg|ago|uni|mlt|flw|chg|ago", "4|5computed|5redisted");
        for (int o = 0; o < lower.routers.size(); o++) {
            ipRtr rtr = lower.routers.get(o);
            res.add(cfgRtr.num2name(rtr.routerProtoTyp) + "|" + rtr.routerProcNum + "|" + rtr.routerIfaceCount() + "|" + rtr.routerNeighCount() + "|"
                    + rtr.routerComputedU.size() + "|" + rtr.routerComputedM.size() + "|" + rtr.routerComputedF.size() + "|"
                    + rtr.routerComputeChg + "|" + bits.timePast(rtr.routerComputeTim) + "|"
                    + rtr.routerRedistedU.size() + "|" + rtr.routerRedistedM.size() + "|" + rtr.routerRedistedF.size() + "|"
                    + rtr.routerRedistChg + "|" + bits.timePast(rtr.routerRedistTim));
        }
        return res;
    }

    /**
     * list routing protocols
     *
     * @param lower forwarder
     * @return list of protocols
     */
    public static userFormat statisticShow(ipFwd lower) {
        userFormat res = new userFormat("|", "category|value|addition");
        res.add("vrf name|" + lower.vrfName);
        res.add("vrf number|" + lower.vrfNum);
        res.add("ip version|" + lower.ipVersion);
        res.add("update run|" + lower.updateCount + "|times");
        res.add("update last|" + bits.timePast(lower.updateLast) + "|" + bits.time2str(cfgAll.timeZoneName, lower.updateLast + cfgAll.timeServerOffset, 3));
        res.add("update time|" + lower.updateTime + "|ms");
        res.add("full run|" + lower.updateFullCnt + "|times");
        res.add("full last|" + bits.timePast(lower.updateFullLst) + "|" + bits.time2str(cfgAll.timeZoneName, lower.updateFullLst + cfgAll.timeServerOffset, 3));
        res.add("incr run|" + lower.updateIncrCnt + "|times");
        res.add("incr last|" + bits.timePast(lower.updateIncrLst) + "|" + bits.time2str(cfgAll.timeZoneName, lower.updateIncrLst + cfgAll.timeServerOffset, 3));
        res.add("change run|" + lower.changeCount + "|times");
        res.add("change last|" + bits.timePast(lower.changeLast) + "|" + bits.time2str(cfgAll.timeZoneName, lower.changeLast + cfgAll.timeServerOffset, 3));
        res.add("connected|" + lower.connedR.size() + "|routes");
        res.add("direct|" + lower.directR.size() + "|routes");
        res.add("labeled|" + lower.labeldR.size() + "|routes");
        res.add("unicast|" + lower.actualU.size() + "|routes");
        res.add("multicast|" + lower.actualM.size() + "|routes");
        res.add("flowspec|" + lower.actualF.size() + "|routes");
        return res;
    }

    /**
     * get output for show
     *
     * @param lower forwarder
     * @return output
     */
    public static String vrfListShow(ipFwd lower) {
        return lower.ifaces.size() + "|" + lower.actualU.size() + "|" + lower.actualM.size() + "|" + lower.labeldR.size() + "|" + lower.groups.size() + "|" + lower.actualF.size() + "|" + lower.trafEngs.size() + "|" + lower.mp2mpLsp.size() + "|" + lower.natTrns.size() + "|" + lower.routers.size() + "|" + lower.cntrT.packRx + "|" + lower.cntrT.byteRx;
    }

    /**
     * update nat table
     *
     * @param lower forwarder
     * @param tim time
     */
    protected static void updateTableNat(ipFwd lower, long tim) {
        for (int i = lower.natTrns.size() - 1; i >= 0; i--) {
            tabNatTraN ntry = lower.natTrns.get(i);
            if (ntry == null) {
                continue;
            }
            if ((tim - ntry.lastUsed) < ntry.timeout) {
                continue;
            }
            lower.natTrns.del(ntry);
            lower.natTrns.del(ntry.reverse);
            if (ntry.logEnd) {
                logger.info("removing translation " + ntry);
            }
        }
        for (int i = 0; i < lower.natCfg.size(); i++) {
            tabNatCfgN ntry = lower.natCfg.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.newSrcIface != null) {
                addrIP adr = new addrIP();
                updateTableNat(lower, ntry.newSrcIface, adr);
                ntry.newSrcAddr = adr;
            }
            if (ntry.origTrgIface != null) {
                addrIP adr = new addrIP();
                updateTableNat(lower, ntry.origTrgIface, adr);
                ntry.origTrgAddr = adr;
            }
        }
    }

    private static void updateTableNat(ipFwd lower, cfgIfc iface, addrIP adr) {
        ipFwdIface ifc;
        if (lower.ipVersion == ipCor4.protocolVersion) {
            if (iface.addr4 != null) {
                adr.fromIPv4addr(iface.addr4);
            }
            ifc = iface.fwdIf4;
        } else {
            if (iface.addr6 != null) {
                adr.fromIPv6addr(iface.addr6);
            }
            ifc = iface.fwdIf6;
        }
        if (ifc == null) {
            adr.fillBytes(0);
            return;
        }
        if (ifc.ready) {
            return;
        }
        adr.fillBytes(0);
    }

    /**
     * notify routers about table change
     *
     * @param lower forwarder
     * @param ful full recomputation
     * @param chg main table changed
     * @param tim time
     */
    protected static void notifyRouters(ipFwd lower, boolean ful, boolean chg, long tim) {
        for (int i = 0; i < lower.routers.size(); i++) {
            ipRtr rtr = lower.routers.get(i);
            if (rtr == null) {
                continue;
            }
            tabRoute<addrIP> tabU = new tabRoute<addrIP>("redist");
            tabRoute<addrIP> tabM = new tabRoute<addrIP>("redist");
            tabRoute<addrIP> tabF = new tabRoute<addrIP>("redist");
            for (int o = 0; o < rtr.routerRedisting.size(); o++) {
                ipRtrRed ntry = rtr.routerRedisting.get(o);
                if (ntry == null) {
                    continue;
                }
                ntry.filter(rtrBgpUtil.sfiUnicast, tabU, lower.actualU);
                ntry.filter(rtrBgpUtil.sfiMulticast, tabM, lower.actualM);
                ntry.filter(rtrBgpUtil.sfiFlwSpc, tabF, lower.actualF);
            }
            for (int o = 0; o < rtr.routerAdvInter.size(); o++) {
                ipRtrInt ntry = rtr.routerAdvInter.get(o);
                if (ntry == null) {
                    continue;
                }
                ntry.filter(rtrBgpUtil.sfiUnicast, tabU, lower.actualU, lower);
                ntry.filter(rtrBgpUtil.sfiMulticast, tabM, lower.actualM, lower);
                ntry.filter(rtrBgpUtil.sfiFlwSpc, tabF, lower.actualF, lower);
            }
            for (int o = 0; o < rtr.routerAdverting.size(); o++) {
                ipRtrAdv ntry = rtr.routerAdverting.get(o);
                if (ntry == null) {
                    continue;
                }
                ntry.filter(rtrBgpUtil.sfiUnicast, tabU, lower.actualU);
                ntry.filter(rtrBgpUtil.sfiMulticast, tabM, lower.actualM);
                ntry.filter(rtrBgpUtil.sfiFlwSpc, tabF, lower.actualF);
            }
            if (!ful) {
                boolean diff = tabU.differs(tabRoute.addType.alters, rtr.routerRedistedU) || tabM.differs(tabRoute.addType.alters, rtr.routerRedistedM) || tabF.differs(tabRoute.addType.alters, rtr.routerRedistedF);
                if (chg) {
                    rtr.routerOthersChanged();
                }
                if (!diff) {
                    continue;
                }
            }
            tabU.optimize4lookup();
            tabM.optimize4lookup();
            tabF.optimize4lookup();
            tabU.version = rtr.routerRedistedU.version + 1;
            tabM.version = tabU.version;
            tabF.version = tabU.version;
            rtr.routerRedistedU = tabU;
            rtr.routerRedistedM = tabM;
            rtr.routerRedistedF = tabF;
            rtr.routerRedistChg++;
            rtr.routerRedistTim = tim;
            rtr.routerRedistChanged();
        }
    }

    private static tabRouteEntry<addrIP> convStaticRoute(ipFwdRoute ntry, tabRouteEntry<addrIP> imp, ipFwd lower, tabRoute<addrIP> trg, tabRoute<addrIP> conn) {
        if (imp.best.distance >= tabRouteAttr.distanMax) {
            return null;
        }
        if (ntry.recur == 0) {
            if (ntry.iface == null) {
                tabRouteEntry<addrIP> nh = conn.route(imp.best.nextHop);
                if (nh == null) {
                    return null;
                }
                imp.best.iface = nh.best.iface;
                return imp;
            }
            ipFwdIface ifc = new ipFwdIface(ntry.iface.ifwNum, null);
            ifc = lower.ifaces.find(ifc);
            if (ifc == null) {
                return null;
            }
            if (!ifc.ready) {
                return null;
            }
            if (!ifc.network.matches(imp.best.nextHop)) {
                return null;
            }
            imp.best.iface = ifc;
            return imp;
        }
        tabRouteEntry<addrIP> nh = trg.route(imp.best.nextHop);
        if (nh == null) {
            return null;
        }
        imp.best.iface = nh.best.iface;
        if (nh.best.rouTyp != tabRouteAttr.routeType.conn) {
            if (nh.best.nextHop == null) {
                return null;
            }
            imp.best.nextHop = nh.best.nextHop.copyBytes();
        }
        imp.best.time = nh.best.time;
        imp.best.rouTab = nh.best.rouTab;
        if (nh.best.segrouPrf != null) {
            imp.best.segrouPrf = nh.best.segrouPrf.copyBytes();
        }
        if (nh.best.labelRem != null) {
            imp.best.labelRem = tabLabel.prependLabels(imp.best.labelRem, nh.best.labelRem);
        }
        return imp;
    }

    private static void rstatic2table(ipFwdRoute ntry, tabRoute<addrIP> trg, int mode) {
        if (ntry == null) {
            return;
        }
        if (ntry.recur != mode) {
            return;
        }
        tabRouteEntry<addrIP> imp = ntry.getPrefix();
        if (imp == null) {
            return;
        }
        imp = convStaticRoute(ntry, imp, null, trg, null);
        if (imp == null) {
            return;
        }
        trg.add(tabRoute.addType.ecmp, imp, false, true);
    }

    private static void dstatic2table(ipFwdRoute ntry, tabRoute<addrIP> trg, ipFwd lower, tabRoute<addrIP> conn) {
        if (ntry == null) {
            return;
        }
        if (ntry.recur != 0) {
            return;
        }
        tabRouteEntry<addrIP> imp = ntry.getPrefix();
        if (imp == null) {
            return;
        }
        imp = convStaticRoute(ntry, imp, lower, trg, conn);
        if (imp == null) {
            return;
        }
        trg.add(tabRoute.addType.ecmp, imp, false, true);
    }

    private static void autoRouteTable(tabRoute<addrIP> tab, ipFwdIface ifc) {
        tabRouteEntry<addrIP> ntry = tab.route(ifc.autRouRtr);
        if (ntry == null) {
            return;
        }
        if (ntry.best.rouTyp != ifc.autRouTyp) {
            return;
        }
        if (ntry.best.protoNum != ifc.autRouPrt) {
            return;
        }
        if (ntry.best.srcRtr == null) {
            return;
        }
        for (int i = 0; i < tab.size(); i++) {
            tabRouteEntry<addrIP> prf = tab.get(i);
            if (prf.best.rouTyp != ntry.best.rouTyp) {
                continue;
            }
            if (prf.best.protoNum != ntry.best.protoNum) {
                continue;
            }
            if (ifc.autRouExcld) {
                if (prf.prefix.compare(prf.prefix, ntry.prefix) == 0) {
                    continue;
                }
            }
            if (ntry.best.oldHop != null) {
                if (prf.best.oldHop == null) {
                    continue;
                }
                if (prf.best.oldHop.compare(prf.best.oldHop, ntry.best.oldHop) != 0) {
                    continue;
                }
            } else {
                if (prf.best.srcRtr == null) {
                    continue;
                }
                if (prf.best.srcRtr.getSize() != ntry.best.srcRtr.getSize()) {
                    continue;
                }
                if (prf.best.srcRtr.compare(prf.best.srcRtr, ntry.best.srcRtr) != 0) {
                    continue;
                }
            }
            if (ifc.autRouPfxlst != null) {
                if (!ifc.autRouPfxlst.matches(rtrBgpUtil.sfiUnicast, 0, prf.prefix)) {
                    continue;
                }
            }
            if (ifc.autRouRoumap != null) {
                if (!ifc.autRouRoumap.matches(rtrBgpUtil.sfiUnicast, 0, prf.prefix)) {
                    continue;
                }
            }
            if (ifc.autRouRoupol != null) {
                if (tabRtrplc.doRpl(rtrBgpUtil.sfiUnicast, 0, prf, ifc.autRouRoupol, true) == null) {
                    continue;
                }
            }
            prf.best.iface = ifc;
            prf.best.nextHop = ifc.autRouHop.copyBytes();
            prf.best.labelRem = tabLabel.int2labels(ipMpls.labelImp);
            prf.reduce2best();
        }
    }

    private static boolean updateTableRouteEntry(ipFwd lower, int mode, tabRoute<addrIP> cmp, tabRouteEntry<addrIP> ntry) {
        tabGen<ipFwdRoute> sta = null;
        switch (mode) {
            case 1:
                sta = lower.staticU;
                break;
            case 2:
                sta = lower.staticM;
                break;
            case 3:
                sta = new tabGen<ipFwdRoute>();
        }
        tabRouteEntry<addrIP> best = lower.connedR.find(ntry);
        for (int i = 0; i < sta.size(); i++) {
            ipFwdRoute cur = sta.get(i);
            if (cur == null) {
                continue;
            }
            if (ntry.prefix.compare(ntry.prefix, cur.pref) != 0) {
                continue;
            }
            tabRouteEntry<addrIP> imp = cur.getPrefix();
            if (imp == null) {
                continue;
            }
            imp = convStaticRoute(cur, imp, lower, cmp, lower.directR);
            if (imp == null) {
                continue;
            }
            if (best == null) {
                best = imp;
                continue;
            }
            if (!best.best.isOtherBetter(imp.best, false)) {
                continue;
            }
            best = imp;
        }
        for (int i = 0; i < lower.routers.size(); i++) {
            ipRtr rtr = lower.routers.get(i);
            if (rtr == null) {
                continue;
            }
            tabRoute<addrIP> res = null;
            switch (mode) {
                case 1:
                    res = rtr.routerComputedU;
                    break;
                case 2:
                    res = rtr.routerComputedM;
                    break;
                case 3:
                    res = rtr.routerComputedF;
                    break;
            }
            tabRouteEntry<addrIP> imp = res.find(ntry);
            if (imp == null) {
                continue;
            }
            if (imp.best.distance >= tabRouteAttr.distanMax) {
                continue;
            }
            imp = imp.copyBytes(rtr.getAddMode());
            if ((mode < 3) && (rtr.isBGP() == 1)) {
                if (tabRoute.doNexthopFix(imp, res, lower.directR, rtr.routerRecursions())) {
                    continue;
                }
            }
            if (best == null) {
                best = imp;
                continue;
            }
            if (!best.best.isOtherBetter(imp.best, false)) {
                continue;
            }
            best = imp;
        }
        if (best == null) {
            return !cmp.del(ntry);
        }
        if (mode == 1) {
            updateTableRouteLabels(lower, lower.actualU, best, lower.labeldR.find(ntry));
        }
        cmp.add(tabRoute.addType.always, best, false, false);
        return true;
    }

    private static boolean updateTableRouteIncr(ipFwd lower, int mode, tabRoute<addrIP> chg, tabRoute<addrIP> cmp) {
        boolean chgd = false;
        for (int i = chg.size() - 1; i >= 0; i--) {
            tabRouteEntry<addrIP> ntry = chg.get(i);
            chg.del(ntry);
            chgd |= updateTableRouteEntry(lower, mode, cmp, ntry);
        }
        return chgd;
    }

    /**
     * update route table parts
     *
     * @param lower forwarder
     * @return false if no change, true if updated
     */
    protected static boolean updateTableRouteIncr(ipFwd lower) {
        boolean chg = false;
        chg |= updateTableRouteIncr(lower, 1, lower.changedUni, lower.actualU);
        chg |= updateTableRouteIncr(lower, 2, lower.changedMlt, lower.actualM);
        chg |= updateTableRouteIncr(lower, 3, lower.changedFlw, lower.actualF);
        return chg;
    }

    /**
     * update route table
     *
     * @param lower forwarder
     * @return false if no change, true if updated
     */
    protected static boolean updateTableRouteFull(ipFwd lower) {
        lower.changedUni.clear();
        lower.changedMlt.clear();
        lower.changedFlw.clear();
        tabRoute<addrIP> tabC = new tabRoute<addrIP>("connected");
        tabC.defDist = 0;
        tabC.defMetr = 0;
        tabC.defRouTyp = tabRouteAttr.routeType.conn;
        tabRoute<addrIP> tabL = new tabRoute<addrIP>("labeled");
        tabL.defDist = tabRouteAttr.distanMax;
        tabRoute<addrIP> tabM = new tabRoute<addrIP>("rpf");
        tabM.defDist = tabRouteAttr.distanMax;
        tabRoute<addrIP> tabF = new tabRoute<addrIP>("flwspc");
        tabF.defDist = tabRouteAttr.distanMax;
        tabRoute<addrIP> tabU = new tabRoute<addrIP>("locals");
        tabU.defDist = 0;
        tabU.defMetr = 1;
        tabU.defRouTyp = tabRouteAttr.routeType.local;
        tabGen<tabIndex<addrIP>> tabIU = new tabGen<tabIndex<addrIP>>();
        for (int i = 0; i < lower.ifaces.size(); i++) {
            ipFwdIface ifc = lower.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (!ifc.ready) {
                continue;
            }
            tabRouteEntry<addrIP> prf = tabC.add(tabRoute.addType.always, ifc.network, null);
            prf.best.iface = ifc;
            prf.best.rouTyp = tabRouteAttr.routeType.conn;
            if (ifc.gateLoc) {
                prf = tabU.add(tabRoute.addType.always, new addrPrefix<addrIP>(ifc.addr, ifc.addr.maxBits()), null);
                prf.best.iface = ifc;
                prf.best.rouTyp = tabRouteAttr.routeType.local;
            }
            if (ifc.linkLocal) {
                addrPrefix<addrIP> pre;
                if (lower.ipCore.getVersion() == ipCor4.protocolVersion) {
                    addrIPv4 adr4 = addrIPv4.genLinkLocal();
                    pre = addrPrefix.ip4toIP(new addrPrefix<addrIPv4>(adr4, 16));
                } else {
                    addrIPv6 adr6 = addrIPv6.genLinkLocal(new addrMac());
                    pre = addrPrefix.ip6toIP(new addrPrefix<addrIPv6>(adr6, 64));
                }
                prf = tabC.add(tabRoute.addType.always, pre, null);
                prf.best.iface = ifc;
                prf.best.rouTyp = tabRouteAttr.routeType.conn;
            }
            addrIP gtw = ifc.gateAddr;
            if (gtw == null) {
                continue;
            }
            if (ifc.gateRem) {
                prf = tabC.add(tabRoute.addType.always, new addrPrefix<addrIP>(gtw, gtw.maxBits()), null);
                prf.best.iface = ifc;
                prf.best.rouTyp = tabRouteAttr.routeType.remote;
                prf.best.nextHop = gtw.copyBytes();
            }
            tabListing<tabPrfxlstN, addrIP> pfl = ifc.gatePrfx;
            if (pfl == null) {
                continue;
            }
            for (int o = 0; o < pfl.size(); o++) {
                prf = new tabRouteEntry<addrIP>();
                prf.best.distance = 0;
                prf.best.metric = 2;
                prf.prefix = pfl.get(o).getPrefix();
                prf.best.nextHop = gtw.copyBytes();
                prf.best.rouTyp = tabRouteAttr.routeType.defpref;
                prf.best.iface = ifc;
                tabRoute.addUpdatedEntry(tabRoute.addType.better, tabU, rtrBgpUtil.sfiUnicast, 0, prf, true, ifc.gateRtmp, null, null);
            }
        }
        tabL.mergeFrom(tabRoute.addType.better, tabC, tabRouteAttr.distanLim);
        tabL.mergeFrom(tabRoute.addType.better, tabU, tabRouteAttr.distanLim);
        tabM.mergeFrom(tabRoute.addType.better, tabC, tabRouteAttr.distanLim);
        tabM.mergeFrom(tabRoute.addType.better, tabU, tabRouteAttr.distanLim);
        for (int i = 0; i < lower.staticU.size(); i++) {
            dstatic2table(lower.staticU.get(i), tabL, lower, tabC);
        }
        for (int i = 0; i < lower.staticM.size(); i++) {
            dstatic2table(lower.staticM.get(i), tabM, lower, tabC);
        }
        tabL.delDistance(tabRouteAttr.distanMax);
        tabM.delDistance(tabRouteAttr.distanMax);
        tabF.delDistance(tabRouteAttr.distanMax);
        tabL.preserveTime(lower.actualU);
        tabM.preserveTime(lower.actualM);
        tabF.preserveTime(lower.actualF);
        for (int i = 0; i < lower.routers.size(); i++) {
            ipRtr rtr = lower.routers.get(i);
            if (rtr == null) {
                continue;
            }
            if (rtr.isBGP() != 0) {
                continue;
            }
            tabRoute.addType adm = rtr.getAddMode();
            tabL.mergeFrom(adm, rtr.routerComputedU, tabRouteAttr.distanMax);
            tabM.mergeFrom(adm, rtr.routerComputedM, tabRouteAttr.distanMax);
            tabF.mergeFrom(adm, rtr.routerComputedF, tabRouteAttr.distanMax);
            tabIndex.mergeTable(tabIU, rtr.routerComputedI);
        }
        for (int i = 0; i < lower.staticU.size(); i++) {
            rstatic2table(lower.staticU.get(i), tabL, 1);
        }
        for (int i = 0; i < lower.staticM.size(); i++) {
            rstatic2table(lower.staticM.get(i), tabM, 1);
        }
        for (int i = 0; i < lower.ifaces.size(); i++) {
            ipFwdIface ifc = lower.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (!ifc.ready) {
                continue;
            }
            if (ifc.autRouTyp == null) {
                continue;
            }
            if (ifc.autRouRec) {
                continue;
            }
            if (!ifc.autRouUnic) {
                autoRouteTable(tabL, ifc);
            }
            if (ifc.autRouMcst) {
                autoRouteTable(tabM, ifc);
            }
        }
        tabU = new tabRoute<addrIP>("locals");
        tabU.mergeFrom(tabRoute.addType.ecmp, tabL, tabRouteAttr.distanLim);
        tabRoute<addrIP> tabD = new tabRoute<addrIP>("locals");
        tabD.mergeFrom(tabRoute.addType.ecmp, tabL, tabRouteAttr.distanLim);
        for (int i = 0; i < lower.routers.size(); i++) {
            ipRtr rtr = lower.routers.get(i);
            if (rtr == null) {
                continue;
            }
            if (rtr.isBGP() != 1) {
                continue;
            }
            tabRoute.addType adm = rtr.getAddMode();
            int rec = rtr.routerRecursions();
            tabU.mergeFrom(adm, rtr.routerComputedU, tabL, rec, tabRouteAttr.distanMax);
            tabM.mergeFrom(adm, rtr.routerComputedM, tabL, rec, tabRouteAttr.distanMax);
            tabF.mergeFrom(adm, rtr.routerComputedF, tabRouteAttr.distanMax);
            tabIndex.mergeTable(tabIU, rtr.routerComputedI);
        }
        for (int i = 0; i < lower.staticU.size(); i++) {
            rstatic2table(lower.staticU.get(i), tabU, 2);
        }
        for (int i = 0; i < lower.staticM.size(); i++) {
            rstatic2table(lower.staticM.get(i), tabM, 2);
        }
        for (int i = 0; i < lower.routers.size(); i++) {
            ipRtr rtr = lower.routers.get(i);
            if (rtr == null) {
                continue;
            }
            if (rtr.isBGP() != 2) {
                continue;
            }
            tabRoute.addType adm = rtr.getAddMode();
            tabU.mergeFrom(adm, rtr.routerComputedU, tabRouteAttr.distanMax);
            tabM.mergeFrom(adm, rtr.routerComputedM, tabRouteAttr.distanMax);
            tabF.mergeFrom(adm, rtr.routerComputedF, tabRouteAttr.distanMax);
            tabIndex.mergeTable(tabIU, rtr.routerComputedI);
        }
        for (int i = 0; i < lower.staticU.size(); i++) {
            rstatic2table(lower.staticU.get(i), tabU, 3);
        }
        for (int i = 0; i < lower.staticM.size(); i++) {
            rstatic2table(lower.staticM.get(i), tabM, 3);
        }
        lower.incrCandid = true;
        for (int i = 0; i < lower.ifaces.size(); i++) {
            ipFwdIface ifc = lower.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (!ifc.ready) {
                continue;
            }
            if (ifc.autRouTyp == null) {
                continue;
            }
            if (!ifc.autRouRec) {
                continue;
            }
            if (!ifc.autRouUnic) {
                lower.incrCandid = false;
                autoRouteTable(tabU, ifc);
            }
            if (ifc.autRouMcst) {
                lower.incrCandid = false;
                autoRouteTable(tabM, ifc);
            }
        }
        for (int i = 0; i < tabU.size(); i++) {
            tabRouteEntry<addrIP> ntry = tabU.get(i);
            tabRouteEntry<addrIP> old = lower.actualU.find(ntry);
            if (old == null) {
                continue;
            }
            ntry.cntr = old.cntr;
            ntry.hwCntr = old.hwCntr;
        }
        switch (lower.prefixMode) {
            case igp:
                break;
            case host:
                for (int i = tabL.size() - 1; i >= 0; i--) {
                    tabRouteEntry<addrIP> ntry = tabL.get(i);
                    if (ntry == null) {
                        continue;
                    }
                    if (ntry.prefix.maskLen >= (addrIP.size * 8)) {
                        continue;
                    }
                    tabL.del(ntry);
                }
                break;
            case all:
                lower.incrCandid = false;
                tabL = new tabRoute<addrIP>("labeled");
                tabL.mergeFrom(tabRoute.addType.ecmp, tabU, tabRouteAttr.distanLim);
                break;
            default:
                tabL = new tabRoute<addrIP>("labeled");
                break;
        }
        tabRoute.filterTable(rtrBgpUtil.sfiUnicast, 0, tabL, lower.labelFilter);
        for (int i = 0; i < lower.labeldR.size(); i++) {
            tabRouteEntry<addrIP> old = lower.labeldR.get(i);
            if (old == null) {
                continue;
            }
            if (old.best.labelLoc == null) {
                continue;
            }
            if (old.best.labelLoc.label == lower.commonLabel.label) {
                continue;
            }
            if (tabLabel.find(old.best.labelLoc.label) == null) {
                continue;
            }
            tabRouteEntry<addrIP> cur = tabL.find(old);
            if (cur == null) {
                tabLabel.release(old.best.labelLoc, 2);
                continue;
            }
            for (int o = 0; o < cur.alts.size(); o++) {
                cur.alts.get(o).labelLoc = old.best.labelLoc;
            }
        }
        for (int i = 0; i < tabL.size(); i++) {
            tabRouteEntry<addrIP> ntry = tabL.get(i);
            if (ntry.best.labelLoc != null) {
                continue;
            }
            if (ntry.best.nextHop == null) {
                ntry.best.labelLoc = lower.commonLabel;
                continue;
            }
            tabLabelEntry lab = tabLabel.allocate(2);
            for (int o = 0; o < ntry.alts.size(); o++) {
                ntry.alts.get(o).labelLoc = lab;
            }
        }
        for (int i = 0; i < tabU.size(); i++) {
            tabRouteEntry<addrIP> ntry = tabU.get(i);
            updateTableRouteLabels(lower, tabU, ntry, tabL.find(ntry));
        }
        lower.commonLabel.setFwdCommon(1, lower);
        tabRoute<addrIP> tabT = new tabRoute<addrIP>("amt");
        for (int i = 0; i < lower.routers.size(); i++) {
            ipRtr rtr = lower.routers.get(i);
            if (rtr == null) {
                continue;
            }
            if (rtr.routerAutoMesh == null) {
                continue;
            }
            lower.incrCandid = false;
            rtr.routerNeighList(tabT);
        }
        for (int i = lower.autoMesh.size(); i >= 0; i--) {
            clntMplsTeP2p clnt = lower.autoMesh.get(i);
            if (clnt == null) {
                continue;
            }
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = new addrPrefix<addrIP>(clnt.target, addrIP.size * 8);
            if (tabT.find(ntry) != null) {
                if (tabU.route(ntry.prefix.network) != null) {
                    continue;
                }
            }
            if (debugger.clntMplsAutMsh) {
                logger.debug("stopping " + clnt);
            }
            lower.autoMesh.del(clnt);
            clnt.workStop();
        }
        for (int i = 0; i < tabT.size(); i++) {
            tabRouteEntry<addrIP> ntry = tabT.get(i);
            tabRouteEntry<addrIP> rou = tabU.find(ntry);
            if (rou != null) {
                if (rou.best.nextHop == null) {
                    continue;
                }
            } else {
                tabRouteEntry<addrIP> old = tabU.route(ntry.prefix.network);
                if (old == null) {
                    continue;
                }
                addrIP hop;
                if (old.best.rouTyp == tabRouteAttr.routeType.conn) {
                    hop = ntry.prefix.network;
                } else {
                    hop = old.best.nextHop;
                }
                if (hop == null) {
                    continue;
                }
                rou = new tabRouteEntry<addrIP>();
                rou.prefix = ntry.prefix;
                rou.best.nextHop = hop;
                rou.best.iface = old.best.iface;
                rou.best.metric = 3;
                rou.best.rouTyp = tabRouteAttr.routeType.automesh;
                tabU.add(tabRoute.addType.better, rou, false, false);
            }
            clntMplsTeP2p clnt = new clntMplsTeP2p();
            clnt.target = ntry.prefix.network.copyBytes();
            clntMplsTeP2p old = lower.autoMesh.find(clnt);
            if (old != null) {
                ipFwdTrfng trf = old.getTraffEng();
                if (trf == null) {
                    continue;
                }
                if (trf.trgIfc != rou.best.iface) {
                    continue;
                }
                if (trf.trgHop.compare(trf.trgHop, rou.best.nextHop) != 0) {
                    continue;
                }
                rou.best.labelRem = tabLabel.prependLabels(rou.best.labelRem, tabLabel.int2labels(trf.trgLab));
                if (rou.best.labelLoc == null) {
                    continue;
                }
                rou.best.labelLoc.setFwdMpls(2, lower, (ipFwdIface) rou.best.iface, rou.best.nextHop, rou.best.labelRem);
                continue;
            }
            lower.autoMesh.add(clnt);
            clnt.fwdCor = lower;
            clnt.fwdIfc = null;
            clnt.descr = cfgAll.hostName + ":automesh";
            clnt.expr = 0;
            clnt.ttl = 255;
            clnt.prioS = 7;
            clnt.prioH = 7;
            clnt.bndwdt = 0;
            clnt.recRou = false;
            clnt.setUpper(new ifcNull(false, false));
            clnt.workStart();
            if (debugger.clntMplsAutMsh) {
                logger.debug("starting " + clnt);
            }
        }
        if (lower.routeLimit > 0) {
            for (int i = tabU.size() - 1; i > lower.routeLimit; i--) {
                tabRouteEntry<addrIP> ntry = tabU.get(i);
                tabU.del(ntry);
            }
            lower.incrCandid = false;
        }
        if ((!tabC.differs(tabRoute.addType.alters, lower.connedR)) && (!tabD.differs(tabRoute.addType.alters, lower.directR)) && (!tabL.differs(tabRoute.addType.alters, lower.labeldR)) && (!tabU.differs(tabRoute.addType.alters, lower.actualU)) && (!tabM.differs(tabRoute.addType.alters, lower.actualM)) && (!tabF.differs(tabRoute.addType.alters, lower.actualF))) {
            return false;
        }
        tabGen<tabIndex<addrIP>> tabIC = new tabGen<tabIndex<addrIP>>();
        for (int i = 0; i < tabIU.size(); i++) {
            tabIndex<addrIP> ntry = tabIU.get(i);
            if (ntry.conned) {
                tabIC.add(ntry);
            }
            tabIndex<addrIP> old = lower.actualIU.find(ntry);
            if (old == null) {
                continue;
            }
            ntry.cntr = old.cntr;
            ntry.hwCntr = old.hwCntr;
        }
        tabC.optimize4lookup();
        tabD.optimize4lookup();
        tabL.optimize4lookup();
        tabU.optimize4lookup();
        tabM.optimize4lookup();
        tabF.optimize4lookup();
        tabU.version = lower.actualU.version + 1;
        tabL.version = tabU.version;
        tabC.version = tabU.version;
        tabD.version = tabU.version;
        tabM.version = tabU.version;
        tabF.version = tabU.version;
        lower.connedR = tabC;
        lower.directR = tabD;
        lower.labeldR = tabL;
        lower.actualU = tabU;
        lower.actualM = tabM;
        lower.actualF = tabF;
        lower.actualIU = tabIU;
        lower.actualIC = tabIC;
        return true;
    }

    private static void updateTableRouteLabels(ipFwd lower, tabRoute<addrIP> tabU, tabRouteEntry<addrIP> ntry, tabRouteEntry<addrIP> locN) {
        for (int o = 0; o < ntry.alts.size(); o++) {
            tabRouteAttr<addrIP> alt = ntry.alts.get(o);
            tabRouteAttr<addrIP> loc = null;
            if (locN != null) {
                loc = locN.sameFwder(alt);
            }
            updateTableRouteLabels(lower, tabU, ntry, alt, loc);
        }
    }

    private static void updateTableRouteLabels(ipFwd lower, tabRoute<addrIP> tabU, tabRouteEntry<addrIP> prefix, tabRouteAttr<addrIP> ntry, tabRouteAttr<addrIP> loc) {
        if (loc != null) {
            ntry.labelLoc = loc.labelLoc;
        }
        if (ntry.nextHop == null) {
            return;
        }
        if (ntry.labelLoc != null) {
            ipFwd vrf = lower;
            ipFwdIface ifc = (ipFwdIface) ntry.iface;
            addrIP hop = ntry.nextHop;
            List<Integer> lrs = ntry.labelRem;
            if (ntry.rouTab != null) {
                vrf = ntry.rouTab;
                tabRouteEntry<addrIP> nh = vrf.actualU.route(hop);
                if (nh != null) {
                    ifc = (ipFwdIface) nh.best.iface;
                    hop = nh.best.nextHop;
                    lrs = tabLabel.prependLabels(new ArrayList<Integer>(), ntry.labelRem);
                    lrs = tabLabel.prependLabels(lrs, nh.best.labelRem);
                }
            }
            if (hop != null) {
                ntry.labelLoc.setFwdMpls(2, vrf, ifc, hop, lrs);
            } else {
                ntry.labelLoc.setFwdDrop(2);
            }
        }
        if (ntry.rouTab != null) {
            return;
        }
        rtrLdpNeigh nei = lower.ldpNeighFind(null, ntry.nextHop, false);
        if (nei == null) {
            if (ntry.oldHop == null) {
                return;
            }
            tabRouteEntry<addrIP> prf = tabU.route(ntry.oldHop);
            if (prf == null) {
                return;
            }
            updateTableRouteLabels(ntry, loc, prf.best);
            return;
        }
        tabRouteEntry<addrIP> rem = nei.prefLearn.find(prefix);
        if (rem != null) {
            updateTableRouteLabels(ntry, loc, rem.best);
            return;
        }
        if (ntry.oldHop == null) {
            return;
        }
        tabRouteEntry<addrIP> prf = tabU.route(ntry.oldHop);
        if (prf == null) {
            return;
        }
        rem = nei.prefLearn.find(prf);
        if (rem == null) {
            updateTableRouteLabels(ntry, loc, prf.best);
        } else {
            updateTableRouteLabels(ntry, loc, rem.best);
        }

    }

    private static void updateTableRouteLabels(tabRouteAttr<addrIP> ntry, tabRouteAttr<addrIP> loc, tabRouteAttr<addrIP> rem) {
        ntry.labelRem = tabLabel.prependLabels(ntry.labelRem, rem.labelRem);
        if (loc != null) {
            loc.labelRem = tabLabel.prependLabels(loc.labelRem, rem.labelRem);
        }
        if (ntry.labelLoc != null) {
            ntry.labelLoc.remoteLab = tabLabel.prependLabels(ntry.labelLoc.remoteLab, rem.labelRem);
        }
    }

    /**
     * send join to one group
     *
     * @param lower forwarder
     * @param grp group to join
     * @param need 1=join, 0=prune
     */
    protected static void joinOneGroup(ipFwd lower, ipFwdMcast grp, int need) {
        if ((need == 1) && (grp.upsVrf != null)) {
            ipFwdMpmp ntry = ipFwdMpmp.create4vpnMcast(false, grp.upstream, lower.rd, grp);
            ntry.vrfRx = lower;
            grp.upsVrf.mldpAdd(ntry);
        }
        ipFwdIface ifc = grp.iface;
        if (ifc == null) {
            return;
        }
        if (ifc.mldpCfg != null) {
            ifc.mldpCfg.sendJoin(grp, need == 1);
        }
        if (ifc.pimCfg != null) {
            ifc.pimCfg.sendJoin(grp, need);
        }
        if (ifc.mhostCfg != null) {
            ifc.mhostCfg.sendJoin(grp, need == 1);
        }
    }

    /**
     * set multicast source interface
     *
     * @param lower forwarder
     * @param grp group to update
     */
    protected static void updateOneGroup(ipFwd lower, ipFwdMcast grp) {
        tabRouteEntry<addrIP> prf = lower.actualM.route(grp.source);
        if (prf == null) {
            grp.iface = null;
            grp.upstream = null;
            return;
        }
        grp.iface = (ipFwdIface) prf.best.iface;
        if (prf.best.nextHop == null) {
            grp.upstream = grp.source.copyBytes();
        } else {
            grp.upstream = prf.best.nextHop.copyBytes();
        }
        if (!lower.mdt) {
            grp.upsVrf = null;
            return;
        }
        grp.upsVrf = prf.best.rouTab;
    }

    /**
     * update group table
     *
     * @param lower forwarder
     * @param tim time
     */
    protected static void updateTableGroup(ipFwd lower, long tim) {
        for (int o = lower.groups.size(); o >= 0; o--) {
            ipFwdMcast grp = lower.groups.get(o);
            if (grp == null) {
                continue;
            }
            addrIP oldup = null;
            if (grp.upstream != null) {
                oldup = grp.upstream.copyBytes();
            }
            updateOneGroup(lower, grp);
            boolean needed = grp.local || (grp.flood.size() > 0);
            for (int i = grp.flood.size(); i >= 0; i--) {
                ipFwdIface ifc = grp.flood.get(i);
                if (ifc == null) {
                    continue;
                }
                if (ifc.expires < 0) {
                    continue;
                }
                if (ifc.expires > tim) {
                    continue;
                }
                grp.flood.del(ifc);
            }
            if (grp.label != null) {
                if (grp.label.vrfUpl != null) {
                    if (grp.label.vrfUpl.mp2mpLsp.find(grp.label) == null) {
                        grp.label = null;
                    }
                } else {
                    if (lower.mp2mpLsp.find(grp.label) == null) {
                        grp.label = null;
                    }
                }
                needed = true;
            }
            if (grp.bier != null) {
                int per = grp.bier.purgePeers(tim);
                grp.bier.updatePeers();
                if (per < 1) {
                    grp.bier = null;
                }
                needed = true;
            }
            if (!needed) {
                lower.groups.del(grp);
                joinOneGroup(lower, grp, 0);
                continue;
            }
            if (grp.upstream == null) {
                continue;
            }
            if (oldup == null) {
                joinOneGroup(lower, grp, 1);
                continue;
            }
            if (oldup.compare(oldup, grp.upstream) == 0) {
                continue;
            }
            joinOneGroup(lower, grp, 1);
        }
    }

    /**
     * update echo table
     *
     * @param lower forwarder
     * @param tim time
     */
    protected static void updateTableEcho(ipFwd lower, long tim) {
        for (int i = lower.echoes.size(); i >= 0; i--) {
            ipFwdEcho ntry = lower.echoes.get(i);
            if (ntry == null) {
                continue;
            }
            if ((tim - ntry.created) < 10000) {
                continue;
            }
            lower.echoes.del(ntry);
            ntry.notif.wakeup();
        }
    }

    /**
     * update traffic engineering table
     *
     * @param lower forwarder
     * @param tim time
     */
    protected static void updateTableTrfng(ipFwd lower, long tim) {
        for (int i = lower.trafEngs.size(); i >= 0; i--) {
            ipFwdTrfng ntry = lower.trafEngs.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.srcLoc == 1) {
                continue;
            }
            if ((tim - ntry.updated) < ntry.timeout) {
                if (ntry.subId == 0) {
                    continue;
                }
                ntry = ntry.getParent();
                ntry = lower.trafEngs.find(ntry);
                if (ntry == null) {
                    continue;
                }
                ntry.updated = tim;
                continue;
            }
            lower.trafEngs.del(ntry);
            ntry.labStop();
        }
    }

    /**
     * fill strict first hop
     *
     * @param lower forwarder to use
     * @param ntry traffeng entry
     * @param pck packet to update
     * @return false on success, true on error
     */
    public static boolean fillRsvpFrst(ipFwd lower, ipFwdTrfng ntry, packRsvp pck) {
        if (ntry.trgHop == null) {
            return true;
        }
        if (pck.expRout.size() > 0) {
            tabHop hop = pck.expRout.get(0);
            if (hop.strict == true) {
                return false;
            }
        }
        tabHop hop = new tabHop();
        hop.adr = ntry.trgHop.copyBytes();
        hop.strict = true;
        pck.expRout.add(0, hop);
        return false;
    }

    private static packRsvp fillRsvpPack(ipFwd lower, ipFwdTrfng ntry) {
        addrIP trg = ntry.trgAdr;
        if (ntry.midAdrs.size() > 0) {
            trg = ntry.midAdrs.get(0).adr;
        }
        tabRouteEntry<addrIP> rt = lower.actualU.route(trg);
        if (rt == null) {
            ntry.srcLoc = 4;
            return null;
        }
        ntry.trgIfc = (ipFwdIface) rt.best.iface;
        if (ntry.trgIfc == null) {
            ntry.srcLoc = 4;
            return null;
        }
        addrIP oldHop = ntry.trgHop;
        ntry.trgHop = new addrIP();
        if (rt.best.nextHop != null) {
            ntry.trgHop.setAddr(rt.best.nextHop);
        } else {
            ntry.trgHop.setAddr(trg);
        }
        if (oldHop != null) {
            if (oldHop.compare(oldHop, ntry.trgHop) != 0) {
                ntry.srcLoc = 4;
                return null;
            }
        }
        packRsvp pck = new packRsvp();
        pck.adsBndwdt = ntry.bwdt;
        pck.adsCmtu = ntry.trgIfc.mtu;
        pck.adsHops = 1;
        pck.adsLtncy = 0;
        pck.expRout = new ArrayList<tabHop>();
        for (int i = 0; i < ntry.midAdrs.size(); i++) {
            pck.expRout.add(ntry.midAdrs.get(i).copyBytes());
        }
        tabHop hop = new tabHop();
        hop.adr = ntry.trgAdr.copyBytes();
        hop.strict = false;
        pck.expRout.add(hop);
        pck.flwSpcPcks = ntry.trgIfc.mtu;
        pck.flwSpcPeak = ntry.bwdt;
        pck.flwSpcPlcd = 0;
        pck.flwSpcRate = ntry.bwdt;
        pck.flwSpcSize = 1000;
        pck.hopAdr = ntry.trgIfc.addr.copyBytes();
        pck.hopId = ntry.trgIfc.ifwNum;
        pck.sessAdr = ntry.trgAdr.copyBytes();
        pck.sessId = ntry.trgId;
        pck.subAddr = ntry.trgAdr.copyBytes();
        if (ntry.asocAdr != null) {
            pck.assocAdr = ntry.asocAdr.copyBytes();
            pck.assocId = ntry.asocId;
            pck.assocGlb = ntry.asocGlb;
            pck.assocTyp = ntry.asocTyp;
        }
        pck.sessHld = 7;
        pck.sessStp = 7;
        pck.sessFlg = 0x04; // se style
        pck.sessNam = "" + ntry.descr;
        if (ntry.recRou) {
            pck.recRout = new ArrayList<tabHop>();
        }
        pck.sndrAdr = ntry.srcAdr;
        pck.sndrId = ntry.srcId;
        pck.sbgrpOrg = ntry.subAdr;
        pck.sbgrpId = ntry.subId;
        pck.styleVal = 18;
        pck.timeVal = lower.untriggeredRecomputation;
        pck.ttl = 255;
        return pck;
    }

    /**
     * send refresh to local traffic engineering tunnel
     *
     * @param lower forwarder
     * @param ntry tunnel to refresh
     */
    protected static void refreshTrfngAdd(ipFwd lower, ipFwdTrfng ntry) {
        packRsvp pckRrp = fillRsvpPack(lower, ntry);
        if (pckRrp == null) {
            return;
        }
        fillRsvpFrst(lower, ntry, pckRrp);
        packHolder pckBin = new packHolder(true, true);
        pckRrp.createHolder(pckBin);
        pckRrp.fillLabReq();
        pckRrp.createDatPatReq(pckBin);
        pckRrp.createHeader(pckBin);
        lower.protoPack(ntry.trgIfc, ntry.trgHop, pckBin);
        if (debugger.rtrRsvpTraf) {
            logger.debug("tx " + pckRrp);
        }
    }

    /**
     * send refresh to local traffic engineering tunnel
     *
     * @param lower forwarder
     * @param ntry tunnel to refresh
     */
    protected static void refreshTrfngDel(ipFwd lower, ipFwdTrfng ntry) {
        packRsvp pckRrp = fillRsvpPack(lower, ntry);
        if (pckRrp == null) {
            return;
        }
        packHolder pckBin = new packHolder(true, true);
        pckBin.clear();
        pckRrp.createHolder(pckBin);
        pckRrp.createDatPatTer(pckBin);
        pckRrp.createHeader(pckBin);
        lower.protoPack(ntry.trgIfc, ntry.trgHop, pckBin);
        if (debugger.rtrRsvpTraf) {
            logger.debug("tx " + pckRrp);
        }
    }

    /**
     * update multipoint ldp table
     *
     * @param lower forwarder
     */
    protected static void updateTableMplsp(ipFwd lower) {
        for (int i = lower.mp2mpLsp.size() - 1; i >= 0; i--) {
            ipFwdMpmp ntry = lower.mp2mpLsp.get(i);
            if (ntry == null) {
                continue;
            }
            ipFwdMcast grp = ipFwdMpmp.decode4multicast(ntry);
            ipFwd vrf = null;
            if (grp != null) {
                if (grp.rd == 0) {
                    vrf = lower;
                } else {
                    ntry.vrfUpl = lower;
                    cfgVrf v = cfgAll.findRd(grp.rd);
                    if (v != null) {
                        vrf = v.getFwd(grp.group);
                        if (!vrf.mdt) {
                            vrf = null;
                        }
                    }
                }
            }
            if (vrf != null) {
                if (vrf.groups.find(grp) == null) {
                    ntry.local = false;
                }
            }
            ntry.updateState(lower);
            if (ntry.local) {
                continue;
            }
            if (vrf != null) {
                if (ntry.selfRoot) {
                    if (ntry.neighs.size() > 0) {
                        vrf.mcastAddFloodMpls(grp.group, grp.source, ntry);
                        continue;
                    } else {
                        vrf.mcastAddFloodMpls(grp.group, grp.source, null);
                    }
                } else {
                    vrf.mcastAddFloodMpls(grp.group, grp.source, null);
                }
            }
            if (ntry.neighs.size() > 0) {
                continue;
            }
            lower.mp2mpLsp.del(ntry);
        }
    }

    /**
     * update every table
     *
     * @param lower forwarder
     * @return false if no change, true if updated
     */
    protected static boolean updateEverything(ipFwd lower) {
        long tim = bits.getTime();
        int ful = lower.needFull.set(0);
        boolean chg = ful > 0;
        chg |= (lower.changedUni.size() + lower.changedMlt.size() + lower.changedFlw.size()) > lower.incrLimit;
        chg |= !lower.incrCandid;
        if (chg) {
            lower.updateFullCnt++;
            lower.updateFullLst = tim;
            chg = updateTableRouteFull(lower);
        } else {
            lower.updateIncrCnt++;
            lower.updateIncrLst = tim;
            chg = updateTableRouteIncr(lower);
        }
        updateTableNat(lower, tim);
        updateTableGroup(lower, tim);
        updateTableEcho(lower, tim);
        updateTableTrfng(lower, tim);
        updateTableMplsp(lower);
        notifyRouters(lower, (ful & 2) != 0, chg, tim);
        lower.tableChanger();
        lower.updateLast = bits.getTime();
        lower.updateCount++;
        lower.updateTime = (int) (lower.updateLast - tim);
        if (chg) {
            lower.changeLast = lower.updateLast;
            lower.changeCount++;
        }
        return chg;
    }

    /**
     * check for stalled vrfs
     */
    public static void checkVrfs() {
        for (int i = cfgAll.vrfs.size() - 1; i >= 0; i--) {
            cfgVrf ntry = cfgAll.vrfs.get(i);
            ntry.fwd4.hstryH.update(ntry.fwd4.cntrH);
            ntry.fwd6.hstryH.update(ntry.fwd6.cntrH);
            ntry.fwd4.hstryT.update(ntry.fwd4.cntrT);
            ntry.fwd6.hstryT.update(ntry.fwd6.cntrT);
            ntry.fwd4.hstryL.update(ntry.fwd4.cntrL);
            ntry.fwd6.hstryL.update(ntry.fwd6.cntrL);
        }
    }

}
