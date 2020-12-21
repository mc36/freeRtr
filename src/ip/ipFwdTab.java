package ip;

import addr.addrIP;
import addr.addrIPv6;
import addr.addrMac;
import addr.addrPrefix;
import cfg.cfgAll;
import cfg.cfgIfc;
import cfg.cfgRtr;
import cfg.cfgVrf;
import clnt.clntMplsTeP2p;
import ifc.ifcNull;
import java.util.ArrayList;
import java.util.List;
import pack.packHolder;
import pack.packRsvp;
import rtr.rtrBfdNeigh;
import rtr.rtrBgpUtil;
import rtr.rtrLdpNeigh;
import tab.tabHop;
import tab.tabLabel;
import tab.tabLabelNtry;
import tab.tabListing;
import tab.tabNatCfgN;
import tab.tabNatTraN;
import tab.tabPrfxlstN;
import tab.tabRoute;
import tab.tabRouteAttr;
import tab.tabRouteEntry;
import user.userFormat;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;

/**
 * calculates ip forwarding tables
 *
 * @author matecsaba
 */
public class ipFwdTab {

    private ipFwdTab() {
    }

    /**
     * convert interface number to name
     *
     * @param fwd forwarding core to use
     * @param iface interface number
     * @return interface name
     */
    public static String iface2name(ipFwd fwd, int iface) {
        if (iface == 0) {
            return "null";
        }
        if (fwd == null) {
            return "#" + iface;
        }
        ipFwdIface ifc = fwd.ifaces.find(new ipFwdIface(iface, null));
        if (ifc == null) {
            return "!" + iface;
        }
        return "" + ifc;
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
            if (ifc.mask >= best.mask) {
                best = ifc;
                seen = true;
            }
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
        lower.protos.dump(lower, l, b);
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
        res.add("change run|" + lower.changeCount + "|times");
        res.add("change last|" + bits.timePast(lower.changeLast) + "|" + bits.time2str(cfgAll.timeZoneName, lower.changeLast + cfgAll.timeServerOffset, 3));
        res.add("connected|" + lower.connedR.size() + "|routes");
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
     */
    protected static void updateTableNat(ipFwd lower) {
        long tim = bits.getTime();
        for (int i = lower.natTrns.size() - 1; i >= 0; i--) {
            tabNatTraN ntry = lower.natTrns.get(i);
            if (ntry == null) {
                continue;
            }
            if ((tim - ntry.lastUsed) < ntry.timeout) {
                continue;
            }
            lower.natTrns.del(ntry);
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
     * @param chg main table changed
     */
    protected static void notifyRouters(ipFwd lower, boolean chg) {
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
            boolean diff = tabU.differs(tabRoute.addType.alters, rtr.routerRedistedU) || tabM.differs(tabRoute.addType.alters, rtr.routerRedistedM) || tabF.differs(tabRoute.addType.alters, rtr.routerRedistedF);
            if (chg) {
                rtr.routerOthersChanged();
            }
            if (!diff) {
                continue;
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
            rtr.routerRedistTim = bits.getTime();
            rtr.routerRedistChanged();
        }
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
        if (imp.best.distance >= tabRouteAttr.distanMax) {
            return;
        }
        tabRouteEntry<addrIP> nh = trg.route(imp.best.nextHop);
        if (nh == null) {
            return;
        }
        imp.best.iface = nh.best.iface;
        if (nh.best.rouTyp != tabRouteAttr.routeType.conn) {
            if (nh.best.nextHop == null) {
                return;
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
        if (ntry.iface == null) {
            tabRouteEntry<addrIP> nh = conn.route(imp.best.nextHop);
            if (nh == null) {
                return;
            }
            imp.best.iface = nh.best.iface;
        } else {
            ipFwdIface ifc = new ipFwdIface(ntry.iface.ifwNum, null);
            ifc = lower.ifaces.find(ifc);
            if (ifc == null) {
                return;
            }
            if (!ifc.ready) {
                return;
            }
            if (!ifc.network.matches(imp.best.nextHop)) {
                return;
            }
            imp.best.iface = ifc;
        }
        trg.add(tabRoute.addType.ecmp, imp, false, true);
    }

    /**
     * update route table
     *
     * @param lower forwarder
     * @return false if no change, true if updated
     */
    protected static boolean updateTableRoute(ipFwd lower) {
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
        for (int i = 0; i < lower.ifaces.size(); i++) {
            ipFwdIface ifc = lower.ifaces.get(i);
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
                addrIPv6 adr6 = addrIPv6.genLinkLocal(new addrMac());
                addrIP adr = new addrIP();
                adr.fromIPv6addr(adr6);
                prf = tabC.add(tabRoute.addType.always, new addrPrefix<addrIP>(adr, 64), null);
                prf.best.iface = ifc;
                prf.best.rouTyp = tabRouteAttr.routeType.conn;
            }
            addrIP gtw = ifc.gateAddr;
            if (gtw == null) {
                continue;
            }
            if (ifc.gateRem) {
                prf = tabU.add(tabRoute.addType.always, new addrPrefix<addrIP>(gtw, gtw.maxBits()), null);
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
        tabL.mergeFrom(tabRoute.addType.better, tabC, null, true, tabRouteAttr.distanLim);
        tabL.mergeFrom(tabRoute.addType.better, tabU, null, true, tabRouteAttr.distanLim);
        tabM.mergeFrom(tabRoute.addType.better, tabC, null, true, tabRouteAttr.distanLim);
        tabM.mergeFrom(tabRoute.addType.better, tabU, null, true, tabRouteAttr.distanLim);
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
            tabL.mergeFrom(rtr.getAddMode(), rtr.routerComputedU, null, true, tabRouteAttr.distanMax);
            tabM.mergeFrom(rtr.getAddMode(), rtr.routerComputedM, null, true, tabRouteAttr.distanMax);
            tabF.mergeFrom(rtr.getAddMode(), rtr.routerComputedF, null, true, tabRouteAttr.distanMax);
        }
        for (int i = 0; i < lower.staticU.size(); i++) {
            rstatic2table(lower.staticU.get(i), tabL, 1);
        }
        for (int i = 0; i < lower.staticM.size(); i++) {
            rstatic2table(lower.staticM.get(i), tabM, 1);
        }
        for (int i = 0; i < lower.ifaces.size(); i++) {
            ipFwdIface ifc = lower.ifaces.get(i);
            if (!ifc.ready) {
                continue;
            }
            if (ifc.autRouTyp == null) {
                continue;
            }
            tabRouteEntry<addrIP> ntry = tabL.route(ifc.autRouRtr);
            if (ntry == null) {
                continue;
            }
            if (ntry.best.rouTyp != ifc.autRouTyp) {
                continue;
            }
            if (ntry.best.protoNum != ifc.autRouPrt) {
                continue;
            }
            if (ntry.best.srcRtr == null) {
                continue;
            }
            for (int o = 0; o < tabL.size(); o++) {
                tabRouteEntry<addrIP> prf = tabL.get(o);
                if (prf.best.rouTyp != ntry.best.rouTyp) {
                    continue;
                }
                if (prf.best.protoNum != ntry.best.protoNum) {
                    continue;
                }
                if (prf.best.srcRtr == null) {
                    continue;
                }
                if (prf.best.srcRtr.getSize() != ntry.best.srcRtr.getSize()) {
                    continue;
                }
                if (prf.best.srcRtr.compare(prf.best.srcRtr, ntry.best.srcRtr) != 0) {
                    continue;
                }
                if (prf.prefix.compare(prf.prefix, ntry.prefix) == 0) {
                    continue;
                }
                prf.best.iface = ifc;
                prf.best.nextHop = ifc.autRouHop.copyBytes();
                prf.best.labelRem = null;
                prf.reduce2best();
            }
        }
        tabU = new tabRoute<addrIP>("locals");
        tabU.mergeFrom(tabRoute.addType.ecmp, tabL, null, true, tabRouteAttr.distanLim);
        for (int i = 0; i < lower.routers.size(); i++) {
            ipRtr rtr = lower.routers.get(i);
            if (rtr == null) {
                continue;
            }
            if (rtr.isBGP() != 1) {
                continue;
            }
            tabU.mergeFrom(rtr.getAddMode(), rtr.routerComputedU, tabL, true, tabRouteAttr.distanMax);
            tabM.mergeFrom(rtr.getAddMode(), rtr.routerComputedM, tabL, true, tabRouteAttr.distanMax);
            tabF.mergeFrom(rtr.getAddMode(), rtr.routerComputedF, null, true, tabRouteAttr.distanMax);
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
            tabU.mergeFrom(rtr.getAddMode(), rtr.routerComputedU, null, true, tabRouteAttr.distanMax);
            tabM.mergeFrom(rtr.getAddMode(), rtr.routerComputedM, null, true, tabRouteAttr.distanMax);
            tabF.mergeFrom(rtr.getAddMode(), rtr.routerComputedF, null, true, tabRouteAttr.distanMax);
        }
        for (int i = 0; i < lower.staticU.size(); i++) {
            rstatic2table(lower.staticU.get(i), tabU, 3);
        }
        for (int i = 0; i < lower.staticM.size(); i++) {
            rstatic2table(lower.staticM.get(i), tabM, 3);
        }
        if (lower.counterMap != null) {
            for (int i = 0; i < tabU.size(); i++) {
                tabRouteEntry<addrIP> ntry = tabU.get(i);
                if (!lower.counterMap.matches(rtrBgpUtil.sfiUnicast, 0, ntry)) {
                    continue;
                }
                tabRouteEntry<addrIP> old = lower.actualU.find(ntry);
                if (old != null) {
                    ntry.cntr = old.cntr;
                    ntry.hwCntr = old.hwCntr;
                }
                if (ntry.cntr == null) {
                    ntry.cntr = new counter();
                }
            }
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
                tabL = new tabRoute<addrIP>("labeled");
                tabL.mergeFrom(tabRoute.addType.ecmp, tabU, null, true, tabRouteAttr.distanLim);
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
            tabLabelNtry lab = tabLabel.allocate(2);
            for (int o = 0; o < ntry.alts.size(); o++) {
                ntry.alts.get(o).labelLoc = lab;
            }
        }
        for (int i = 0; i < tabU.size(); i++) {
            tabRouteEntry<addrIP> ntry = tabU.get(i);
            tabRouteEntry<addrIP> locN = tabL.find(ntry);
            for (int o = 0; o < ntry.alts.size(); o++) {
                tabRouteAttr<addrIP> alt = ntry.alts.get(o);
                tabRouteAttr<addrIP> loc = null;
                if (locN != null) {
                    loc = locN.sameFwder(alt);
                }
                updateTableRouteLabels(lower, tabU, ntry, alt, loc);
            }
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
        if ((!tabC.differs(tabRoute.addType.alters, lower.connedR)) && (!tabL.differs(tabRoute.addType.alters, lower.labeldR)) && (!tabU.differs(tabRoute.addType.alters, lower.actualU)) && (!tabM.differs(tabRoute.addType.alters, lower.actualM)) && (!tabF.differs(tabRoute.addType.alters, lower.actualF))) {
            return false;
        }
        tabC.optimize4lookup();
        tabL.optimize4lookup();
        tabU.optimize4lookup();
        tabM.optimize4lookup();
        tabF.optimize4lookup();
        tabU.version = lower.actualU.version + 1;
        tabL.version = tabU.version;
        tabC.version = tabU.version;
        tabM.version = tabU.version;
        tabF.version = tabU.version;
        lower.connedR = tabC;
        lower.labeldR = tabL;
        lower.actualU = tabU;
        lower.actualM = tabM;
        lower.actualF = tabF;
        return true;
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
     */
    protected static void updateTableGroup(ipFwd lower) {
        long tim = bits.getTime();
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
                if (lower.mp2mpLsp.find(grp.label) == null) {
                    grp.label = null;
                }
                needed = true;
            }
            if (grp.bier != null) {
                if (grp.bier.purgePeers(tim) < 1) {
                    grp.bier.workStop();
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
     */
    protected static void updateTableEcho(ipFwd lower) {
        long tim = bits.getTime();
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
     */
    protected static void updateTableTrfng(ipFwd lower) {
        long tim = bits.getTime();
        for (int i = lower.trafEngs.size(); i >= 0; i--) {
            ipFwdTrfng ntry = lower.trafEngs.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.srcLoc == 1) {
                continue;
            }
            if ((tim - ntry.created) < ntry.timeout) {
                if (ntry.subId == 0) {
                    continue;
                }
                ntry = ntry.getParent();
                ntry = lower.trafEngs.find(ntry);
                if (ntry == null) {
                    continue;
                }
                ntry.created = tim;
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
            ntry.trgLab = -1;
            ntry.srcLoc = 2;
            return null;
        }
        ntry.trgIfc = (ipFwdIface) rt.best.iface;
        if (ntry.trgIfc == null) {
            ntry.trgLab = -1;
            ntry.srcLoc = 2;
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
                ntry.trgLab = -1;
                ntry.srcLoc = 2;
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
        boolean chg = updateTableRoute(lower);
        updateTableNat(lower);
        updateTableGroup(lower);
        updateTableEcho(lower);
        updateTableTrfng(lower);
        updateTableMplsp(lower);
        notifyRouters(lower, chg);
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
            ntry.fwd4.hstryT.update(ntry.fwd4.cntrT);
            ntry.fwd6.hstryT.update(ntry.fwd6.cntrT);
            ntry.fwd4.hstryL.update(ntry.fwd4.cntrL);
            ntry.fwd6.hstryL.update(ntry.fwd6.cntrL);
        }
    }

}
