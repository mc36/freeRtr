package ip;

import addr.addrIP;
import addr.addrIPv6;
import addr.addrMac;
import addr.addrPrefix;
import cfg.cfgAll;
import cfg.cfgRtr;
import cfg.cfgVrf;
import clnt.clntMplsTeP2p;
import ifc.ifcNull;
import java.util.ArrayList;
import java.util.List;
import pack.packHolder;
import pack.packRsvp;
import rtr.rtrBgpUtil;
import rtr.rtrLdpNeigh;
import tab.tabHop;
import tab.tabLabel;
import tab.tabLabelNtry;
import tab.tabListing;
import tab.tabNatCfgN;
import tab.tabNatTraN;
import tab.tabPlcmapN;
import tab.tabPrfxlstN;
import tab.tabRoute;
import tab.tabRouteEntry;
import tab.tabRtrmapN;
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
        if (prf.rouTab != null) {
            return findStableIface(lower);
        }
        return (ipFwdIface) prf.iface;
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
        switch (prf.rouTyp) {
            case conn:
            case remote:
            case defpref:
            case automesh:
                break;
            default:
                return null;
        }
        return (ipFwdIface) prf.iface;
    }

    /**
     * find stable interface
     *
     * @param lower forwarder
     * @return interface id, null if none
     */
    public static ipFwdIface findStableIface(ipFwd lower) {
        ipFwdIface best = new ipFwdIface(0, null);
        for (int i = lower.ifaces.size() - 1; i >= 0; i--) {
            ipFwdIface ifc = lower.ifaces.get(i);
            if (ifc.mask > best.mask) {
                best = ifc;
            }
        }
        if (best.mask < 1) {
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
     * list ldp neighbors
     *
     * @param lower forwarder
     * @return list of neighbors
     */
    public static userFormat ldpNeighShow(ipFwd lower) {
        userFormat l = new userFormat("|", "learn|advert|l2learn|l2advert|mplearn|mpadvert|neighbor|uptime");
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
        userFormat l = new userFormat("|", "interface|address|state|priority");
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
        userFormat l = new userFormat("|", "interface|address|priority");
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
        userFormat res = new userFormat("|", "proto|id|ifc|nei|cmp|chg|ago|red|chg|ago");
        for (int o = 0; o < lower.routers.size(); o++) {
            ipRtr rtr = lower.routers.get(o);
            res.add(cfgRtr.num2name(rtr.routerProtoTyp) + "|" + rtr.routerProcNum + "|" + rtr.routerIfaceCount() + "|" + rtr.routerNeighCount() + "|"
                    + rtr.routerComputedU.size() + "/" + rtr.routerComputedM.size() + "/" + rtr.routerComputedF.size() + "|"
                    + rtr.routerComputeChg + "|" + bits.timePast(rtr.routerComputeTim) + "|"
                    + rtr.routerRedistedU.size() + "/" + rtr.routerRedistedM.size() + "/" + rtr.routerRedistedF.size() + "|"
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
        userFormat res = new userFormat("|", "category|value");
        res.add("vrf name|" + lower.vrfName);
        res.add("vrf number|" + lower.vrfNum);
        res.add("ip version|" + lower.ipVersion);
        res.add("update run|" + lower.updateCount + " times");
        res.add("update last|" + bits.time2str(cfgAll.timeZoneName, lower.updateLast + cfgAll.timeServerOffset, 3) + " (" + bits.timePast(lower.updateLast) + " ago)");
        res.add("update time|" + lower.updateTime + " ms");
        res.add("connected|" + lower.connedR.size() + " routes");
        res.add("labeled|" + lower.labeldR.size() + " routes");
        res.add("unicast|" + lower.actualU.size() + " routes");
        res.add("multicast|" + lower.actualM.size() + " routes");
        res.add("flowspec|" + lower.actualF.size() + " routes");
        return res;
    }

    /**
     * get output for show
     *
     * @param lower forwarder
     * @return output
     */
    public static String vrfListShow(ipFwd lower) {
        return lower.ifaces.size() + "|" + lower.actualU.size() + "|" + lower.actualM.size() + "|" + lower.labeldR.size() + "|" + lower.groups.size() + "|" + lower.actualF.size() + "|" + lower.trafEngs.size() + "|" + lower.mp2mpLsp.size() + "|" + lower.natTrns.size() + "|" + lower.routers.size() + "|" + lower.cntr.packRx + "|" + lower.cntr.byteRx;
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
            if ((tim - ntry.lastUsed) < lower.natTimeout) {
                continue;
            }
            lower.natTrns.del(ntry);
        }
        for (int i = 0; i < lower.natCfg.size(); i++) {
            tabNatCfgN ntry = lower.natCfg.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.newSrcIface == null) {
                continue;
            }
            addrIP adr = new addrIP();
            ipFwdIface ifc;
            if (lower.ipVersion == ipCor4.protocolVersion) {
                if (ntry.newSrcIface.addr4 != null) {
                    adr.fromIPv4addr(ntry.newSrcIface.addr4);
                }
                ifc = ntry.newSrcIface.fwdIf4;
            } else {
                if (ntry.newSrcIface.addr6 != null) {
                    adr.fromIPv6addr(ntry.newSrcIface.addr6);
                }
                ifc = ntry.newSrcIface.fwdIf6;
            }
            if (ifc != null) {
                if (!ifc.ready) {
                    adr.fillBytes(0);
                }
            } else {
                adr.fillBytes(0);
            }
            ntry.newSrcAddr = adr;
        }
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
                ntry.filter(rtrBgpUtil.safiUnicast, tabU, lower.actualU);
                ntry.filter(rtrBgpUtil.safiMulticast, tabM, lower.actualM);
                ntry.filter(rtrBgpUtil.safiFlwSpc, tabF, lower.actualF);
            }
            for (int o = 0; o < rtr.routerAdvInter.size(); o++) {
                ipRtrInt ntry = rtr.routerAdvInter.get(o);
                if (ntry == null) {
                    continue;
                }
                ntry.filter(rtrBgpUtil.safiUnicast, tabU, lower.actualU, lower);
                ntry.filter(rtrBgpUtil.safiMulticast, tabM, lower.actualM, lower);
                ntry.filter(rtrBgpUtil.safiFlwSpc, tabF, lower.actualF, lower);
            }
            for (int o = 0; o < rtr.routerAdverting.size(); o++) {
                ipRtrAdv ntry = rtr.routerAdverting.get(o);
                if (ntry == null) {
                    continue;
                }
                ntry.filter(rtrBgpUtil.safiUnicast, tabU, lower.actualU);
                ntry.filter(rtrBgpUtil.safiMulticast, tabM, lower.actualM);
                ntry.filter(rtrBgpUtil.safiFlwSpc, tabF, lower.actualF);
            }
            boolean diff = tabU.differs(rtr.routerRedistedU) || tabM.differs(rtr.routerRedistedM) || tabF.differs(rtr.routerRedistedF);
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

    private static void static2table(ipFwdRoute ntry, tabRoute<addrIP> trg, ipFwd lower, tabRoute<addrIP> conn) {
        if (ntry == null) {
            return;
        }
        tabRouteEntry<addrIP> imp = ntry.getPrefix();
        if (imp == null) {
            return;
        }
        if (ntry.iface == null) {
            tabRouteEntry<addrIP> nh = conn.route(imp.nextHop);
            if (nh == null) {
                return;
            }
            imp.iface = nh.iface;
        } else {
            ipFwdIface ifc = new ipFwdIface(ntry.iface.ifwNum, null);
            ifc = lower.ifaces.find(ifc);
            if (ifc == null) {
                return;
            }
            if (!ifc.ready) {
                return;
            }
            if (!ifc.network.matches(imp.nextHop)) {
                return;
            }
            imp.iface = ifc;
        }
        trg.add(tabRoute.addType.better, imp, false, true);
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
        tabC.defRouTyp = tabRouteEntry.routeType.conn;
        tabRoute<addrIP> tabL = new tabRoute<addrIP>("labeled");
        tabL.defDist = tabRouteEntry.distanMax;
        tabRoute<addrIP> tabM = new tabRoute<addrIP>("rpf");
        tabM.defDist = tabRouteEntry.distanMax;
        tabRoute<addrIP> tabF = new tabRoute<addrIP>("flwspc");
        tabF.defDist = tabRouteEntry.distanMax;
        tabRoute<addrIP> tabU = new tabRoute<addrIP>("locals");
        tabU.defDist = 0;
        tabU.defMetr = 1;
        tabU.defRouTyp = tabRouteEntry.routeType.local;
        for (int i = 0; i < lower.ifaces.size(); i++) {
            ipFwdIface ifc = lower.ifaces.get(i);
            if (!ifc.ready) {
                continue;
            }
            tabRouteEntry<addrIP> prf = tabC.add(tabRoute.addType.always, ifc.network, null);
            prf.iface = ifc;
            prf.rouTyp = tabRouteEntry.routeType.conn;
            prf = tabU.add(tabRoute.addType.always, new addrPrefix<addrIP>(ifc.addr, ifc.addr.maxBits()), null);
            prf.iface = ifc;
            prf.rouTyp = tabRouteEntry.routeType.local;
            if (ifc.linkLocal) {
                addrIPv6 adr6 = addrIPv6.genLinkLocal(new addrMac());
                addrIP adr = new addrIP();
                adr.fromIPv6addr(adr6);
                prf = tabC.add(tabRoute.addType.always, new addrPrefix<addrIP>(adr, 64), null);
                prf.iface = ifc;
                prf.rouTyp = tabRouteEntry.routeType.conn;
            }
            addrIP gtw = ifc.gateAddr;
            if (gtw == null) {
                continue;
            }
            prf = tabU.add(tabRoute.addType.always, new addrPrefix<addrIP>(gtw, gtw.maxBits()), null);
            prf.iface = ifc;
            prf.rouTyp = tabRouteEntry.routeType.remote;
            tabListing<tabPrfxlstN, addrIP> pfl = ifc.gatePrfx;
            if (pfl == null) {
                continue;
            }
            for (int o = 0; o < pfl.size(); o++) {
                prf = new tabRouteEntry<addrIP>();
                tabU.updateBase(prf);
                prf.metric = 2;
                prf.prefix = pfl.get(o).getPrefix();
                prf.nextHop = gtw.copyBytes();
                prf.rouTyp = tabRouteEntry.routeType.defpref;
                prf.iface = ifc;
                tabRoute.addUpdatedEntry(tabRoute.addType.better, tabU, rtrBgpUtil.safiUnicast, prf, ifc.gateRtmp, null, null);
            }
        }
        tabL.mergeFrom(tabRoute.addType.better, tabC, null, true, tabRouteEntry.distanLim);
        tabL.mergeFrom(tabRoute.addType.better, tabU, null, true, tabRouteEntry.distanLim);
        tabM.mergeFrom(tabRoute.addType.better, tabC, null, true, tabRouteEntry.distanLim);
        tabM.mergeFrom(tabRoute.addType.better, tabU, null, true, tabRouteEntry.distanLim);
        for (int i = 0; i < lower.staticU.size(); i++) {
            static2table(lower.staticU.get(i), tabL, lower, tabC);
        }
        for (int i = 0; i < lower.staticM.size(); i++) {
            static2table(lower.staticM.get(i), tabM, lower, tabC);
        }
        tabL.delDistance(tabRouteEntry.distanMax);
        tabM.delDistance(tabRouteEntry.distanMax);
        tabF.delDistance(tabRouteEntry.distanMax);
        for (int i = 0; i < lower.routers.size(); i++) {
            ipRtr rtr = lower.routers.get(i);
            if (rtr == null) {
                continue;
            }
            if (rtr.isBGP() != 0) {
                continue;
            }
            tabRoute<addrIP> tabT = rtr.routerComputedU;
            tabT.setProto(rtr.routerProtoTyp, rtr.routerProcNum);
            tabL.mergeFrom(tabRoute.addType.better, tabT, null, true, tabRouteEntry.distanMax);
            tabT = rtr.routerComputedM;
            tabT.setProto(rtr.routerProtoTyp, rtr.routerProcNum);
            tabM.mergeFrom(tabRoute.addType.better, tabT, null, true, tabRouteEntry.distanMax);
            tabT = rtr.routerComputedF;
            tabT.setProto(rtr.routerProtoTyp, rtr.routerProcNum);
            tabF.mergeFrom(tabRoute.addType.better, tabT, null, true, tabRouteEntry.distanMax);
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
            if (ntry.rouTyp != ifc.autRouTyp) {
                continue;
            }
            if (ntry.protoNum != ifc.autRouPrt) {
                continue;
            }
            if (ntry.srcRtr == null) {
                continue;
            }
            for (int o = 0; o < tabL.size(); o++) {
                tabRouteEntry<addrIP> prf = tabL.get(o);
                if (prf.rouTyp != ntry.rouTyp) {
                    continue;
                }
                if (prf.protoNum != ntry.protoNum) {
                    continue;
                }
                if (prf.srcRtr == null) {
                    continue;
                }
                if (prf.srcRtr.getSize() != ntry.srcRtr.getSize()) {
                    continue;
                }
                if (prf.srcRtr.compare(prf.srcRtr, ntry.srcRtr) != 0) {
                    continue;
                }
                if (prf.prefix.compare(prf.prefix, ntry.prefix) == 0) {
                    continue;
                }
                prf.iface = ifc;
                prf.nextHop = ifc.autRouHop.copyBytes();
                prf.labelRem = null;
            }
        }
        tabU = new tabRoute<addrIP>("locals");
        tabU.mergeFrom(tabRoute.addType.better, tabL, null, true, tabRouteEntry.distanLim);
        for (int i = 0; i < lower.routers.size(); i++) {
            ipRtr rtr = lower.routers.get(i);
            if (rtr == null) {
                continue;
            }
            if (rtr.isBGP() != 1) {
                continue;
            }
            tabU.mergeFrom(tabRoute.addType.better, rtr.routerComputedU, tabL, true, tabRouteEntry.distanMax);
            tabM.mergeFrom(tabRoute.addType.better, rtr.routerComputedM, tabL, true, tabRouteEntry.distanMax);
            tabF.mergeFrom(tabRoute.addType.better, rtr.routerComputedF, null, true, tabRouteEntry.distanMax);
        }
        for (int i = 0; i < lower.routers.size(); i++) {
            ipRtr rtr = lower.routers.get(i);
            if (rtr == null) {
                continue;
            }
            if (rtr.isBGP() != 2) {
                continue;
            }
            tabU.mergeFrom(tabRoute.addType.better, rtr.routerComputedU, null, true, tabRouteEntry.distanMax);
            tabM.mergeFrom(tabRoute.addType.better, rtr.routerComputedM, null, true, tabRouteEntry.distanMax);
            tabF.mergeFrom(tabRoute.addType.better, rtr.routerComputedF, null, true, tabRouteEntry.distanMax);
        }
        if (lower.counterMap != null) {
            for (int i = 0; i < tabU.size(); i++) {
                tabRouteEntry<addrIP> ntry = tabU.get(i);
                tabRtrmapN rmn = lower.counterMap.find(1, ntry);
                if (rmn == null) {
                    continue;
                }
                if (rmn.action != tabPlcmapN.actionType.actPermit) {
                    continue;
                }
                tabRouteEntry<addrIP> old = lower.actualU.find(ntry);
                if (old != null) {
                    ntry.cntr = old.cntr;
                }
                if (ntry.cntr == null) {
                    ntry.cntr = new counter();
                }
            }
        }
        switch (lower.prefixMode) {
            case igp:
                break;
            case all:
                tabL = new tabRoute<addrIP>("labeled");
                tabL.mergeFrom(tabRoute.addType.better, tabU, null, true, tabRouteEntry.distanLim);
                break;
            default:
                tabL = new tabRoute<addrIP>("labeled");
                break;
        }
        tabRoute.filterTable(rtrBgpUtil.safiUnicast, tabL, lower.labelFilter);
        for (int i = 0; i < lower.labeldR.size(); i++) {
            tabRouteEntry<addrIP> old = lower.labeldR.get(i);
            if (old == null) {
                continue;
            }
            if (old.labelLoc == null) {
                continue;
            }
            if (old.labelLoc.getValue() == lower.commonLabel.getValue()) {
                continue;
            }
            if (tabLabel.find(old.labelLoc.getValue()) == null) {
                continue;
            }
            tabRouteEntry<addrIP> cur = tabL.find(old);
            if (cur == null) {
                tabLabel.release(old.labelLoc, 2);
                continue;
            }
            cur.labelLoc = old.labelLoc;
        }
        for (int i = 0; i < tabL.size(); i++) {
            tabRouteEntry<addrIP> ntry = tabL.get(i);
            if (ntry.labelLoc != null) {
                continue;
            }
            if (ntry.nextHop == null) {
                ntry.labelLoc = lower.commonLabel;
                continue;
            }
            tabLabelNtry lab = tabLabel.allocate(2);
            ntry.labelLoc = lab;
        }
        for (int i = 0; i < tabU.size(); i++) {
            tabRouteEntry<addrIP> ntry = tabU.get(i);
            tabRouteEntry<addrIP> loc = tabL.find(ntry);
            if (loc != null) {
                ntry.labelLoc = loc.labelLoc;
            }
            if (ntry.nextHop == null) {
                continue;
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
                        ifc = (ipFwdIface) nh.iface;
                        hop = nh.nextHop;
                        lrs = tabLabel.prependLabels(new ArrayList<Integer>(), ntry.labelRem);
                        lrs = tabLabel.prependLabels(lrs, nh.labelRem);
                    }
                }
                if (hop != null) {
                    ntry.labelLoc.setFwdMpls(2, vrf, ifc, hop, lrs);
                } else {
                    ntry.labelLoc.setFwdDrop(2);
                }
            }
            if (ntry.rouTab != null) {
                continue;
            }
            rtrLdpNeigh nei = lower.ldpNeighFind(null, ntry.nextHop, false);
            if (nei == null) {
                if (ntry.oldHop == null) {
                    continue;
                }
                tabRouteEntry<addrIP> prf = tabU.route(ntry.oldHop);
                if (prf == null) {
                    continue;
                }
                updateTableRouteLabels(ntry, loc, prf);
                continue;
            }
            tabRouteEntry<addrIP> rem = null;
            if (ntry.oldHop == null) {
                rem = nei.prefLearn.find(ntry);
            } else {
                tabRouteEntry<addrIP> prf = tabU.route(ntry.oldHop);
                if (prf == null) {
                    continue;
                }
                rem = nei.prefLearn.find(prf);
            }
            if (rem == null) {
                continue;
            }
            updateTableRouteLabels(ntry, loc, rem);
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
                if (rou.nextHop == null) {
                    continue;
                }
            } else {
                tabRouteEntry<addrIP> old = tabU.route(ntry.prefix.network);
                if (old == null) {
                    continue;
                }
                addrIP hop;
                if (old.rouTyp == tabRouteEntry.routeType.conn) {
                    hop = ntry.prefix.network;
                } else {
                    hop = old.nextHop;
                }
                if (hop == null) {
                    continue;
                }
                rou = new tabRouteEntry<addrIP>();
                rou.prefix = ntry.prefix;
                rou.nextHop = hop;
                rou.iface = old.iface;
                rou.metric = 3;
                rou.rouTyp = tabRouteEntry.routeType.automesh;
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
                if (trf.trgIfc != rou.iface) {
                    continue;
                }
                if (trf.trgHop.compare(trf.trgHop, rou.nextHop) != 0) {
                    continue;
                }
                rou.labelRem = tabLabel.prependLabels(rou.labelRem, tabLabel.int2labels(trf.trgLab));
                if (rou.labelLoc == null) {
                    continue;
                }
                rou.labelLoc.setFwdMpls(2, lower, (ipFwdIface) rou.iface, rou.nextHop, rou.labelRem);
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
        if ((!tabU.differs(lower.actualU)) && (!tabM.differs(lower.actualM)) && (!tabF.differs(lower.actualF))) {
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

    private static void updateTableRouteLabels(tabRouteEntry<addrIP> ntry, tabRouteEntry<addrIP> loc, tabRouteEntry<addrIP> rem) {
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
        grp.iface = (ipFwdIface) prf.iface;
        if (prf.nextHop == null) {
            grp.upstream = grp.source.copyBytes();
        } else {
            grp.upstream = prf.nextHop.copyBytes();
        }
        if (!lower.mdt) {
            grp.upsVrf = null;
            return;
        }
        grp.upsVrf = prf.rouTab;
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
     * @param pck packet to update
     * @return false on success, true on error
     */
    public static boolean fillRsvpFrst(ipFwd lower, packRsvp pck) {
        tabRouteEntry<addrIP> ntry = lower.actualU.route(pck.getTrg());
        if (ntry == null) {
            return true;
        }
        if (ntry.nextHop == null) {
            return false;
        }
        if (pck.expRout.size() > 0) {
            tabHop hop = pck.expRout.get(0);
            if (hop.strict == true) {
                return false;
            }
        }
        tabHop hop = new tabHop();
        hop.adr = ntry.nextHop.copyBytes();
        hop.strict = true;
        pck.expRout.add(0, hop);
        return false;
    }

    private static packRsvp fillRsvpPack(ipFwd lower, ipFwdTrfng ntry) {
        ntry.trgIfc = findSendingIface(lower, ntry.trgAdr);
        if (ntry.trgIfc == null) {
            return null;
        }
        packRsvp pck = new packRsvp();
        pck.adsBndwdt = ntry.bwdt;
        pck.adsCmtu = ntry.trgIfc.mtu;
        pck.adsHops = 1;
        pck.adsLtncy = 0;
        pck.expRout = new ArrayList<tabHop>();
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
        pck.ttl = 254;
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
        fillRsvpFrst(lower, pckRrp);
        packHolder pckBin = new packHolder(true, true);
        pckRrp.createHolder(pckBin);
        pckRrp.fillLabReq();
        pckRrp.createDatPatReq(pckBin);
        pckRrp.createHeader(pckBin);
        lower.protoPack(ntry.trgIfc, pckBin);
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
        lower.protoPack(ntry.trgIfc, pckBin);
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
        return chg;
    }

    /**
     * check for stalled interfaces
     */
    public static void checkVrfs() {
        for (int i = cfgAll.vrfs.size() - 1; i >= 0; i--) {
            cfgVrf ntry = cfgAll.vrfs.get(i);
            ntry.fwd4.hstry.update(ntry.fwd4.cntr);
            ntry.fwd6.hstry.update(ntry.fwd6.cntr);
        }
    }

}
