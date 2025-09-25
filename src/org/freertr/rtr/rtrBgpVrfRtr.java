package org.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgPlymp;
import org.freertr.cfg.cfgRtr;
import org.freertr.cfg.cfgVrf;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipRtr;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabPlcmapN;
import org.freertr.tab.tabQos;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRouteUtil;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * bgp4 vrf router
 *
 * @author matecsaba
 */
public class rtrBgpVrfRtr extends ipRtr {

    /**
     * import distance
     */
    public int distance;

    /**
     * flow specification
     */
    public tabListing<tabPlcmapN, addrIP> flowSpec;

    /**
     * install flow specification
     */
    public boolean flowInst;

    /**
     * originating interface
     */
    public cfgIfc iface;

    /**
     * mvpn advertisement source
     */
    public cfgIfc mvpn;

    /**
     * mdt advertisement source
     */
    public cfgIfc mdtI;

    /**
     * mdt advertisement group
     */
    public addrIP mdtG;

    /**
     * srv6 advertisement source
     */
    public cfgIfc srv6;

    /**
     * default information originate
     */
    public boolean defRou;

    /**
     * forwarder override
     */
    public ipFwd setVrfF;

    /**
     * vrf afi type updater
     */
    public boolean setVrfT;

    /**
     * forwarder to use
     */
    protected final ipFwd fwd;

    private final rtrBgp parent;

    private final cfgVrf vrf;

    private final boolean other;

    private final boolean ipv4;

    private tabGen<addrIP> peers = new tabGen<addrIP>();

    /**
     * unregister from ip
     */
    public void unregister2ip() {
        if (debugger.rtrBgpEvnt) {
            logger.debug("stop " + vrf);
        }
        fwd.routerDel(this);
    }

    /**
     * register to ip
     */
    public void register2ip() {
        if (debugger.rtrBgpEvnt) {
            logger.debug("start " + vrf);
        }
        fwd.routerAdd(this, parent.rouTyp, parent.rtrNum);
    }

    /**
     * create new instance
     *
     * @param p parent to use
     * @param v vrf to use
     * @param o other afi
     */
    public rtrBgpVrfRtr(rtrBgp p, cfgVrf v, boolean o) {
        if (o ^ (p.rouTyp == tabRouteAttr.routeType.bgp4)) {
            fwd = v.fwd4;
            ipv4 = true;
        } else {
            fwd = v.fwd6;
            ipv4 = false;
        }
        parent = p;
        other = o;
        vrf = v;
        routerVpn = true;
        distance = -1;
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "bgp on " + parent.fwdCore;
    }

    /**
     * create computed table
     */
    public synchronized void routerCreateComputed() {
    }

    /**
     * redistribution changed
     */
    public void routerRedistChanged() {
        parent.routerRedistChanged();
    }

    /**
     * others changed
     */
    public void routerOthersChanged() {
        if (fwd.prefixMode == ipFwd.labelMode.common) {
            return;
        }
        parent.routerRedistChanged();
    }

    private void doExportRoute(int afi, tabRouteEntry<addrIP> ntry, tabRoute<addrIP> trg, List<Long> rt) {
        if (ntry == null) {
            return;
        }
        ntry = ntry.copyBytes(tabRoute.addType.ecmp);
        ntry.rouDst = fwd.rd;
        for (int i = 0; i < ntry.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(i);
            if (attr.labelLoc == null) {
                attr.labelLoc = fwd.commonLabel;
            }
            attr.extComm = tabRouteUtil.appendLongList(attr.extComm, rt);
            attr.rouSrc = rtrBgpUtil.peerOriginate;
        }
        if (iface != null) {
            addrIP adr = null;
            if (ipv4) {
                if (iface.addr4 != null) {
                    adr = new addrIP();
                    adr.fromIPv4addr(iface.addr4);
                }
            } else {
                if (iface.addr6 != null) {
                    adr = new addrIP();
                    adr.fromIPv6addr(iface.addr6);
                }
            }
            if (adr != null) {
                for (int i = 0; i < ntry.alts.size(); i++) {
                    tabRouteAttr<addrIP> attr = ntry.alts.get(i);
                    attr.nextHop = adr.copyBytes();
                }
            }
        }
        tabRouteUtil.generateSrv6pfx(ntry, srv6, ntry.best.labelLoc);
        if (afi != rtrBgpUtil.sfiEthVpn) {
            tabRoute.addUpdatedEntry(tabRoute.addType.ecmp, trg, afi, 0, ntry, true, fwd.exportMap, fwd.exportPol, fwd.exportList);
            return;
        }
        for (int i = 0; i < ntry.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(i);
            attr.evpnLab = attr.labelLoc.label << 4;
        }
        byte[] buf = new byte[addrIP.size];
        ntry.prefix.network.toBuffer(buf, 0);
        ntry.prefix.broadcast.fromBuf(buf, 0);
        buf = new byte[addrIP.size];
        ntry.prefix.wildcard.fromBuf(buf, 0);
        buf[0] = 5;
        ntry.prefix.network.fromBuf(buf, 0);
        afi = rtrBgpUtil.sfiUnicast;
        tabRoute.addUpdatedEntry(tabRoute.addType.ecmp, trg, afi, 0, ntry, true, fwd.exportMap, fwd.exportPol, fwd.exportList);
    }

    /**
     * merge routes to table
     *
     * @param afi address family
     * @param nUni unicast table to update
     * @param nMlt multicast table to update
     * @param nFlw flowspec table to update
     * @param nMvpn mvpn table to update
     */
    public void doAdvertise(int afi, tabRoute<addrIP> nUni, tabRoute<addrIP> nMlt, tabRoute<addrIP> nFlw, tabRoute<addrIP> nMvpn) {
        final List<Long> rt = new ArrayList<Long>();
        for (int i = 0; i < fwd.rtExp.size(); i++) {
            rt.add(tabRouteUtil.rt2comm(fwd.rtExp.get(i)));
        }
        for (int i = 0; i < fwd.clrExp.size(); i++) {
            rt.add(tabRouteUtil.clr2comm(fwd.clrExp.get(i)));
        }
        for (int i = 0; i < fwd.rtImp.size(); i++) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = tabRouteUtil.extcomm2rtfilter(parent.localAs, tabRouteUtil.rt2comm(fwd.rtImp.get(i)));
            ntry.best.rouSrc = rtrBgpUtil.peerOriginate;
            parent.newlyRtf.add(tabRoute.addType.always, ntry, false, true);
        }
        if (defRou) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = rtrBgpUtil.defaultRoute(other ? parent.afiOuni : parent.afiUni);
            ntry.best.aggrRtr = new addrIP();
            ntry.best.aggrRtr.fromIPv4addr(parent.routerID);
            ntry.best.aggrAs = parent.localAs;
            doExportRoute(afi, ntry, nUni, rt);
            ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = rtrBgpUtil.defaultRoute(other ? parent.afiOuni : parent.afiUni);
            ntry.best.aggrRtr = new addrIP();
            ntry.best.aggrRtr.fromIPv4addr(parent.routerID);
            ntry.best.aggrAs = parent.localAs;
            doExportRoute(rtrBgpUtil.sfiMulticast, ntry, nMlt, rt);
        }
        for (int i = 0; i < routerRedistedU.size(); i++) {
            doExportRoute(afi, routerRedistedU.get(i), nUni, rt);
        }
        for (int i = 0; i < routerRedistedM.size(); i++) {
            doExportRoute(rtrBgpUtil.sfiMulticast, routerRedistedM.get(i), nMlt, rt);
        }
        for (int i = 0; i < routerRedistedF.size(); i++) {
            doExportRoute(rtrBgpUtil.sfiFlwSpc, routerRedistedF.get(i), nFlw, rt);
        }
        tabRoute<addrIP> tab = new tabRoute<addrIP>("agg");
        routerDoAggregates(parent.afiUni, nUni, tab, fwd.commonLabel, parent.routerID, parent.localAs);
        for (int i = 0; i < tab.size(); i++) {
            doExportRoute(afi, tab.get(i), nUni, rt);
        }
        tab = new tabRoute<addrIP>("agg");
        routerDoAggregates(parent.afiMlt, nMlt, tab, fwd.commonLabel, parent.routerID, parent.localAs);
        for (int i = 0; i < tab.size(); i++) {
            doExportRoute(rtrBgpUtil.sfiMulticast, tab.get(i), nMlt, rt);
        }
        if (flowSpec != null) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.best.extComm = new ArrayList<Long>();
            ntry.rouDst = fwd.rd;
            ntry.best.extComm.addAll(rt);
            rtrBgpFlow.doAdvertise(nFlw, flowSpec, ntry, other ^ (parent.afiUni == rtrBgpUtil.safiIp6uni), parent.localAs);
        }
        if (mdtI != null) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = rtrBgpUtil.defaultRoute(parent.afiUni);
            ntry.best.extComm = new ArrayList<Long>();
            ntry.rouDst = fwd.rd;
            ntry.best.extComm.addAll(rt);
            byte[] buf1 = new byte[addrIP.size];
            byte[] buf2 = new byte[addrIP.size];
            if (parent.rouTyp == tabRouteAttr.routeType.bgp4) {
                mdtI.addr4.toBuffer(buf1, 0);
                mdtG.toIPv4().toBuffer(buf2, 0);
            } else {
                mdtI.addr6.toBuffer(buf1, 0);
                mdtG.toIPv6().toBuffer(buf2, 0);
            }
            ntry.prefix.network.fromBuf(buf1, 0);
            ntry.prefix.broadcast.fromBuf(buf2, 0);
            ntry.best.rouSrc = rtrBgpUtil.peerOriginate;
            tabRoute.addUpdatedEntry(tabRoute.addType.better, parent.newlyMdt, parent.afiMdt, 0, ntry, true, fwd.exportMap, fwd.exportPol, fwd.exportList);
        }
        if (mvpn == null) {
            return;
        }
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = rtrBgpUtil.defaultRoute(parent.afiUni);
        byte[] buf = new byte[128];
        if (ipv4) {
            addrIPv4 adr = mvpn.addr4;
            if (adr != null) {
                adr.toBuffer(buf, 2);
            }
            buf[0] = addrIPv4.size;
        } else {
            addrIPv6 adr = mvpn.addr6;
            if (adr != null) {
                adr.toBuffer(buf, 2);
            }
            buf[0] = addrIPv6.size;
        }
        buf[0]++;
        buf[1] = 1; // intra-as pmsi
        ntry.prefix.network.fromBuf(buf, 0);
        ntry.prefix.broadcast.fromBuf(buf, 16);
        ntry.prefix.wildcard.fromBuf(buf, 32);
        ntry.prefix.mask.fromBuf(buf, 48);
        ntry.best.extComm = new ArrayList<Long>();
        ntry.rouDst = fwd.rd;
        ntry.best.extComm.addAll(rt);
        ntry.best.rouSrc = rtrBgpUtil.peerOriginate;
        tabRoute.addUpdatedEntry(tabRoute.addType.better, nMvpn, other ? parent.afiVpoM : parent.afiVpnM, 0, ntry, true, fwd.exportMap, fwd.exportPol, fwd.exportList);
    }

    private List<Long> getRtList() {
        final List<Long> rt = new ArrayList<Long>();
        for (int i = 0; i < fwd.rtImp.size(); i++) {
            rt.add(tabRouteUtil.rt2comm(fwd.rtImp.get(i)));
        }
        for (int i = 0; i < fwd.clrImp.size(); i++) {
            rt.add(tabRouteUtil.clr2comm(fwd.clrImp.get(i)));
        }
        return rt;
    }

    private addrPrefix<addrIP> evpn2prefix(int afi, addrPrefix<addrIP> prefix) {
        if (afi != rtrBgpUtil.sfiEthVpn) {
            return prefix;
        }
        if (ipv4 != prefix.broadcast.isIPv4()) {
            return null;
        }
        return tabRouteUtil.convertL3evpn(prefix);
    }

    private boolean doImportRoute(int afi, tabRouteEntry<addrIP> ntry, tabRoute<addrIP> trg, List<Long> rt) {
        if (ntry.best.rouSrc == rtrBgpUtil.peerOriginate) {
            return true;
        }
        if (ntry.best.extComm == null) {
            return true;
        }
        boolean needed = false;
        for (int i = 0; i < rt.size(); i++) {
            needed |= tabRouteUtil.findLongList(ntry.best.extComm, rt.get(i)) >= 0;
            if (needed) {
                break;
            }
        }
        if (!needed) {
            return true;
        }
        ntry = ntry.copyBytes(tabRoute.addType.ecmp);
        ntry.oldDst = ntry.rouDst;
        ntry.rouDst = 0;
        if (afi == rtrBgpUtil.sfiEthVpn) {
            ntry.prefix = evpn2prefix(rtrBgpUtil.sfiEthVpn, ntry.prefix);
            if (ntry.prefix == null) {
                return true;
            }
            for (int i = 0; i < ntry.alts.size(); i++) {
                tabRouteAttr<addrIP> attr = ntry.alts.get(i);
                attr.labelRem = tabLabel.prependLabel(attr.labelRem, attr.evpnLab >>> 4);
            }
            afi = rtrBgpUtil.sfiUnicast;
        }
        for (int i = 0; i < ntry.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(i);
            if (attr.rouTab == null) {
                attr.rouTab = parent.fwdCore;
            }
            if (attr.segrouPrf != null) {
                attr.rouTab = parent.vrfCore.fwd6;
            }
            if (setVrfF != null) {
                attr.rouTab = setVrfF;
            }
            if (distance > 0) {
                attr.distance = distance;
            }
        }
        tabRoute.addUpdatedEntry(tabRoute.addType.always, trg, afi, 0, ntry, false, fwd.importMap, fwd.importPol, fwd.importList);
        if (parent.routerAutoMesh == null) {
            return false;
        }
        peers.add(ntry.best.nextHop);
        return false;
    }

    /**
     * full import routes from table
     *
     * @param afi address family
     * @param cmpU unicast table to read
     * @param cmpM multicast table to read
     * @param cmpF flowspec table to read
     * @return other changes trigger full recomputation
     */
    public boolean doPeersFull(int afi, tabRoute<addrIP> cmpU, tabRoute<addrIP> cmpM, tabRoute<addrIP> cmpF) {
        routerChangedU = null;
        routerChangedM = null;
        routerChangedF = null;
        final List<Long> rt = getRtList();
        tabRoute<addrIP> tabU = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> tabM = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> tabF = new tabRoute<addrIP>("bgp");
        peers = new tabGen<addrIP>();
        for (int i = 0; i < cmpU.size(); i++) {
            doImportRoute(afi, cmpU.get(i), tabU, rt);
        }
        for (int i = 0; i < cmpM.size(); i++) {
            doImportRoute(rtrBgpUtil.sfiMulticast, cmpM.get(i), tabM, rt);
        }
        for (int i = 0; i < cmpF.size(); i++) {
            doImportRoute(rtrBgpUtil.sfiFlwSpc, cmpF.get(i), tabF, rt);
        }
        if (flowSpec != null) {
            rtrBgpFlow.doAdvertise(tabF, flowSpec, new tabRouteEntry<addrIP>(), other ^ (parent.afiUni == rtrBgpUtil.safiIp6uni), parent.localAs);
        }
        if ((!tabU.differs(tabRoute.addType.alters, routerComputedU)) && (!tabU.differs(tabRoute.addType.alters, routerComputedM)) && (!tabF.differs(tabRoute.addType.alters, routerComputedF))) {
            return fwd.prefixMode != ipFwd.labelMode.common;
        }
        routerComputedU = tabU;
        routerComputedM = tabM;
        routerComputedF = tabF;
        routerComputedI = new tabGen<tabIndex<addrIP>>();
        fwd.routerChg(this, true);
        if (flowInst) {
            fwd.flowspec = tabQos.convertPolicy(rtrBgpFlow.doDecode(tabF, other ^ (parent.afiUni == rtrBgpUtil.safiIp6uni)));
        }
        return fwd.prefixMode != ipFwd.labelMode.common;
    }

    private void doUpdateRoute(int afi, tabRouteEntry<addrIP> ntry, tabRoute<addrIP> chg, tabRoute<addrIP> trg, tabRoute<addrIP> cmp, List<Long> rt) {
        addrPrefix<addrIP> pfx = evpn2prefix(afi, ntry.prefix);
        if (pfx == null) {
            return;
        }
        tabRouteEntry<addrIP> res = trg.find(pfx);
        if (res == null) {
            res = cmp.find(ntry);
            if (res == null) {
                return;
            }
            if (doImportRoute(afi, res, trg, rt)) {
                return;
            }
            if (chg == null) {
                return;
            }
            chg.add(tabRoute.addType.always, pfx, null);
            return;
        }
        if (ntry.rouDst != res.oldDst) {
            return;
        }
        res = cmp.find(ntry);
        if (res == null) {
            trg.del(pfx);
            if (chg == null) {
                return;
            }
            chg.add(tabRoute.addType.always, pfx, null);
            return;
        }
        if (doImportRoute(afi, res, trg, rt)) {
            trg.del(pfx);
        }
        if (chg == null) {
            return;
        }
        chg.add(tabRoute.addType.always, pfx, null);
    }

    /**
     * incremental import routes from table
     *
     * @param afi address family
     * @param cmpU unicast table to read
     * @param cmpM multicast table to read
     * @param cmpF flowspec table to read
     * @param chgU unicast table to process
     * @param chgM multicast table to process
     * @param chgF flowspec table to process
     * @return other changes trigger full recomputation
     */
    public boolean doPeersIncr(int afi, tabRoute<addrIP> cmpU, tabRoute<addrIP> cmpM, tabRoute<addrIP> cmpF, tabRoute<addrIP> chgU, tabRoute<addrIP> chgM, tabRoute<addrIP> chgF) {
        if ((chgU == null) || (chgM == null) || (chgF == null)) {
            if (debugger.rtrBgpFull) {
                logger.debug("changes disappeared");
            }
            parent.needFull.add(1);
            parent.compute.wakeup();
            return true;
        }
        final List<Long> rt = getRtList();
        routerChangedU = new tabRoute<addrIP>("chg");
        routerChangedM = new tabRoute<addrIP>("chg");
        routerChangedF = new tabRoute<addrIP>("chg");
        for (int i = 0; i < chgU.size(); i++) {
            doUpdateRoute(afi, chgU.get(i), routerChangedU, routerComputedU, cmpU, rt);
        }
        for (int i = 0; i < chgM.size(); i++) {
            doUpdateRoute(rtrBgpUtil.sfiMulticast, chgM.get(i), routerChangedM, routerComputedM, cmpM, rt);
        }
        for (int i = 0; i < chgF.size(); i++) {
            doUpdateRoute(rtrBgpUtil.sfiFlwSpc, chgF.get(i), routerChangedF, routerComputedF, cmpF, rt);
        }
        if ((routerChangedU.size() + routerChangedM.size() + routerChangedF.size()) < 1) {
            return fwd.prefixMode != ipFwd.labelMode.common;
        }
        fwd.routerChg(this, fwd.prefixMode != ipFwd.labelMode.common);
        if (flowInst && (chgF.size() > 0)) {
            tabRoute<addrIP> tabF = new tabRoute<addrIP>("bgp");
            fwd.flowspec = tabQos.convertPolicy(rtrBgpFlow.doDecode(tabF, other ^ (parent.afiUni == rtrBgpUtil.safiIp6uni)));
            routerComputedF = tabF;
        }
        return fwd.prefixMode != ipFwd.labelMode.common;
    }

    /**
     * get help
     *
     * @param l list
     */
    public void routerGetHelp(userHelp l) {
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void routerGetConfig(List<String> l, String beg, int filter) {
    }

    /**
     * configure
     *
     * @param cmd command
     * @return false if success, true if error
     */
    public boolean routerConfigure(cmds cmd) {
        return true;
    }

    /**
     * stop work
     */
    public void routerCloseNow() {
    }

    /**
     * get help
     *
     * @param l list
     * @param p start number
     */
    public static void getHelp(userHelp l, int p) {
        l.add(null, false, p + 0, new int[]{-1}, "enable", "enable processing");
        l.add(null, false, p + 0, new int[]{-1}, "default-originate", "generate default route");
        l.add(null, false, p + 0, new int[]{p + 1}, "srv6", "srv6 advertisement");
        l.add(null, false, p + 1, new int[]{-1}, "<name:ifc>", "select source to advertise");
        l.add(null, false, p + 0, new int[]{p + 1}, "distance", "set import distance");
        l.add(null, false, p + 1, new int[]{-1}, "<num>", "distance");
        l.add(null, false, p + 0, new int[]{-1}, "flowspec-install", "specify flowspec installation");
        l.add(null, false, p + 0, new int[]{p + 1}, "flowspec-advert", "specify flowspec parameter");
        l.add(null, false, p + 1, new int[]{-1}, "<name:pm>", "name of policy map");
        l.add(null, false, p + 0, new int[]{p + 1}, "mdt", "mdt advertisement");
        l.add(null, false, p + 1, new int[]{p + 2}, "<name:ifc>", "select source to advertise");
        l.add(null, false, p + 2, new int[]{-1}, "<addr>", "select group to advertise");
        l.add(null, false, p + 0, new int[]{p + 1}, "mvpn", "mvpn advertisement");
        l.add(null, false, p + 1, new int[]{-1}, "<name:ifc>", "select source to advertise");
        l.add(null, false, p + 0, new int[]{p + 1}, "update-source", "select source to advertise");
        l.add(null, false, p + 1, new int[]{-1}, "<name:ifc>", "name of interface");
        l.add(null, false, p + 0, new int[]{p + 1}, "set-vrf", "configure forwarder override");
        l.add(null, false, p + 1, new int[]{p + 2}, "<name:vrf>", "select vrf to use");
        l.add(null, false, p + 2, new int[]{-1}, "ipv4", "select ipv4 to use");
        l.add(null, false, p + 2, new int[]{-1}, "ipv6", "select ipv6 to use");
    }

    /**
     * get config
     *
     * @param l list to append
     * @param beg1 beginning
     * @param beg2 beginning
     */
    public void getConfig(List<String> l, String beg1, String beg2) {
        beg2 += vrf.name + " ";
        l.add(beg1 + beg2 + "enable");
        l.add(beg1 + beg2 + "distance " + distance);
        cmds.cfgLine(l, !defRou, beg1, beg2 + "default-originate", "");
        cmds.cfgLine(l, !flowInst, beg1, beg2 + "flowspec-install", "");
        cmds.cfgLine(l, flowSpec == null, beg1, beg2 + "flowspec-advert", "" + flowSpec);
        if (mdtI != null) {
            l.add(beg1 + beg2 + "mdt " + mdtI.name + " " + mdtG);
        }
        if (mvpn != null) {
            l.add(beg1 + beg2 + "mvpn " + mvpn.name);
        }
        if (srv6 != null) {
            l.add(beg1 + beg2 + "srv6 " + srv6.name);
        }
        if (setVrfF != null) {
            l.add(beg1 + beg2 + "set-vrf " + setVrfF.cfgName + " " + (setVrfT ? "ipv4" : "ipv6"));
        }
        if (iface != null) {
            l.add(beg1 + beg2 + "update-source " + iface.name);
        }
        cfgRtr.getShRedist(l, beg1 + beg2, this);
        l.add(beg1 + cmds.comment);
    }

    /**
     * configure the vrf
     *
     * @param negated negated command effect
     * @param cmd command parameters
     * @param s command to execute
     */
    public void doConfig(boolean negated, cmds cmd, String s) {
        if (s.equals("distance")) {
            distance = bits.str2num(cmd.word());
            parent.needFull.add(1);
            parent.compute.wakeup();
            return;
        }
        if (s.equals("mdt")) {
            if (negated) {
                mdtI = null;
                mdtG = null;
            } else {
                mdtI = cfgAll.ifcFind(cmd.word(), 0);
                mdtG = new addrIP();
                mdtG.fromString(cmd.word());
            }
            parent.needFull.add(1);
            parent.compute.wakeup();
            return;
        }
        if (s.equals("mvpn")) {
            if (negated) {
                mvpn = null;
            } else {
                mvpn = cfgAll.ifcFind(cmd.word(), 0);
            }
            parent.needFull.add(1);
            parent.compute.wakeup();
            return;
        }
        if (s.equals("update-source")) {
            if (negated) {
                iface = null;
                parent.needFull.add(1);
                parent.compute.wakeup();
                return;
            }
            cfgIfc res = cfgAll.ifcFind(cmd.word(), 0);
            if (res == null) {
                cmd.error("no such interface");
                return;
            }
            if (res.vrfFor != parent.vrfCore) {
                cmd.error("in other vrf");
                return;
            }
            iface = res;
            parent.needFull.add(1);
            parent.compute.wakeup();
            return;
        }
        if (s.equals("srv6")) {
            if (negated) {
                srv6 = null;
            } else {
                srv6 = cfgAll.ifcFind(cmd.word(), 0);
            }
            parent.needFull.add(1);
            parent.compute.wakeup();
            return;
        }
        if (s.equals("set-vrf")) {
            if (negated) {
                setVrfF = null;
                setVrfT = false;
                parent.needFull.add(1);
                parent.compute.wakeup();
                return;
            }
            cfgVrf vrf = cfgAll.vrfFind(cmd.word(), false);
            if (vrf == null) {
                cmd.error("no such vrf");
                return;
            }
            s = cmd.word();
            if (s.equals("ipv4")) {
                setVrfF = vrf.fwd4;
                setVrfT = true;
            } else {
                setVrfF = vrf.fwd6;
                setVrfT = false;
            }
            parent.needFull.add(1);
            parent.compute.wakeup();
            return;
        }
        if (s.equals("default-originate")) {
            defRou = !negated;
            parent.needFull.add(1);
            parent.compute.wakeup();
            return;
        }
        if (s.equals("flowspec-install")) {
            flowInst = !negated;
            if (negated) {
                fwd.flowspec = null;
            }
            parent.needFull.add(1);
            parent.compute.wakeup();
            return;
        }
        if (s.equals("flowspec-advert")) {
            if (negated) {
                flowSpec = null;
                parent.needFull.add(1);
                parent.compute.wakeup();
                return;
            }
            cfgPlymp ntry = cfgAll.plmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such policy map");
                return;
            }
            flowSpec = ntry.plcmap;
            parent.needFull.add(1);
            parent.compute.wakeup();
            return;
        }
        if (cfgRtr.doCfgRedist(this, fwd, negated, s, cmd)) {
            cmd.badCmd();
        }
        parent.needFull.add(1);
        parent.compute.wakeup();
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
     * get neighbor list
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

    /**
     * get state information
     *
     * @param lst list to append
     */
    public void routerStateGet(List<String> lst) {
    }

    /**
     * set state information
     *
     * @param cmd string to append
     * @return true on error, false on success
     */
    public boolean routerStateSet(cmds cmd) {
        return true;
    }

    /**
     * get peer list
     *
     * @param tab list to append
     */
    public void getPeerList(tabRoute<addrIP> tab) {
        for (int i = 0; i < peers.size(); i++) {
            addrIP adr = peers.get(i);
            if (adr == null) {
                continue;
            }
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, tab, parent.afiUni, 0, ntry, true, null, null, parent.routerAutoMesh);
        }
    }

}
