package net.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgRtr;
import net.freertr.cfg.cfgVrf;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipMpls;
import net.freertr.ip.ipRtr;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabIndex;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabPlcmapN;
import net.freertr.tab.tabQos;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRtrmapN;
import net.freertr.user.userHelping;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;

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
     * mvpn advertisement source
     */
    public cfgIfc mvpn;

    /**
     * srv6 advertisement source
     */
    public cfgIfc srv6;

    /**
     * default information originate
     */
    public boolean defRou;

    /**
     * forwarder to use
     */
    protected final ipFwd fwd;

    private final rtrBgp parent;

    private final cfgVrf vrf;

    private final boolean other;

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
        } else {
            fwd = v.fwd6;
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
        fwd.routerChg(this);
    }

    /**
     * others changed
     */
    public void routerOthersChanged() {
        if (fwd.prefixMode == ipFwd.labelMode.common) {
            return;
        }
        parent.routerRedistChanged();
        fwd.routerChg(this);
    }

    private void doExportRoute(int afi, tabRouteEntry<addrIP> ntry, tabRoute<addrIP> trg, List<Long> rt) {
        if (ntry == null) {
            return;
        }
        ntry = ntry.copyBytes(tabRoute.addType.ecmp);
        ntry.rouDst = vrf.rd;
        for (int i = 0; i < ntry.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(i);
            if (attr.labelLoc == null) {
                attr.labelLoc = fwd.commonLabel;
            }
            if (attr.extComm == null) {
                attr.extComm = new ArrayList<Long>();
            }
            attr.extComm.addAll(rt);
            attr.rouSrc = rtrBgpUtil.peerOriginate;
        }
        ipMpls.putSrv6prefix(ntry, srv6, ntry.best.labelLoc);
        tabRoute.addUpdatedEntry(tabRoute.addType.ecmp, trg, afi, 0, ntry, true, fwd.exportMap, fwd.exportPol, fwd.exportList);
    }

    /**
     * merge routes to table
     *
     * @param nUni unicast table to update
     * @param nMlt multicast table to update
     * @param nFlw flowspec table to update
     * @param nMvpn mvpn table to update
     */
    public void doAdvertise(tabRoute<addrIP> nUni, tabRoute<addrIP> nMlt, tabRoute<addrIP> nFlw, tabRoute<addrIP> nMvpn) {
        final List<Long> rt = new ArrayList<Long>();
        for (int i = 0; i < vrf.rtExp.size(); i++) {
            rt.add(tabRtrmapN.rt2comm(vrf.rtExp.get(i)));
        }
        if (defRou) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = rtrBgpUtil.defaultRoute(other ? parent.afiOtrU : parent.afiUni);
            ntry.best.aggrRtr = new addrIP();
            ntry.best.aggrRtr.fromIPv4addr(parent.routerID);
            ntry.best.aggrAs = parent.localAs;
            doExportRoute(rtrBgpUtil.sfiUnicast, ntry, nUni, rt);
            ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = rtrBgpUtil.defaultRoute(other ? parent.afiOtrU : parent.afiUni);
            ntry.best.aggrRtr = new addrIP();
            ntry.best.aggrRtr.fromIPv4addr(parent.routerID);
            ntry.best.aggrAs = parent.localAs;
            doExportRoute(rtrBgpUtil.sfiMulticast, ntry, nMlt, rt);
        }
        for (int i = 0; i < routerRedistedU.size(); i++) {
            doExportRoute(rtrBgpUtil.sfiUnicast, routerRedistedU.get(i), nUni, rt);
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
            doExportRoute(rtrBgpUtil.sfiUnicast, tab.get(i), nUni, rt);
        }
        tab = new tabRoute<addrIP>("agg");
        routerDoAggregates(parent.afiMlt, nMlt, tab, fwd.commonLabel, parent.routerID, parent.localAs);
        for (int i = 0; i < tab.size(); i++) {
            doExportRoute(rtrBgpUtil.sfiMulticast, tab.get(i), nMlt, rt);
        }
        if (flowSpec != null) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.best.extComm = new ArrayList<Long>();
            ntry.rouDst = vrf.rd;
            ntry.best.extComm.addAll(rt);
            rtrBgpFlow.doAdvertise(nFlw, flowSpec, ntry, other ^ (parent.afiUni == rtrBgpUtil.safiIp6uni), parent.localAs);
        }
        if (mvpn != null) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = rtrBgpUtil.defaultRoute(parent.afiUni);
            byte[] buf = new byte[128];
            if (ntry.prefix.network.isIPv4()) {
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
            ntry.rouDst = vrf.rd;
            ntry.best.extComm.addAll(rt);
            ntry.best.rouSrc = rtrBgpUtil.peerOriginate;
            tabRoute.addUpdatedEntry(tabRoute.addType.better, nMvpn, other ? parent.afiVpoM : parent.afiVpnM, 0, ntry, true, fwd.exportMap, fwd.exportPol, fwd.exportList);
        }
    }

    private void doImportRoute(int afi, tabRouteEntry<addrIP> ntry, tabRoute<addrIP> trg, List<Long> rt) {
        if (ntry.best.rouSrc == rtrBgpUtil.peerOriginate) {
            return;
        }
        if (ntry.best.extComm == null) {
            return;
        }
        boolean needed = false;
        for (int i = 0; i < rt.size(); i++) {
            needed |= rtrBgpUtil.findLongList(ntry.best.extComm, rt.get(i)) >= 0;
            if (needed) {
                break;
            }
        }
        if (!needed) {
            return;
        }
        ntry = ntry.copyBytes(tabRoute.addType.ecmp);
        ntry.rouDst = 0;
        for (int i = 0; i < ntry.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(i);
            attr.rouTab = parent.fwdCore;
            if (attr.segrouPrf != null) {
                attr.rouTab = parent.vrfCore.fwd6;
            }
            if (distance > 0) {
                attr.distance = distance;
            }
        }
        tabRoute.addUpdatedEntry(tabRoute.addType.ecmp, trg, afi, 0, ntry, false, fwd.importMap, fwd.importPol, fwd.importList);
        if (parent.routerAutoMesh == null) {
            return;
        }
        peers.add(ntry.best.nextHop);
    }

    /**
     * import routes from table
     *
     * @param cmpU unicast table to read
     * @param cmpM multicast table to read
     * @param cmpF flowspec table to read
     * @return other changes trigger full recomputation
     */
    public boolean doPeers(tabRoute<addrIP> cmpU, tabRoute<addrIP> cmpM, tabRoute<addrIP> cmpF) {
        final List<Long> rt = new ArrayList<Long>();
        for (int i = 0; i < vrf.rtImp.size(); i++) {
            rt.add(tabRtrmapN.rt2comm(vrf.rtImp.get(i)));
        }
        tabRoute<addrIP> tabU = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> tabM = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> tabF = new tabRoute<addrIP>("bgp");
        peers = new tabGen<addrIP>();
        for (int i = 0; i < cmpU.size(); i++) {
            doImportRoute(rtrBgpUtil.sfiUnicast, cmpU.get(i), tabU, rt);
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
        routerComputedU = tabU;
        routerComputedM = tabM;
        routerComputedF = tabF;
        routerComputedI = new tabGen<tabIndex<addrIP>>();
        fwd.routerChg(this);
        if (flowInst) {
            fwd.flowspec = tabQos.convertPolicy(rtrBgpFlow.doDecode(tabF, other ^ (parent.afiUni == rtrBgpUtil.safiIp6uni)));
        }
        return fwd.prefixMode != ipFwd.labelMode.common;
    }

    /**
     * get help
     *
     * @param l list
     */
    public void routerGetHelp(userHelping l) {
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
     * get config
     *
     * @param l list to append
     * @param beg1 beginning
     */
    public void getConfig(List<String> l, String beg1) {
        String beg2;
        if (other) {
            beg2 = "afi-ovrf ";
        } else {
            beg2 = "afi-vrf ";
        }
        beg2 += vrf.name + " ";
        l.add(beg1 + beg2 + "enable");
        l.add(beg1 + beg2 + "distance " + distance);
        cmds.cfgLine(l, !defRou, beg1, beg2 + "default-originate", "");
        cmds.cfgLine(l, !flowInst, beg1, beg2 + "flowspec-install", "");
        cmds.cfgLine(l, flowSpec == null, beg1, beg2 + "flowspec-advert", "" + flowSpec);
        if (mvpn != null) {
            l.add(beg1 + beg2 + "mvpn " + mvpn.name);
        }
        if (srv6 != null) {
            l.add(beg1 + beg2 + "srv6 " + srv6.name);
        }
        cfgRtr.getShRedist(l, beg1 + beg2, this);
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
