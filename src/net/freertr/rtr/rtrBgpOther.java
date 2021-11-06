package net.freertr.rtr;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgRtr;
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
import net.freertr.user.userHelping;
import net.freertr.util.cmds;

/**
 * bgp4 other router
 *
 * @author matecsaba
 */
public class rtrBgpOther extends ipRtr {

    /**
     * enabled
     */
    public boolean enabled;

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
     * srv6 advertisement source
     */
    public cfgIfc srv6;

    /**
     * forwarder to use
     */
    protected final ipFwd fwd;

    private final rtrBgp parent;

    private tabGen<addrIP> peers = new tabGen<addrIP>();

    /**
     * unregister from ip
     */
    public void unregister2ip() {
        if (!enabled) {
            return;
        }
        enabled = false;
        fwd.routerDel(this);
    }

    /**
     * register to ip
     */
    public void register2ip() {
        if (enabled) {
            return;
        }
        enabled = true;
        fwd.routerAdd(this, parent.rouTyp, parent.rtrNum);
    }

    /**
     * create new instance
     *
     * @param p parent to use
     * @param f vrf to use
     */
    public rtrBgpOther(rtrBgp p, ipFwd f) {
        fwd = f;
        parent = p;
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

    private void doExportRoute(int afi, tabRouteEntry<addrIP> ntry, tabRoute<addrIP> trg) {
        if (ntry == null) {
            return;
        }
        ntry = ntry.copyBytes(tabRoute.addType.ecmp);
        for (int i = 0; i < ntry.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(i);
            if (attr.labelLoc == null) {
                attr.labelLoc = fwd.commonLabel;
            }
            attr.rouSrc = rtrBgpUtil.peerOriginate;
        }
        ipMpls.putSrv6prefix(ntry, srv6, ntry.best.labelLoc);
        tabRoute.addUpdatedEntry(tabRoute.addType.ecmp, trg, afi, 0, ntry, true, null, null, null);
    }

    /**
     * merge routes to table
     *
     * @param nUni unicast table to update
     * @param nMlt multicast table to update
     * @param nFlw flowspec table to update
     */
    public void doAdvertise(tabRoute<addrIP> nUni, tabRoute<addrIP> nMlt, tabRoute<addrIP> nFlw) {
        if (!enabled) {
            return;
        }
        for (int i = 0; i < routerRedistedU.size(); i++) {
            doExportRoute(rtrBgpUtil.sfiUnicast, routerRedistedU.get(i), nUni);
        }
        for (int i = 0; i < routerRedistedM.size(); i++) {
            doExportRoute(rtrBgpUtil.sfiMulticast, routerRedistedM.get(i), nMlt);
        }
        for (int i = 0; i < routerRedistedF.size(); i++) {
            doExportRoute(rtrBgpUtil.sfiFlwSpc, routerRedistedF.get(i), nFlw);
        }
        if (flowSpec != null) {
            rtrBgpFlow.doAdvertise(nFlw, flowSpec, new tabRouteEntry<addrIP>(), parent.afiUni != rtrBgpUtil.safiIp6uni, parent.localAs);
        }
    }

    private void doImportRoute(int afi, tabRouteEntry<addrIP> ntry, tabRoute<addrIP> trg) {
        if (ntry.best.rouSrc == rtrBgpUtil.peerOriginate) {
            return;
        }
        ntry = ntry.copyBytes(tabRoute.addType.ecmp);
        for (int i = 0; i < ntry.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(i);
            if (attr.labelRem != null) {
                attr.rouTab = parent.fwdCore;
            }
            if (attr.segrouPrf != null) {
                attr.rouTab = parent.vrfCore.fwd6;
            }
            if (distance > 0) {
                attr.distance = distance;
            }
        }
        tabRoute.addUpdatedEntry(tabRoute.addType.ecmp, trg, afi, 0, ntry, false, null, null, null);
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
        if (!enabled) {
            return false;
        }
        tabRoute<addrIP> tabU = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> tabM = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> tabF = new tabRoute<addrIP>("bgp");
        peers = new tabGen<addrIP>();
        for (int i = 0; i < cmpU.size(); i++) {
            doImportRoute(rtrBgpUtil.sfiUnicast, cmpU.get(i), tabU);
        }
        for (int i = 0; i < cmpM.size(); i++) {
            doImportRoute(rtrBgpUtil.sfiMulticast, cmpM.get(i), tabM);
        }
        for (int i = 0; i < cmpF.size(); i++) {
            doImportRoute(rtrBgpUtil.sfiFlwSpc, cmpF.get(i), tabF);
        }
        if (flowSpec != null) {
            rtrBgpFlow.doAdvertise(tabF, flowSpec, new tabRouteEntry<addrIP>(), parent.afiUni != rtrBgpUtil.safiIp6uni, parent.localAs);
        }
        routerComputedU = tabU;
        routerComputedM = tabM;
        routerComputedF = tabF;
        routerComputedI = new tabGen<tabIndex<addrIP>>();
        fwd.routerChg(this);
        if (flowInst) {
            fwd.flowspec = tabQos.convertPolicy(rtrBgpFlow.doDecode(tabF, parent.afiUni != rtrBgpUtil.safiIp6uni));
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
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        if (enabled) {
            l.add(beg + "enable");
        } else {
            l.add(cmds.tabulator + "no" + beg + "enable");
        }
        if (routerVpn) {
            l.add(beg + "vpn-mode");
        } else {
            l.add(cmds.tabulator + "no" + beg + "vpn-mode");
        }
        l.add(beg + "distance " + distance);
        if (flowInst) {
            l.add(beg + "flowspec-install");
        }
        if (flowSpec != null) {
            l.add(beg + "flowspec-advert " + flowSpec);
        }
        if (srv6 != null) {
            l.add(beg + "srv6 " + srv6.name);
        }
        cfgRtr.getShRedist(l, beg, this);
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
        return parent.recursion;
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
        if (!enabled) {
            return;
        }
        for (int i = 0; i < peers.size(); i++) {
            addrIP adr = peers.get(i);
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, tab, parent.afiUni, 0, ntry, true, null, null, parent.routerAutoMesh);
        }
    }

}
