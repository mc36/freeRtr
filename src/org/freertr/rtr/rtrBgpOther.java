package org.freertr.rtr;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgRtr;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipRtr;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabPlcmapN;
import org.freertr.tab.tabQos;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRouteUtil;
import org.freertr.user.userHelp;
import org.freertr.util.cmds;

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
     * default information originate
     */
    public boolean defRou;

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
        tabRouteUtil.generateSrv6pfx(ntry, srv6, ntry.best.labelLoc);
        tabRoute.addUpdatedEntry(tabRoute.addType.ecmp, trg, afi, 0, ntry, true, null, null, null);
    }

    /**
     * merge routes to table
     */
    public void doAdvertise() {
        if (!enabled) {
            return;
        }
        for (int i = 0; i < routerRedistedU.size(); i++) {
            doExportRoute(rtrBgpUtil.sfiUnicast, routerRedistedU.get(i), parent.freshly[rtrBgpParam.idxOuni]);
        }
        for (int i = 0; i < routerRedistedM.size(); i++) {
            doExportRoute(rtrBgpUtil.sfiMulticast, routerRedistedM.get(i), parent.freshly[rtrBgpParam.idxOmlt]);
        }
        for (int i = 0; i < routerRedistedF.size(); i++) {
            doExportRoute(rtrBgpUtil.sfiFlwSpc, routerRedistedF.get(i), parent.freshly[rtrBgpParam.idxOflw]);
        }
        if (flowSpec != null) {
            rtrBgpFlow.doAdvertise(parent.freshly[rtrBgpParam.idxOflw], flowSpec, new tabRouteEntry<addrIP>(), parent.afiUni != rtrBgpUtil.safiIp6uni, parent.localAs);
        }
    }

    private boolean doImportRoute(int afi, tabRouteEntry<addrIP> ntry, tabRoute<addrIP> trg) {
        if (ntry.best.rouSrc == rtrBgpUtil.peerOriginate) {
            return true;
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
        tabRoute.addUpdatedEntry(tabRoute.addType.always, trg, afi, 0, ntry, false, null, null, null);
        if (parent.routerAutoMesh == null) {
            return false;
        }
        peers.add(ntry.best.nextHop);
        return false;
    }

    /**
     * full import routes from table
     *
     * @return other changes trigger full recomputation
     */
    public boolean doPeersFull() {
        if (!enabled) {
            routerChangedU = null;
            routerChangedM = null;
            routerChangedF = null;
            return false;
        }
        tabRoute<addrIP> tabU = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> tabM = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> tabF = new tabRoute<addrIP>("bgp");
        peers = new tabGen<addrIP>();
        for (int i = 0; i < parent.freshly[rtrBgpParam.idxOuni].size(); i++) {
            doImportRoute(rtrBgpUtil.sfiUnicast, parent.freshly[rtrBgpParam.idxOuni].get(i), tabU);
        }
        for (int i = 0; i < parent.freshly[rtrBgpParam.idxOmlt].size(); i++) {
            doImportRoute(rtrBgpUtil.sfiMulticast, parent.freshly[rtrBgpParam.idxOmlt].get(i), tabM);
        }
        for (int i = 0; i < parent.freshly[rtrBgpParam.idxOflw].size(); i++) {
            doImportRoute(rtrBgpUtil.sfiFlwSpc, parent.freshly[rtrBgpParam.idxOflw].get(i), tabF);
        }
        if (flowSpec != null) {
            rtrBgpFlow.doAdvertise(tabF, flowSpec, new tabRouteEntry<addrIP>(), parent.afiUni != rtrBgpUtil.safiIp6uni, parent.localAs);
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
            fwd.flowspec = tabQos.convertPolicy(rtrBgpFlow.doDecode(tabF, parent.afiUni != rtrBgpUtil.safiIp6uni));
        }
        return fwd.prefixMode != ipFwd.labelMode.common;
    }

    private void doUpdateRoute(int afi, tabRouteEntry<addrIP> ntry, tabRoute<addrIP> trg, tabRoute<addrIP> cmp) {
        tabRouteEntry<addrIP> res = cmp.find(ntry);
        if (res == null) {
            trg.del(ntry);
            return;
        }
        if (doImportRoute(afi, res, trg)) {
            trg.del(ntry);
        }
    }

    /**
     * incremental import routes from table
     *
     * @return other changes trigger full recomputation
     */
    public boolean doPeersIncr() {
        if (!enabled) {
            routerChangedU = null;
            routerChangedM = null;
            routerChangedF = null;
            return false;
        }
        tabRoute<addrIP> chgU = routerChangedU;
        tabRoute<addrIP> chgM = routerChangedM;
        tabRoute<addrIP> chgF = routerChangedF;
        if (chgU == null) {
            chgU = new tabRoute<addrIP>("empty");
        }
        if (chgM == null) {
            chgM = new tabRoute<addrIP>("empty");
        }
        if (chgF == null) {
            chgF = new tabRoute<addrIP>("empty");
        }
        for (int i = 0; i < chgU.size(); i++) {
            doUpdateRoute(rtrBgpUtil.sfiUnicast, chgU.get(i), routerComputedU, parent.computd[rtrBgpParam.idxOuni]);
        }
        for (int i = 0; i < chgM.size(); i++) {
            doUpdateRoute(rtrBgpUtil.sfiMulticast, chgM.get(i), routerComputedM, parent.computd[rtrBgpParam.idxOmlt]);
        }
        for (int i = 0; i < chgF.size(); i++) {
            doUpdateRoute(rtrBgpUtil.sfiFlwSpc, chgF.get(i), routerComputedF, parent.computd[rtrBgpParam.idxOflw]);
        }
        fwd.routerChg(this, fwd.prefixMode != ipFwd.labelMode.common);
        if (flowInst && (chgF.size() > 0)) {
            tabRoute<addrIP> tabF = new tabRoute<addrIP>("bgp");
            fwd.flowspec = tabQos.convertPolicy(rtrBgpFlow.doDecode(tabF, parent.afiUni != rtrBgpUtil.safiIp6uni));
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
     * get config
     *
     * @param l list to append
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        if (enabled) {
            l.add(beg + "enable");
        } else {
            l.add(cmds.tabulator + cmds.negated + beg + "enable");
        }
        if (defRou) {
            l.add(beg + "default-originate");
        } else {
            l.add(cmds.tabulator + cmds.negated + beg + "default-originate");
        }
        if (routerVpn) {
            l.add(beg + "vpn-mode");
        } else {
            l.add(cmds.tabulator + cmds.negated + beg + "vpn-mode");
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
