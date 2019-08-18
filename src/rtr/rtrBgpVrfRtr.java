package rtr;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrPrefix;
import cfg.cfgIfc;
import cfg.cfgRtr;
import cfg.cfgVrf;
import ip.ipFwd;
import ip.ipRtr;
import java.util.ArrayList;
import java.util.List;
import tab.tabGen;
import tab.tabListing;
import tab.tabPlcmapN;
import tab.tabRoute;
import tab.tabRouteEntry;
import tab.tabRtrmapN;
import user.userHelping;
import util.cmds;
import util.debugger;
import util.logger;

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
     * mvpn advertisement source
     */
    public cfgIfc mvpn;

    private final rtrBgp parent;

    private final cfgVrf vrf;

    private final ipFwd fwd;

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
        if (o ^ (p.rouTyp == tabRouteEntry.routeType.bgp4)) {
            fwd = v.fwd4;
        } else {
            fwd = v.fwd6;
        }
        parent = p;
        other = o;
        vrf = v;
        routerVpn = true;
        distance = 200;
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
    }

    private void doExportRoute(int afi, tabRouteEntry<addrIP> ntry, tabRoute<addrIP> trg, List<Long> rt) {
        if (ntry == null) {
            return;
        }
        ntry = ntry.copyBytes();
        if (ntry.labelLoc == null) {
            ntry.labelLoc = fwd.commonLabel;
        }
        if (ntry.extComm == null) {
            ntry.extComm = new ArrayList<Long>();
        }
        ntry.rouDst = vrf.rd;
        ntry.extComm.addAll(rt);
        ntry.rouSrc = rtrBgpUtil.peerOriginate;
        tabRoute.addUpdatedEntry(tabRoute.addType.better, trg, afi, ntry, fwd.exportMap, fwd.exportPol, fwd.exportList);
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
        for (int i = 0; i < routerRedistedU.size(); i++) {
            doExportRoute(rtrBgpUtil.safiUnicast, routerRedistedU.get(i), nUni, rt);
        }
        for (int i = 0; i < routerRedistedM.size(); i++) {
            doExportRoute(rtrBgpUtil.safiMulticast, routerRedistedM.get(i), nMlt, rt);
        }
        for (int i = 0; i < routerRedistedF.size(); i++) {
            doExportRoute(rtrBgpUtil.safiFlwSpc, routerRedistedF.get(i), nFlw, rt);
        }
        if (flowSpec != null) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.extComm = new ArrayList<Long>();
            ntry.rouDst = vrf.rd;
            ntry.extComm.addAll(rt);
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
            ntry.extComm = new ArrayList<Long>();
            ntry.rouDst = vrf.rd;
            ntry.extComm.addAll(rt);
            ntry.rouSrc = rtrBgpUtil.peerOriginate;
            tabRoute.addUpdatedEntry(tabRoute.addType.better, nMvpn, other ? parent.afiVpoM : parent.afiVpnM, ntry, fwd.exportMap, fwd.exportPol, fwd.exportList);
        }
    }

    private void doImportRoute(int afi, tabRouteEntry<addrIP> ntry, tabRoute<addrIP> trg, List<Long> rt) {
        if (ntry.rouSrc == rtrBgpUtil.peerOriginate) {
            return;
        }
        if (ntry.extComm == null) {
            return;
        }
        boolean needed = false;
        for (int i = 0; i < rt.size(); i++) {
            needed |= rtrBgpUtil.findLongList(ntry.extComm, rt.get(i)) >= 0;
            if (needed) {
                break;
            }
        }
        if (!needed) {
            return;
        }
        ntry = ntry.copyBytes();
        ntry.rouDst = 0;
        ntry.rouTab = parent.fwdCore;
        ntry.distance = distance;
        tabRoute.addUpdatedEntry(tabRoute.addType.better, trg, afi, ntry, fwd.importMap, fwd.importPol, fwd.importList);
        if (parent.routerAutoMesh == null) {
            return;
        }
        peers.add(ntry.nextHop);
    }

    /**
     * import routes from table
     *
     * @param cmpU unicast table to read
     * @param cmpM multicast table to read
     * @param cmpF flowspec table to read
     */
    public void doPeers(tabRoute<addrIP> cmpU, tabRoute<addrIP> cmpM, tabRoute<addrIP> cmpF) {
        final List<Long> rt = new ArrayList<Long>();
        for (int i = 0; i < vrf.rtImp.size(); i++) {
            rt.add(tabRtrmapN.rt2comm(vrf.rtImp.get(i)));
        }
        tabRoute<addrIP> tabU = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> tabM = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> tabF = new tabRoute<addrIP>("bgp");
        peers = new tabGen<addrIP>();
        for (int i = 0; i < cmpU.size(); i++) {
            doImportRoute(rtrBgpUtil.safiUnicast, cmpU.get(i), tabU, rt);
        }
        for (int i = 0; i < cmpM.size(); i++) {
            doImportRoute(rtrBgpUtil.safiMulticast, cmpM.get(i), tabM, rt);
        }
        for (int i = 0; i < cmpF.size(); i++) {
            doImportRoute(rtrBgpUtil.safiFlwSpc, cmpF.get(i), tabF, rt);
        }
        routerDoAggregates(parent.afiUni, tabU, null, fwd.commonLabel, rtrBgpUtil.peerOriginate, parent.routerID, parent.localAs);
        routerDoAggregates(parent.afiMlt, tabM, null, fwd.commonLabel, rtrBgpUtil.peerOriginate, parent.routerID, parent.localAs);
        routerComputedU = tabU;
        routerComputedM = tabM;
        routerComputedF = tabF;
        fwd.routerChg(this);
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
    public void routerGetConfig(List<String> l, String beg, boolean filter) {
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
        cmds.cfgLine(l, flowSpec == null, beg1, beg2 + "flowspec", "" + flowSpec);
        if (mvpn != null) {
            l.add(beg1 + beg2 + "mvpn " + mvpn.name);
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
     * get peer list
     *
     * @param tab list to append
     */
    public void getPeerList(tabRoute<addrIP> tab) {
        for (int i = 0; i < peers.size(); i++) {
            addrIP adr = peers.get(i);
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, tab, parent.afiUni, ntry, null, null, parent.routerAutoMesh);
        }
    }

}
