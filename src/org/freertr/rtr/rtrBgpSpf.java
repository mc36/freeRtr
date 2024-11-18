package org.freertr.rtr;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.enc.encTlv;
import org.freertr.pack.packHolder;
import org.freertr.spf.spfCalc;
import org.freertr.spf.spfLnkst;
import org.freertr.tab.tabLabelEntry;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRtrmapN;
import org.freertr.tab.tabRtrplcN;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * bgp4 shortest path first
 *
 * @author matecsaba
 */
public class rtrBgpSpf {

    /**
     * enabled
     */
    public boolean enabled;

    /**
     * import distance
     */
    public int distance;

    /**
     * default information originate
     */
    public boolean defRou;

    /**
     * ingress prefix list
     */
    public tabListing<tabPrfxlstN, addrIP> prflstIn;

    /**
     * ingress route map
     */
    public tabListing<tabRtrmapN, addrIP> roumapIn;

    /**
     * ingress route policy
     */
    public tabListing<tabRtrplcN, addrIP> roupolIn;

    /**
     * last spf
     */
    protected spfCalc<addrIPv4> lastSpf;

    /**
     * last routes
     */
    protected tabRoute<addrIP> routes;

    private final rtrBgp parent;

    private final boolean ipv4;

    /**
     * create new instance
     *
     * @param p parent to use
     */
    public rtrBgpSpf(rtrBgp p) {
        parent = p;
        distance = 60;
        lastSpf = new spfCalc<addrIPv4>(null);
        routes = new tabRoute<addrIP>("bst");
        ipv4 = p.rouTyp == tabRouteAttr.routeType.bgp4;
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
     * get config
     *
     * @param l list to append
     * @param beg1 beginning
     * @param beg2 beginning
     */
    public void getConfig(List<String> l, String beg1, String beg2) {
        cmds.cfgLine(l, !enabled, beg1, beg2 + "enable", "");
        l.add(beg1 + beg2 + "distance " + distance);
        l.add(beg1 + beg2 + "spf-log " + lastSpf.logSize);
        cmds.cfgLine(l, lastSpf.topoLog.get() == 0, beg1, beg2 + "spf-topolog", lastSpf.getTopoLogMode());
        cmds.cfgLine(l, lastSpf.bidir.get() == 0, beg1, beg2 + "spf-bidir", "");
        cmds.cfgLine(l, lastSpf.hops.get() == 0, beg1, beg2 + "spf-hops", "");
        cmds.cfgLine(l, lastSpf.ecmp.get() == 0, beg1, beg2 + "spf-ecmp", "");
        cmds.cfgLine(l, !defRou, beg1, beg2 + "default-originate", "");
        cmds.cfgLine(l, prflstIn == null, beg1, beg2 + "prefix-list", "" + prflstIn);
        cmds.cfgLine(l, roumapIn == null, beg1, beg2 + "route-map", "" + roumapIn);
        cmds.cfgLine(l, roupolIn == null, beg1, beg2 + "route-policy", "" + roupolIn);
    }

    private void doSpfNei(spfCalc<addrIPv4> spf, rtrBgpNeigh nei) {
        if (nei == null) {
            return;
        }
        if (!nei.conn.ready2adv) {
            return;
        }
        if ((nei.conn.peerAfis & rtrBgpParam.mskSpf) == 0) {
            return;
        }
        spf.addNextHop(nei.spfMetric, nei.conn.peerRouterID, nei.peerAddr, nei.localIfc, null, null);
    }

    private void doAdvertNei(encTlv tlv, packHolder pck, packHolder hlp, rtrBgpNeigh nei) {
        if (nei == null) {
            return;
        }
        if (!nei.conn.ready2adv) {
            return;
        }
        if ((nei.conn.peerAfis & rtrBgpParam.mskSpf) == 0) {
            return;
        }
        spfLnkst.listLinkStateHdr(tlv, pck, 4, 2);
        spfLnkst.listSpfNod(tlv, pck, hlp, parent.localAs, parent.routerID, 256); // local node
        spfLnkst.listSpfNod(tlv, pck, hlp, nei.remoteAs, nei.conn.peerRouterID, 257); // remote node
        spfLnkst.listSpfLnk(tlv, pck, nei.localAddr, nei.peerAddr);
        spfLnkst.listLinkStateAdd(parent.newlySpf, tlv, pck, 4, nei.spfMetric, 0);
    }

    private void doAdvertPfx(encTlv tlv, packHolder pck, packHolder hlp, tabRouteEntry<addrIP> rou) {
        if (rou == null) {
            return;
        }
        spfLnkst.listLinkStateHdr(tlv, pck, 4, spfLnkst.getPrefixType(rou));
        spfLnkst.listSpfNod(tlv, pck, hlp, parent.localAs, parent.routerID, 256); // local node
        spfLnkst.listLinkStatePrf(parent.newlySpf, tlv, pck, hlp, rou, 0);
    }

    /**
     * merge routes to table
     */
    public void doAdvertise() {
        if (!enabled) {
            return;
        }
        encTlv tlv = spfLnkst.listLinkStateTlv();
        packHolder pck = new packHolder(true, true);
        packHolder hlp = new packHolder(true, true);
        spfLnkst.listLinkStateHdr(tlv, pck, 4, 1);
        spfLnkst.listSpfNod(tlv, pck, hlp, parent.localAs, parent.routerID, 256); // local node
        spfLnkst.listLinkStateAdd(parent.newlySpf, tlv, pck, 0, 0, 0);
        for (int i = 0; i < parent.neighs.size(); i++) {
            doAdvertNei(tlv, pck, hlp, parent.neighs.get(i));
        }
        for (int i = 0; i < parent.lstnNei.size(); i++) {
            doAdvertNei(tlv, pck, hlp, parent.lstnNei.get(i));
        }
        if (defRou) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = rtrBgpUtil.defaultRoute(parent.afiUni);
            doAdvertPfx(tlv, pck, hlp, ntry);
        }
        for (int i = 0; i < parent.routerRedistedU.size(); i++) {
            doAdvertPfx(tlv, pck, hlp, parent.routerRedistedU.get(i));
        }
    }

    /**
     * full import routes from table
     *
     * @return other changes trigger full recomputation
     */
    public boolean doPeersFull() {
        if (!enabled) {
            return false;
        }
        spfCalc<addrIPv4> spf = new spfCalc<addrIPv4>(lastSpf);
        encTlv tlv = spfLnkst.listLinkStateTlv();
        packHolder pck = new packHolder(true, true);
        packHolder hlp = new packHolder(true, true);
        for (int i = 0; i < parent.newlySpf.size(); i++) {
            tabRouteEntry<addrIP> rou = parent.newlySpf.get(i);
            if (rou == null) {
                continue;
            }
            if (rou.nlri == null) {
                continue;
            }
            pck.clear();
            pck.putCopy(rou.nlri, 0, 0, rou.nlri.length);
            pck.putSkip(rou.nlri.length);
            pck.merge2beg();
            int o = pck.msbGetW(0); // type
            if (pck.getByte(2) != 4) { // protocol
                continue;
            }
            pck.getSkip(11); // header
            if (rou.best.linkStat != null) {
                pck.putCopy(rou.best.linkStat, 0, 0, rou.best.linkStat.length);
                pck.putSkip(rou.best.linkStat.length);
                pck.merge2end();
            }
            switch (o) {
                case 1: // node
                    spfLnkst.readSpfNode(spf, tlv, pck, hlp);
                    break;
                case 2: // link
                    spfLnkst.readSpfLink(spf, tlv, pck, hlp);
                    break;
                case 3: // ipv4 prefix
                    spfLnkst.readSpfPref(spf, tlv, pck, hlp, parent.afiUni, distance);
                    break;
                case 4: // ipv6 prefix
                    spfLnkst.readSpfPref(spf, tlv, pck, hlp, parent.afiUni, distance);
                    break;
            }
        }
        spf.doWork(null, parent.routerID, null);
        for (int i = 0; i < parent.neighs.size(); i++) {
            doSpfNei(spf, parent.neighs.get(i));
        }
        for (int i = 0; i < parent.lstnNei.size(); i++) {
            doSpfNei(spf, parent.lstnNei.get(i));
        }
        tabRoute<addrIP> tab1 = spf.getRoutes(parent.fwdCore, tabLabelEntry.owner.bgpSrgb, null, null);
        tabRoute<addrIP> tab2 = new tabRoute<addrIP>("routes");
        tabRoute.addUpdatedTable(tabRoute.addType.ecmp, rtrBgpUtil.sfiUnicast, 0, tab2, tab1, true, roumapIn, roupolIn, prflstIn);
        parent.routerDoAggregates(rtrBgpUtil.sfiUnicast, tab2, tab2, parent.fwdCore.commonLabel, null, 0);
        if (debugger.rtrBgpComp) {
            logger.debug("unreachable:" + spf.listReachablility(false));
            logger.debug("reachable:" + spf.listReachablility(true));
        }
        lastSpf = spf;
        tab2.setProto(parent.rouTyp, parent.rtrNum);
        tab2.preserveTime(routes);
        routes = tab2;
        parent.newlyUni.mergeFrom(tabRoute.addType.better, routes, tabRouteAttr.distanLim);
        parent.newlyMlt.mergeFrom(tabRoute.addType.better, routes, tabRouteAttr.distanLim);
        return false;
    }

    /**
     * incremental import routes from table
     *
     * @return other changes trigger full recomputation
     */
    public boolean doPeersIncr() {
        if (!enabled) {
            return false;
        }
        parent.routerComputedU.mergeFrom(tabRoute.addType.better, routes, tabRouteAttr.distanLim);
        parent.routerComputedM.mergeFrom(tabRoute.addType.better, routes, tabRouteAttr.distanLim);
        return false;
    }
}
