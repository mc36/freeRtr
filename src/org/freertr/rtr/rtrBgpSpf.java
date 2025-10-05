package org.freertr.rtr;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.enc.encTlv;
import org.freertr.pack.packHolder;
import org.freertr.spf.spfCalc;
import org.freertr.spf.spfLnkst;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.tab.tabLabelBier;
import org.freertr.tab.tabLabelEntry;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRtrmapN;
import org.freertr.tab.tabRtrplcN;
import org.freertr.util.bits;
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
     * stub flag
     */
    public boolean stub;

    /**
     * hostname
     */
    public boolean hostname;

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
        hostname = true;
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
        cmds.cfgLine(l, !stub, beg1, beg2 + "stub", "");
        cmds.cfgLine(l, !hostname, beg1, beg2 + "hostname", "");
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
        if (!nei.conn.peerAfis[rtrBgpParam.idxSpf]) {
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
        if (!nei.conn.peerAfis[rtrBgpParam.idxSpf]) {
            return;
        }
        spfLnkst.createHeader(tlv, pck, spfLnkst.protoDirect, spfLnkst.nlriTypLink);
        spfLnkst.createSpfNode(tlv, pck, hlp, parent.localAs, parent.routerID, spfLnkst.typNodeLocal);
        spfLnkst.createSpfNode(tlv, pck, hlp, nei.remoteAs, nei.conn.peerRouterID, spfLnkst.typNodeRemote);
        spfLnkst.createSpfLink(tlv, pck, nei.localAddr, nei.peerAddr);
        hlp.clear();
        if (nei.spfStub) {
            tlv.valDat[0] = 1; // no transit
            tlv.valSiz = 1;
            tlv.putBytes(hlp, spfLnkst.typSpfStat);
        }
        spfLnkst.createEntry(parent.freshly[rtrBgpParam.idxSpf], parent.computedSpf, tlv, pck, hlp, 4, nei.spfMetric);
    }

    private void doAdvertPfx(encTlv tlv, packHolder pck, packHolder hlp, tabRouteEntry<addrIP> rou) {
        if (rou == null) {
            return;
        }
        spfLnkst.createHeader(tlv, pck, spfLnkst.protoDirect, spfLnkst.getPrefixType(rou));
        spfLnkst.createSpfNode(tlv, pck, hlp, parent.localAs, parent.routerID, spfLnkst.typNodeLocal);
        spfLnkst.createPrefix(parent.freshly[rtrBgpParam.idxSpf], parent.computedSpf, tlv, pck, hlp, rou);
    }

    /**
     * merge routes to table
     */
    public void doAdvertise() {
        if (!enabled) {
            return;
        }
        encTlv tlv = spfLnkst.getTlv();
        packHolder pck = new packHolder(true, true);
        packHolder hlp = new packHolder(true, true);
        spfLnkst.createHeader(tlv, pck, spfLnkst.protoDirect, spfLnkst.nlriTypNode);
        spfLnkst.createSpfNode(tlv, pck, hlp, parent.localAs, parent.routerID, spfLnkst.typNodeLocal);
        hlp.clear();
        if (stub) {
            tlv.valDat[0] = 2; // no transit
            tlv.valSiz = 1;
            tlv.putBytes(hlp, spfLnkst.typSpfStat);
        }
        if (hostname) {
            tlv.putStr(hlp, spfLnkst.typNodeName, cfgAll.hostName);
        }
        if (parent.segrouLab != null) {
            bits.msbPutW(tlv.valDat, 0, 0); // flags
            bits.msbPutD(tlv.valDat, 2, parent.segrouLab.length << 8); // length
            bits.msbPutD(tlv.valDat, 5, parent.segrouLab[0].label << 8); // label
            tlv.valSiz = 8;
            tlv.putBytes(hlp, spfLnkst.typSrCapa);
        }
        spfLnkst.createEntry(parent.freshly[rtrBgpParam.idxSpf], parent.computedSpf, tlv, pck, hlp, 0, 0);
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
            tabRouteEntry<addrIP> ntry = parent.routerRedistedU.get(i);
            if (parent.segrouLab != null) {
                ntry = ntry.copyBytes(tabRoute.addType.ecmp);
                ntry.best.segrouIdx = parent.segrouIdx;
            }
            if (parent.bierLab != null) {
                ntry = ntry.copyBytes(tabRoute.addType.ecmp);
                ntry.best.bierBeg = parent.bierLab[0].label;
                ntry.best.bierIdx = parent.bierIdx;
                ntry.best.bierSub = parent.bierSub;
                ntry.best.bierHdr = tabLabelBier.num2bsl(parent.bierLen);
                ntry.best.bierSiz = parent.bierLab.length;
            }
            doAdvertPfx(tlv, pck, hlp, ntry);
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
        encTlv tlv = spfLnkst.getTlv();
        packHolder pck = new packHolder(true, true);
        packHolder hlp = new packHolder(true, true);
        for (int i = 0; i < parent.freshly[rtrBgpParam.idxSpf].size(); i++) {
            tabRouteEntry<addrIP> rou = parent.freshly[rtrBgpParam.idxSpf].get(i);
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
            if (pck.getByte(2) != spfLnkst.protoDirect) {
                continue;
            }
            pck.getSkip(spfLnkst.nlriHdrSize);
            if (rou.best.linkStat != null) {
                pck.putCopy(rou.best.linkStat, 0, 0, rou.best.linkStat.length);
                pck.putSkip(rou.best.linkStat.length);
                pck.merge2end();
            }
            switch (o) {
                case spfLnkst.nlriTypNode:
                    spfLnkst.readSpfNode(spf, tlv, pck, hlp);
                    break;
                case spfLnkst.nlriTypLink:
                    spfLnkst.readSpfLink(spf, tlv, pck, hlp);
                    break;
                case spfLnkst.nlriTypIpv4:
                    spfLnkst.readSpfPref(spf, tlv, pck, hlp, parent.afiUni, distance);
                    break;
                case spfLnkst.nlriTypIpv6:
                    spfLnkst.readSpfPref(spf, tlv, pck, hlp, parent.afiUni, distance);
                    break;
            }
        }
        spf.doWork(parent.routerID);
        for (int i = 0; i < parent.neighs.size(); i++) {
            doSpfNei(spf, parent.neighs.get(i));
        }
        for (int i = 0; i < parent.lstnNei.size(); i++) {
            doSpfNei(spf, parent.lstnNei.get(i));
        }
        tabGen<tabIndex<addrIP>> segrouUsd = null;
        if (parent.segrouLab != null) {
            segrouUsd = new tabGen<tabIndex<addrIP>>();
        }
        tabRoute<addrIP> tab1 = spf.getRoutes(parent.fwdCore, tabLabelEntry.owner.bgpSrgb, parent.segrouLab, segrouUsd);
        if (segrouUsd != null) {
            tabIndex.add2table(segrouUsd, new tabIndex<addrIP>(parent.segrouIdx, new addrPrefix<addrIP>(new addrIP(), 0)));
            parent.segrouLab[parent.segrouIdx].setFwdCommon(tabLabelEntry.owner.bgpSrgb, parent.fwdCore);
            for (int i = 0; i < parent.segrouLab.length; i++) {
                if (segrouUsd.find(new tabIndex<addrIP>(i, null)) != null) {
                    continue;
                }
                parent.segrouLab[i].setFwdDrop(tabLabelEntry.owner.bgpSrgb);
            }
        }
        tabRoute<addrIP> tab2 = new tabRoute<addrIP>("routes");
        tabRoute.addUpdatedTable(tabRoute.addType.ecmp, rtrBgpUtil.sfiUnicast, 0, tab2, tab1, true, roumapIn, roupolIn, prflstIn);
        parent.routerDoAggregates(rtrBgpUtil.sfiUnicast, tab2, tab2, parent.fwdCore.commonLabel, null, 0);
        if (parent.bierLab != null) {
            tabLabelBier res = spf.getBierI(parent.fwdCore, parent.bierLab[0].label, tabLabelBier.num2bsl(parent.bierLen));
            res.idx = parent.bierIdx;
            for (int i = 0; i < parent.bierLab.length; i++) {
                parent.bierLab[i].setBierMpls(tabLabelEntry.owner.bgpBier, parent.fwdCore, res);
            }
        }
        if (debugger.rtrBgpComp) {
            logger.debug("unreachable:" + spf.listReachablility(false));
            logger.debug("reachable:" + spf.listReachablility(true));
        }
        lastSpf = spf;
        tab2.setProto(parent.rouTyp, parent.rtrNum);
        tab2.preserveTime(routes);
        routes = tab2;
        parent.freshly[rtrBgpParam.idxUni].mergeFrom(tabRoute.addType.altEcmp, routes, tabRouteAttr.distanLim);
        parent.freshly[rtrBgpParam.idxMlt].mergeFrom(tabRoute.addType.altEcmp, routes, tabRouteAttr.distanLim);
        parent.routerComputedI = segrouUsd;
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
        parent.routerComputedU.mergeFrom(tabRoute.addType.altEcmp, routes, tabRouteAttr.distanLim);
        parent.routerComputedM.mergeFrom(tabRoute.addType.altEcmp, routes, tabRouteAttr.distanLim);
        return false;
    }

}
