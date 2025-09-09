package org.freertr.rtr;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipRtr;
import org.freertr.prt.prtTcp;
import org.freertr.prt.prtUdp;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabLabelBier;
import org.freertr.tab.tabLabelBierN;
import org.freertr.tab.tabLabelEntry;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.keyword;
import org.freertr.util.logger;
import org.freertr.util.notifier;
import org.freertr.util.state;

/**
 * path vector routing protocol
 *
 * @author matecsaba
 */
public class rtrPvrp extends ipRtr implements Runnable {

    /**
     * port number
     */
    public final static int port = 1547;

    /**
     * forwarding core
     */
    public final ipFwd fwdCore;

    /**
     * udp core
     */
    protected final prtUdp udpCore;

    /**
     * tcp core
     */
    protected final prtTcp tcpCore;

    /**
     * router id
     */
    public addrIPv4 routerID;

    /**
     * stub flag
     */
    public boolean stub;

    /**
     * routes needed to advertise
     */
    public tabRoute<addrIP> need2adv;

    /**
     * list of interfaces
     */
    protected tabGen<rtrPvrpIface> ifaces;

    /**
     * advertise labels
     */
    public boolean labels = false;

    /**
     * segment routing index
     */
    public int segrouIdx = 0;

    /**
     * segment routing maximum
     */
    public int segrouMax = 0;

    /**
     * segment routing base
     */
    public int segrouBase = 0;

    /**
     * bier index
     */
    public int bierIdx = 0;

    /**
     * bier subdomain
     */
    public int bierSub = 0;

    /**
     * bier length
     */
    public int bierLen = 0;

    /**
     * bier maximum
     */
    public int bierMax = 0;

    /**
     * suppress interface addresses
     */
    public boolean suppressAddr = false;

    /**
     * segment routing labels
     */
    protected tabLabelEntry[] segrouLab;

    /**
     * bier labels
     */
    protected tabLabelEntry[] bierLab;

    /**
     * notified on route change
     */
    protected notifier notif = new notifier();

    private boolean need2run = true;

    /**
     * create one pvrp process
     *
     * @param forwarder the ip protocol
     * @param udp the udp protocol
     * @param tcp the tcp protocol
     * @param id process id
     */
    public rtrPvrp(ipFwd forwarder, prtUdp udp, prtTcp tcp, int id) {
        fwdCore = forwarder;
        udpCore = udp;
        tcpCore = tcp;
        routerID = new addrIPv4();
        ifaces = new tabGen<rtrPvrpIface>();
        tabRouteAttr.routeType rouTyp = null;
        switch (fwdCore.ipVersion) {
            case ipCor4.protocolVersion:
                rouTyp = tabRouteAttr.routeType.pvrp4;
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteAttr.routeType.pvrp6;
                break;
            default:
                break;
        }
        routerCreateComputed();
        fwdCore.routerAdd(this, rouTyp, id);
        new Thread(this).start();
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "pvrp on " + fwdCore;
    }

    /**
     * add one interface to work on
     *
     * @param ifc ip forwarder interface
     * @return false if successful, true if error happened
     */
    public rtrPvrpIface addInterface(ipFwdIface ifc) {
        if (debugger.rtrPvrpEvnt) {
            logger.debug("add iface " + ifc);
        }
        if (ifc == null) {
            return null;
        }
        rtrPvrpIface ntry = new rtrPvrpIface(this, ifc);
        rtrPvrpIface old = ifaces.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.register2udp();
        routerCreateComputed();
        return ntry;
    }

    /**
     * delete one interface
     *
     * @param ifc interface to delete
     */
    public void delInterface(ipFwdIface ifc) {
        if (debugger.rtrPvrpEvnt) {
            logger.debug("del iface " + ifc);
        }
        if (ifc == null) {
            return;
        }
        rtrPvrpIface ntry = new rtrPvrpIface(this, ifc);
        ntry = ifaces.del(ntry);
        if (ntry == null) {
            return;
        }
        ntry.unregister2udp();
        ntry.closeNeighbors();
        routerCreateComputed();
    }

    /**
     * list of neighbors
     *
     * @param brief only briefly
     * @return list
     */
    public userFormat showNeighs(boolean brief) {
        userFormat res;
        if (brief) {
            res = new userFormat("|", "router|name|uptime");
        } else {
            res = new userFormat("|", "iface|router|name|peerif|peer|learned|adverted|uptime");
        }
        for (int i = 0; i < ifaces.size(); i++) {
            rtrPvrpIface ifc = ifaces.get(i);
            ifc.showNeighs(res, brief);
        }
        return res;
    }

    /**
     * list of neighbors
     *
     * @return list
     */
    public userFormat showMetrics() {
        userFormat res = new userFormat("|", "iface|router|name|peer|metric|gotmet|delay");
        for (int i = 0; i < ifaces.size(); i++) {
            rtrPvrpIface ifc = ifaces.get(i);
            ifc.showMetrics(res);
        }
        return res;
    }

    /**
     * find one neighbor
     *
     * @param adr address of peer
     * @return neighbor, null if not found
     */
    public rtrPvrpNeigh findNeigh(addrIP adr) {
        for (int i = 0; i < ifaces.size(); i++) {
            rtrPvrpIface ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            rtrPvrpNeigh r = ifc.findNeigh(adr);
            if (r != null) {
                return r;
            }
        }
        return null;
    }

    /**
     * list statistics
     *
     * @param iface forwarding interface
     * @return list of interfaces
     */
    public userFormat showStats(ipFwdIface iface) {
        if (iface == null) {
            return null;
        }
        rtrPvrpIface ifc = new rtrPvrpIface(this, iface);
        ifc = ifaces.find(ifc);
        if (ifc == null) {
            return null;
        }
        return keyword.dump(ifc.msgStatRx, ifc.msgStatTx);
    }

    /**
     * list interfaces
     *
     * @return list of interfaces
     */
    public userFormat showIfaces() {
        userFormat l = new userFormat("|", "interface|neighbors");
        for (int i = 0; i < ifaces.size(); i++) {
            rtrPvrpIface ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            l.add(ifc.iface + "|" + ifc.neighs.size());
        }
        return l;
    }

    /**
     * get ip protocol version
     *
     * @return protocol version
     */
    public int getProtoVer() {
        return fwdCore.ipVersion;
    }

    /**
     * create computed
     */
    public synchronized void routerCreateComputed() {
        if (debugger.rtrPvrpEvnt) {
            logger.debug("create table");
        }
        tabRoute<addrIP> tab1 = new tabRoute<addrIP>("lernd");
        tabRouteEntry<addrIP> ntry;
        for (int i = 0; i < ifaces.size(); i++) {
            rtrPvrpIface ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
                continue;
            }
            if ((suppressAddr || ifc.suppressAddr) && (!ifc.unsuppressAddr)) {
                continue;
            }
            ntry = tab1.add(tabRoute.addType.better, ifc.iface.network, null);
            ntry.best.rouTyp = tabRouteAttr.routeType.conn;
            ntry.best.iface = ifc.iface;
            ntry.best.distance = tabRouteAttr.distanIfc;
            if (ifc.segrouIdx >= 0) {
                ntry.best.segrouIdx = ifc.segrouIdx;
            } else {
                ntry.best.segrouIdx = segrouIdx;
            }
            if (ifc.bierIdx >= 0) {
                ntry.best.bierIdx = ifc.bierIdx;
                ntry.best.bierSub = ifc.bierSub;
            } else {
                ntry.best.bierIdx = bierIdx;
                ntry.best.bierSub = bierSub;
            }
        }
        for (int o = 0; o < ifaces.size(); o++) {
            rtrPvrpIface ifc = ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
                continue;
            }
            for (int i = 0; i < ifc.neighs.size(); i++) {
                rtrPvrpNeigh nei = ifc.neighs.get(i);
                if (nei == null) {
                    continue;
                }
                tab1.mergeFrom(tabRoute.addType.ecmp, nei.learned, tabRouteAttr.distanLim);
            }
        }
        routerDoAggregates(rtrBgpUtil.sfiUnicast, tab1, tab1, fwdCore.commonLabel, null, 0);
        tabRoute<addrIP> tab2 = tab1;
        tab1 = new tabRoute<addrIP>("ned2adv");
        tab1.mergeFrom(tabRoute.addType.ecmp, tab2, tabRouteAttr.distanLim);
        for (int i = 0; i < routerRedistedU.size(); i++) {
            ntry = routerRedistedU.get(i);
            if (ntry == null) {
                continue;
            }
            ntry = ntry.copyBytes(tabRoute.addType.notyet);
            ntry.best.distance = tabRouteAttr.distanIfc + 1;
            ntry.best.rouSrc = 1;
            ntry.best.segrouIdx = segrouIdx;
            ntry.best.bierIdx = bierIdx;
            ntry.best.bierSub = bierSub;
            tab1.add(tabRoute.addType.better, ntry, false, false);
        }
        if (labels) {
            for (int i = 0; i < tab1.size(); i++) {
                ntry = tab1.get(i);
                tabRouteEntry<addrIP> org = fwdCore.labeldR.find(ntry);
                if (org == null) {
                    continue;
                }
                ntry.best.labelLoc = org.best.labelLoc;
            }
        }
        need2adv = tab1;
        for (int o = 0; o < ifaces.size(); o++) {
            rtrPvrpIface ifc = ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            tab1 = new tabRoute<addrIP>("ned2adv");
            tabRoute.addUpdatedTable(tabRoute.addType.always, rtrBgpUtil.sfiUnicast, 0, tab1, need2adv, true, ifc.roumapOut, ifc.roupolOut, ifc.prflstOut);
            if (ifc.defOrigin) {
                ntry = new tabRouteEntry<addrIP>();
                ntry.prefix = addrPrefix.defaultRoute(getProtoVer());
                if (labels) {
                    ntry.best.labelLoc = fwdCore.commonLabel;
                }
                if (ifc.segrouIdx >= 0) {
                    ntry.best.segrouIdx = ifc.segrouIdx;
                } else {
                    ntry.best.segrouIdx = segrouIdx;
                }
                if (ifc.bierIdx >= 0) {
                    ntry.best.bierIdx = ifc.bierIdx;
                    ntry.best.bierSub = ifc.bierSub;
                } else {
                    ntry.best.bierIdx = bierIdx;
                    ntry.best.bierSub = bierSub;
                }
                tab1.add(tabRoute.addType.always, ntry, true, true);
            }
            if (ifc.splitHorizon) {
                tab1.delIface(ifc.iface);
            }
            if ((stub || ifc.stub) && (!ifc.unstub)) {
                for (int i = tab1.size() - 1; i >= 0; i--) {
                    ntry = tab1.get(i);
                    if (ntry == null) {
                        continue;
                    }
                    if (ntry.best.clustList == null) {
                        continue;
                    }
                    if (ntry.best.clustList.size() < 1) {
                        continue;
                    }
                    tab1.del(ntry);
                }
            }
            ifc.need2adv = tab1;
            for (int i = 0; i < ifc.neighs.size(); i++) {
                rtrPvrpNeigh nei = ifc.neighs.get(i);
                if (nei == null) {
                    continue;
                }
                nei.notif.wakeup();
            }
        }
        tab2.setProto(routerProtoTyp, routerProcNum);
        if (tab2.preserveTime(routerComputedU)) {
            return;
        }
        tabGen<tabIndex<addrIP>> segrouUsd = new tabGen<tabIndex<addrIP>>();
        if (segrouLab != null) {
            for (int i = 0; i < tab2.size(); i++) {
                ntry = tab2.get(i);
                if (ntry == null) {
                    continue;
                }
                if (ntry.best.segrouBeg < 1) {
                    continue;
                }
                if ((ntry.best.segrouIdx <= 0) || (ntry.best.segrouIdx >= segrouMax)) {
                    continue;
                }
                List<Integer> lab = tabLabel.int2labels(ntry.best.segrouBeg + ntry.best.segrouIdx);
                segrouLab[ntry.best.segrouIdx].setFwdMpls(tabLabelEntry.owner.pvrpSrgb, fwdCore, (ipFwdIface) ntry.best.iface, ntry.best.nextHop, lab);
                tabIndex.add2table(segrouUsd, new tabIndex<addrIP>(ntry.best.segrouIdx, ntry.prefix));
            }
            if (segrouIdx > 0) {
                segrouLab[segrouIdx].setFwdCommon(tabLabelEntry.owner.pvrpSrgb, fwdCore);
                tabIndex.add2table(segrouUsd, new tabIndex<addrIP>(segrouIdx, new addrPrefix<addrIP>(new addrIP(), 0)));
            }
            for (int o = 0; o < ifaces.size(); o++) {
                rtrPvrpIface ifc = ifaces.get(o);
                if (ifc == null) {
                    continue;
                }
                if (ifc.iface.lower.getState() != state.states.up) {
                    continue;
                }
                if (ifc.segrouIdx < 1) {
                    continue;
                }
                segrouLab[ifc.segrouIdx].setFwdCommon(tabLabelEntry.owner.pvrpSrgb, fwdCore);
                tabIndex.add2table(segrouUsd, new tabIndex<addrIP>(ifc.segrouIdx, new addrPrefix<addrIP>(new addrIP(), 0)));
            }
            for (int i = 0; i < segrouLab.length; i++) {
                if (segrouUsd.find(new tabIndex<addrIP>(i, null)) != null) {
                    continue;
                }
                segrouLab[i].setFwdDrop(tabLabelEntry.owner.pvrpSrgb);
            }
        }
        if (bierLab != null) {
            tabLabelBier res = new tabLabelBier(bierLab[0].label, tabLabelBier.num2bsl(bierLen));
            res.idx = bierIdx;
            for (int o = 0; o < ifaces.size(); o++) {
                rtrPvrpIface ifc = ifaces.get(o);
                if (ifc == null) {
                    continue;
                }
                if (ifc.iface.lower.getState() != state.states.up) {
                    continue;
                }
                if (ifc.bierIdx < 1) {
                    continue;
                }
                if (res.idx < 1) {
                    res.idx = ifc.bierIdx;
                } else {
                    res.idx2 = ifc.bierIdx;
                }
            }
            for (int i = 0; i < tab2.size(); i++) {
                ntry = tab2.get(i);
                if (ntry == null) {
                    continue;
                }
                if (ntry.best.bierBeg < 1) {
                    continue;
                }
                if ((ntry.best.bierIdx <= 0) || (ntry.best.bierIdx >= bierMax)) {
                    continue;
                }
                tabLabelBierN per = new tabLabelBierN(fwdCore, ntry.best.iface, ntry.best.nextHop, ntry.best.bierBeg, 0);
                tabLabelBierN old = res.peers.add(per);
                if (old != null) {
                    per = old;
                }
                per.setBit(ntry.best.bierIdx - 1);
            }
            for (int i = 0; i < bierLab.length; i++) {
                bierLab[i].setBierMpls(tabLabelEntry.owner.pvrpBier, fwdCore, res);
            }
        }
        routerComputedU = tab2;
        routerComputedM = tab2;
        routerComputedF = new tabRoute<addrIP>("rx");
        routerComputedI = segrouUsd;
        fwdCore.routerChg(this, false);
    }

    /**
     * redistribution changed
     */
    public void routerRedistChanged() {
        routerCreateComputed();
    }

    /**
     * others changed
     */
    public void routerOthersChanged() {
        if (labels) {
            routerCreateComputed();
            return;
        }
    }

    /**
     * stop work
     */
    public void routerCloseNow() {
        if (debugger.rtrPvrpEvnt) {
            logger.debug("shutdown");
        }
        need2run = false;
        for (int i = 0; i < ifaces.size(); i++) {
            rtrPvrpIface ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            ifc.unregister2udp();
            ifc.closeNeighbors();
        }
        tabLabel.release(segrouLab, tabLabelEntry.owner.pvrpSrgb);
        tabLabel.release(bierLab, tabLabelEntry.owner.pvrpBier);
    }

    /**
     * get help
     *
     * @param l list
     */
    public void routerGetHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "router-id", "specify router id");
        l.add(null, false, 2, new int[]{-1}, "<addr>", "router id");
        l.add(null, false, 1, new int[]{-1}, "labels", "specify label mode");
        l.add(null, false, 1, new int[]{-1}, "stub", "stub router");
        l.add(null, false, 1, new int[]{-1}, "suppress-prefix", "do not advertise interfaces");
        l.add(null, false, 1, new int[]{2}, "segrout", "segment routing parameters");
        l.add(null, false, 2, new int[]{3}, "<num>", "maximum index");
        l.add(null, false, 3, new int[]{4, -1}, "<num>", "this node index");
        l.add(null, false, 4, new int[]{5}, "base", "specify base");
        l.add(null, false, 5, new int[]{4, -1}, "<num>", "label base");
        l.add(null, false, 1, new int[]{2}, "bier", "bier parameters");
        l.add(null, false, 2, new int[]{3}, "<num>", "bitstring length");
        l.add(null, false, 3, new int[]{4}, "<num>", "maximum index");
        l.add(null, false, 4, new int[]{5, -1}, "<num>", "node index");
        l.add(null, false, 5, new int[]{-1}, "<num>", "node subdomain");
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void routerGetConfig(List<String> l, String beg, int filter) {
        l.add(beg + "router-id " + routerID);
        cmds.cfgLine(l, !stub, beg, "stub", "");
        cmds.cfgLine(l, !labels, beg, "labels", "");
        cmds.cfgLine(l, !suppressAddr, beg, "suppress-prefix", "");
        String a = "";
        if (segrouBase != 0) {
            a += " base " + segrouBase;
        }
        cmds.cfgLine(l, segrouMax < 1, beg, "segrout", segrouMax + " " + segrouIdx + a);
        cmds.cfgLine(l, bierMax < 1, beg, "bier", bierLen + " " + bierMax + " " + bierIdx + " " + bierSub);
    }

    /**
     * configure
     *
     * @param cmd command
     * @return false if success, true if error
     */
    public boolean routerConfigure(cmds cmd) {
        String s = cmd.word();
        boolean negated = false;
        if (s.equals(cmds.negated)) {
            s = cmd.word();
            negated = true;
        }
        if (s.equals("router-id")) {
            s = cmd.word();
            routerID.fromString(s);
            cfgIfc ifc = cfgAll.ifcFind(s, 0);
            if (ifc != null) {
                if (ifc.addr4 != null) {
                    routerID.setAddr(ifc.addr4);
                }
            }
            if (negated) {
                routerID = new addrIPv4();
            }
            return false;
        }
        if (s.equals("labels")) {
            labels = !negated;
            notif.wakeup();
            return false;
        }
        if (s.equals("stub")) {
            stub = !negated;
            notif.wakeup();
            return false;
        }
        if (s.equals("suppress-prefix")) {
            suppressAddr = !negated;
            notif.wakeup();
            return false;
        }
        if (s.equals("segrout")) {
            tabLabel.release(segrouLab, tabLabelEntry.owner.pvrpSrgb);
            segrouLab = null;
            if (negated) {
                segrouIdx = 0;
                segrouMax = 0;
                segrouBase = 0;
                notif.wakeup();
                return false;
            }
            segrouMax = bits.str2num(cmd.word());
            segrouIdx = bits.str2num(cmd.word());
            segrouBase = 0;
            for (;;) {
                s = cmd.word();
                if (s.length() < 1) {
                    break;
                }
                if (s.equals("base")) {
                    segrouBase = bits.str2num(cmd.word());
                    continue;
                }
            }
            segrouLab = tabLabel.allocate(tabLabelEntry.owner.pvrpSrgb, segrouBase, segrouMax);
            notif.wakeup();
            return false;
        }
        if (s.equals("bier")) {
            tabLabel.release(bierLab, tabLabelEntry.owner.pvrpBier);
            bierLab = null;
            if (negated) {
                bierIdx = 0;
                bierSub = 0;
                bierMax = 0;
                bierLen = 0;
                notif.wakeup();
                return false;
            }
            bierLen = tabLabelBier.normalizeBsl(bits.str2num(cmd.word()));
            bierMax = bits.str2num(cmd.word());
            bierIdx = bits.str2num(cmd.word());
            bierSub = bits.str2num(cmd.word());
            bierLab = tabLabel.allocate(tabLabelEntry.owner.pvrpBier, (bierMax + bierLen - 1) / bierLen);
            notif.wakeup();
            return false;
        }
        return true;
    }

    public void run() {
        for (;;) {
            notif.misleep(30000);
            if (!need2run) {
                return;
            }
            try {
                routerCreateComputed();
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
    }

    /**
     * get neighbor count
     *
     * @return count
     */
    public int routerNeighCount() {
        int o = 0;
        for (int i = 0; i < ifaces.size(); i++) {
            o += ifaces.get(i).neighs.size();
        }
        return o;
    }

    /**
     * list neighbors
     *
     * @param tab list
     */
    public void routerNeighList(tabRoute<addrIP> tab) {
        for (int o = 0; o < ifaces.size(); o++) {
            rtrPvrpIface ifc = ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
                continue;
            }
            for (int i = 0; i < ifc.neighs.size(); i++) {
                rtrPvrpNeigh nei = ifc.neighs.get(i);
                if (nei == null) {
                    continue;
                }
                tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
                ntry.prefix = new addrPrefix<addrIP>(nei.peer, addrIP.size * 8);
                tabRoute.addUpdatedEntry(tabRoute.addType.better, tab, rtrBgpUtil.sfiUnicast, 0, ntry, true, null, null, routerAutoMesh);
            }
        }
    }

    /**
     * get interface count
     *
     * @return count
     */
    public int routerIfaceCount() {
        return ifaces.size();
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

}
