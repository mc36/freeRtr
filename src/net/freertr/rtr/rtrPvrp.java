package net.freertr.rtr;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrPrefix;
import net.freertr.ip.ipCor4;
import net.freertr.ip.ipCor6;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipRtr;
import net.freertr.prt.prtTcp;
import net.freertr.prt.prtUdp;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabIndex;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.notifier;
import net.freertr.util.state;

/**
 * path vector routing protocol
 *
 * @author matecsaba
 */
public class rtrPvrp extends ipRtr implements Runnable {

    /**
     * port number
     */
    public static final int port = 1547;

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
     * suppress interface addresses
     */
    public boolean suppressAddr = false;

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
        routerComputedU = tab2;
        routerComputedM = tab2;
        routerComputedF = new tabRoute<addrIP>("rx");
        routerComputedI = new tabGen<tabIndex<addrIP>>();
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
    }

    /**
     * get help
     *
     * @param l list
     */
    public void routerGetHelp(userHelping l) {
        l.add(null, "1 2   router-id                   specify router id");
        l.add(null, "2 .     <addr>                    router id");
        l.add(null, "1 .   labels                      specify label mode");
        l.add(null, "1 .   stub                        stub router");
        l.add(null, "1 .   suppress-prefix             do not advertise interfaces");
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
        if (s.equals("no")) {
            s = cmd.word();
            negated = true;
        }
        if (s.equals("router-id")) {
            routerID.fromString(cmd.word());
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

}
