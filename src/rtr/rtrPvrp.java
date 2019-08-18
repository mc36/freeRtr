package rtr;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrPrefix;
import ip.ipCor4;
import ip.ipCor6;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipRtr;
import java.util.List;
import prt.prtTcp;
import prt.prtUdp;
import tab.tabGen;
import tab.tabRoute;
import tab.tabRouteEntry;
import user.userFormat;
import user.userHelping;
import util.cmds;
import util.debugger;
import util.logger;
import util.notifier;
import util.state;

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
    public boolean labels;

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
        tabRouteEntry.routeType rouTyp = null;
        switch (fwdCore.ipVersion) {
            case ipCor4.protocolVersion:
                rouTyp = tabRouteEntry.routeType.pvrp4;
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteEntry.routeType.pvrp6;
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
        ntry.routerCloseNow();
        routerCreateComputed();
    }

    /**
     * list of neighbors
     *
     * @return list
     */
    public userFormat showNeighs() {
        userFormat res = new userFormat("|", "iface|router|name|peer|learned|adverted|uptime");
        for (int i = 0; i < ifaces.size(); i++) {
            rtrPvrpIface ifc = ifaces.get(i);
            ifc.showNeighs(res);
        }
        return res;
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
            l.add(ifc.iface + "|" + ifc.neighs.size());
        }
        return l;
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
            rtrPvrpNeigh r = ifc.findNeigh(adr);
            if (r != null) {
                return r;
            }
        }
        return null;
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
            if (ifc.suppressAddr) {
                continue;
            }
            ntry = tab1.add(tabRoute.addType.better, ifc.iface.network, null);
            ntry.rouTyp = tabRouteEntry.routeType.conn;
            ntry.iface = ifc.iface;
            ntry.distance = tabRouteEntry.distanIfc;
        }
        for (int o = 0; o < ifaces.size(); o++) {
            rtrPvrpIface ifc = ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            for (int i = 0; i < ifc.neighs.size(); i++) {
                rtrPvrpNeigh nei = ifc.neighs.get(i);
                if (nei == null) {
                    continue;
                }
                tab1.mergeFrom(tabRoute.addType.better, nei.learned, null, true, tabRouteEntry.distanLim);
            }
        }
        routerDoAggregates(rtrBgpUtil.safiUnicast, tab1, null, fwdCore.commonLabel, 0, null, 0);
        tabRoute<addrIP> tab2 = tab1;
        tab1 = new tabRoute<addrIP>("ned2adv");
        tab1.mergeFrom(tabRoute.addType.better, tab2, null, true, tabRouteEntry.distanLim);
        tab1.mergeFrom(tabRoute.addType.better, routerRedistedU, null, true, tabRouteEntry.distanLim);
        if (labels) {
            for (int i = 0; i < tab1.size(); i++) {
                ntry = tab1.get(i);
                tabRouteEntry<addrIP> org = fwdCore.labeldR.find(ntry);
                if (org == null) {
                    continue;
                }
                ntry.labelLoc = org.labelLoc;
            }
        }
        need2adv = tab1;
        for (int o = 0; o < ifaces.size(); o++) {
            rtrPvrpIface ifc = ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            tab1 = new tabRoute<addrIP>("ned2adv");
            if (ifc.defOrigin) {
                ntry = new tabRouteEntry<addrIP>();
                ntry.prefix = addrPrefix.defaultRoute(getProtoVer());
                tab1.add(tabRoute.addType.always, ntry, true, true);
            }
            tabRoute.addUpdatedTable(tabRoute.addType.always, rtrBgpUtil.safiUnicast, tab1, need2adv, ifc.roumapOut, ifc.roupolOut, ifc.prflstOut);
            if (ifc.splitHorizon) {
                tab1.delIface(ifc.iface);
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
        routerComputedU = tab2;
        fwdCore.routerChg(this);
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
            ifc.routerCloseNow();
        }
    }

    /**
     * get help
     *
     * @param l list
     */
    public void routerGetHelp(userHelping l) {
        l.add("1 2   router-id                   specify router id");
        l.add("2 .     <addr>                    router id");
        l.add("1 .   labels                      specify label mode");
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void routerGetConfig(List<String> l, String beg, boolean filter) {
        l.add(beg + "router-id " + routerID);
        cmds.cfgLine(l, !labels, beg, "labels", "");
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
        return true;
    }

    public void run() {
        for (;;) {
            notif.misleep(10000);
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
            for (int i = 0; i < ifc.neighs.size(); i++) {
                rtrPvrpNeigh nei = ifc.neighs.get(i);
                if (nei == null) {
                    continue;
                }
                tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
                ntry.prefix = new addrPrefix<addrIP>(nei.peer, addrIP.size * 8);
                tabRoute.addUpdatedEntry(tabRoute.addType.better, tab, rtrBgpUtil.safiUnicast, ntry, null, null, routerAutoMesh);
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

}
