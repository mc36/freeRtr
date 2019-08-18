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
import tab.tabGen;
import tab.tabRoute;
import tab.tabRouteEntry;
import user.userFormat;
import user.userHelping;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;
import util.notifier;
import util.state;
import util.typLenVal;

/**
 * enhanced interior gateway routing (rfc7868) protocol
 *
 * @author matecsaba
 */
public class rtrEigrp extends ipRtr implements Runnable {

    /**
     * eigrp protocol number
     */
    public static final int protoNum = 88;

    /**
     * protocol version number
     */
    public final static int verNum = 2;

    /**
     * eigrp header size
     */
    public static final int sizeHead = 20;

    /**
     * forwarding core
     */
    public final ipFwd fwdCore;

    /**
     * router id
     */
    public addrIPv4 routerID;

    /**
     * routes needed to advertise
     */
    public tabRoute<addrIP> need2adv;

    /**
     * stub value
     */
    public int stub;

    /**
     * k1
     */
    public int k1;

    /**
     * k2
     */
    public int k2;

    /**
     * k3
     */
    public int k3;

    /**
     * k4
     */
    public int k4;

    /**
     * k5
     */
    public int k5;

    /**
     * as
     */
    public int as;

    /**
     * list of interfaces
     */
    protected tabGen<rtrEigrpIface> ifaces;

    /**
     * notified on route change
     */
    protected notifier notif = new notifier();

    private boolean need2run = true;

    /**
     * create one eigrp process
     *
     * @param forwarder the ip protocol
     * @param id process id
     */
    public rtrEigrp(ipFwd forwarder, int id) {
        fwdCore = forwarder;
        routerID = new addrIPv4();
        ifaces = new tabGen<rtrEigrpIface>();
        tabRouteEntry.routeType rouTyp = null;
        switch (fwdCore.ipVersion) {
            case ipCor4.protocolVersion:
                rouTyp = tabRouteEntry.routeType.eigrp4;
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteEntry.routeType.eigrp6;
                break;
            default:
                break;
        }
        k1 = 1;
        k3 = 1;
        routerCreateComputed();
        fwdCore.routerAdd(this, rouTyp, id);
        new Thread(this).start();
    }

    /**
     * get tlv handler
     *
     * @return handler
     */
    protected static typLenVal getTlv() {
        return new typLenVal(0, 16, 16, 16, 1, 4, 4, 1, 0, 1024, true);
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "eigrp on " + fwdCore;
    }

    /**
     * add one interface to work on
     *
     * @param ifc ip forwarder interface
     * @return false if successful, true if error happened
     */
    public rtrEigrpIface addInterface(ipFwdIface ifc) {
        if (debugger.rtrEigrpEvnt) {
            logger.debug("add iface " + ifc);
        }
        if (ifc == null) {
            return null;
        }
        rtrEigrpIface ntry = new rtrEigrpIface(this, ifc);
        rtrEigrpIface old = ifaces.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.register2ip();
        ntry.restartTimer(false);
        routerCreateComputed();
        return ntry;
    }

    /**
     * delete one interface
     *
     * @param ifc interface to delete
     */
    public void delInterface(ipFwdIface ifc) {
        if (debugger.rtrEigrpEvnt) {
            logger.debug("del iface " + ifc);
        }
        if (ifc == null) {
            return;
        }
        rtrEigrpIface ntry = new rtrEigrpIface(this, ifc);
        ntry = ifaces.del(ntry);
        if (ntry == null) {
            return;
        }
        ntry.restartTimer(true);
        ntry.unregister2ip();
        ntry.closeNeighbors();
        routerCreateComputed();
    }

    /**
     * list of neighbors
     *
     * @return list
     */
    public userFormat showNeighs() {
        userFormat res = new userFormat("|", "iface|peer|learned|adverted|uptime");
        for (int i = 0; i < ifaces.size(); i++) {
            rtrEigrpIface ifc = ifaces.get(i);
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
            rtrEigrpIface ifc = ifaces.get(i);
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
    public rtrEigrpNeigh findNeigh(addrIP adr) {
        for (int i = 0; i < ifaces.size(); i++) {
            rtrEigrpIface ifc = ifaces.get(i);
            rtrEigrpNeigh r = ifc.findNeigh(adr);
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
        if (debugger.rtrEigrpEvnt) {
            logger.debug("create table");
        }
        tabRoute<addrIP> tab1 = new tabRoute<addrIP>("lernd");
        tabRouteEntry<addrIP> ntry;
        for (int i = 0; i < ifaces.size(); i++) {
            rtrEigrpIface ifc = ifaces.get(i);
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
            rtrEigrpIface ifc = ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            for (int i = 0; i < ifc.neighs.size(); i++) {
                rtrEigrpNeigh nei = ifc.neighs.get(i);
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
        need2adv = tab1;
        for (int o = 0; o < ifaces.size(); o++) {
            rtrEigrpIface ifc = ifaces.get(o);
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
                rtrEigrpNeigh nei = ifc.neighs.get(i);
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
        if (debugger.rtrEigrpEvnt) {
            logger.debug("shutdown");
        }
        need2run = false;
        for (int i = 0; i < ifaces.size(); i++) {
            rtrEigrpIface ifc = ifaces.get(i);
            ifc.restartTimer(true);
            ifc.unregister2ip();
            ifc.closeNeighbors();
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
        l.add("1 2   kvals                       specify k values");
        l.add("2 3     <num>                     k1");
        l.add("3 4       <num>                   k2");
        l.add("4 5         <num>                 k3");
        l.add("5 6           <num>               k4");
        l.add("6 .             <num>             k5");
        l.add("1 2   stub                        specify stub");
        l.add("2 2,.   conn                      connected");
        l.add("2 2,.   stat                      static");
        l.add("2 2,.   sum                       summary");
        l.add("2 2,.   red                       redistribute");
        l.add("2 2,.   leak                      leak map");
        l.add("2 2,.   rx                        receive only");
        l.add("1 2   as                          specify as number");
        l.add("2 .     <num>                     as");
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
        l.add(beg + "as " + as);
        String a = "";
        if ((stub & 0x01) != 0) {
            a += " conn";
        }
        if ((stub & 0x02) != 0) {
            a += " stat";
        }
        if ((stub & 0x04) != 0) {
            a += " sum";
        }
        if ((stub & 0x08) != 0) {
            a += " red";
        }
        if ((stub & 0x10) != 0) {
            a += " leak";
        }
        if ((stub & 0x20) != 0) {
            a += " rx";
        }
        l.add(beg + "stub" + a);
        l.add(beg + "kvals " + k1 + " " + k2 + " " + k3 + " " + k4 + " " + k5);
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
        if (s.equals("kvals")) {
            k1 = bits.str2num(cmd.word());
            k2 = bits.str2num(cmd.word());
            k3 = bits.str2num(cmd.word());
            k4 = bits.str2num(cmd.word());
            k5 = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("stub")) {
            stub = 0;
            if (negated) {
                return false;
            }
            for (;;) {
                s = cmd.word();
                if (s.length() < 1) {
                    break;
                }
                if (s.equals("conn")) {
                    stub |= 0x01;
                }
                if (s.equals("stat")) {
                    stub |= 0x02;
                }
                if (s.equals("sum")) {
                    stub |= 0x04;
                }
                if (s.equals("red")) {
                    stub |= 0x08;
                }
                if (s.equals("leak")) {
                    stub |= 0x10;
                }
                if (s.equals("rx")) {
                    stub |= 0x20;
                }
            }
            return false;
        }
        if (s.equals("as")) {
            as = bits.str2num(cmd.word());
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
            rtrEigrpIface ifc = ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            for (int i = 0; i < ifc.neighs.size(); i++) {
                rtrEigrpNeigh nei = ifc.neighs.get(i);
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
