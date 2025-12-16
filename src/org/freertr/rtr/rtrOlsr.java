package org.freertr.rtr;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrPrefix;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipRtr;
import org.freertr.pack.packHolder;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServP;
import org.freertr.prt.prtUdp;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * optimized link state routing (rfc3626) protocol
 *
 * @author matecsaba
 */
public class rtrOlsr extends ipRtr implements prtServP {

    /**
     * port number
     */
    public final static int port = 698;

    /**
     * lq hello
     */
    public final static int typLqHello = 201;

    /**
     * lq tc
     */
    public final static int typLqTc = 202;

    /**
     * hello
     */
    public final static int typHello = 1;

    /**
     * tc
     */
    public final static int typTc = 2;

    /**
     * mid
     */
    public final static int typMid = 3;

    /**
     * hna
     */
    public final static int typHna = 4;

    /**
     * the udp protocol
     */
    protected prtUdp udpCore;

    /**
     * forwarding core
     */
    public final ipFwd fwdCore;

    /**
     * suppress interface addresses
     */
    public boolean suppressAddr;

    /**
     * neighbors
     */
    protected tabGen<rtrOlsrNeigh> neighs;

    /**
     * interfaces
     */
    protected tabGen<rtrOlsrIface> ifaces;

    /**
     * create one olsr process
     *
     * @param forwarder the ip protocol
     * @param protocol the udp protocol
     * @param id process id
     */
    public rtrOlsr(ipFwd forwarder, prtUdp protocol, int id) {
        if (debugger.rtrOlsrEvnt) {
            logger.debug("startup");
        }
        fwdCore = forwarder;
        udpCore = protocol;
        tabRouteAttr.routeType rouTyp = null;
        switch (fwdCore.ipVersion) {
            case ipCor4.protocolVersion:
                rouTyp = tabRouteAttr.routeType.olsr4;
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteAttr.routeType.olsr6;
                break;
            default:
                break;
        }
        ifaces = new tabGen<rtrOlsrIface>();
        neighs = new tabGen<rtrOlsrNeigh>();
        routerCreateComputed();
        fwdCore.routerAdd(this, rouTyp, id);
    }

    /**
     * mantissa to time
     *
     * @param i mantissa
     * @return time in ms
     */
    public static int mant2tim(int i) {
        int a = (i >> 4) & 0xf;
        int b = i & 0xf;
        if (b >= 8) {
            return ((16 + a) << (b - 8)) * 1000;
        } else {
            return ((16 + a) * 1000) >> (8 - b);
        }
    }

    /**
     * time to mantissa
     *
     * @param i time
     * @return mantissa
     */
    public static int tim2mant(int i) {
        int ui = (i * 2) / 125;
        int a;
        int b = 0;
        while (ui >= (1 << b)) {
            b++;
        }
        if (b == 0) {
            a = 1;
            b = 0;
        } else {
            b--;
            if (b > 15) {
                a = 15;
                b = 15;
            } else {
                if (b >= 5) {
                    a = (i - (125 << (b - 1))) / (125 << (b - 5));
                } else {
                    a = (i - (125 << (b - 1))) * (1 << (5 - b)) / 125;
                }
                b += a >> 4;
                a &= 0xf;
            }
        }
        return (a << 4) | (b & 0xf);
    }

    /**
     * type to string
     *
     * @param i type
     * @return string
     */
    public static String type2string(int i) {
        switch (i) {
            case typLqHello:
                return "lq-hello";
            case typLqTc:
                return "lq-tc";
            case typHello:
                return "hello";
            case typTc:
                return "tc";
            case typMid:
                return "mid";
            case typHna:
                return "hna";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * get neighbor count
     *
     * @return count
     */
    public int routerNeighCount() {
        return neighs.size();
    }

    /**
     * list neighbors
     *
     * @param tab list
     */
    public void routerNeighList(tabRoute<addrIP> tab) {
        for (int i = 0; i < neighs.size(); i++) {
            rtrOlsrNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            if (nei.iface.iface.lower.getState() != state.states.up) {
                continue;
            }
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = new addrPrefix<addrIP>(nei.conn.peerAddr, addrIP.size * 8);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, tab, rtrBgpUtil.sfiUnicast, 0, ntry, true, null, null, routerAutoMesh);
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

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "olsr on " + fwdCore;
    }

    /**
     * add one interface to work on
     *
     * @param ifc ip forwarder interface
     * @return false if successful, true if error happened
     */
    public rtrOlsrIface addInterface(ipFwdIface ifc) {
        if (debugger.rtrOlsrEvnt) {
            logger.debug("add iface " + ifc);
        }
        if (ifc == null) {
            return null;
        }
        rtrOlsrIface ntry = new rtrOlsrIface(this, ifc);
        rtrOlsrIface old = ifaces.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.register2udp();
        routerCreateComputed();
        return ntry;
    }

    /**
     * close interface
     *
     * @param iface interface
     */
    public void closedInterface(ipFwdIface iface) {
        rtrOlsrIface ifc = new rtrOlsrIface(this, iface);
        ifc = ifaces.del(ifc);
        if (ifc == null) {
            return;
        }
        ifc.unregister2udp();
        for (int i = neighs.size() - 1; i >= 0; i--) {
            rtrOlsrNeigh nei = neighs.get(i);
            if (nei.iface.iface.ifwNum != iface.ifwNum) {
                continue;
            }
            neighs.del(nei);
            nei.unregister2udp();
        }
        routerCreateComputed();
    }

    /**
     * start connection
     *
     * @param id connection
     * @return false if success, true if error
     */
    public boolean datagramAccept(prtGenConn id) {
        rtrOlsrIface ifc = new rtrOlsrIface(this, id.iface);
        ifc = ifaces.find(ifc);
        if (ifc == null) {
            logger.warn("no interface " + id);
            return true;
        }
        if ((ifc.connectedCheck) && (!ifc.iface.network.matches(id.peerAddr))) {
            logger.info("got from out of subnet peer " + id);
            return true;
        }
        logger.warn("neighbor " + id.peerAddr + " up");
        rtrOlsrNeigh ntry = new rtrOlsrNeigh(id, ifc);
        rtrOlsrNeigh old = neighs.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.conn = id;
        ntry.iface.neiSeq++;
        if (ifc.bfdTrigger) {
            ifc.iface.bfdAdd(id.peerAddr, ntry, "olsr");
        }
        return false;
    }

    /**
     * connection ready
     *
     * @param id connection
     */
    public void datagramReady(prtGenConn id) {
    }

    /**
     * stop connection
     *
     * @param id connection
     */
    public void datagramClosed(prtGenConn id) {
        rtrOlsrNeigh ntry = new rtrOlsrNeigh(id, null);
        ntry = neighs.del(ntry);
        if (ntry == null) {
            return;
        }
        ntry.iface.neiSeq++;
        logger.error("neighbor " + id.peerAddr + " down");
        id.iface.bfdDel(id.peerAddr, ntry);
        routerCreateComputed();
    }

    /**
     * work connection
     *
     * @param id connection
     */
    public void datagramWork(prtGenConn id) {
        rtrOlsrNeigh nei = new rtrOlsrNeigh(id, null);
        nei = neighs.find(nei);
        if (nei != null) {
            if (nei.doWork()) {
                routerCreateComputed();
            }
            return;
        }
        rtrOlsrIface ifc = new rtrOlsrIface(this, id.iface);
        ifc = ifaces.find(ifc);
        if (ifc != null) {
            ifc.doWork();
            return;
        }
        id.setClosing();
    }

    /**
     * received error
     *
     * @param id connection
     * @param pck packet
     * @param rtr reporting router
     * @param err error happened
     * @param lab error label
     * @return false on success, true on error
     */
    public boolean datagramError(prtGenConn id, packHolder pck, addrIP rtr, counter.reasons err, int lab) {
        return false;
    }

    /**
     * notified that state changed
     *
     * @param id id number to reference connection
     * @param stat state
     * @return return false if successful, true if error happened
     */
    public boolean datagramState(prtGenConn id, state.states stat) {
        rtrOlsrNeigh ntry = new rtrOlsrNeigh(id, null);
        ntry = neighs.find(ntry);
        if (ntry == null) {
            return false;
        }
        ntry.bfdPeerDown();
        return false;
    }

    /**
     * received packet
     *
     * @param id connection
     * @param pck packet
     * @return false if success, true if error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        rtrOlsrNeigh ntry = new rtrOlsrNeigh(id, null);
        ntry = neighs.find(ntry);
        if (ntry == null) {
            id.setClosing();
            return false;
        }
        if (ntry.gotPack(pck)) {
            return false;
        }
        routerCreateComputed();
        return false;
    }

    /**
     * create computed
     */
    public synchronized void routerCreateComputed() {
        if (debugger.rtrOlsrEvnt) {
            logger.debug("create table");
        }
        tabRoute<addrIP> tab = new tabRoute<addrIP>("olsr");
        tabRouteEntry<addrIP> ntry;
        for (int i = 0; i < ifaces.size(); i++) {
            rtrOlsrIface ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
                continue;
            }
            if ((suppressAddr || ifc.suppressAddr) && (!ifc.unsuppressAddr)) {
                continue;
            }
            ntry = tab.add(tabRoute.addType.better, ifc.iface.network, null);
            ntry.best.rouTyp = tabRouteAttr.routeType.conn;
            ntry.best.iface = ifc.iface;
            ntry.best.distance = tabRouteAttr.distanIfc;
        }
        for (int i = 0; i < neighs.size(); i++) {
            rtrOlsrNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            if (nei.iface.iface.lower.getState() != state.states.up) {
                continue;
            }
            tab.mergeFrom(tabRoute.addType.ecmp, nei.learned, tabRouteAttr.distanLim);
        }
        routerDoAggregates(rtrBgpUtil.sfiUnicast, tab, tab, fwdCore.commonLabel, null, 0);
        tab.setProto(routerProtoTyp, routerProcNum);
        if (tab.preserveTime(routerComputedU)) {
            return;
        }
        routerComputedU = tab;
        routerComputedM = tab;
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
    }

    /**
     * stop work
     */
    public void routerCloseNow() {
        for (int i = ifaces.size() - 1; i >= 0; i--) {
            rtrOlsrIface ntry = ifaces.get(i);
            if (ntry == null) {
                continue;
            }
            ifaces.del(ntry);
            ntry.unregister2udp();
        }
        for (int i = neighs.size() - 1; i >= 0; i--) {
            rtrOlsrNeigh ntry = neighs.get(i);
            if (ntry == null) {
                continue;
            }
            neighs.del(ntry);
            ntry.unregister2udp();
        }
        fwdCore.routerDel(this);
    }

    /**
     * get help
     *
     * @param l list
     */
    public void routerGetHelp(userHelp l) {
        l.add(null, false, 1, new int[]{-1}, "suppress-prefix", "do not advertise interfaces");
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void routerGetConfig(List<String> l, String beg, int filter) {
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
        if (s.equals(cmds.negated)) {
            s = cmd.word();
            negated = true;
        }
        if (s.equals("suppress-prefix")) {
            suppressAddr = !negated;
            return false;
        }
        return true;
    }

    /**
     * list neighbors
     *
     * @return list of neighbors
     */
    public userFormat showNeighs() {
        userFormat l = new userFormat("|", "interface|learn|neighbor|uptime");
        for (int i = 0; i < neighs.size(); i++) {
            rtrOlsrNeigh ntry = neighs.get(i);
            if (ntry == null) {
                continue;
            }
            l.add(ntry.iface.iface + "|" + ntry.learned.size() + "|" + ntry.conn.peerAddr + "|" + bits.timePast(ntry.upTime));
        }
        return l;
    }

    /**
     * list interfaces
     *
     * @return list of interfaces
     */
    public userFormat showIfaces() {
        userFormat l = new userFormat("|", "interface|neighbors");
        for (int i = 0; i < ifaces.size(); i++) {
            rtrOlsrIface ifc = ifaces.get(i);
            l.add(ifc.iface + "|" + countNeighs(ifc.iface.ifwNum));
        }
        return l;
    }

    private int countNeighs(int ifc) {
        int o = 0;
        for (int i = 0; i < neighs.size(); i++) {
            rtrOlsrNeigh nei = neighs.get(i);
            if (nei.iface.iface.ifwNum == ifc) {
                o++;
            }
        }
        return o;
    }

    /**
     * find peer
     *
     * @param addr address to find
     * @return neighbor, null if not found
     */
    public rtrOlsrNeigh findPeer(addrIP addr) {
        for (int i = 0; i < neighs.size(); i++) {
            rtrOlsrNeigh ntry = neighs.get(i);
            if (ntry == null) {
                continue;
            }
            if (addr.compareTo(ntry.conn.peerAddr) != 0) {
                continue;
            }
            return ntry;
        }
        return null;
    }

}
