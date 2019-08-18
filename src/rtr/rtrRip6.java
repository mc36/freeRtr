package rtr;

import addr.addrIP;
import addr.addrPrefix;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipRtr;
import java.util.List;
import pack.packHolder;
import prt.prtGenConn;
import prt.prtServP;
import prt.prtUdp;
import tab.tabGen;
import tab.tabRoute;
import tab.tabRouteEntry;
import user.userFormat;
import user.userHelping;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;
import util.state;

/**
 * routing information protocol (rfc2080) version 6
 *
 * @author matecsaba
 */
public class rtrRip6 extends ipRtr implements prtServP {

    /**
     * port number
     */
    public final static int port = 521;

    /**
     * version number
     */
    public final static int version = 1;

    /**
     * size of header
     */
    public final static int sizeHead = 4;

    /**
     * size of routing entry
     */
    public final static int sizeNtry = 20;

    /**
     * minimum metric
     */
    public final static int metricMin = 1;

    /**
     * maximum metric this is the inaccessibly one
     */
    public final static int metricMax = 16;

    /**
     * entries per update
     */
    public final static int entryPerUpdate = 23;

    /**
     * the udp protocol
     */
    protected prtUdp udpCore;

    /**
     * forwarding core
     */
    public ipFwd fwdCore;

    private tabGen<rtrRip6iface> ifaces;

    private tabGen<rtrRip6neigh> neighs;

    /**
     * create one rip process
     *
     * @param forwarder the ip protocol
     * @param protocol the udp protocol
     * @param id process id
     */
    public rtrRip6(ipFwd forwarder, prtUdp protocol, int id) {
        if (debugger.rtrRip6evnt) {
            logger.debug("startup");
        }
        fwdCore = forwarder;
        udpCore = protocol;
        ifaces = new tabGen<rtrRip6iface>();
        neighs = new tabGen<rtrRip6neigh>();
        routerCreateComputed();
        fwdCore.routerAdd(this, tabRouteEntry.routeType.rip6, id);
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
            rtrRip6neigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = new addrPrefix<addrIP>(nei.conn.peerAddr, addrIP.size * 8);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, tab, rtrBgpUtil.safiUnicast, ntry, null, null, routerAutoMesh);
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
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "rip on " + fwdCore;
    }

    /**
     * add one interface to work on
     *
     * @param ifc ip forwarder interface
     * @return false if successful, true if error happened
     */
    public rtrRip6iface addInterface(ipFwdIface ifc) {
        if (debugger.rtrRip6evnt) {
            logger.debug("add iface " + ifc);
        }
        if (ifc == null) {
            return null;
        }
        rtrRip6iface ntry = new rtrRip6iface(this, ifc);
        rtrRip6iface old = ifaces.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.register2udp();
        routerCreateComputed();
        return ntry;
    }

    /**
     * closed interface
     *
     * @param iface interface
     */
    public void closedInterface(ipFwdIface iface) {
        rtrRip6iface ifc = new rtrRip6iface(this, iface);
        ifc = ifaces.del(ifc);
        if (ifc == null) {
            return;
        }
        ifc.unregister2udp();
        for (int i = neighs.size() - 1; i >= 0; i--) {
            rtrRip6neigh nei = neighs.get(i);
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
     * @return false on success, true on error
     */
    public boolean datagramAccept(prtGenConn id) {
        rtrRip6iface ifc = new rtrRip6iface(this, id.iface);
        ifc = ifaces.find(ifc);
        if (ifc == null) {
            logger.warn("no interface " + id);
            return true;
        }
        if ((ifc.connectedCheck) && (!ifc.iface.lower.checkConnected(id.peerAddr))) {
            logger.warn("got from far " + id);
            return true;
        }
        logger.warn("neighbor " + id.peerAddr + " up");
        rtrRip6neigh ntry = new rtrRip6neigh(id);
        rtrRip6neigh old = neighs.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.iface = ifc;
        ntry.conn = id;
        if (ifc.bfdTrigger) {
            ifc.iface.bfdAdd(id.peerAddr, ntry, "rip");
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
        rtrRip6neigh ntry = new rtrRip6neigh(id);
        ntry = neighs.del(ntry);
        if (ntry == null) {
            return;
        }
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
        rtrRip6neigh nei = new rtrRip6neigh(id);
        nei = neighs.find(nei);
        if (nei != null) {
            if (nei.doWork()) {
                routerCreateComputed();
            }
            return;
        }
        rtrRip6iface ifc = new rtrRip6iface(this, id.iface);
        ifc = ifaces.find(ifc);
        if (ifc != null) {
            ifc.doWork();
            return;
        }
        id.setClosing();
    }

    /**
     * received packet
     *
     * @param id connection
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        rtrRip6neigh ntry = new rtrRip6neigh(id);
        ntry = neighs.find(ntry);
        if (ntry == null) {
            id.setClosing();
            return false;
        }
        if (ntry.gotPack(pck)) {
            routerCreateComputed();
        }
        return false;
    }

    /**
     * create computed
     */
    public synchronized void routerCreateComputed() {
        if (debugger.rtrRip6evnt) {
            logger.debug("create table");
        }
        tabRoute<addrIP> tab = new tabRoute<addrIP>("rip");
        tabRouteEntry<addrIP> ntry;
        for (int i = 0; i < ifaces.size(); i++) {
            rtrRip6iface ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
                continue;
            }
            if (ifc.suppressAddr) {
                continue;
            }
            ntry = tab.add(tabRoute.addType.better, ifc.iface.network, null);
            ntry.rouTyp = tabRouteEntry.routeType.conn;
            ntry.iface = ifc.iface;
            ntry.distance = tabRouteEntry.distanIfc;
        }
        for (int i = 0; i < neighs.size(); i++) {
            rtrRip6neigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            if (!nei.iface.allowRx) {
                continue;
            }
            tab.mergeFrom(tabRoute.addType.better, nei.learned, null, true, tabRouteEntry.distanLim);
        }
        routerDoAggregates(rtrBgpUtil.safiUnicast, tab, null, fwdCore.commonLabel, 0, null, 0);
        routerComputedU = tab;
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
        rtrRip6iface ntryi = new rtrRip6iface(null, null);
        for (int i = ifaces.size() - 1; i >= 0; i--) {
            ntryi = ifaces.get(i);
            if (ntryi == null) {
                continue;
            }
            ifaces.del(ntryi);
            ntryi.unregister2udp();
        }
        rtrRip6neigh ntryn = new rtrRip6neigh(null);
        for (int i = neighs.size() - 1; i >= 0; i--) {
            ntryn = neighs.get(i);
            if (ntryn == null) {
                continue;
            }
            neighs.del(ntryn);
            ntryn.unregister2udp();
        }
        fwdCore.routerDel(this);
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
     * @return false on success, true on error
     */
    public boolean routerConfigure(cmds cmd) {
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
            rtrRip6neigh ntry = neighs.get(i);
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
            rtrRip6iface ifc = ifaces.get(i);
            l.add(ifc.iface + "|" + countNeighs(ifc.iface.ifwNum));
        }
        return l;
    }

    private int countNeighs(int ifc) {
        int o = 0;
        for (int i = 0; i < neighs.size(); i++) {
            rtrRip6neigh nei = neighs.get(i);
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
    public rtrRip6neigh findPeer(addrIP addr) {
        for (int i = 0; i < neighs.size(); i++) {
            rtrRip6neigh ntry = neighs.get(i);
            if (ntry == null) {
                continue;
            }
            if (addr.compare(addr, ntry.conn.peerAddr) != 0) {
                continue;
            }
            return ntry;
        }
        return null;
    }

}
