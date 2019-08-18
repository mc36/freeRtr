package rtr;

import addr.addrIP;
import addr.addrEui;
import addr.addrPrefix;
import ip.ipCor4;
import ip.ipCor6;
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
import util.typLenVal;

/**
 * babel routing protocol (rfc6126) version 2
 *
 * @author matecsaba
 */
public class rtrBabel extends ipRtr implements prtServP {

    /**
     * port number
     */
    public final static int port = 6696;

    /**
     * version number
     */
    public final static int version = 2;

    /**
     * magic number
     */
    public final static int magic = 42;

    /**
     * header size
     */
    public final static int size = 4;

    /**
     * pad1 tlv
     */
    public final static int tlvPad1 = 0;

    /**
     * padN tlv
     */
    public final static int tlvPadN = 1;

    /**
     * ack request tlv
     */
    public final static int tlvAckReq = 2;

    /**
     * ack response tlv
     */
    public final static int tlvAckRep = 3;

    /**
     * hello tlv
     */
    public final static int tlvHello = 4;

    /**
     * ihu tlv
     */
    public final static int tlvIhu = 5;

    /**
     * router id tlv
     */
    public final static int tlvRtrId = 6;

    /**
     * next hop tlv
     */
    public final static int tlvNxtHop = 7;

    /**
     * update tlv
     */
    public final static int tlvUpdate = 8;

    /**
     * route req tlv
     */
    public final static int tlvRouReq = 9;

    /**
     * seqno req tlv
     */
    public final static int tlvSeqReq = 10;

    /**
     * the udp protocol
     */
    protected prtUdp udpCore;

    /**
     * forwarding core
     */
    public final ipFwd fwdCore;

    /**
     * router id
     */
    public addrEui routerID = new addrEui();

    /**
     * neighbors
     */
    protected tabGen<rtrBabelNeigh> neighs;

    /**
     * interfaces
     */
    protected tabGen<rtrBabelIface> ifaces;

    /**
     * sequence number
     */
    protected int seqno;

    /**
     * create one babel process
     *
     * @param forwarder the ip protocol
     * @param protocol the udp protocol
     * @param id process id
     */
    public rtrBabel(ipFwd forwarder, prtUdp protocol, int id) {
        if (debugger.rtrBabelEvnt) {
            logger.debug("startup");
        }
        fwdCore = forwarder;
        udpCore = protocol;
        tabRouteEntry.routeType rouTyp = null;
        switch (fwdCore.ipVersion) {
            case ipCor4.protocolVersion:
                rouTyp = tabRouteEntry.routeType.babel4;
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteEntry.routeType.babel6;
                break;
            default:
                break;
        }
        ifaces = new tabGen<rtrBabelIface>();
        neighs = new tabGen<rtrBabelNeigh>();
        seqno = bits.randomW();
        routerCreateComputed();
        fwdCore.routerAdd(this, rouTyp, id);
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
            rtrBabelNeigh nei = neighs.get(i);
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
     * update sequence number
     */
    protected void incSeq() {
        seqno = (seqno + 1) & 0xffff;
    }

    /**
     * get tlv handler
     *
     * @return handler
     */
    protected static typLenVal getTlv() {
        return new typLenVal(0, 8, 8, 8, 1, 0, 2, 1, 0, 512, true);
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "babel on " + fwdCore;
    }

    /**
     * add one interface to work on
     *
     * @param ifc ip forwarder interface
     * @return false if successful, true if error happened
     */
    public rtrBabelIface addInterface(ipFwdIface ifc) {
        if (debugger.rtrBabelEvnt) {
            logger.debug("add iface " + ifc);
        }
        if (ifc == null) {
            return null;
        }
        rtrBabelIface ntry = new rtrBabelIface(this, ifc);
        rtrBabelIface old = ifaces.add(ntry);
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
        rtrBabelIface ifc = new rtrBabelIface(this, iface);
        ifc = ifaces.del(ifc);
        if (ifc == null) {
            return;
        }
        ifc.unregister2udp();
        for (int i = neighs.size() - 1; i >= 0; i--) {
            rtrBabelNeigh nei = neighs.get(i);
            if (nei.iface.iface.ifwNum != iface.ifwNum) {
                continue;
            }
            neighs.del(nei);
            nei.unregister2udp();
        }
        routerCreateComputed();
    }

    /**
     * accept connection
     *
     * @param id connection
     * @return false if success, true if error
     */
    public boolean datagramAccept(prtGenConn id) {
        rtrBabelIface ifc = new rtrBabelIface(this, id.iface);
        ifc = ifaces.find(ifc);
        if (ifc == null) {
            logger.warn("no interface " + id);
            return true;
        }
        logger.warn("neighbor " + id.peerAddr + " up");
        rtrBabelNeigh ntry = new rtrBabelNeigh(id);
        rtrBabelNeigh old = neighs.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.iface = ifc;
        ntry.conn = id;
        if (ifc.bfdTrigger) {
            ifc.iface.bfdAdd(id.peerAddr, ntry, "babel");
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
     * close connection
     *
     * @param id connection
     */
    public void datagramClosed(prtGenConn id) {
        rtrBabelNeigh ntry = new rtrBabelNeigh(id);
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
        rtrBabelNeigh nei = new rtrBabelNeigh(id);
        nei = neighs.find(nei);
        if (nei != null) {
            if (nei.doWork()) {
                routerCreateComputed();
            }
            return;
        }
        rtrBabelIface ifc = new rtrBabelIface(this, id.iface);
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
     * @return false if success, true if error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        rtrBabelNeigh ntry = new rtrBabelNeigh(id);
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
     * create computed table
     */
    public synchronized void routerCreateComputed() {
        if (debugger.rtrBabelEvnt) {
            logger.debug("create table");
        }
        tabRoute<addrIP> tab = new tabRoute<addrIP>("babel");
        tabRouteEntry<addrIP> ntry;
        for (int i = 0; i < ifaces.size(); i++) {
            rtrBabelIface ifc = ifaces.get(i);
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
            rtrBabelNeigh nei = neighs.get(i);
            if (nei == null) {
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
        rtrBabelIface ntryi = new rtrBabelIface(null, null);
        for (int i = ifaces.size() - 1; i >= 0; i--) {
            ntryi = ifaces.get(i);
            if (ntryi == null) {
                continue;
            }
            ifaces.del(ntryi);
            ntryi.unregister2udp();
        }
        rtrBabelNeigh ntryn = new rtrBabelNeigh(null);
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
        l.add("1 2   router-id                   specify router id");
        l.add("2 .     <addr>                    router id");
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
                routerID = new addrEui();
            }
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
            rtrBabelNeigh ntry = neighs.get(i);
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
            rtrBabelIface ifc = ifaces.get(i);
            l.add(ifc.iface + "|" + countNeighs(ifc.iface.ifwNum));
        }
        return l;
    }

    private int countNeighs(int ifc) {
        int o = 0;
        for (int i = 0; i < neighs.size(); i++) {
            rtrBabelNeigh nei = neighs.get(i);
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
    public rtrBabelNeigh findPeer(addrIP addr) {
        for (int i = 0; i < neighs.size(); i++) {
            rtrBabelNeigh ntry = neighs.get(i);
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
