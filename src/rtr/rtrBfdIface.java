package rtr;

import addr.addrIP;
import ip.ipFwdIface;
import pack.packBfd;
import pack.packHolder;
import prt.prtGenConn;
import prt.prtServP;
import prt.prtUdp;
import tab.tabGen;
import user.userFormat;
import util.debugger;
import util.logger;

/**
 * bidirectional forwarding detection protocol (rfc5880) interface
 *
 * @author matecsaba
 */
public class rtrBfdIface implements prtServP {

    /**
     * udp handler
     */
    protected prtUdp udp;

    /**
     * ip interface
     */
    protected ipFwdIface ifc;

    /**
     * interval between rx packets
     */
    public int intervalRx = 1000;

    /**
     * interval between tx packets
     */
    public int intervalTx = 1000;

    /**
     * multiplier
     */
    public int multiplier = 3;

    /**
     * list of neighbors
     */
    protected final tabGen<rtrBfdNeigh> neighs;

    /**
     * create one instance
     *
     * @param dgrm udp handler
     * @param iface the ip interface to work on
     */
    public rtrBfdIface(prtUdp dgrm, ipFwdIface iface) {
        udp = dgrm;
        ifc = iface;
        neighs = new tabGen<rtrBfdNeigh>();
    }

    public String toString() {
        return "bfd on " + ifc;
    }

    /**
     * unregister from udp
     */
    public void unregister2udp() {
        if (debugger.rtrBfdEvnt) {
            logger.debug("stopping on " + ifc);
        }
        udp.listenStop(ifc, packBfd.portLoc, null, 0, 0);
        for (int i = 0; i < neighs.size(); i++) {
            neighs.get(i).stopNow();
        }
    }

    /**
     * register to udp
     */
    public void register2udp() {
        if (debugger.rtrBfdEvnt) {
            logger.debug("starting on " + ifc);
        }
        udp.packetListen(this, ifc, packBfd.portLoc, null, 0, 0, "bfd", null, -1);
    }

    /**
     * list neighbors
     *
     * @param l list to append
     */
    public void getShNeighs(userFormat l) {
        for (int i = 0; i < neighs.size(); i++) {
            rtrBfdNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            l.add(ifc + "|" + nei.getShNeigh());
        }
    }

    /**
     * add one client
     *
     * @param adr address of peer
     * @param clnt client to notify
     * @param nam name of client
     * @return false on success, true on error
     */
    public boolean clientAdd(addrIP adr, rtrBfdClnt clnt, String nam) {
        if (clnt == null) {
            return true;
        }
        if (debugger.rtrBfdEvnt) {
            logger.debug("adding " + nam + " client to " + adr);
        }
        rtrBfdNeigh nei = new rtrBfdNeigh(this, adr);
        rtrBfdNeigh old = neighs.add(nei);
        if (old != null) {
            nei = old;
        } else if (nei.startNow()) {
            neighs.del(nei);
            return true;
        }
        return nei.clientAdd(clnt, nam);
    }

    /**
     * delete one client
     *
     * @param adr address of peer
     * @param clnt client to notify
     * @return false on success, true on error
     */
    public boolean clientDel(addrIP adr, rtrBfdClnt clnt) {
        if (debugger.rtrBfdEvnt) {
            logger.debug("removing client from " + adr);
        }
        rtrBfdNeigh nei = new rtrBfdNeigh(this, adr);
        nei = neighs.find(nei);
        if (nei == null) {
            return true;
        }
        boolean res = nei.clientDel(clnt);
        if (nei.hasClients()) {
            return res;
        }
        if (debugger.rtrBfdEvnt) {
            logger.debug("no more clients of " + nei);
        }
        nei.stopNow();
        neighs.del(nei);
        return res;
    }

    /**
     * find bfd neighbor
     *
     * @param adr address
     * @return bfd neighbor, null if not found
     */
    public rtrBfdNeigh clientFind(addrIP adr) {
        return neighs.find(new rtrBfdNeigh(this, adr));
    }

    /**
     * close interface
     *
     * @param ifc interface
     */
    public void closedInterface(ipFwdIface ifc) {
    }

    /**
     * start connection
     *
     * @param id connection
     * @return false if success, true if error
     */
    public boolean datagramAccept(prtGenConn id) {
        rtrBfdNeigh nei = neighs.find(new rtrBfdNeigh(this, id.peerAddr));
        if (nei == null) {
            return true;
        }
        if (nei.connRx != null) {
            nei.connRx.setClosing();
        }
        nei.connRx = id;
        return false;
    }

    /**
     * connection ready
     *
     * @param id connection
     */
    public void datagramReady(prtGenConn id) {
        rtrBfdNeigh nei = neighs.find(new rtrBfdNeigh(this, id.peerAddr));
        if (nei == null) {
            id.setClosing();
            return;
        }
    }

    /**
     * close connection
     *
     * @param id connection
     */
    public void datagramClosed(prtGenConn id) {
    }

    /**
     * work connection
     *
     * @param id connection
     */
    public void datagramWork(prtGenConn id) {
    }

    /**
     * received packet
     *
     * @param id connection
     * @param pck packet
     * @return false if success, true if error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        rtrBfdNeigh nei = neighs.find(new rtrBfdNeigh(this, id.peerAddr));
        if (nei == null) {
            id.setClosing();
            return false;
        }
        nei.recvPack(pck);
        return false;
    }

}
