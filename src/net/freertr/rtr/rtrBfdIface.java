package net.freertr.rtr;

import net.freertr.addr.addrIP;
import net.freertr.ip.ipFwdIface;
import net.freertr.pack.packBfd;
import net.freertr.pack.packHolder;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServP;
import net.freertr.prt.prtUdp;
import net.freertr.tab.tabGen;
import net.freertr.user.userFormat;
import net.freertr.util.bits;
import net.freertr.util.counter;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.state;

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
     * key id
     */
    public int keyId;

    /**
     * password
     */
    public String password;

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
        udp.listenStop(ifc, packBfd.portLoc, null, 0);
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
        udp.packetListen(this, ifc, packBfd.portLoc, null, 0, "bfd", null, -1);
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
     * @param tim maximum time to wait
     * @return false on success, true on error
     */
    public boolean clientWait(addrIP adr, int tim) {
        if (debugger.rtrBfdEvnt) {
            logger.debug("waiting for client " + adr);
        }
        long beg = bits.getTime();
        for (;;) {
            bits.sleep(intervalTx);
            if ((bits.getTime() - beg) > tim) {
                return true;
            }
            rtrBfdNeigh nei = new rtrBfdNeigh(this, adr);
            nei = neighs.find(nei);
            if (nei == null) {
                return true;
            }
            if (nei.getState()) {
                return false;
            }
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
        if (stat == state.states.up) {
            return false;
        }
        rtrBfdNeigh nei = neighs.find(new rtrBfdNeigh(this, id.peerAddr));
        if (nei == null) {
            return false;
        }
        nei.stopNow();
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
        rtrBfdNeigh nei = neighs.find(new rtrBfdNeigh(this, id.peerAddr));
        if (nei == null) {
            id.setClosing();
            return false;
        }
        nei.recvPack(pck);
        return false;
    }

}
