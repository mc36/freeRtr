package org.freertr.ip;

import org.freertr.addr.addrIP;
import org.freertr.pack.packHolder;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServP;
import org.freertr.prt.prtTcp;
import org.freertr.prt.prtUdp;
import org.freertr.tab.tabGen;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * dynamic link exchange (rfc8175) protocol
 *
 * @author matecsaba
 */
public class ipDlepIface implements prtServP, Runnable {

    /**
     * port number
     */
    public final static int portNum = 854;

    /**
     * discovery
     */
    public final static int sgnlDisc = 1;

    /**
     * offer
     */
    public final static int sgnlOffr = 2;

    /**
     * packet magic
     */
    public final static int packMagic = 0x444c4550;

    /**
     * counter
     */
    public counter cntr = new counter();

    /**
     * forwarder to use
     */
    public final ipFwd fwdCore;

    /**
     * udp to use
     */
    public final prtUdp udpCore;

    /**
     * tcp to use
     */
    public final prtTcp tcpCore;

    /**
     * interface to use
     */
    public final ipFwdIface ifcFwd;

    /**
     * interface to use
     */
    public final ipIfc ifcIp;

    /**
     * client or server mode
     */
    public final boolean client;

    /**
     * heartbeat interval
     */
    public int heartBeat = 5000;

    private prtGenConn conn;

    /**
     * list of neighbors
     */
    protected final tabGen<ipDlepNeigh> neighs = new tabGen<ipDlepNeigh>();

    /**
     * create one interface handler
     *
     * @param fwdr forwarder to use
     * @param udpr udp to use
     * @param tcpr tcp to use
     * @param ifi interface to use
     * @param ipi interface to use
     * @param clnt true for client or false for server
     */
    public ipDlepIface(ipFwd fwdr, prtUdp udpr, prtTcp tcpr, ipFwdIface ifi, ipIfc ipi, boolean clnt) {
        fwdCore = fwdr;
        udpCore = udpr;
        tcpCore = tcpr;
        ifcFwd = ifi;
        ifcIp = ipi;
        client = clnt;
        udpCore.packetListen(this, ifi, portNum, null, 0, "dlep", -1, null, -1, -1);
        if (clnt) {
            return;
        }
        addrIP adr = new addrIP();
        if (ifi.addr.isIPv4()) {
            adr.fromString("224.0.0.117");
        } else {
            adr.fromString("ff02::1:7");
        }
        conn = udpCore.packetConnect(this, ifi, portNum, adr, portNum, "dlep", -1, null, -1, -1);
        if (conn == null) {
            return;
        }
        conn.timeout = 0;
        new Thread(this).start();
    }

    public String toString() {
        return client ? "client" : "server";
    }

    /**
     * stop working
     */
    public void stopWork() {
        udpCore.listenStop(ifcFwd, portNum, null, 0);
        if (conn == null) {
            return;
        }
        conn.setClosing();
        conn = null;
    }

    public void closedInterface(ipFwdIface ifc) {
    }

    public boolean datagramAccept(prtGenConn id) {
        id.timeout = 30000;
        return false;
    }

    public void datagramReady(prtGenConn id) {
    }

    public void datagramClosed(prtGenConn id) {
    }

    public void datagramWork(prtGenConn id) {
    }

    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        cntr.rx(pck);
        if (pck.dataSize() < 8) {
            cntr.drop(pck, counter.reasons.badSiz);
            return true;
        }
        if (pck.msbGetD(0) != packMagic) {
            cntr.drop(pck, counter.reasons.badVer);
            return true;
        }
        int i = pck.msbGetW(4); // type
        if (client) {
            i -= sgnlDisc;
        } else {
            i -= sgnlOffr;
        }
        if (i != 0) {
            cntr.drop(pck, counter.reasons.badTyp);
            return true;
        }
        pck.clear();
        pck.msbPutD(0, packMagic);
        pck.msbPutW(4, sgnlOffr); // type
        pck.msbPutW(6, 0); // size
        pck.putSkip(8);
        id.send2net(pck);
        id.setClosing();
        ipDlepNeigh ntry = new ipDlepNeigh(this, id.peerAddr, id.portRem);
        if (neighs.find(ntry) != null) {
            return true;
        }
        neighs.put(ntry);
        new Thread(ntry).start();
        return true;
    }

    public boolean datagramError(prtGenConn id, packHolder pck, addrIP rtr, counter.reasons err, int lab) {
        return false;
    }

    public boolean datagramState(prtGenConn id, state.states stat) {
        return false;
    }

    private boolean doRound() {
        if (conn == null) {
            return true;
        }
        packHolder pck = new packHolder(true, true);
        pck.msbPutD(0, packMagic);
        pck.msbPutW(4, sgnlDisc); // type
        pck.msbPutW(6, 0); // size
        pck.putSkip(8);
        conn.send2net(pck);
        return false;
    }

    public void run() {
        try {
            for (;;) {
                bits.sleep(heartBeat);
                if (doRound()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    /**
     * get show
     *
     * @return show
     */
    public userFormat getShNeigh() {
        userFormat l = new userFormat("|", "peer|clients");
        for (int i = 0; i < neighs.size(); i++) {
            ipDlepNeigh ntry = neighs.get(i);
            l.add(ntry.peer + "|" + ntry.found.size());
        }
        return l;
    }

    /**
     * get show
     *
     * @return show
     */
    public userFormat getShClnts() {
        userFormat l = new userFormat("|", "peer|client");
        for (int i = 0; i < neighs.size(); i++) {
            ipDlepNeigh ntry = neighs.get(i);
            ntry.getShClnts(l);
        }
        return l;
    }

}
