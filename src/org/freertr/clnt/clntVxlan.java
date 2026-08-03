package org.freertr.clnt;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgVrf;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwdIface;
import org.freertr.pack.packHolder;
import org.freertr.pack.packVxlan;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServP;
import org.freertr.prt.prtUdp;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * vxlan (rfc7348) client
 *
 * @author matecsaba
 */
public class clntVxlan implements Runnable, prtServP, ifcDn {

    /**
     * create instance
     */
    public clntVxlan() {
    }

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * preferred ip protocol version
     */
    public int prefer = 0;

    /**
     * target of tunnel
     */
    public String target = null;

    /**
     * instance id
     */
    public int inst;

    /**
     * protocol
     */
    public int prot;

    /**
     * vrf of target
     */
    public cfgVrf vrf = null;

    /**
     * source interface
     */
    public cfgIfc srcIfc = null;

    /**
     * wildcard port listener
     */
    public boolean wildcard = false;

    /**
     * sending ttl value, -1 means maps out
     */
    public int sendingTTL = 255;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingTOS = -1;

    /**
     * sending df value, -1 means maps out
     */
    public int sendingDFN = -1;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingFLW = -1;

    /**
     * counter
     */
    public counter cntr = new counter();

    private prtGenConn conn;

    private boolean working = true;

    private addrIP trg;

    private prtUdp udp;

    private ipFwdIface fwdIfc;

    public String toString() {
        return "vxlan to " + target;
    }

    /**
     * get remote address
     *
     * @return address
     */
    public addrIP getRemAddr() {
        if (conn == null) {
            return null;
        }
        return trg.copyBytes();
    }

    /**
     * get local address
     *
     * @return address
     */
    public addrIP getLocAddr() {
        if (conn == null) {
            return null;
        }
        return conn.iface.addr.copyBytes();
    }

    /**
     * get remote port
     *
     * @return address
     */
    public int getRemPort() {
        if (conn == null) {
            return 0;
        }
        return conn.portRem;
    }

    /**
     * get local port
     *
     * @return address
     */
    public int getLocPort() {
        if (conn == null) {
            return 0;
        }
        return conn.portLoc;
    }

    /**
     * get hw address
     *
     * @return address
     */
    public addrType getHwAddr() {
        return addrMac.getRandom();
    }

    /**
     * set filter
     *
     * @param promisc promiscous mode
     */
    public void setFilter(boolean promisc) {
    }

    /**
     * get state
     *
     * @return state
     */
    public state.states getState() {
        return state.states.up;
    }

    /**
     * close interface
     */
    public void closeDn() {
        clearState();
    }

    /**
     * flap interface
     */
    public void flapped() {
        clearState();
    }

    /**
     * set upper layer
     *
     * @param server upper layer
     */
    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

    /**
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        return 1400;
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return 4000000;
    }

    /**
     * send packet
     *
     * @param pckBin packet
     */
    public void sendPack(packHolder pckBin) {
        pckBin.merge2beg();
        if (conn == null) {
            return;
        }
        cntr.tx(pckBin);
        packVxlan pckVxl = new packVxlan();
        pckVxl.instance = inst;
        pckVxl.protocol = prot;
        pckVxl.createHeader(pckBin);
        pckBin.merge2beg();
        pckBin.putDefaults();
        conn.send2net(pckBin);
    }

    /**
     * start connection
     */
    public void workStart() {
        logger.startThread(this);
    }

    /**
     * stop connection
     */
    public void workStop() {
        working = false;
        clearState();
    }

    public void run() {
        for (;;) {
            if (!working) {
                break;
            }
            try {
                clearState();
                workDoer();
            } catch (Exception e) {
                logger.traceback(e);
            }
            clearState();
            bits.sleep(1000);
        }
    }

    private void workDoer() {
        trg = clntDns.justResolv(target, prefer);
        if (trg == null) {
            return;
        }
        udp = vrf.getUdp(trg);
        fwdIfc = null;
        if (srcIfc != null) {
            fwdIfc = srcIfc.getFwdIfc(trg);
        }
        if (wildcard) {
            udp.packetListen(this, fwdIfc, packVxlan.port, trg, 0, "vxlan", -1, null, -1, -1);
        }
        conn = udp.packetConnect(this, fwdIfc, packVxlan.port, trg, packVxlan.port, "vxlan", -1, null, -1, -1);
        if (conn == null) {
            return;
        }
        conn.timeout = 120000;
        conn.sendTOS = sendingTOS;
        conn.sendDFN = sendingDFN;
        conn.sendFLW = sendingFLW;
        conn.sendTTL = sendingTTL;
        for (;;) {
            bits.sleep(1000);
            if (!working) {
                return;
            }
            if (conn.txBytesFree() < 0) {
                return;
            }
        }
    }

    private void clearState() {
        if (conn != null) {
            conn.setClosing();
            if (wildcard) {
                udp.listenStop(fwdIfc, packVxlan.port, trg, 0);
            }
        }
    }

    /**
     * closed interface
     *
     * @param ifc interface
     */
    public void closedInterface(ipFwdIface ifc) {
    }

    /**
     * accept connection
     *
     * @param id connection
     * @return false on success, true on error
     */
    public boolean datagramAccept(prtGenConn id) {
        if (!wildcard) {
            return true;
        }
        id.timeout = 120000;
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
     * closed connection
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
        return false;
    }

    /**
     * received packet
     *
     * @param id connection
     * @param pckBin packet
     * @return false on success, true on error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pckBin) {
        cntr.rx(pckBin);
        packVxlan pckVxl = new packVxlan();
        if (pckVxl.parseHeader(pckBin)) {
            return false;
        }
        if (pckVxl.instance != inst) {
            return false;
        }
        upper.recvPack(pckBin);
        return false;
    }

}
