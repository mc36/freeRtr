package org.freertr.clnt;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgVrf;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcEther;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipIcmp6;
import org.freertr.ip.ipMhost4;
import org.freertr.pack.packHolder;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServP;
import org.freertr.prt.prtUdp;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * automatic multicast tunneling (rfc7450) protocol client
 *
 * @author matecsaba
 */
public class clntAmt implements Runnable, prtServP, ifcDn {

    /**
     * create instance
     */
    public clntAmt() {
    }

    /**
     * port number of my
     */
    public final static int portNum = 2268;

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
     * sending flow value, -1 means maps out
     */
    public int sendingFLW = -1;

    /**
     * preferred ip protocol version
     */
    public int prefer = 0;

    /**
     * target of tunnel
     */
    public String target = null;

    /**
     * remote port number
     */
    public int prtR;

    /**
     * local port number
     */
    public int prtL;

    /**
     * vrf of target
     */
    public cfgVrf vrf = null;

    /**
     * source interface
     */
    public cfgIfc srcIfc = null;

    /**
     * negotiate
     */
    public boolean negotiate = false;

    private ifcUp upper = new ifcNull();

    private counter cntr = new counter();

    private boolean working = true;

    private prtGenConn conn;

    private ipFwd fwdr;

    private int lasTyp;

    private int nonce;

    private addrMac rspMac = new addrMac();

    public String toString() {
        return "amt to " + target;
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
        return conn.peerAddr.copyBytes();
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
     * get local address
     *
     * @return peer address, null if no session
     */
    public ipFwd getFwd() {
        return fwdr;
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
     * set connection
     *
     * @param id connection
     * @param ip forwarder
     */
    public void setConnection(prtGenConn id, ipFwd ip) {
        conn = id;
        fwdr = ip;
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
        return 8000000;
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
     * get hw address
     *
     * @return hw address
     */
    public addrType getHwAddr() {
        return new addrEmpty();
    }

    /**
     * set filter
     *
     * @param promisc promiscous mode
     */
    public void setFilter(boolean promisc) {
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
     * get state
     *
     * @return state
     */
    public state.states getState() {
        return state.states.up;
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
        addrIP trg = clntDns.justResolv(target, prefer);
        if (trg == null) {
            return;
        }
        prtUdp udp = vrf.getUdp(trg);
        ipFwdIface fwdIfc = null;
        if (srcIfc != null) {
            fwdIfc = srcIfc.getFwdIfc(trg);
        }
        if (prtR == 0) {
            prtR = portNum;
        }
        if (prtL == 0) {
            prtL = prtR;
        }
        conn = udp.packetConnect(this, fwdIfc, prtL, trg, prtR, "amt", -1, null, -1, -1);
        if (conn == null) {
            return;
        }
        conn.timeout = 120000;
        conn.sendTOS = sendingTOS;
        conn.sendDFN = sendingDFN;
        conn.sendFLW = sendingFLW;
        conn.sendTTL = sendingTTL;
        if (negotiate) {
            nonce = bits.randomD();
            packHolder pck = new packHolder(true, true);
            pck.clear();
            pck.msbPutW(0, 0x100); // discovery
            pck.msbPutW(2, 0); // reserved
            pck.msbPutD(4, nonce);
            pck.putSkip(8);
            pck.merge2beg();
            pck.putDefaults();
            conn.send2net(pck);
            if (wait4pack() != 2) {
                return;
            }
            pck.clear();
            pck.msbPutW(0, 0x300); // request
            pck.msbPutW(2, 0); // reserved
            pck.msbPutD(4, nonce);
            pck.putSkip(8);
            pck.merge2beg();
            pck.putDefaults();
            conn.send2net(pck);
            if (wait4pack() != 4) {
                return;
            }
        }
        for (;;) {
            if (conn.txBytesFree() < 0) {
                return;
            }
            bits.sleep(1000);
        }
    }

    private int wait4pack() {
        lasTyp = -1;
        for (int i = 0; i < 50; i++) {
            bits.sleep(100);
            if (lasTyp > 0) {
                break;
            }
        }
        return lasTyp;
    }

    private void clearState() {
        if (conn != null) {
            conn.setClosing();
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
        return true;
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
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        cntr.tx(pck);
        pck.getSkip(2); // ethertype
        if (negotiate && ((pck.IPprt == ipMhost4.protoNum) || (pck.IPprt == ipIcmp6.protoNum))) {
            pck.msbPutW(0, 0x0500); // report
            pck.putAddr(2, rspMac);
            pck.msbPutD(8, nonce);
            pck.putSkip(12);
        } else {
            pck.msbPutW(0, 0x600); // mcast data
            pck.putSkip(2);
        }
        pck.merge2beg();
        pck.putDefaults();
        conn.send2net(pck);
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
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean datagramRecv(prtGenConn id, packHolder pck) {
        cntr.rx(pck);
        lasTyp = pck.getByte(0);
        switch (lasTyp) {
            case 6: // data
                pck.getSkip(2);
                break;
            case 4: // query
                rspMac = new addrMac();
                pck.getAddr(rspMac, 2);
                nonce = pck.msbGetD(8);
                pck.getSkip(12);
                break;
            case 5: // report
                rspMac = new addrMac();
                pck.getAddr(rspMac, 2);
                nonce = pck.msbGetD(8);
                pck.getSkip(12);
                break;
            default:
                cntr.drop(pck, counter.reasons.badCod);
                return false;
        }
        int typ = ifcEther.guessEtherType(pck);
        if (typ < 0) {
            cntr.drop(pck, counter.reasons.badVal);
            return false;
        }
        pck.msbPutW(0, typ);
        pck.putSkip(2);
        pck.merge2beg();
        upper.recvPack(pck);
        return false;
    }

    /**
     * get show
     *
     * @return state
     */
    public userFormat getShow() {
        userFormat res = new userFormat("|", "category|value");
        res.add("conn|" + conn);
        res.add("upper|" + upper);
        res.add("cntr|" + cntr);
        res.add("nonce|" + nonce);
        return res;
    }

}
