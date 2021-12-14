package net.freertr.clnt;

import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrMac;
import net.freertr.addr.addrType;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgVrf;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcEther;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipIcmp6;
import net.freertr.ip.ipMhost4;
import net.freertr.pack.packHolder;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServP;
import net.freertr.prt.prtUdp;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.counter;
import net.freertr.util.logger;
import net.freertr.util.state;

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
        new Thread(this).start();
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
        addrIP trg = userTerminal.justResolv(target, prefer);
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
        conn = udp.packetConnect(this, fwdIfc, prtL, trg, prtR, "amt", null, -1);
        if (conn == null) {
            return;
        }
        conn.timeout = 120000;
        conn.sendTOS = sendingTOS;
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

}
