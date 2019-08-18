package clnt;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import cfg.cfgIfc;
import cfg.cfgVrf;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwdIface;
import pack.packHolder;
import prt.prtGenConn;
import prt.prtGre;
import prt.prtServP;
import prt.prtUdp;
import user.userTerminal;
import util.bits;
import util.counter;
import util.logger;
import util.state;

/**
 * handle udpgre (rfc8086) packets
 *
 * @author matecsaba
 */
public class clntUdpGre implements Runnable, prtServP, ifcDn {

    /**
     * port number of my
     */
    public final static int portNum = 4754;

    /**
     * sending ttl value, -1 means maps out
     */
    public int sendingTTL = 255;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingTOS = -1;

    /**
     * tunnel key to use, 0 means disabled
     */
    public int tunnelKey = 0;

    /**
     * tunnel mask to use, -1 means disabled
     */
    public int tunnelMsk = -1;

    /**
     * send checksum in packets
     */
    public boolean tunnelSum = false;

    /**
     * send sequence number in packets
     */
    public boolean tunnelSeq = false;

    /**
     * target of tunnel
     */
    public String target = null;

    /**
     * vrf of target
     */
    public cfgVrf vrf = null;

    /**
     * source interface
     */
    public cfgIfc srcIfc = null;

    private ifcUp upper = new ifcNull();

    private addrIP remote = new addrIP();

    private counter cntr = new counter();

    private int seqTx;

    private boolean working = true;

    private prtGenConn conn;

    public String toString() {
        return "udpgre to " + target;
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
        addrIP trg = userTerminal.justResolv(target, 0);
        if (trg == null) {
            return;
        }
        prtUdp udp = vrf.getUdp(trg);
        ipFwdIface fwdIfc = null;
        if (srcIfc != null) {
            fwdIfc = srcIfc.getFwdIfc(trg);
        }
        conn = udp.packetConnect(this, fwdIfc, portNum, trg, portNum, "udpgre", null, -1);
        if (conn == null) {
            return;
        }
        conn.timeout = 120000;
        conn.sendTOS = sendingTOS;
        conn.sendTTL = sendingTTL;
        for (;;) {
            if (conn.txBytesFree() < 0) {
                return;
            }
            bits.sleep(1000);
        }
    }

    private void clearState() {
        if (conn != null) {
            conn.setClosing();
        }
        seqTx = 0;
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
        int typ = pck.msbGetW(0);
        pck.getSkip(2);
        int hdr = 0;
        if (tunnelSum) {
            hdr |= 0x8000;
        }
        if (tunnelKey != 0) {
            hdr |= 0x2000;
        }
        if (tunnelSeq) {
            hdr |= 0x1000;
        }
        pck.msbPutW(0, hdr); // header
        pck.msbPutW(2, typ); // ethertype
        pck.putSkip(prtGre.size);
        if (tunnelSum) {
            pck.msbPutD(0, 0); // sum
            pck.putSkip(prtGre.size);
        }
        if (tunnelKey != 0) {
            pck.msbPutD(0, tunnelKey); // key
            pck.putSkip(prtGre.size);
        }
        if (tunnelSeq) {
            seqTx++;
            pck.msbPutD(0, seqTx); // sequence
            pck.putSkip(prtGre.size);
        }
        if (tunnelSum) {
            pck.merge2beg();
            int sum = pck.getIPsum(0, pck.dataSize(), 0);
            pck.unMergeBytes(prtGre.size * 2);
            pck.lsbPutW(-prtGre.size, 0xffff - sum); // checksum
        }
        pck.merge2beg();
        pck.putDefaults();
        conn.send2net(pck);
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
        int hdr = pck.msbGetW(0); // header
        int typ = pck.msbGetW(2); // ethertype
        int vers = hdr & 7; // version
        boolean sump = (hdr & 0x8000) != 0; // sum present
        boolean keyp = (hdr & 0x2000) != 0; // key present
        boolean seqp = (hdr & 0x1000) != 0; // seq present
        pck.getSkip(prtGre.size);
        if (vers != 0) {
            logger.info("got bad version from " + remote);
            cntr.drop(pck, counter.reasons.badVer);
            return true;
        }
        if (sump != tunnelSum) {
            logger.info("got mismatching header from " + remote);
            cntr.drop(pck, counter.reasons.badHdr);
            return true;
        }
        if (keyp != (tunnelKey != 0)) {
            logger.info("got mismatching header from " + remote);
            cntr.drop(pck, counter.reasons.badHdr);
            return true;
        }
        if (seqp != tunnelSeq) {
            logger.info("got mismatching header from " + remote);
            cntr.drop(pck, counter.reasons.badHdr);
            return true;
        }
        if (sump) {
            int sum = pck.getIPsum(-prtGre.size, pck.dataSize() + prtGre.size, 0);
            pck.getSkip(prtGre.size);
            if (sum != 0xffff) {
                logger.info("got invalid checksum from " + remote);
                cntr.drop(pck, counter.reasons.badSum);
                return true;
            }
        }
        if (keyp) {
            int key = pck.msbGetD(0); // key
            pck.getSkip(prtGre.size);
            if ((key & tunnelMsk) != tunnelKey) {
                logger.info("got bad key from " + remote);
                cntr.drop(pck, counter.reasons.badKey);
                return true;
            }
        }
        if (seqp) {
            // pck.msbGetD(0); // sequence
            pck.getSkip(prtGre.size);
        }
        pck.putStart();
        pck.msbPutW(0, typ);
        pck.putSkip(2);
        pck.merge2beg();
        upper.recvPack(pck);
        return false;
    }

}
