package net.freertr.clnt;

import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrType;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgVrf;
import net.freertr.cry.cryEncrGeneric;
import net.freertr.cry.cryHashGeneric;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcEther;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.ip.ipFwdIface;
import net.freertr.pack.packHolder;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServP;
import net.freertr.prt.prtUdp;
import net.freertr.sec.secTransform;
import net.freertr.tab.tabWindow;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.counter;
import net.freertr.util.logger;
import net.freertr.util.state;

/**
 * openvpn protocol
 *
 * @author matecsaba
 */
public class clntOpenvpn implements Runnable, prtServP, ifcDn {

    /**
     * create instance
     */
    public clntOpenvpn() {
    }

    /**
     * port number
     */
    public static final int port = 1194;

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

    /**
     * preferred ip protocol version
     */
    public int prefer = 0;

    /**
     * do replay checking
     */
    public int replayCheck = 1024;

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

    /**
     * source port
     */
    public int prtL;

    /**
     * target port
     */
    public int prtR;

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
     * preshared secret
     */
    public String preshared;

    /**
     * transform set to use
     */
    public secTransform transform;

    /**
     * counter
     */
    public counter cntr = new counter();

    /**
     * encryption keys
     */
    public byte[] keyEncr = null;

    /**
     * authentication keys
     */
    public byte[] keyHash = null;

    /**
     * cipher size
     */
    public int cphrSiz;

    /**
     * hash size
     */
    public int hashSiz;

    /**
     * key timestamp
     */
    public int timRx;

    /**
     * key timestamp
     */
    public int timTx;

    private prtGenConn conn;

    private boolean working = true;

    private cryHashGeneric hashRx;

    private cryEncrGeneric cphrRx;

    private cryHashGeneric hashTx;

    private cryEncrGeneric cphrTx;

    private tabWindow<packHolder> sequence;

    private int seqRx;

    private int seqTx;

    public String toString() {
        return "openvpn to " + target;
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
        return 8000000;
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public synchronized void sendPack(packHolder pck) {
        pck.merge2beg();
        if (conn == null) {
            return;
        }
        if (ifcEther.stripEtherType(pck)) {
            return;
        }
        pck.msbPutD(0, seqTx);
        pck.msbPutD(4, timTx);
        pck.putSkip(8);
        pck.merge2beg();
        int i = pck.dataSize() % cphrSiz;
        i = cphrSiz - i;
        pck.putFill(0, i, i); // padding
        pck.putSkip(i);
        pck.merge2end();
        byte[] buf = new byte[cphrSiz];
        for (i = 0; i < buf.length; i++) {
            buf[i] = (byte) bits.randomB();
        }
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        pck.merge2beg();
        pck.encrData(cphrTx, 0, pck.dataSize());
        seqTx++;
        hashTx.init();
        pck.hashData(hashTx, 0, pck.dataSize());
        byte[] hsh = hashTx.finish();
        pck.putCopy(hsh, 0, 0, hsh.length);
        pck.putSkip(hsh.length);
        pck.merge2beg();
        cntr.tx(pck);
        pck.putDefaults();
        conn.send2net(pck);
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
        byte[] buf1 = new byte[preshared.length() / 2];
        for (int i = 0; i < buf1.length; i++) {
            try {
                buf1[i] = (byte) Integer.parseInt(preshared.substring(i * 2, i * 2 + 2), 16);
            } catch (Exception e) {
            }
        }
        hashSiz = transform.getHash().getHashSize();
        byte[] buf2 = new byte[hashSiz];
        bits.byteCopy(buf1, 64, buf2, 0, buf2.length);
        hashRx = transform.getHmac(buf2);
        hashTx = transform.getHmac(buf2);
        keyHash = buf2;
        cphrRx = transform.getEncr();
        cphrTx = transform.getEncr();
        buf2 = new byte[transform.getKeyS()];
        byte[] buf3 = new byte[cphrTx.getBlockSize()];
        bits.byteCopy(buf1, 0, buf2, 0, buf2.length);
        bits.byteCopy(buf1, 0, buf3, 0, buf3.length);
        cphrTx.init(buf2, buf3, true);
        cphrRx.init(buf2, buf3, false);
        keyEncr = buf2;
        cphrSiz = buf3.length;
        if (replayCheck > 0) {
            sequence = new tabWindow<packHolder>(replayCheck);
        }
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
            prtR = port;
        }
        if (prtL == 0) {
            prtL = prtR;
        }
        conn = udp.packetConnect(this, fwdIfc, prtL, trg, prtR, "openvpn", null, -1);
        if (conn == null) {
            return;
        }
        conn.timeout = 120000;
        conn.sendTOS = sendingTOS;
        conn.sendFLW = sendingFLW;
        conn.sendTTL = sendingTTL;
        for (;;) {
            if (conn == null) {
                return;
            }
            if (conn.txBytesFree() < 0) {
                return;
            }
            bits.sleep(1000);
        }
    }

    private synchronized void clearState() {
        if (conn != null) {
            conn.setClosing();
        }
        conn = null;
        seqRx = 0;
        seqTx = 0;
        timTx = (int) (bits.getTime() / 1000);
    }

    /**
     * close interface
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
     * connection closed
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
     * @param pck packet
     * @return false on success, true on error
     */
    public synchronized boolean datagramRecv(prtGenConn id, packHolder pck) {
        if (pck.dataSize() < (hashSiz + cphrSiz + 8)) {
            logger.info("got too small from " + target);
            cntr.drop(pck, counter.reasons.badLen);
            return false;
        }
        byte[] sum = new byte[hashSiz];
        pck.getCopy(sum, 0, 0, hashSiz);
        pck.getSkip(hashSiz);
        hashRx.init();
        pck.hashData(hashRx, 0, pck.dataSize());
        if (bits.byteComp(sum, 0, hashRx.finish(), 0, hashSiz) != 0) {
            logger.info("got bad hash from " + target);
            cntr.drop(pck, counter.reasons.badSum);
            return false;
        }
        if ((pck.dataSize() % cphrSiz) != 0) {
            logger.info("got bad padding from " + target);
            cntr.drop(pck, counter.reasons.badLen);
            return false;
        }
        pck.encrData(cphrRx, 0, pck.dataSize());
        pck.getSkip(cphrSiz);
        seqRx = pck.msbGetD(0);
        timRx = pck.msbGetD(4);
        pck.getSkip(8);
        if (sequence != null) {
            if (sequence.gotDat(seqRx)) {
                cntr.drop(pck, counter.reasons.badRxSeq);
                logger.info("replay check failed from " + target);
                return false;
            }
        }
        int i = ifcEther.guessEtherType(pck);
        if (i < 0) {
            logger.info("got bad protocol from " + target);
            cntr.drop(pck, counter.reasons.badProto);
            return false;
        }
        pck.msbPutW(0, i); // ethertype
        pck.putSkip(2);
        pck.merge2beg();
        cntr.rx(pck);
        upper.recvPack(pck);
        return false;
    }

}
