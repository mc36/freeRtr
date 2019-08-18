package sec;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import cfg.cfgIpsec;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwd;
import ip.ipFwdIface;
import pack.packEsp;
import pack.packHolder;
import pack.packIke;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtUdp;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * internet key exchange protocol (rfc5996) handler
 *
 * @author matecsaba
 */
public class secIke implements ifcDn, ifcUp {

    /**
     * role in session
     */
    public cfgIpsec.roleMode role;

    /**
     * ike pipe
     */
    public pipeSide pipe;

    /**
     * sending ttl value, -1 means maps out
     */
    public int sendingTTL = 255;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingTOS = -1;

    /**
     * preshared key
     */
    public String preshared;

    /**
     * transform set to use
     */
    public secTransform transform;

    /**
     * do replay checking
     */
    public int replayCheck = 1024;

    /**
     * ipv6 sa
     */
    public boolean ipv6 = false;

    private addrIP localAddr;

    private addrIP remoteAddr;

    private ipFwdIface fwdIfc;

    private prtUdp fwdUdp;

    private packIke conn;

    private packEsp espRx;

    private packEsp espTx;

    private ifcUp upper = new ifcNull();

    private int retry;

    private int txSeq;

    private boolean need2work;

    private long keepLast;

    /**
     * start up the process
     */
    public secIke() {
        conn = null;
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
    }

    /**
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
    }

    /**
     * close interface
     */
    public void closeUp() {
    }

    /**
     * flap interface
     */
    public void flapped() {
    }

    /**
     * get state
     *
     * @return state
     */
    public state.states getState() {
        return espRx.lowerGetState();
    }

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return espRx.getCounter();
    }

    /**
     * get mtu size
     *
     * @return mtu size
     */
    public int getMTUsize() {
        return espRx.lowerGetMTU();
    }

    /**
     * get bandwidth
     *
     * @return bandwidth
     */
    public long getBandwidth() {
        return espRx.lowerGetBW();
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
     * receive packet
     *
     * @param pck packet holder
     */
    public void recvPack(packHolder pck) {
        upper.recvPack(pck);
    }

    /**
     * send packet
     *
     * @param pck packet
     */
    public void sendPack(packHolder pck) {
        espTx.sendPacket(pck);
    }

    /**
     * start working
     *
     * @param fwd forwarder of esp packets
     * @param udp forwarder of udp packets
     * @param ifc interface to source from
     * @param trg ip address of remote
     * @return false if successful, true if error happened
     */
    public boolean workStart(ipFwd fwd, prtUdp udp, ipFwdIface ifc, addrIP trg) {
        need2work = true;
        localAddr = ifc.addr.copyBytes();
        remoteAddr = trg.copyBytes();
        fwdIfc = ifc;
        fwdUdp = udp;
        espRx = new packEsp(this);
        espTx = new packEsp(this);
        espRx.lowerSetup(fwd, ifc, trg, sendingTOS, sendingTTL);
        espTx.lowerSetup(fwd, ifc, trg, sendingTOS, sendingTTL);
        espRx.ipv6 = ipv6;
        espTx.ipv6 = ipv6;
        espRx.replayCheck = replayCheck;
        if (espRx.lowerRegister()) {
            return true;
        }
        if (role == cfgIpsec.roleMode.staticKeys) {
            packIke cn = newIkePck();
            cn.skeyidD = preshared.getBytes();
            cn.nonceI = new byte[0];
            cn.nonceR = new byte[0];
            cn.updateEsp(espRx, 0, false, false);
            cn.updateEsp(espTx, 0, true, false);
            espRx.replayCheck = 0;
            espTx.replayCheck = 0;
            return false;
        }
        new secIkeRx(this);
        new secIkeTx(this);
        return false;
    }

    /**
     * stop working
     */
    public void workStop() {
        need2work = false;
        if (pipe != null) {
            pipe.setClose();
        }
        espRx.lowerUnregister();
    }

    private packIke newIkePck() {
        packIke n = new packIke();
        n.transform = transform.copyBytes();
        n.preshared = "" + preshared;
        n.initiator = role == cfgIpsec.roleMode.initiator;
        return n;
    }

    private void makeEsps() {
        keepLast = bits.getTime();
        if (role == cfgIpsec.roleMode.initiator) {
            conn.updateEsp(espRx, conn.spiValI, false, true);
            conn.updateEsp(espTx, conn.spiValR, true, false);
        } else {
            conn.updateEsp(espRx, conn.spiValR, false, false);
            conn.updateEsp(espTx, conn.spiValI, true, true);
        }
    }

    private void sendIke(packIke pck, boolean justOnce) {
        if (!justOnce) {
            conn.pckDat = pck.pckDat;
        }
        if (pipe == null) {
            return;
        }
        pck.pckDat.pipeSend(pipe, 0, pck.pckDat.dataSize(), 1);
    }

    private boolean recvIke(packIke pck) {
        if (pipe == null) {
            return true;
        }
        int i = pck.pckDat.pipeRecv(pipe, 0, 4096, 143);
        if (i < 0) {
            return true;
        }
        return false;
    }

    private void stopRetrans() {
        retry = 0;
        conn.pckDat.clear();
    }

    private synchronized boolean doKeepalive() {
        if (!need2work) {
            return true;
        }
        if (pipe == null) {
            pipe = fwdUdp.streamConnect(new pipeLine(32768, true), fwdIfc, packIke.port, remoteAddr, packIke.port, "ike", null, -1);
            clearState();
        }
        if (pipe == null) {
            bits.sleep(1000);
            return false;
        }
        if (pipe.isClosed() != 0) {
            pipe.setClose();
            pipe = null;
            return false;
        }
        if (keepLast < 0) {
            return false;
        }
        long tim = bits.getTime();
        if ((tim - keepLast) < 10000) {
            return false;
        }
        keepLast = tim;
        if (conn.pckDat.dataSize() > 0) {
            return false;
        }
        if (espRx.badSpi != 0) {
            packIke pckTx = conn.copyBytes();
            pckTx.setFlags(false);
            pckTx.spiValI = espRx.badSpi;
            pckTx.deleteCreate();
            pckTx.msgId = txSeq;
            pckTx.encryptCreate();
            pckTx.headerCreate();
            sendIke(pckTx, true);
            txSeq++;
            espRx.badSpi = 0;
        }
        packIke pckTx = conn.copyBytes();
        pckTx.setFlags(false);
        pckTx.xchgTyp = packIke.xchgInfo;
        pckTx.msgId = txSeq;
        pckTx.encryptCreate();
        pckTx.headerCreate();
        sendIke(pckTx, false);
        txSeq++;
        return false;
    }

    private synchronized void clearState() {
        if (debugger.secIkeTraf) {
            logger.debug("restarting");
        }
        conn = newIkePck();
        keepLast = -1;
        retry = 0;
        txSeq = 0;
        if (role != cfgIpsec.roleMode.initiator) {
            return;
        }
        conn.cookieI = bits.randomQ();
        conn.nonceI = packIke.nonceFill();
        packIke pckTx = conn.copyBytes();
        pckTx.setFlags(false);
        pckTx.nonceCreate();
        pckTx.keyXchgFill();
        pckTx.keyXchgCreate();
        pckTx.secAssFill(true);
        pckTx.secAssCreate();
        pckTx.msgId = txSeq;
        pckTx.headerCreate();
        conn.msgI = pckTx.pckBin;
        conn.diffie = pckTx.diffie;
        sendIke(pckTx, false);
        txSeq++;
    }

    private void initRx() {
        clearState();
        for (;;) {
            if (doKeepalive()) {
                return;
            }
            packIke pckRx = conn.copyBytes();
            if (recvIke(pckRx)) {
                continue;
            }
            if (pckRx.headerParse()) {
                continue;
            }
            if (pckRx.cookieI != conn.cookieI) {
                continue;
            }
            if (pckRx.xchgTyp == packIke.xchgIkeSa) {
                if (pckRx.secAssParse(true)) {
                    continue;
                }
                if (pckRx.nonceParse()) {
                    continue;
                }
                if (pckRx.keyXchgParse()) {
                    continue;
                }
                conn.msgR = pckRx.pckBin;
                conn.cookieR = pckRx.cookieR;
                conn.diffie = pckRx.diffie;
                conn.nonceR = pckRx.nonceI;
                conn.computeKeys();
                conn.spiValI = bits.randomD();
                packIke pckTx = conn.copyBytes();
                pckTx.identFill(localAddr);
                pckTx.identCreate();
                conn.idnI = pckTx.headerCurr();
                pckTx = conn.copyBytes();
                pckTx.setFlags(false);
                pckTx.trafselCreate(true, ipv6);
                pckTx.trafselCreate(false, ipv6);
                pckTx.secAssFill(false);
                pckTx.secAssCreate();
                pckTx.authenCreate();
                pckTx.identFill(localAddr);
                pckTx.identCreate();
                pckTx.msgId = txSeq;
                pckTx.encryptCreate();
                pckTx.headerCreate();
                sendIke(pckTx, false);
                txSeq++;
                continue;
            }
            if (!pckRx.checkPeer(conn)) {
                logger.info("invalid cookies");
                continue;
            }
            if (pckRx.encryptParse()) {
                logger.info("failed to decrypt packet");
                continue;
            }
            if (pckRx.xchgTyp == packIke.xchgAuth) {
                if (pckRx.identParse()) {
                    continue;
                }
                conn.idnR = pckRx.idnR;
                if (pckRx.authenParse()) {
                    continue;
                }
                if (pckRx.secAssParse(false)) {
                    continue;
                }
                if (pckRx.trafselParse(true, ipv6)) {
                    continue;
                }
                if (pckRx.trafselParse(false, ipv6)) {
                    continue;
                }
                conn.spiValR = pckRx.spiValI;
                stopRetrans();
                makeEsps();
                continue;
            }
            if (pckRx.xchgTyp == packIke.xchgChild) {
                if (pckRx.secAssParse(false)) {
                    continue;
                }
                if (pckRx.nonceParse()) {
                    continue;
                }
                if (pckRx.trafselParse(true, ipv6)) {
                    continue;
                }
                if (pckRx.trafselParse(false, ipv6)) {
                    continue;
                }
                conn.spiOldI = conn.spiValI;
                conn.spiOldR = conn.spiValR;
                conn.nonceR = pckRx.nonceI;
                conn.nonceI = packIke.nonceFill();
                conn.spiValR = pckRx.spiValI;
                conn.spiValI = bits.randomD();
                packIke pckTx = conn.copyBytes();
                pckTx.setFlags(true);
                pckTx.trafselCreate(true, ipv6);
                pckTx.trafselCreate(false, ipv6);
                pckTx.nonceI = conn.nonceR;
                pckTx.nonceCreate();
                pckTx.secAssFill(false);
                pckTx.spiValI = conn.spiValI;
                pckTx.secAssCreate();
                pckTx.msgId = pckRx.msgId;
                pckTx.encryptCreate();
                pckTx.headerCreate();
                sendIke(pckTx, true);
                makeEsps();
                continue;
            }
            if (pckRx.xchgTyp != packIke.xchgInfo) {
                continue;
            }
            if (pckRx.isEmpty()) {
                if (pckRx.isReply()) {
                    stopRetrans();
                    continue;
                }
                packIke pckTx = conn.copyBytes();
                pckTx.setFlags(true);
                pckTx.xchgTyp = packIke.xchgInfo;
                pckTx.msgId = pckRx.msgId;
                pckTx.encryptCreate();
                pckTx.headerCreate();
                sendIke(pckTx, true);
                continue;
            }
            if (!pckRx.deleteParse()) {
                packIke pckTx = conn.copyBytes();
                pckTx.setFlags(true);
                pckTx.spiValI = conn.spiOldI;
                pckTx.deleteCreate();
                pckTx.msgId = pckRx.msgId;
                pckTx.encryptCreate();
                pckTx.headerCreate();
                sendIke(pckTx, true);
                continue;
            }
            packIke pckTx = conn.copyBytes();
            pckTx.setFlags(true);
            pckTx.xchgTyp = packIke.xchgInfo;
            pckTx.msgId = pckRx.msgId;
            pckTx.encryptCreate();
            pckTx.headerCreate();
            sendIke(pckTx, true);
        }
    }

    private void respRx() {
        clearState();
        for (;;) {
            if (doKeepalive()) {
                return;
            }
            packIke pckRx = conn.copyBytes();
            if (recvIke(pckRx)) {
                continue;
            }
            if (pckRx.headerParse()) {
                continue;
            }
            if (pckRx.xchgTyp == packIke.xchgIkeSa) {
                if (pckRx.secAssParse(true)) {
                    continue;
                }
                if (pckRx.nonceParse()) {
                    continue;
                }
                if (pckRx.keyXchgParse()) {
                    continue;
                }
                clearState();
                conn.msgI = pckRx.pckBin;
                conn.cookieI = pckRx.cookieI;
                conn.cookieR = bits.randomQ();
                conn.diffie = pckRx.diffie;
                conn.nonceI = pckRx.nonceI;
                conn.nonceR = packIke.nonceFill();
                packIke pckTx = conn.copyBytes();
                pckTx.setFlags(true);
                pckTx.nonceI = conn.nonceR;
                pckTx.nonceCreate();
                pckTx.keyXchgFill();
                pckTx.keyXchgCreate();
                pckTx.secAssFill(true);
                pckTx.secAssCreate();
                pckTx.msgId = pckRx.msgId;
                pckTx.headerCreate();
                conn.msgR = pckTx.pckBin;
                sendIke(pckTx, true);
                conn.computeKeys();
                pckTx = conn.copyBytes();
                pckTx.identFill(localAddr);
                pckTx.identCreate();
                conn.idnR = pckTx.headerCurr();
                continue;
            }
            if (!pckRx.checkPeer(conn)) {
                logger.info("invalid cookies");
                continue;
            }
            if (pckRx.encryptParse()) {
                logger.info("failed to decrypt packet");
                continue;
            }
            if (pckRx.xchgTyp == packIke.xchgAuth) {
                if (pckRx.identParse()) {
                    continue;
                }
                conn.idnI = pckRx.idnI;
                if (pckRx.authenParse()) {
                    continue;
                }
                if (pckRx.secAssParse(false)) {
                    continue;
                }
                if (pckRx.trafselParse(true, ipv6)) {
                    continue;
                }
                if (pckRx.trafselParse(false, ipv6)) {
                    continue;
                }
                conn.spiValI = pckRx.spiValI;
                conn.spiValR = bits.randomD();
                packIke pckTx = conn.copyBytes();
                pckTx.setFlags(true);
                pckTx.trafselCreate(true, ipv6);
                pckTx.trafselCreate(false, ipv6);
                pckTx.secAssFill(false);
                pckTx.spiValI = conn.spiValR;
                pckTx.secAssCreate();
                pckTx.authenCreate();
                pckTx.identFill(localAddr);
                pckTx.identCreate();
                pckTx.msgId = pckRx.msgId;
                pckTx.encryptCreate();
                pckTx.headerCreate();
                sendIke(pckTx, true);
                makeEsps();
                continue;
            }
            if (pckRx.xchgTyp == packIke.xchgChild) {
                if (pckRx.secAssParse(false)) {
                    continue;
                }
                if (pckRx.nonceParse()) {
                    continue;
                }
                if (pckRx.trafselParse(true, ipv6)) {
                    continue;
                }
                if (pckRx.trafselParse(false, ipv6)) {
                    continue;
                }
                conn.spiOldI = conn.spiValI;
                conn.spiOldR = conn.spiValR;
                conn.nonceI = pckRx.nonceI;
                conn.nonceR = packIke.nonceFill();
                conn.spiValI = pckRx.spiValI;
                conn.spiValR = bits.randomD();
                packIke pckTx = conn.copyBytes();
                pckTx.setFlags(true);
                pckTx.trafselCreate(true, ipv6);
                pckTx.trafselCreate(false, ipv6);
                pckTx.nonceI = conn.nonceR;
                pckTx.nonceCreate();
                pckTx.secAssFill(false);
                pckTx.spiValI = conn.spiValR;
                pckTx.secAssCreate();
                pckTx.msgId = pckRx.msgId;
                pckTx.encryptCreate();
                pckTx.headerCreate();
                sendIke(pckTx, true);
                makeEsps();
                continue;
            }
            if (pckRx.xchgTyp != packIke.xchgInfo) {
                continue;
            }
            if (pckRx.isEmpty()) {
                if (pckRx.isReply()) {
                    stopRetrans();
                    continue;
                }
                packIke pckTx = conn.copyBytes();
                pckTx.setFlags(true);
                pckTx.xchgTyp = packIke.xchgInfo;
                pckTx.msgId = pckRx.msgId;
                pckTx.encryptCreate();
                pckTx.headerCreate();
                sendIke(pckTx, true);
                continue;
            }
            if (!pckRx.deleteParse()) {
                packIke pckTx = conn.copyBytes();
                pckTx.setFlags(true);
                pckTx.spiValI = conn.spiOldR;
                pckTx.deleteCreate();
                pckTx.msgId = pckRx.msgId;
                pckTx.encryptCreate();
                pckTx.headerCreate();
                sendIke(pckTx, true);
                continue;
            }
            packIke pckTx = conn.copyBytes();
            pckTx.setFlags(true);
            pckTx.xchgTyp = packIke.xchgInfo;
            pckTx.msgId = pckRx.msgId;
            pckTx.encryptCreate();
            pckTx.headerCreate();
            sendIke(pckTx, true);
        }
    }

    /**
     * rx worker
     */
    protected void workRx() {
        if (role == cfgIpsec.roleMode.initiator) {
            initRx();
        } else {
            respRx();
        }
    }

    /**
     * tx worker
     */
    protected void workTx() {
        for (;;) {
            if (doKeepalive()) {
                return;
            }
            bits.sleep(5000);
            if (conn.pckDat.dataSize() < 1) {
                continue;
            }
            retry++;
            if (retry > 16) {
                clearState();
                continue;
            }
            if (debugger.secIkeTraf) {
                logger.debug("retransmitting");
            }
            sendIke(conn, true);
        }
    }

}

class secIkeRx implements Runnable {

    private secIke lower;

    public secIkeRx(secIke parent) {
        lower = parent;
        new Thread(this).start();
    }

    public void run() {
        try {
            lower.workRx();
        } catch (Exception e) {
            logger.traceback(e);
        }
        if (debugger.secIkeTraf) {
            logger.debug("rx stopped");
        }
        lower.workStop();
    }

}

class secIkeTx implements Runnable {

    private secIke lower;

    public secIkeTx(secIke parent) {
        lower = parent;
        new Thread(this).start();
    }

    public void run() {
        try {
            lower.workTx();
        } catch (Exception e) {
            logger.traceback(e);
        }
        if (debugger.secIkeTraf) {
            logger.debug("tx stopped");
        }
        lower.workStop();
    }

}
