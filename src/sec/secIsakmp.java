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
import pack.packIsakmp;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtUdp;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.state;

/**
 * internet security association and key management protocol (rfc2408) handler
 *
 * @author matecsaba
 */
public class secIsakmp implements ifcDn, ifcUp {

    /**
     * role in session
     */
    public cfgIpsec.roleMode role;

    /**
     * isakmp pipe
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

    private packIsakmp conn;

    private packEsp espRx;

    private packEsp espTx;

    private ifcUp upper = new ifcNull();

    private int retry;

    private boolean need2work;

    private long keepLast;

    private int keepSeq;

    private int keepTry;

    /**
     * start up the process
     */
    public secIsakmp() {
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
            packIsakmp cn = newIkePck();
            cn.skeyidD = preshared.getBytes();
            cn.nonceI = new byte[0];
            cn.nonceR = new byte[0];
            cn.updateEsp(espRx, 0, false);
            cn.updateEsp(espTx, 0, true);
            espRx.replayCheck = 0;
            espTx.replayCheck = 0;
            return false;
        }
        new secIsakmpRx(this);
        new secIsakmpTx(this);
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

    private packIsakmp newIkePck() {
        packIsakmp n = new packIsakmp();
        n.transform = transform.copyBytes();
        n.preshared = "" + preshared;
        n.initiator = role == cfgIpsec.roleMode.initiator;
        return n;
    }

    private void makeEsps() {
        keepLast = 0;
        if (role == cfgIpsec.roleMode.initiator) {
            conn.updateEsp(espRx, conn.spiValI, false);
            conn.updateEsp(espTx, conn.spiValR, true);
        } else {
            conn.updateEsp(espRx, conn.spiValR, false);
            conn.updateEsp(espTx, conn.spiValI, true);
        }
    }

    private void sendIke(packIsakmp pck, boolean justOnce) {
        if (!justOnce) {
            if (role == cfgIpsec.roleMode.initiator) {
                conn.pckDat = pck.pckDat;
            }
        }
        if (pipe == null) {
            return;
        }
        pck.pckDat.pipeSend(pipe, 0, pck.pckDat.dataSize(), 1);
    }

    private boolean recvIke(packIsakmp pck) {
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
        conn.pckDat.clear();
    }

    private void doInform(packIsakmp pckRx) {
        if (pckRx.hashParse()) {
            return;
        }
        if (pckRx.hashVerify(pckRx.hashGenIM())) {
            return;
        }
        if (!pckRx.deleteParse()) {
            return;
        }
        if (pckRx.notifyParse()) {
            return;
        }
        if (pckRx.notifyTyp == packIsakmp.notfDedPerRep) {
            keepTry = 0;
            return;
        }
        if (pckRx.notifyTyp != packIsakmp.notfDedPerReq) {
            return;
        }
        keepTry = 0;
        packIsakmp pckTx = conn.copyBytes();
        pckTx.notifyDat = pckRx.notifyDat;
        pckTx.msgId = pckRx.msgId;
        pckTx.notifyFill(true);
        pckTx.notifyCreate();
        pckTx.hash2got = pckTx.hashGenIM();
        pckTx.hashCreate();
        pckTx.doEncrypt();
        pckTx.headerCreate();
        sendIke(pckTx, true);
    }

    private synchronized boolean doKeepalive() {
        if (!need2work) {
            return true;
        }
        if (pipe == null) {
            pipe = fwdUdp.streamConnect(new pipeLine(32768, true), fwdIfc, packIsakmp.port, remoteAddr, packIsakmp.port, "isakmp", null, -1);
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
        keepSeq++;
        keepTry++;
        if (keepTry > 10) {
            clearState();
            return false;
        }
        if (espRx.badSpi != 0) {
            packIsakmp pckTx = conn.copyBytes();
            pckTx.msgId = bits.randomD();
            pckTx.spiValI = espRx.badSpi;
            pckTx.deleteCreate();
            pckTx.hash2got = pckTx.hashGenIM();
            pckTx.hashCreate();
            pckTx.doEncrypt();
            pckTx.headerCreate();
            sendIke(pckTx, true);
            espRx.badSpi = 0;
        }
        packIsakmp pckTx = conn.copyBytes();
        pckTx.notifyDat = keepSeq;
        pckTx.msgId = bits.randomD();
        pckTx.notifyFill(false);
        pckTx.notifyCreate();
        pckTx.hash2got = pckTx.hashGenIM();
        pckTx.hashCreate();
        pckTx.doEncrypt();
        pckTx.headerCreate();
        sendIke(pckTx, true);
        return false;
    }

    private synchronized void clearState() {
        if (debugger.secIkeTraf) {
            logger.debug("restarting");
        }
        conn = newIkePck();
        keepLast = -1;
        keepSeq = bits.randomD();
        keepTry = 0;
        retry = 0;
        if (role != cfgIpsec.roleMode.initiator) {
            return;
        }
        conn.cookieI = bits.randomQ();
        conn.MMsecAssFill();
        conn.secAssCreate(true);
        conn.hash2sai = conn.secAssRead(true);
        conn.headerCreate();
    }

    private void respRx() {
        clearState();
        for (;;) {
            if (doKeepalive()) {
                return;
            }
            packIsakmp pckRx = conn.copyBytes();
            if (recvIke(pckRx)) {
                continue;
            }
            if (pckRx.headerParse()) {
                continue;
            }
            if (!pckRx.secAssParse(true)) {
                clearState();
                conn.cookieI = pckRx.cookieI;
                conn.cookieR = bits.randomQ();
                packIsakmp pckTx = conn.copyBytes();
                pckTx.MMsecAssFill();
                pckTx.secAssCreate(true);
                pckTx.headerCreate();
                sendIke(pckTx, false);
                conn.hash2sai = pckRx.secAssRead(false);
                continue;
            }
            if (!pckRx.checkPeer(conn)) {
                logger.info("invalid cookies");
                continue;
            }
            if (!pckRx.keyXchgParse()) {
                conn.diffie = pckRx.diffie;
                if (pckRx.nonceParse()) {
                    continue;
                }
                conn.nonceI = pckRx.nonceI;
                conn.nonceR = packIsakmp.nonceFill();
                packIsakmp pckTx = conn.copyBytes();
                pckTx.dedPerCapCreate();
                pckTx.nonceI = conn.nonceR;
                pckTx.nonceCreate();
                pckTx.keyXchgFill();
                pckTx.keyXchgCreate();
                pckTx.headerCreate();
                sendIke(pckTx, false);
                conn.computeKeys();
                continue;
            }
            if (pckRx.doDecrypt()) {
                logger.info("failed to decrypt packet");
                continue;
            }
            if (pckRx.xchgTyp == packIsakmp.xchgInfo) {
                doInform(pckRx);
                continue;
            }
            if (pckRx.msgId == 0) {
                if (pckRx.MMidentParse()) {
                    continue;
                }
                pckRx.hash2idi = pckRx.identRead(1);
                if (pckRx.hashParse()) {
                    continue;
                }
                if (pckRx.hashVerify(pckRx.hashGenMM(true))) {
                    continue;
                }
                pckRx.pckDat.clear();
                pckRx.MMidentFill(localAddr);
                pckRx.MMidentCreate();
                packIsakmp pckTx = conn.copyBytes();
                pckTx.phase1iv2 = pckRx.phase1iv2;
                pckTx.hash2idi = pckRx.identRead(0);
                pckTx.hash2got = pckTx.hashGenMM(false);
                pckTx.hashCreate();
                pckTx.MMidentFill(localAddr);
                pckTx.MMidentCreate();
                pckTx.doEncrypt();
                pckTx.headerCreate();
                sendIke(pckTx, false);
                conn.phase1iv2 = pckTx.phase1iv2;
                continue;
            }
            if (pckRx.msgId == conn.phase2id) {
                if (pckRx.hashParse()) {
                    continue;
                }
                if (pckRx.hashVerify(conn.hash3genQM())) {
                    continue;
                }
                continue;
            }
            if (pckRx.secAssParse(false)) {
                continue;
            }
            if (pckRx.nonceParse()) {
                continue;
            }
            if (pckRx.QMidentParse(1)) {
                continue;
            }
            if (pckRx.QMidentParse(2)) {
                continue;
            }
            if (pckRx.hashParse()) {
                continue;
            }
            if (pckRx.hashVerify(pckRx.hash1genQM())) {
                continue;
            }
            conn.nonceI = pckRx.nonceI;
            conn.nonceR = packIsakmp.nonceFill();
            conn.spiValI = pckRx.spiValI;
            conn.spiValR = bits.randomD();
            packIsakmp pckTx = conn.copyBytes();
            pckTx.nonceI = conn.nonceR;
            pckTx.nonceR = conn.nonceI;
            pckTx.spiValI = conn.spiValR;
            pckTx.phase2id = pckRx.phase2id;
            pckTx.phase2iv = pckRx.phase2iv;
            pckTx.msgId = pckRx.msgId;
            pckTx.spiValI = conn.spiValR;
            pckTx.QMidentFill(ipv6);
            pckTx.QMidentCreate();
            pckTx.QMidentCreate();
            pckTx.nonceCreate();
            pckTx.QMsecAssFill();
            pckTx.secAssCreate(false);
            pckTx.hash2got = pckTx.hash2genQM();
            pckTx.hashCreate();
            pckTx.doEncrypt();
            pckTx.headerCreate();
            sendIke(pckTx, false);
            conn.phase2iv = pckTx.phase2iv;
            conn.phase2id = pckTx.phase2id;
            makeEsps();
        }
    }

    private void respTx() {
        for (;;) {
            bits.sleep(1000);
            if (doKeepalive()) {
                return;
            }
        }
    }

    private void initRx() {
        clearState();
        for (;;) {
            if (doKeepalive()) {
                return;
            }
            packIsakmp pckRx = conn.copyBytes();
            if (recvIke(pckRx)) {
                continue;
            }
            if (pckRx.headerParse()) {
                continue;
            }
            if (pckRx.cookieI != conn.cookieI) {
                continue;
            }
            conn.cookieR = pckRx.cookieR;
            if (!pckRx.secAssParse(true)) {
                conn.nonceI = packIsakmp.nonceFill();
                conn.keyXchgFill();
                packIsakmp pckTx = conn.copyBytes();
                pckTx.dedPerCapCreate();
                pckTx.nonceCreate();
                pckTx.keyXchgFill();
                pckTx.keyXchgCreate();
                pckTx.headerCreate();
                sendIke(pckTx, false);
                continue;
            }
            if (!pckRx.checkPeer(conn)) {
                logger.info("invalid cookies");
                continue;
            }
            if (!pckRx.keyXchgParse()) {
                conn.diffie = pckRx.diffie;
                if (pckRx.nonceParse()) {
                    continue;
                }
                conn.nonceR = pckRx.nonceI;
                conn.computeKeys();
                pckRx.pckDat.clear();
                pckRx.MMidentFill(localAddr);
                pckRx.MMidentCreate();
                conn.hash2idi = pckRx.identRead(0);
                packIsakmp pckTx = conn.copyBytes();
                pckTx.hash2idi = pckRx.identRead(0);
                pckTx.hash2got = pckTx.hashGenMM(true);
                pckTx.hashCreate();
                pckTx.MMidentFill(localAddr);
                pckTx.MMidentCreate();
                pckTx.doEncrypt();
                pckTx.headerCreate();
                conn.phase1iv2 = pckTx.phase1iv2;
                sendIke(pckTx, false);
                continue;
            }
            if (pckRx.doDecrypt()) {
                logger.info("failed to decrypt packet");
                continue;
            }
            if (pckRx.xchgTyp == packIsakmp.xchgInfo) {
                doInform(pckRx);
                continue;
            }
            if (pckRx.msgId == 0) {
                if (pckRx.MMidentParse()) {
                    continue;
                }
                conn.hash2idi = pckRx.identRead(1);
                if (pckRx.hashParse()) {
                    continue;
                }
                if (pckRx.hashVerify(conn.hashGenMM(false))) {
                    continue;
                }
                conn.msgId = bits.randomD();
                conn.nonceI = packIsakmp.nonceFill();
                conn.spiValI = bits.randomD();
                packIsakmp pckTx = conn.copyBytes();
                pckTx.QMidentFill(ipv6);
                pckTx.QMidentCreate();
                pckTx.QMidentCreate();
                pckTx.nonceCreate();
                pckTx.QMsecAssFill();
                pckTx.secAssCreate(false);
                pckTx.hash2got = pckTx.hash1genQM();
                pckTx.hashCreate();
                pckTx.doEncrypt();
                pckTx.headerCreate();
                sendIke(pckTx, false);
                conn.phase2iv = pckTx.phase2iv;
                conn.phase2id = pckTx.phase2id;
                continue;
            }
            if (pckRx.msgId != conn.msgId) {
                logger.info("invalid message id");
                continue;
            }
            if (pckRx.secAssParse(false)) {
                continue;
            }
            if (pckRx.nonceParse()) {
                continue;
            }
            if (pckRx.QMidentParse(1)) {
                continue;
            }
            if (pckRx.QMidentParse(2)) {
                continue;
            }
            if (pckRx.hashParse()) {
                continue;
            }
            conn.spiValR = pckRx.spiValI;
            conn.nonceR = pckRx.nonceI;
            pckRx.nonceR = conn.nonceI;
            if (pckRx.hashVerify(pckRx.hash2genQM())) {
                continue;
            }
            packIsakmp pckTx = conn.copyBytes();
            pckTx.hash2got = pckTx.hash3genQM();
            pckTx.hashCreate();
            pckTx.doEncrypt();
            pckTx.headerCreate();
            sendIke(pckTx, false);
            stopRetrans();
            makeEsps();
        }
    }

    private void initTx() {
        for (;;) {
            bits.sleep(5000);
            if (doKeepalive()) {
                return;
            }
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
        if (role == cfgIpsec.roleMode.initiator) {
            initTx();
        } else {
            respTx();
        }
    }

}

class secIsakmpRx implements Runnable {

    private secIsakmp lower;

    public secIsakmpRx(secIsakmp parent) {
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

class secIsakmpTx implements Runnable {

    private secIsakmp lower;

    public secIsakmpTx(secIsakmp parent) {
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
