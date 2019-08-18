package clnt;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrType;
import auth.autherChap;
import cfg.cfgAll;
import cfg.cfgIfc;
import cfg.cfgVrf;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipFwdTab;
import ip.ipPrt;
import java.util.ArrayList;
import java.util.List;
import pack.packHolder;
import pack.packL2tp;
import pack.packL2tp3;
import pack.packLdpPwe;
import user.userTerminal;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.notifier;
import util.state;

/**
 * layer two tunneling protocol v3 (rfc3931) client
 *
 * @author matecsaba
 */
public class clntL2tp3 implements Runnable, ipPrt, ifcDn {

    /**
     * upper layer
     */
    public ifcUp upper = new ifcNull();

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
     * vc id
     */
    public String vcid;

    /**
     * pseudowire type
     */
    public int pwType;

    /**
     * direction, true=outgoing, false=incoming
     */
    public boolean direction = true;

    /**
     * hostname
     */
    public String hostname;

    /**
     * password
     */
    public String password;

    /**
     * counter
     */
    public counter cntr = new counter();

    /**
     * sending ttl value, -1 means maps out
     */
    public int sendingTTL = 255;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingTOS = -1;

    private boolean working = true;

    private ipFwd fwdCor;

    private addrIP fwdTrg;

    private ipFwdIface fwdIfc;

    private List<packL2tp3> queue;

    private int seqRx;

    private int seqTx;

    private int conLoc;

    private int conRem;

    private int sesLoc;

    private int sesRem;

    private int keep;

    private int txed;

    private notifier notif;

    private packL2tp3 pckRx;

    public String toString() {
        return "l2tp3 to " + fwdTrg;
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
    public void sendPack(packHolder pck) {
        pck.merge2beg();
        if (sesRem == 0) {
            return;
        }
        if (pwType == packLdpPwe.pwtPpp) {
            pck.getSkip(2);
        }
        packL2tp3 tx = new packL2tp3();
        tx.ctrl = false;
        tx.sesID = sesRem;
        tx.createHeader(pck);
        cntr.tx(pck);
        pck.putDefaults();
        sendProto(pck);
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
        fwdTrg = userTerminal.justResolv(target, 0);
        if (fwdTrg == null) {
            return;
        }
        fwdCor = vrf.getFwd(fwdTrg);
        fwdIfc = null;
        if (srcIfc != null) {
            fwdIfc = srcIfc.getFwdIfc(fwdTrg);
        }
        if (fwdIfc == null) {
            fwdIfc = ipFwdTab.findSendingIface(fwdCor, fwdTrg);
        }
        if (fwdIfc == null) {
            return;
        }
        fwdCor.protoAdd(this, fwdIfc, fwdTrg);
        conLoc = bits.randomD();
        sesLoc = bits.randomD();
        byte[] chlng = null;
        if (password != null) {
            chlng = new byte[16];
            for (int i = 0; i < chlng.length; i++) {
                chlng[i] = (byte) bits.randomB();
            }
        }
        if (direction) {
            // outgoing
            enQueue(packL2tp3.createSCCRQ(conLoc, fwdIfc.addr.toIPv4(), hostname == null ? cfgAll.hostName : hostname, packL2tp.maxTieBreak, chlng));
            if (wait4msg(packL2tp.typSCCRP)) {
                return;
            }
            conRem = pckRx.valConnId;
            byte[] res = null;
            if (password != null) {
                if (pckRx.valResponse == null) {
                    return;
                }
                res = autherChap.calcAuthHash(pckRx.valMsgTyp, password, chlng);
                if (res.length != pckRx.valResponse.length) {
                    return;
                }
                if (bits.byteComp(res, 0, pckRx.valResponse, 0, res.length) != 0) {
                    return;
                }
                res = autherChap.calcAuthHash(pckRx.valMsgTyp + 1, password, pckRx.valChallen);
            }
            enQueue(packL2tp3.createSCCCN(res));
            enQueue(packL2tp3.createICRQ(sesLoc, vcid, pwType, packL2tp.maxTieBreak));
            if (wait4msg(packL2tp.typICRP)) {
                return;
            }
            sesRem = pckRx.valLocSesId;
            enQueue(packL2tp3.createICCN(sesRem, sesLoc));
        } else {
            // incoming
            if (wait4msg(packL2tp.typSCCRQ)) {
                return;
            }
            conRem = pckRx.valConnId;
            byte[] res = null;
            if (password != null) {
                if (pckRx.valChallen == null) {
                    return;
                }
                res = autherChap.calcAuthHash(pckRx.valMsgTyp + 1, password, pckRx.valChallen);
            }
            enQueue(packL2tp3.createSCCRP(conLoc, fwdIfc.addr.toIPv4(), hostname == null ? cfgAll.hostName : hostname, chlng, res));
            if (wait4msg(packL2tp.typSCCCN)) {
                return;
            }
            if (password != null) {
                if (pckRx.valResponse == null) {
                    return;
                }
                res = autherChap.calcAuthHash(pckRx.valMsgTyp, password, chlng);
                if (res.length != pckRx.valResponse.length) {
                    return;
                }
                if (bits.byteComp(res, 0, pckRx.valResponse, 0, res.length) != 0) {
                    return;
                }
            }
            sendAck();
            if (wait4msg(packL2tp.typICRQ)) {
                return;
            }
            if (pckRx.valPwTyp != pwType) {
                return;
            }
            if (!vcid.equals(pckRx.valRemEndId)) {
                return;
            }
            sesRem = pckRx.valLocSesId;
            enQueue(packL2tp3.createICRP(sesRem, sesLoc, vcid, pwType));
            if (wait4msg(packL2tp.typICCN)) {
                return;
            }
        }
        sendAck();
        enQueue(packL2tp3.createSLI(sesRem, sesLoc));
        for (;;) {
            if (txDoer()) {
                return;
            }
            pckRx.valMsgTyp = 0;
            notif.sleep(1000);
            switch (pckRx.valMsgTyp) {
                case packL2tp.typHELLO:
                    sendAck();
                    break;
                case packL2tp.typICCN:
                    sendAck();
                    break;
                case packL2tp.typICRQ:
                case packL2tp.typOCRQ:
                    enQueue(packL2tp3.createCDN(pckRx.valLocSesId, bits.randomW()));
                    break;
                case packL2tp.typSLI:
                    sendAck();
                    break;
                case packL2tp.typCDN:
                case packL2tp.typSCCNO:
                    sendAck();
                    return;
            }
        }
    }

    private boolean wait4msg(int typ) {
        pckRx.valMsgTyp = -1;
        for (int rnd = 0;; rnd++) {
            if (rnd > 10) {
                return true;
            }
            if (txDoer()) {
                return true;
            }
            notif.sleep(5000);
            if (pckRx.valMsgTyp == typ) {
                return false;
            }
        }
    }

    private void enQueue(packL2tp3 pck) {
        synchronized (queue) {
            queue.add(pck);
        }
    }

    private void sendAck() {
        packL2tp3 pckTx = new packL2tp3();
        pckTx.patchHeader(conRem, seqRx, seqTx);
        packHolder pckBin = new packHolder(true, true);
        pckTx.createHeader(pckBin);
        cntr.tx(pckBin);
        sendProto(pckBin);
    }

    private void clearState() {
        if (fwdIfc != null) {
            fwdCor.protoDel(this, fwdIfc, fwdTrg);
            fwdIfc = null;
        }
        queue = new ArrayList<packL2tp3>();
        seqRx = 0;
        seqTx = 0;
        sesLoc = 0;
        sesRem = 0;
        sesLoc = 0;
        sesRem = 0;
        keep = 0;
        txed = 0;
        notif = new notifier();
        pckRx = new packL2tp3();
    }

    private boolean txDoer() {
        if (!working) {
            return true;
        }
        packL2tp3 pckTx;
        packHolder pckBin = new packHolder(true, true);
        synchronized (queue) {
            if (queue.size() < 1) {
                keep++;
                if (keep < 15) {
                    return false;
                }
                keep = 0;
                if (conRem == 0) {
                    return false;
                }
                enQueue(packL2tp3.createHELLO());
                return false;
            }
            pckTx = queue.get(0);
            pckTx.patchHeader(conRem, seqRx, seqTx);
            pckTx.createTLVs(pckBin);
            pckTx.createHeader(pckBin);
            cntr.tx(pckBin);
            txed++;
        }
        sendProto(pckBin);
        if (debugger.clntL2tp3traf) {
            logger.debug("tx " + pckTx.dump());
        }
        if (txed < 8) {
            return false;
        }
        return true;
    }

    private void sendProto(packHolder pck) {
        pck.merge2beg();
        if (sendingTTL >= 0) {
            pck.IPttl = sendingTTL;
        }
        if (sendingTOS >= 0) {
            pck.IPtos = sendingTOS;
        }
        pck.IPprt = packL2tp3.prot;
        pck.IPsrc.setAddr(fwdIfc.addr);
        pck.IPtrg.setAddr(fwdTrg);
        fwdCor.protoPack(fwdIfc, pck);
    }

    /**
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return packL2tp3.prot;
    }

    /**
     * close interface
     *
     * @param iface
     */
    public void closeUp(ipFwdIface iface) {
    }

    /**
     * set state
     *
     * @param iface interface
     * @param stat state
     */
    public void setState(ipFwdIface iface, state.states stat) {
    }

    /**
     * received packet
     *
     * @param rxIfc interface
     * @param pckBin packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pckBin) {
        pckRx = new packL2tp3();
        if (pckRx.parseHeader(pckBin)) {
            cntr.drop(pckBin, counter.reasons.badHdr);
            return;
        }
        keep = 0;
        if (!pckRx.ctrl) {
            if (pckRx.sesID != sesLoc) {
                cntr.drop(pckBin, counter.reasons.badID);
                return;
            }
            if (pwType == packLdpPwe.pwtPpp) {
                pckBin.msbPutW(0, 0xff03);
                pckBin.putSkip(2);
                pckBin.merge2beg();
            }
            cntr.rx(pckBin);
            upper.recvPack(pckBin);
            return;
        }
        if (pckRx.seqTx != seqRx) {
            cntr.drop(pckBin, counter.reasons.badRxSeq);
            return;
        }
        synchronized (queue) {
            if ((pckRx.seqRx == ((seqTx + 1) & 0xffff)) && (queue.size() > 0)) {
                seqTx = (seqTx + 1) & 0xffff;
                txed = 0;
                queue.remove(0);
            }
        }
        pckRx.parseTLVs(pckBin);
        cntr.rx(pckBin);
        if (debugger.clntL2tp3traf) {
            logger.debug("rx " + pckRx.dump());
        }
        if (pckRx.valMsgTyp == packL2tp.typZLB) {
            return;
        }
        seqRx = (seqRx + 1) & 0xffff;
        notif.wakeup();
    }

    /**
     * alert packet
     *
     * @param rxIfc interface
     * @param pck packet
     * @return false on success, true on error
     */
    public boolean alertPack(ipFwdIface rxIfc, packHolder pck) {
        return true;
    }

    /**
     * error packet
     *
     * @param err error code
     * @param rtr address
     * @param rxIfc interface
     * @param pck packet
     */
    public void errorPack(counter.reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck) {
    }

}
