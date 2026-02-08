package org.freertr.serv;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.auth.autherChap;
import org.freertr.cfg.cfgAll;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.pack.packHolder;
import org.freertr.pack.packL2tp;
import org.freertr.pack.packL2tp3;
import org.freertr.pack.packLdpPwe;
import org.freertr.tab.tabGen;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.notifier;

/**
 * layer two tunneling protocol v3 (rfc3931) connection
 *
 * @author matecsaba
 */
public class servL2tp3conn implements Runnable, Comparable<servL2tp3conn> {

    /**
     * lower layer
     */
    protected final servL2tp3 lower;

    /**
     * interface to use
     */
    protected final ipFwdIface iface;

    /**
     * peer address
     */
    protected final addrIP peer;

    /**
     * list of sessions
     */
    protected final tabGen<servL2tp3sess> session = new tabGen<servL2tp3sess>();

    /**
     * connection id local
     */
    protected int conLoc = 0;

    /**
     * connection id remote
     */
    protected int conRem = 0;

    /**
     * time created
     */
    protected long created;

    private boolean need2run;

    private List<packL2tp3> queue = new ArrayList<packL2tp3>();

    private counter cntr = new counter();

    private notifier notif = new notifier();

    private int txed = 0;

    private int keep = 0;

    private int seqRx = 0;

    private int seqTx = 0;

    private ipFwd fwdCor;

    private byte[] chlng = null;

    /**
     * create instance
     *
     * @param ifc interface
     * @param adr address
     * @param parent parent
     */
    public servL2tp3conn(ipFwdIface ifc, addrIP adr, servL2tp3 parent) {
        iface = ifc;
        peer = adr.copyBytes();
        lower = parent;
    }

    public String toString() {
        return lower + " with " + peer;
    }

    public int compareTo(servL2tp3conn o) {
        int i = iface.compareTo(o.iface);
        if (i != 0) {
            return i;
        }
        return peer.compareTo(o.peer);
    }

    /**
     * close connection
     */
    protected void setClosed() {
        need2run = false;
        notif.wakeup();
        for (int i = session.size(); i >= 0; i--) {
            servL2tp3sess ses = session.get(i);
            if (ses == null) {
                continue;
            }
            ses.closeDn();
        }
        lower.conns.del(this);
        fwdCor.protoDel(lower, iface, peer);
    }

    /**
     * start peer
     */
    protected void doStartup() {
        if (debugger.servL2tp3traf) {
            logger.debug("starting");
        }
        need2run = true;
        fwdCor = lower.srvVrf.getFwd(peer);
        conLoc = bits.randomD();
        if (lower.password != null) {
            chlng = new byte[16];
            for (int i = 0; i < chlng.length; i++) {
                chlng[i] = (byte) bits.randomB();
            }
        }
        created = bits.getTime();
        logger.startThread(this);
    }

    public void run() {
        if (lower.srvCheckAcceptIp(iface, peer, lower)) {
            setClosed();
            return;
        }
        try {
            for (;;) {
                doWork();
                if (!need2run) {
                    break;
                }
                notif.sleep(5000);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        setClosed();
        if (debugger.servL2tp3traf) {
            logger.debug("disconnected");
        }
    }

    /**
     * queue one packet
     *
     * @param pck packet to enqueue
     */
    protected void enQueue(packL2tp3 pck) {
        synchronized (queue) {
            queue.add(pck);
        }
    }

    /**
     * send acknowledge
     */
    protected void sendAck() {
        packL2tp3 pckTx = new packL2tp3();
        pckTx.patchHeader(conRem, seqRx, seqTx);
        packHolder pckBin = new packHolder(true, true);
        pckTx.createHeader(pckBin);
        cntr.tx(pckBin);
        sendProto(pckBin);
    }

    /**
     * send protocol packet
     *
     * @param pck packet to send
     */
    protected void sendProto(packHolder pck) {
        pck.merge2beg();
        if (lower.sendingTTL >= 0) {
            pck.IPttl = lower.sendingTTL;
        }
        if (lower.sendingTOS >= 0) {
            pck.IPtos = lower.sendingTOS;
        }
        if (lower.sendingDFN >= 0) {
            pck.IPdf = lower.sendingDFN == 1;
        }
        if (lower.sendingFLW >= 0) {
            pck.IPid = lower.sendingFLW;
        }
        pck.IPprt = packL2tp3.prot;
        pck.IPsrc.setAddr(iface.addr);
        pck.IPtrg.setAddr(peer);
        fwdCor.protoPack(iface, null, pck);
    }

    /**
     * perform work
     */
    protected void doWork() {
        packL2tp3 pckTx;
        packHolder pckBin = new packHolder(true, true);
        synchronized (queue) {
            if (queue.size() < 1) {
                keep++;
                if (keep < lower.helloTicks) {
                    return;
                }
                keep = 0;
                enQueue(packL2tp3.createHELLO());
                return;
            }
            pckTx = queue.get(0);
            pckTx.patchHeader(conRem, seqRx, seqTx);
            pckTx.createTLVs(pckBin);
            pckTx.createHeader(pckBin);
            cntr.tx(pckBin);
            sendProto(pckBin);
            txed++;
        }
        if (debugger.servL2tp3traf) {
            logger.debug("tx " + pckTx.dump());
        }
        if (txed < lower.retryTicks) {
            return;
        }
        need2run = false;
    }

    /**
     * receive one packet
     *
     * @param pckBin packet received
     */
    protected void doRecv(packHolder pckBin) {
        packL2tp3 pckRx = new packL2tp3();
        if (pckRx.parseHeader(pckBin)) {
            cntr.drop(pckBin, counter.reasons.badHdr);
            return;
        }
        keep = 0;
        if (!pckRx.ctrl) {
            servL2tp3sess ntry = new servL2tp3sess(this);
            ntry.sesLoc = pckRx.sesID;
            ntry = session.find(ntry);
            if (ntry == null) {
                return;
            }
            ntry.doRecv(pckBin);
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
        if (pckRx.seqTx != seqRx) {
            cntr.drop(pckBin, counter.reasons.badRxSeq);
            if (pckRx.valMsgTyp != packL2tp.typHELLO) {
                return;
            }
            sendAck();
            return;
        }
        cntr.rx(pckBin);
        if (debugger.servL2tp3traf) {
            logger.debug("rx " + pckRx.dump());
        }
        if (pckRx.valMsgTyp == packL2tp.typZLB) {
            return;
        }
        seqRx = (seqRx + 1) & 0xffff;
        byte[] res = null;
        switch (pckRx.valMsgTyp) {
            case packL2tp.typHELLO:
                sendAck();
                break;
            case packL2tp.typSCCRQ:
                conRem = pckRx.valConnId;
                if (chlng != null) {
                    if (pckRx.valChallen == null) {
                        return;
                    }
                    res = autherChap.calcAuthHash(pckRx.valMsgTyp + 1, lower.password, pckRx.valChallen);
                }
                enQueue(packL2tp3.createSCCRP(conLoc, iface.addr.toIPv4(), cfgAll.hostName, chlng, res));
                break;
            case packL2tp.typSCCCN:
                if (chlng != null) {
                    if (pckRx.valResponse == null) {
                        return;
                    }
                    res = autherChap.calcAuthHash(pckRx.valMsgTyp, lower.password, chlng);
                    if (res.length != pckRx.valResponse.length) {
                        return;
                    }
                    if (bits.byteComp(res, 0, pckRx.valResponse, 0, res.length) != 0) {
                        return;
                    }
                }
                sendAck();
                break;
            case packL2tp.typSCCNO:
                need2run = false;
                sendAck();
                return;
            case packL2tp.typCDN:
                servL2tp3sess ntry = new servL2tp3sess(this);
                ntry.sesLoc = pckRx.valRemSesId;
                ntry = session.find(ntry);
                if (ntry != null) {
                    ntry.closeDn();
                }
                sendAck();
                break;
            case packL2tp.typICCN:
                sendAck();
                break;
            case packL2tp.typICRQ:
                ntry = newSess(pckRx);
                if (ntry == null) {
                    enQueue(packL2tp3.createCDN(pckRx.valLocSesId, bits.randomW()));
                } else {
                    enQueue(packL2tp3.createICRP(ntry.sesRem, ntry.sesLoc, ntry.vcid, ntry.pwType));
                    enQueue(packL2tp3.createSLI(ntry.sesRem, ntry.sesLoc));
                }
                break;
            case packL2tp.typOCRQ:
                enQueue(packL2tp3.createCDN(pckRx.valLocSesId, bits.randomW()));
                break;
            case packL2tp.typSLI:
                sendAck();
                break;
        }
        notif.wakeup();
    }

    /**
     * create new session
     *
     * @param pck packet to use
     * @return session created
     */
    protected servL2tp3sess newSess(packL2tp3 pck) {
        servL2tp3sess ntry = new servL2tp3sess(this);
        ntry.sesRem = pck.valLocSesId;
        ntry.pwType = pck.valPwTyp;
        ntry.vcid = pck.valRemEndId;
        switch (ntry.pwType) {
            case packLdpPwe.pwtEthPort:
            case packLdpPwe.pwtEthVlan:
                if (lower.brdgIfc == null) {
                    return null;
                }
                ntry.brdgIfc = lower.brdgIfc.bridgeHed.newIface(lower.physInt, true, false);
                ntry.setUpper(ntry.brdgIfc);
                break;
            case packLdpPwe.pwtHdlc:
            case packLdpPwe.pwtPpp:
            case packLdpPwe.pwtFrDlci:
            case packLdpPwe.pwtAtmAal5:
                if (lower.dialIfc == null) {
                    return null;
                }
                ntry.dialIfc = lower.dialIfc.cloneStart(ntry);
                break;
            default:
                return null;
        }
        for (;;) {
            ntry.sesLoc = bits.randomD();
            if (session.add(ntry) != null) {
                continue;
            }
            return ntry;
        }
    }

}
