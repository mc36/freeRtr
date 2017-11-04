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
import ip.ipFwdIface;
import java.util.ArrayList;
import java.util.List;
import pack.packHolder;
import pack.packL2tp;
import pack.packL2tp2;
import prt.prtGenConn;
import prt.prtServP;
import prt.prtUdp;
import serv.servL2tp2;
import user.userTerminal;
import util.bits;
import util.counter;
import util.debugger;
import util.logger;
import util.notifier;
import util.state;

/**
 * layer two tunneling protocol (rfc2661) client
 *
 * @author matecsaba
 */
public class clntL2tp2 implements Runnable, prtServP, ifcDn {

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
     * direction, true=outgoing, false=incoming
     */
    public boolean direction = true;

    /**
     * telephone number
     */
    public String called;

    /**
     * telephone number
     */
    public String calling;

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

    private boolean working = true;

    private prtGenConn conn;

    private List<packL2tp2> queue;

    private int seqRx;

    private int seqTx;

    private int tunLoc;

    private int tunRem;

    private int sesLoc;

    private int sesRem;

    private int keep;

    private int txed;

    private notifier notif;

    private packL2tp2 pckRx;

    public String toString() {
        return "l2tp2 to " + target;
    }

    public addrType getHwAddr() {
        return new addrEmpty();
    }

    public void setFilter(boolean promisc) {
    }

    public state.states getState() {
        return state.states.up;
    }

    public void closeDn() {
        clearState();
    }

    public void flapped() {
        clearState();
    }

    public void setUpper(ifcUp server) {
        upper = server;
        upper.setParent(this);
    }

    public counter getCounter() {
        return cntr;
    }

    public int getMTUsize() {
        return 1400;
    }

    public long getBandwidth() {
        return 8000000;
    }

    public void sendPack(packHolder pck) {
        if (sesRem == 0) {
            return;
        }
        packL2tp2 tx = new packL2tp2();
        tx.ctrl = false;
        tx.sesID = sesRem;
        tx.tunID = tunRem;
        tx.createHeader(pck);
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
        addrIP trg = userTerminal.justResolv(target, 0);
        if (trg == null) {
            return;
        }
        prtUdp udp = vrf.getUdp(trg);
        servL2tp2 srv = new servL2tp2();
        ipFwdIface fwdIfc = null;
        if (srcIfc != null) {
            fwdIfc = srcIfc.getFwdIfc(trg);
        }
        conn = udp.packetConnect(this, fwdIfc, 0, trg, srv.srvPort(), srv.srvName(), null, -1);
        if (conn == null) {
            return;
        }
        conn.timeout = 120000;
        tunLoc = bits.randomW();
        byte[] chlng = null;
        if (password != null) {
            chlng = new byte[16];
            for (int i = 0; i < chlng.length; i++) {
                chlng[i] = (byte) bits.randomB();
            }
        }
        enQueue(packL2tp2.createSCCRQ(tunLoc, hostname == null ? cfgAll.hostName : hostname, chlng));
        if (wait4msg(packL2tp.typSCCRP)) {
            return;
        }
        tunRem = pckRx.valTunId;
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
        enQueue(packL2tp2.createSCCCN(res));
        sesLoc = bits.randomW();
        if (direction) {
            // outgoint
            enQueue(packL2tp2.createOCRQ(sesLoc, called, calling));
            if (wait4msg(packL2tp.typOCRP)) {
                return;
            }
            sesRem = pckRx.valSesId;
            sendAck();
            if (wait4msg(packL2tp.typOCCN)) {
                return;
            }
        } else {
            // incoming
            enQueue(packL2tp2.createICRQ(sesLoc, called, calling));
            if (wait4msg(packL2tp.typICRP)) {
                return;
            }
            sesRem = pckRx.valSesId;
            enQueue(packL2tp2.createICCN(sesRem));
        }
        sendAck();
        for (;;) {
            if (conn.txBytesFree() < 0) {
                return;
            }
            pckRx.valMsgTyp = 0;
            notif.sleep(1000);
            switch (pckRx.valMsgTyp) {
                case packL2tp.typHELLO:
                    sendAck();
                    break;
                case packL2tp.typICRQ:
                case packL2tp.typOCRQ:
                    enQueue(packL2tp2.createCDN(pckRx.valSesId, bits.randomW()));
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

    private void enQueue(packL2tp2 pck) {
        synchronized (queue) {
            queue.add(pck);
        }
    }

    private void sendAck() {
        packL2tp2 pckTx = new packL2tp2();
        pckTx.patchHeader(tunRem, seqRx, seqTx);
        packHolder pckBin = new packHolder(true, true);
        pckTx.createHeader(pckBin);
        cntr.tx(pckBin);
        conn.send2net(pckBin);
    }

    private boolean wait4msg(int typ) {
        for (;;) {
            notif.sleep(1000);
            if (pckRx.valMsgTyp == typ) {
                return false;
            }
            if (conn.txBytesFree() < 0) {
                return true;
            }
        }
    }

    private void clearState() {
        if (conn != null) {
            conn.setClosing();
        }
        queue = new ArrayList<packL2tp2>();
        seqRx = 0;
        seqTx = 0;
        tunLoc = 0;
        tunRem = 0;
        sesLoc = 0;
        sesRem = 0;
        keep = 0;
        txed = 0;
        notif = new notifier();
        pckRx = new packL2tp2();
    }

    public void closedInterface(ipFwdIface ifc) {
    }

    public boolean datagramAccept(prtGenConn id) {
        return true;
    }

    public void datagramReady(prtGenConn id) {
    }

    public void datagramClosed(prtGenConn id) {
    }

    public void datagramWork(prtGenConn id) {
        packL2tp2 pckTx;
        packHolder pckBin = new packHolder(true, true);
        synchronized (queue) {
            if (queue.size() < 1) {
                keep++;
                if (keep < 5) {
                    return;
                }
                keep = 0;
                enQueue(packL2tp2.createHELLO());
                return;
            }
            pckTx = queue.get(0);
            pckTx.patchHeader(tunRem, seqRx, seqTx);
            pckTx.createTLVs(pckBin);
            pckTx.createHeader(pckBin);
            cntr.tx(pckBin);
            txed++;
        }
        conn.send2net(pckBin);
        if (debugger.clntL2tp2traf) {
            logger.debug("tx " + pckTx.dump());
        }
        if (txed < 8) {
            return;
        }
        conn.setClosing();
    }

    public boolean datagramRecv(prtGenConn id, packHolder pckBin) {
        pckRx = new packL2tp2();
        if (pckRx.parseHeader(pckBin)) {
            cntr.drop(pckBin, counter.reasons.badHdr);
            return false;
        }
        if (pckRx.tunID != tunLoc) {
            cntr.drop(pckBin, counter.reasons.badID);
            return false;
        }
        keep = 0;
        if (!pckRx.ctrl) {
            if (pckRx.sesID != sesLoc) {
                cntr.drop(pckBin, counter.reasons.badID);
                return false;
            }
            cntr.rx(pckBin);
            upper.recvPack(pckBin);
            return false;
        }
        if (pckRx.seqTx != seqRx) {
            cntr.drop(pckBin, counter.reasons.badRxSeq);
            return false;
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
        if (debugger.clntL2tp2traf) {
            logger.debug("rx " + pckRx.dump());
        }
        if (pckRx.valMsgTyp == packL2tp.typZLB) {
            return false;
        }
        seqRx = (seqRx + 1) & 0xffff;
        notif.wakeup();
        return false;
    }

}
