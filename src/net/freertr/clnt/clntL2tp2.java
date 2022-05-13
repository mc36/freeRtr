package net.freertr.clnt;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrEmpty;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrType;
import net.freertr.auth.autherChap;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgVrf;
import net.freertr.ifc.ifcDn;
import net.freertr.ifc.ifcNull;
import net.freertr.ifc.ifcUp;
import net.freertr.ip.ipFwdIface;
import net.freertr.pack.packHolder;
import net.freertr.pack.packL2tp;
import net.freertr.pack.packL2tp2;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServP;
import net.freertr.prt.prtUdp;
import net.freertr.serv.servL2tp2;
import net.freertr.user.userFormat;
import net.freertr.user.userTerminal;
import net.freertr.util.bits;
import net.freertr.util.counter;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.notifier;
import net.freertr.util.state;

/**
 * layer two tunneling protocol (rfc2661) client
 *
 * @author matecsaba
 */
public class clntL2tp2 implements Runnable, prtServP, ifcDn {

    /**
     * create instance
     */
    public clntL2tp2() {
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
     * flap connection
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
     * get local port number
     *
     * @return session id, 0 if no session
     */
    public int getPortLoc() {
        if (sesRem == 0) {
            return 0;
        }
        return conn.portLoc;
    }

    /**
     * get remote port number
     *
     * @return session id, 0 if no session
     */
    public int getPortRem() {
        if (sesRem == 0) {
            return 0;
        }
        return conn.portRem;
    }

    /**
     * get remote session id
     *
     * @return session id, 0 if no session
     */
    public int getSessRem() {
        return sesRem;
    }

    /**
     * get remote tunn id
     *
     * @return session id, 0 if no tunnel
     */
    public int getTunnRem() {
        return tunRem;
    }

    /**
     * send packet
     *
     * @param pck packet
     */
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
        addrIP trg = userTerminal.justResolv(target, prefer);
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
        conn.sendFLW = sendingFLW;
        conn.sendTOS = sendingTOS;
        conn.sendTTL = sendingTTL;
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
            res = autherChap.calcAuthHash(pckRx.valMsgTyp, password, chlng);
            if (pckRx.valResponse == null) {
                return;
            }
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
     * close connection
     *
     * @param id connection
     */
    public void datagramClosed(prtGenConn id) {
    }

    /**
     * connection work
     *
     * @param id connection
     */
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
        synchronized (queue) {
            if ((pckRx.seqRx == ((seqTx + 1) & 0xffff)) && (queue.size() > 0)) {
                seqTx = (seqTx + 1) & 0xffff;
                txed = 0;
                queue.remove(0);
            }
        }
        if (pckRx.seqTx != seqRx) {
            cntr.drop(pckBin, counter.reasons.badRxSeq);
            if (queue.size() < 1) {
                sendAck();
            }
            return false;
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
        res.add("tunloc|" + tunLoc);
        res.add("tunrem|" + tunRem);
        res.add("sesloc|" + sesLoc);
        res.add("sesrem|" + sesRem);
        return res;
    }

}
