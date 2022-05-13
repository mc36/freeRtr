package net.freertr.serv;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.auth.autherChap;
import net.freertr.cfg.cfgAll;
import net.freertr.pack.packHolder;
import net.freertr.pack.packL2tp;
import net.freertr.pack.packL2tp2;
import net.freertr.prt.prtGenConn;
import net.freertr.tab.tabGen;
import net.freertr.util.bits;
import net.freertr.util.counter;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * layer two tunneling protocol (rfc2661) connection
 *
 * @author matecsaba
 */
public class servL2tp2conn implements Comparator<servL2tp2conn> {

    /**
     * lower layer
     */
    protected servL2tp2 lower;

    /**
     * connection
     */
    protected prtGenConn conn;

    /**
     * local tunnel id
     */
    protected int tunLoc = 0;

    /**
     * remote tunnel id
     */
    protected int tunRem = 0;

    /**
     * sessions
     */
    protected tabGen<servL2tp2sess> session = new tabGen<servL2tp2sess>();

    /**
     * creation time
     */
    protected long created;

    private byte[] chlng = null;

    private counter cntr = new counter();

    private List<packL2tp2> queue = new ArrayList<packL2tp2>();

    private int txed = 0;

    private int keep = 0;

    private int seqRx = 0;

    private int seqTx = 0;

    public int compare(servL2tp2conn o1, servL2tp2conn o2) {
        return o1.conn.compare(o1.conn, o2.conn);
    }

    /**
     * create instance
     *
     * @param id conn
     * @param parent parent
     */
    public servL2tp2conn(prtGenConn id, servL2tp2 parent) {
        conn = id;
        lower = parent;
        if (lower.password == null) {
            return;
        }
        chlng = new byte[16];
        for (int i = 0; i < chlng.length; i++) {
            chlng[i] = (byte) bits.randomB();
        }
    }

    public String toString() {
        return "l2tp with " + conn.peerAddr;
    }

    /**
     * close sessions
     */
    public void setClosed() {
        if (debugger.servL2tp2traf) {
            logger.debug("disconnected");
        }
        for (int i = session.size(); i >= 0; i--) {
            servL2tp2sess ses = session.get(i);
            if (ses == null) {
                continue;
            }
            ses.closeDn();
        }
        lower.connDel(conn);
        conn.setClosing();
    }

    /**
     * find session
     *
     * @param loc local id
     * @return session, null if not found
     */
    public servL2tp2sess sesFind(int loc) {
        servL2tp2sess ses = new servL2tp2sess(this);
        ses.sesLoc = loc;
        return session.find(ses);
    }

    /**
     * delete session
     *
     * @param loc local id
     * @return session, null if not found
     */
    public servL2tp2sess sesDel(int loc) {
        servL2tp2sess ses = new servL2tp2sess(this);
        ses.sesLoc = loc;
        return session.del(ses);
    }

    /**
     * add session
     *
     * @param ses session
     */
    public void sesAdd(servL2tp2sess ses) {
        for (;;) {
            ses.sesLoc = bits.randomW();
            if (session.add(ses) == null) {
                break;
            }
        }
    }

    /**
     * session got data
     *
     * @param ses session
     * @param pckBin packet
     */
    public void sesData(servL2tp2sess ses, packHolder pckBin) {
        pckBin.merge2beg();
        packL2tp2 pckTx = new packL2tp2();
        pckTx.ctrl = false;
        pckTx.sesID = ses.sesRem;
        pckTx.tunID = tunRem;
        pckTx.createHeader(pckBin);
        cntr.tx(pckBin);
        conn.send2net(pckBin);
    }

    /**
     * do retransmission work
     */
    public void doWork() {
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
        if (debugger.servL2tp2traf) {
            logger.debug("tx " + pckTx.dump());
        }
        if (txed < 8) {
            return;
        }
        setClosed();
    }

    /**
     * enqueue packet
     *
     * @param pck packet
     */
    public void enQueue(packL2tp2 pck) {
        synchronized (queue) {
            queue.add(pck);
        }
    }

    /**
     * send acknowledgement
     */
    public void sendAck() {
        packL2tp2 pckTx = new packL2tp2();
        pckTx.patchHeader(tunRem, seqRx, seqTx);
        packHolder pckBin = new packHolder(true, true);
        pckTx.createHeader(pckBin);
        cntr.tx(pckBin);
        conn.send2net(pckBin);
        if (debugger.servL2tp2traf) {
            logger.debug("tx " + pckTx.dump());
        }
    }

    /**
     * got one packet
     *
     * @param pckBin packet
     */
    public void doRecv(packHolder pckBin) {
        packL2tp2 pckRx = new packL2tp2();
        if (pckRx.parseHeader(pckBin)) {
            cntr.drop(pckBin, counter.reasons.badHdr);
            return;
        }
        if (pckRx.tunID != tunLoc) {
            cntr.drop(pckBin, counter.reasons.badID);
            return;
        }
        keep = 0;
        if (!pckRx.ctrl) {
            servL2tp2sess ses = sesFind(pckRx.sesID);
            if (ses == null) {
                cntr.drop(pckBin, counter.reasons.badID);
                return;
            }
            ses.send2upper(pckBin);
            return;
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
            return;
        }
        pckRx.parseTLVs(pckBin);
        cntr.rx(pckBin);
        if (debugger.servL2tp2traf) {
            logger.debug("rx " + pckRx.dump());
        }
        if (pckRx.valMsgTyp == packL2tp.typZLB) {
            return;
        }
        seqRx = (seqRx + 1) & 0xffff;
        servL2tp2sess ses;
        byte[] res = null;
        switch (pckRx.valMsgTyp) {
            case packL2tp.typSCCRQ:
                tunRem = pckRx.valTunId;
                tunLoc = bits.randomW();
                if (chlng != null) {
                    if (pckRx.valChallen == null) {
                        return;
                    }
                    res = autherChap.calcAuthHash(pckRx.valMsgTyp + 1, lower.password, pckRx.valChallen);
                }
                enQueue(packL2tp2.createSCCRP(tunLoc, cfgAll.hostName, chlng, res));
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
            case packL2tp.typOCRQ:
                ses = new servL2tp2sess(this);
                ses.sesRem = pckRx.valSesId;
                sesAdd(ses);
                enQueue(packL2tp2.createOCRP(ses.sesLoc, ses.sesRem));
                enQueue(packL2tp2.createOCCN(ses.sesRem));
                ses.doStartup();
                break;
            case packL2tp.typICRQ:
                ses = new servL2tp2sess(this);
                ses.sesRem = pckRx.valSesId;
                sesAdd(ses);
                enQueue(packL2tp2.createICRP(ses.sesLoc, ses.sesRem));
                break;
            case packL2tp.typICCN:
                ses = sesFind(pckRx.sesID);
                if (ses == null) {
                    break;
                }
                ses.doStartup();
                sendAck();
                break;
            case packL2tp.typCDN:
                ses = sesDel(pckRx.sesID);
                if (ses == null) {
                    break;
                }
                ses.closeDn();
                break;
            case packL2tp.typSCCNO:
                sendAck();
                setClosed();
                break;
            case packL2tp.typHELLO:
            case packL2tp.typSLI:
            case packL2tp.typWEN:
                sendAck();
                break;
        }
    }

}
