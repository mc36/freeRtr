package org.freertr.serv;

import org.freertr.auth.autherChap;
import org.freertr.cfg.cfgAll;
import org.freertr.pack.packHolder;
import org.freertr.pack.packL2f;
import org.freertr.prt.prtGenConn;
import org.freertr.tab.tabGen;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * l2f handler
 *
 * @author matecsaba
 */
public class servL2fConn implements Comparable<servL2fConn> {

    /**
     * connection
     */
    public prtGenConn conn;

    /**
     * lower layer
     */
    public servL2f lower;

    /**
     * counter
     */
    public counter cntr = new counter();

    /**
     * sessions
     */
    public tabGen<servL2fSess> session = new tabGen<servL2fSess>();

    /**
     * transmitted
     */
    public int txed = 0;

    /**
     * keepalived
     */
    public int keep = 0;

    /**
     * local tunnel id
     */
    public int tunLoc = 0;

    /**
     * remote tunnel id
     */
    public int tunRem = 0;

    /**
     * local key
     */
    public int keyLoc = 0;

    /**
     * remote key
     */
    public int keyRem = 0;

    /**
     * local challenge
     */
    public byte[] chlLoc = null;

    /**
     * remote challenge
     */
    public byte[] chlRem = null;

    /**
     * creation time
     */
    public long created;

    public int compareTo(servL2fConn o) {
        return conn.compareTo(o.conn);
    }

    /**
     * create instance
     *
     * @param id conn
     * @param parent parent
     */
    public servL2fConn(prtGenConn id, servL2f parent) {
        conn = id;
        lower = parent;
    }

    public String toString() {
        return "l2f with " + conn.peerAddr;
    }

    /**
     * close sessions
     */
    public void setClosed() {
        if (debugger.servL2fTraf) {
            logger.debug("disconnected");
        }
        for (int i = session.size(); i >= 0; i--) {
            servL2fSess ses = session.get(i);
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
    public servL2fSess sesFind(int loc) {
        servL2fSess ses = new servL2fSess(this);
        ses.multi = loc;
        return session.find(ses);
    }

    /**
     * delete session
     *
     * @param loc local id
     * @param snd inform peer
     * @return session, null if not found
     */
    public servL2fSess sesDel(int loc, boolean snd) {
        if (snd) {
            packL2f pckTx = new packL2f();
            packHolder pckBin = new packHolder(true, true);
            pckTx.createClose(pckBin, 4);
            pckTx.seq = bits.randomB();
            pckTx.client = tunRem;
            pckTx.multi = loc;
            pckTx.key = keyRem;
            pckTx.createHeader(pckBin);
            cntr.tx(pckBin);
            txed++;
            conn.send2net(pckBin);
            if (debugger.servL2fTraf) {
                logger.debug("tx " + pckTx.dump());
            }
        }
        servL2fSess ses = new servL2fSess(this);
        ses.multi = loc;
        return session.del(ses);
    }

    /**
     * add session
     *
     * @param loc session
     */
    public void sesAdd(int loc) {
        servL2fSess ses = new servL2fSess(this);
        ses.multi = loc;
        if (session.add(ses) != null) {
            return;
        }
        ses.doStartup();
    }

    /**
     * session got data
     *
     * @param ses session
     * @param pckBin packet
     */
    public void sesData(servL2fSess ses, packHolder pckBin) {
        pckBin.merge2beg();
        packL2f pckTx = new packL2f();
        pckTx.proto = packL2f.prtPpp;
        pckTx.client = tunRem;
        pckTx.key = keyRem;
        pckTx.multi = ses.multi;
        pckTx.createHeader(pckBin);
        cntr.tx(pckBin);
        conn.send2net(pckBin);
    }

    /**
     * do retransmission work
     */
    public void doWork() {
        packL2f pckTx = new packL2f();
        packHolder pckBin = new packHolder(true, true);
        keep++;
        if (keep < lower.helloTicks) {
            return;
        }
        pckTx.valResp = new byte[1];
        pckTx.valResp[0] = (byte) bits.randomB();
        pckTx.createEchoReq(pckBin, pckTx.valResp);
        pckTx.seq = bits.randomB();
        pckTx.client = tunRem;
        pckTx.key = keyRem;
        pckTx.createHeader(pckBin);
        cntr.tx(pckBin);
        txed++;
        conn.send2net(pckBin);
        if (debugger.servL2fTraf) {
            logger.debug("tx " + pckTx.dump());
        }
        if (txed < lower.retryTicks) {
            return;
        }
        setClosed();
    }

    /**
     * got one packet
     *
     * @param pckBin packet
     */
    public void doRecv(packHolder pckBin) {
        packL2f pckRx = new packL2f();
        if (pckRx.parseHeader(pckBin)) {
            cntr.drop(pckBin, counter.reasons.badHdr);
            return;
        }
        keep = 0;
        if (pckRx.proto != packL2f.prtMgmt) {
            servL2fSess ses = sesFind(pckRx.multi);
            if (ses == null) {
                return;
            }
            ses.cntr.rx(pckBin);
            ses.upper.recvPack(pckBin);
            return;
        }
        packL2f pckTx = new packL2f();
        txed = 0;
        switch (pckRx.type) {
            case packL2f.typConf:
                if (pckRx.parseConf(pckBin)) {
                    return;
                }
                if (debugger.servL2fTraf) {
                    logger.debug("rx " + pckRx.dump());
                }
                tunRem = pckRx.valClid;
                chlRem = pckRx.valChal;
                pckBin.clear();
                pckTx.createConf(pckBin, cfgAll.hostName, chlLoc, tunLoc);
                break;
            case packL2f.typOpen:
                if (pckRx.parseOpen(pckBin)) {
                    return;
                }
                if (debugger.servL2fTraf) {
                    logger.debug("rx " + pckRx.dump());
                }
                if (pckRx.multi != 0) {
                    sesAdd(pckRx.multi);
                    pckTx.multi = pckRx.multi;
                    pckBin.clear();
                    pckTx.createOpen(pckBin, null);
                    break;
                }
                byte[] res = null;
                if (chlLoc != null) {
                    if (pckRx.valResp == null) {
                        return;
                    }
                    res = autherChap.calcAuthHash(tunLoc, lower.password, chlLoc);
                    if (res.length != pckRx.valResp.length) {
                        return;
                    }
                    if (bits.byteComp(res, 0, pckRx.valResp, 0, res.length) != 0) {
                        return;
                    }
                    keyLoc = packL2f.calcKey(res);
                    res = autherChap.calcAuthHash(tunRem, lower.password, chlRem);
                    keyRem = packL2f.calcKey(res);
                }
                pckBin.clear();
                pckTx.createOpen(pckBin, res);
                break;
            case packL2f.typClose:
                if (pckRx.parseClose(pckBin)) {
                    return;
                }
                if (debugger.servL2fTraf) {
                    logger.debug("rx " + pckRx.dump());
                }
                servL2fSess ses = sesDel(pckRx.multi, false);
                if (ses == null) {
                    return;
                }
                ses.closeDn();
                return;
            case packL2f.typEchoReq:
                pckRx.parseEcho(pckBin);
                if (debugger.servL2fTraf) {
                    logger.debug("rx " + pckRx.dump());
                }
                pckBin.clear();
                pckTx.createEchoRes(pckBin, pckRx.valResp);
                break;
            case packL2f.typEchoRes:
                pckRx.parseEcho(pckBin);
                if (debugger.servL2fTraf) {
                    logger.debug("rx " + pckRx.dump());
                }
                return;
            default:
                return;
        }
        pckTx.seq = pckRx.seq;
        pckTx.client = tunRem;
        pckTx.chksum = pckRx.chksum;
        pckTx.key = keyRem;
        pckTx.createHeader(pckBin);
        conn.send2net(pckBin);
        if (debugger.servL2fTraf) {
            logger.debug("tx " + pckTx.dump());
        }
    }

}
