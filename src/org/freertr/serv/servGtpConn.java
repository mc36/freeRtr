package org.freertr.serv;

import org.freertr.addr.addrIP;
import org.freertr.ifc.ifcPpp;
import org.freertr.pack.packGtp;
import org.freertr.pack.packHolder;
import org.freertr.prt.prtGenConn;
import org.freertr.tab.tabGen;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * gtp server connection
 *
 * @author matecsaba
 */
public class servGtpConn implements Comparable<servGtpConn> {

    /**
     * peer address
     */
    public addrIP peer;

    /**
     * control connection
     */
    public prtGenConn connC;

    /**
     * data connection
     */
    public prtGenConn connD;

    /**
     * lower layer
     */
    public servGtp lower;

    /**
     * counter
     */
    public counter cntr = new counter();

    /**
     * sessions
     */
    public tabGen<servGtpSess> session = new tabGen<servGtpSess>();

    /**
     * control sequence
     */
    public int seqCtr;

    /**
     * keepalive
     */
    public int keep;

    /**
     * creation time
     */
    public long created;

    public int compareTo(servGtpConn o) {
        return peer.compareTo(o.peer);
    }

    /**
     * create instance
     *
     * @param id connection
     * @param parent lower
     */
    public servGtpConn(addrIP id, servGtp parent) {
        peer = id.copyBytes();
        lower = parent;
        seqCtr = 1;
    }

    /**
     * close instance
     */
    public void setClosed() {
        if (debugger.servGtpTraf) {
            logger.debug("disconnected");
        }
        for (int i = session.size(); i >= 0; i--) {
            servGtpSess ses = session.get(i);
            if (ses == null) {
                continue;
            }
            ses.closeDn();
        }
        lower.connDel(peer);
        if (connC != null) {
            connC.setClosing();
        }
        if (connD != null) {
            connD.setClosing();
        }
    }

    /**
     * find session
     *
     * @param loc local id
     * @return session, null if not found
     */
    public servGtpSess sesFind(int loc) {
        servGtpSess ses = new servGtpSess(this);
        ses.teidLoc = loc;
        return session.find(ses);
    }

    /**
     * delete session
     *
     * @param loc local id
     * @return session, null if not found
     */
    public servGtpSess sesDel(int loc) {
        servGtpSess ses = new servGtpSess(this);
        ses.teidLoc = loc;
        return session.del(ses);
    }

    /**
     * add session
     *
     * @param ses session
     */
    public void sesAdd(servGtpSess ses) {
        for (;;) {
            ses.teidLoc = bits.randomD();
            if (session.add(ses) == null) {
                break;
            }
        }
    }

    /**
     * send session data
     *
     * @param ses session
     * @param pck packet
     */
    public void sesData(servGtpSess ses, packHolder pck) {
        if (connD == null) {
            cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        if (ses.ppp) {
            pck.getSkip(2);
        }
        packGtp gtp = new packGtp();
        gtp.flags = packGtp.flgNothing;
        gtp.msgTyp = packGtp.typGPDU;
        gtp.tunId = ses.teidDat;
        gtp.seqNum = ses.seqDat++;
        gtp.createHeader(pck);
        connD.send2net(pck);
    }

    /**
     * transmit work
     *
     * @param id connection
     */
    public void doWork(prtGenConn id) {
        if (connC == null) {
            setClosed();
            return;
        }
        if (connC.txBytesFree() < 0) {
            setClosed();
            return;
        }
        if (connD != null) {
            if (connD.txBytesFree() < 0) {
                setClosed();
                return;
            }
        }
        if (id.compareTo(connC) != 0) {
            return;
        }
        keep++;
        if (keep < 3) {
            return;
        }
        keep = 0;
        packGtp gtp = new packGtp();
        gtp.seqNum = seqCtr++;
        gtp.msgTyp = packGtp.typEchoReq;
        connC.send2net(gtp.createPacket());
        if (debugger.servGtpTraf) {
            logger.debug("tx " + gtp.dump());
        }
    }

    /**
     * receive work
     *
     * @param pck packet
     * @param ctrl true if control
     */
    public void doRecv(packHolder pck, boolean ctrl) {
        packGtp gtp = new packGtp();
        servGtpSess ses;
        if (gtp.parseHeader(pck)) {
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        if (!ctrl) {
            ses = sesFind(gtp.tunId);
            if (ses == null) {
                cntr.drop(pck, counter.reasons.noIface);
                return;
            }
            if (ses.ppp) {
                pck.msbPutW(0, ifcPpp.preamble);
                pck.putSkip(2);
                pck.merge2beg();
            }
            ses.upper.recvPack(pck);
            return;
        }
        for (;;) {
            if (gtp.parseExtHdr(pck)) {
                break;
            }
        }
        gtp.parsePacket(pck);
        if (debugger.servGtpTraf) {
            logger.debug("rx " + gtp.dump());
        }
        keep = 0;
        int i;
        switch (gtp.msgTyp) {
            case packGtp.typEchoReq:
                gtp.msgTyp = packGtp.typEchoRep;
                connC.send2net(gtp.createPacket());
                if (debugger.servGtpTraf) {
                    logger.debug("tx " + gtp.dump());
                }
                break;
            case packGtp.typDeleteReq:
                ses = sesFind(gtp.tunId);
                if (ses == null) {
                    cntr.drop(pck, counter.reasons.noIface);
                    return;
                }
                ses.closeDn();
                i = gtp.seqNum;
                gtp = new packGtp();
                gtp.msgTyp = packGtp.typDeleteRep;
                gtp.seqNum = i;
                gtp.tunId = ses.teidCtr;
                gtp.valCause = 0x80; // accepted
                connC.send2net(gtp.createPacket());
                if (debugger.servGtpTraf) {
                    logger.debug("tx " + gtp.dump());
                }
                break;
            case packGtp.typCreateReq:
                ses = new servGtpSess(this);
                if ((gtp.valTeidCp == 0) || (gtp.valTeid1 == 0)) {
                    cntr.drop(pck, counter.reasons.badID);
                    return;
                }
                ses.teidCtr = gtp.valTeidCp;
                ses.teidDat = gtp.valTeid1;
                ses.doStartup();
                sesAdd(ses);
                i = gtp.seqNum;
                gtp = new packGtp();
                gtp.msgTyp = packGtp.typCreateRep;
                gtp.seqNum = i;
                gtp.tunId = ses.teidCtr;
                gtp.valCause = 0x80; // accepted
                gtp.valReordReq = 0xff; // reordering required
                gtp.valTeid1 = ses.teidLoc; // tunnel endpoint id
                gtp.valTeidCp = ses.teidLoc; // tunnel endpoint id
                gtp.valChargID = ses.teidLoc; // charging id id
                gtp.fillEndUserAddr(ses.ifc, true);
                gtp.valGSNaddr = connC.iface.addr.copyBytes(); // gsn address
                gtp.valQOSpro = 0xb921f; // best effort
                connC.send2net(gtp.createPacket());
                if (debugger.servGtpTraf) {
                    logger.debug("tx " + gtp.dump());
                }
                break;
        }
    }

}
