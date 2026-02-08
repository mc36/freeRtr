package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipPrt;
import org.freertr.pack.packHolder;
import org.freertr.pack.packPptp;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtGre;
import org.freertr.prt.prtServS;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabRouteIface;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * point to point tunneling protocol (rfc2637) server
 *
 * @author matecsaba
 */
public class servPptp extends servGeneric implements ipPrt, prtServS {

    /**
     * create instance
     */
    public servPptp() {
    }

    /**
     * interface to use
     */
    public cfgIfc clnIfc;

    /**
     * sending ttl value, -1 means maps out
     */
    public int sendingTTL = 255;

    /**
     * sending tos value, -1 means maps out
     */
    public int sendingTOS = -1;

    /**
     * sending df value, -1 means maps out
     */
    public int sendingDFN = -1;

    /**
     * sending flow value, -1 means maps out
     */
    public int sendingFLW = -1;

    /**
     * list of connections
     */
    public tabGen<servPptpConn> conns = new tabGen<servPptpConn>();

    /**
     * counter
     */
    public counter cntr;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server pptp .*", cmds.tabulator + "port " + packPptp.port, null),
        new userFilter("server pptp .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null)
    };

    public userFilter[] srvDefFlt() {
        return defaultF;
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        if (clnIfc == null) {
            l.add(beg + "no clone");
        } else {
            l.add(beg + "clone " + clnIfc.name);
        }
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("clone")) {
            clnIfc = cfgAll.ifcFind(cmd.word(), 0);
            if (clnIfc == null) {
                cmd.error("no such interface");
                return false;
            }
            if (clnIfc.type != tabRouteIface.ifaceType.dialer) {
                cmd.error("not dialer interface");
                clnIfc = null;
                return false;
            }
            return false;
        }
        if (!s.equals(cmds.negated)) {
            return true;
        }
        s = cmd.word();
        if (s.equals("clone")) {
            clnIfc = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "clone", "set interface to clone");
        l.add(null, false, 2, new int[]{-1}, "<name:ifc>", "name of interface");
    }

    public String srvName() {
        return "pptp";
    }

    public int srvPort() {
        return packPptp.port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(32768, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        servPptpConn ntry = new servPptpConn(this, pipe, id.iface, id.peerAddr.copyBytes());
        if (conns.add(ntry) != null) {
            return true;
        }
        ntry.doStartup();
        return false;
    }

    /**
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return prtGre.protoNum;
    }

    public String toString() {
        return "pptp";
    }

    /**
     * close interface
     *
     * @param iface interface
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
     * @param pck packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pck) {
        servPptpConn ntry = new servPptpConn(this, null, rxIfc, pck.IPsrc);
        ntry = conns.find(ntry);
        if (ntry == null) {
            return;
        }
        ntry.doRecv(pck);
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

    /**
     * get counter
     *
     * @return counter
     */
    public counter getCounter() {
        return cntr;
    }

}

class servPptpConn implements Runnable, Comparable<servPptpConn> {

    public servPptp lower;

    public pipeSide conn;

    public addrIP peer;

    public ipFwdIface iface;

    public counter cntr = new counter();

    public boolean need2run;

    public ipFwd fwdCor;

    public tabGen<servPptpSess> session = new tabGen<servPptpSess>();

    public servPptpConn(servPptp parent, pipeSide pip, ipFwdIface ifc, addrIP adr) {
        lower = parent;
        conn = pip;
        iface = ifc;
        peer = adr;
    }

    public String toString() {
        return lower + " with " + peer;
    }

    public int compareTo(servPptpConn o) {
        if (iface.ifwNum < o.iface.ifwNum) {
            return -1;
        }
        if (iface.ifwNum > o.iface.ifwNum) {
            return +1;
        }
        return peer.compareTo(o.peer);
    }

    public void setClosed() {
        need2run = false;
        conn.setClose();
        fwdCor.protoDel(lower, iface, peer);
        for (int i = session.size(); i >= 0; i--) {
            servPptpSess ses = session.get(i);
            if (ses == null) {
                continue;
            }
            ses.closeDn();
        }
        lower.conns.del(this);
    }

    public void doStartup() {
        if (debugger.servPptpTraf) {
            logger.debug("starting");
        }
        need2run = true;
        fwdCor = lower.srvVrf.getFwd(peer);
        fwdCor.protoAdd(lower, iface, peer);
        logger.startThread(this);
        new servPptpKeep(this).doStartup();
    }

    public void run() {
        try {
            for (;;) {
                if (doWork()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        setClosed();
        if (debugger.servPptpTraf) {
            logger.debug("disconnected");
        }
    }

    public boolean doWork() {
        if (conn.isClosed() != 0) {
            return true;
        }
        if (!need2run) {
            return true;
        }
        packHolder pckBin = new packHolder(true, true);
        packPptp pckPtp = new packPptp();
        if (pckPtp.recvPack(conn, pckBin)) {
            return true;
        }
        if (pckPtp.parseControl(pckBin)) {
            return true;
        }
        switch (pckPtp.typ) {
            case packPptp.msgEchoReq:
                if (pckPtp.parseEcho(pckBin, true)) {
                    return true;
                }
                if (debugger.servPptpTraf) {
                    logger.debug("rx " + pckPtp.dump());
                }
                pckPtp.fillEcho(false, pckPtp.callLoc);
                pckPtp.createEcho(pckBin);
                pckPtp.createControl(pckBin);
                pckPtp.sendPack(conn, pckBin);
                if (debugger.servPptpTraf) {
                    logger.debug("tx " + pckPtp.dump());
                }
                return false;
            case packPptp.msgEchoRep:
                return false;
            case packPptp.msgInReq:
                if (pckPtp.parseInReq(pckBin)) {
                    return true;
                }
                if (debugger.servPptpTraf) {
                    logger.debug("rx " + pckPtp.dump());
                }
                servPptpSess ses = new servPptpSess(this);
                ses.callRem = pckPtp.callLoc;
                ses.callLoc = bits.randomW();
                ses.doStartup();
                session.add(ses);
                pckPtp.fillInRep(ses.callLoc, ses.callRem);
                pckPtp.createInRep(pckBin);
                pckPtp.createControl(pckBin);
                pckPtp.sendPack(conn, pckBin);
                if (debugger.servPptpTraf) {
                    logger.debug("tx " + pckPtp.dump());
                }
                return false;
            case packPptp.msgInCon:
                if (pckPtp.parseInCon(pckBin)) {
                    return true;
                }
                if (debugger.servPptpTraf) {
                    logger.debug("rx " + pckPtp.dump());
                }
                return false;
            case packPptp.msgClrReq:
                if (pckPtp.parseClrReq(pckBin)) {
                    return true;
                }
                if (debugger.servPptpTraf) {
                    logger.debug("rx " + pckPtp.dump());
                }
                ses = new servPptpSess(this);
                ses.callLoc = pckPtp.callRem;
                session.del(ses);
                return false;
            case packPptp.msgDscNot:
                if (pckPtp.parseDscNot(pckBin)) {
                    return true;
                }
                if (debugger.servPptpTraf) {
                    logger.debug("rx " + pckPtp.dump());
                }
                ses = new servPptpSess(this);
                ses.callLoc = pckPtp.callRem;
                session.del(ses);
                return false;
            case packPptp.msgLnkInf:
            case packPptp.msgWanErr:
                return false;
            case packPptp.msgOutReq:
                if (pckPtp.parseOutReq(pckBin)) {
                    return true;
                }
                if (debugger.servPptpTraf) {
                    logger.debug("rx " + pckPtp.dump());
                }
                ses = new servPptpSess(this);
                ses.callRem = pckPtp.callLoc;
                ses.callLoc = bits.randomW();
                ses.doStartup();
                session.add(ses);
                pckPtp.fillOutRep(ses.callLoc, ses.callRem);
                pckPtp.createOutRep(pckBin);
                pckPtp.createControl(pckBin);
                pckPtp.sendPack(conn, pckBin);
                if (debugger.servPptpTraf) {
                    logger.debug("tx " + pckPtp.dump());
                }
                return false;
            case packPptp.msgStartReq:
                if (pckPtp.parseStart(pckBin, true)) {
                    return true;
                }
                if (debugger.servPptpTraf) {
                    logger.debug("rx " + pckPtp.dump());
                }
                pckPtp.fillStart(false);
                pckPtp.createStart(pckBin);
                pckPtp.createControl(pckBin);
                pckPtp.sendPack(conn, pckBin);
                if (debugger.servPptpTraf) {
                    logger.debug("tx " + pckPtp.dump());
                }
                return false;
            default:
                if (debugger.servPptpTraf) {
                    logger.debug("rx " + pckPtp.dump());
                }
                return true;
        }
    }

    public void doRecv(packHolder pckBin) {
        packPptp pckRx = new packPptp();
        if (pckRx.parseData(pckBin)) {
            cntr.drop(pckBin, counter.reasons.badHdr);
            return;
        }
        servPptpSess ses = new servPptpSess(this);
        ses.callLoc = pckRx.callRem;
        ses = session.find(ses);
        if (ses == null) {
            cntr.drop(pckBin, counter.reasons.badID);
            return;
        }
        if (pckBin.dataSize() < 1) {
            return;
        }
        ses.nedTx = pckRx.seqTx >= 0;
        ses.nedRx = pckRx.seqRx >= 0;
        if (ses.nedTx) {
            ses.seqRx = pckRx.seqTx;
        }
        ses.cntr.rx(pckBin);
        ses.upper.recvPack(pckBin);
    }

    public void doSend(servPptpSess ses, packHolder pck) {
        pck.merge2beg();
        ses.seqTx++;
        packPptp tx = new packPptp();
        tx.callRem = ses.callRem;
        if (ses.nedTx) {
            tx.seqTx = ses.seqTx;
        }
        if (ses.nedRx) {
            tx.seqRx = ses.seqRx;
        }
        tx.createData(pck);
        cntr.tx(pck);
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
        pck.IPprt = prtGre.protoNum;
        pck.IPsrc.setAddr(iface.addr);
        pck.IPtrg.setAddr(peer);
        fwdCor.protoPack(iface, null, pck);
    }

    public boolean sendKeep() {
        packPptp pckPtp = new packPptp();
        pckPtp.fillEcho(true, bits.randomD());
        packHolder pckBin = new packHolder(true, true);
        pckPtp.createEcho(pckBin);
        pckPtp.createControl(pckBin);
        boolean b = pckPtp.sendPack(conn, pckBin);
        if (debugger.servPptpTraf) {
            logger.debug("tx " + pckPtp.dump());
        }
        return b;
    }

}

class servPptpKeep implements Runnable {

    private final servPptpConn parent;

    public servPptpKeep(servPptpConn lower) {
        parent = lower;
    }

    public void doStartup() {
        logger.startThread(this);
    }

    public void run() {
        try {
            for (;;) {
                bits.sleep(30000);
                if (parent.sendKeep()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class servPptpSess implements ifcDn, Comparable<servPptpSess> {

    public int callLoc;

    public int callRem;

    public int seqRx;

    public int seqTx;

    public boolean nedRx;

    public boolean nedTx;

    public servPptpConn lower;

    public ifcUp upper = new ifcNull();

    public cfgIfc ifc;

    public counter cntr = new counter();

    public servPptpSess(servPptpConn parent) {
        lower = parent;
    }

    public int compareTo(servPptpSess o) {
        if (callLoc < o.callLoc) {
            return -1;
        }
        if (callLoc > o.callLoc) {
            return +1;
        }
        return 0;
    }

    public void doStartup() {
        upper = new ifcNull();
        ifc = lower.lower.clnIfc.cloneStart(this);
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
        lower.session.del(this);
        upper.closeUp();
        if (ifc != null) {
            ifc.cloneStop();
        }
    }

    public void flapped() {
        closeDn();
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
        cntr.tx(pck);
        pck.putDefaults();
        lower.doSend(this, pck);
    }

}
