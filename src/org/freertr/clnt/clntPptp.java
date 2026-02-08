package org.freertr.clnt;

import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgVrf;
import org.freertr.ifc.ifcDn;
import org.freertr.ifc.ifcNull;
import org.freertr.ifc.ifcUp;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdTab;
import org.freertr.ip.ipPrt;
import org.freertr.pack.packHolder;
import org.freertr.pack.packPptp;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGre;
import org.freertr.serv.servGeneric;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.state;

/**
 * point to point tunneling protocol (rfc2637) client
 *
 * @author matecsaba
 */
public class clntPptp implements Runnable, ipPrt, ifcDn {

    /**
     * create instance
     */
    public clntPptp() {
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
     * telephone number
     */
    public String called;

    /**
     * direction, true=outgoing, false=incoming
     */
    public boolean direction = true;

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
     * counter
     */
    public counter cntr = new counter();

    private pipeSide conn;

    private ipFwd fwdCor;

    private addrIP fwdTrg;

    private ipFwdIface fwdIfc;

    private int callLoc;

    private int callRem;

    private int seqRx;

    private int seqTx;

    private boolean nedRx;

    private boolean nedTx;

    private boolean working = true;

    public String toString() {
        return "pptp to " + target;
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
        return 4000000;
    }

    /**
     * send packet
     *
     * @param pckBin packet
     */
    public void sendPack(packHolder pckBin) {
        pckBin.merge2beg();
        if (callRem == 0) {
            return;
        }
        seqTx++;
        packPptp tx = new packPptp();
        tx.callRem = callRem;
        if (nedTx) {
            tx.seqTx = seqTx;
        }
        if (nedRx) {
            tx.seqRx = seqRx;
        }
        tx.createData(pckBin);
        cntr.tx(pckBin);
        pckBin.putDefaults();
        if (sendingTTL >= 0) {
            pckBin.IPttl = sendingTTL;
        }
        if (sendingTOS >= 0) {
            pckBin.IPtos = sendingTOS;
        }
        if (sendingDFN >= 0) {
            pckBin.IPdf = sendingDFN == 1;
        }
        if (sendingFLW >= 0) {
            pckBin.IPid = sendingFLW;
        }
        pckBin.IPprt = prtGre.protoNum;
        pckBin.IPsrc.setAddr(fwdIfc.addr);
        pckBin.IPtrg.setAddr(fwdTrg);
        fwdCor.protoPack(fwdIfc, null, pckBin);
    }

    /**
     * start connection
     */
    public void workStart() {
        logger.startThread(this);
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
        fwdTrg = clntDns.justResolv(target, prefer);
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
        conn = clntProxy.makeTemp(vrf, srcIfc).doConnect(servGeneric.protoTcp, fwdTrg, packPptp.port, "pptp");
        if (conn == null) {
            return;
        }
        conn.setTime(120000);
        packHolder pckBin = new packHolder(true, true);
        packPptp pckPtp = new packPptp();
        pckPtp.fillStart(true);
        pckPtp.createStart(pckBin);
        pckPtp.createControl(pckBin);
        pckPtp.sendPack(conn, pckBin);
        if (debugger.clntPptpTraf) {
            logger.debug("tx " + pckPtp.dump());
        }
        pckPtp = new packPptp();
        if (pckPtp.recvPack(conn, pckBin)) {
            return;
        }
        if (pckPtp.parseControl(pckBin)) {
            return;
        }
        if (pckPtp.parseStart(pckBin, false)) {
            return;
        }
        if (debugger.clntPptpTraf) {
            logger.debug("rx " + pckPtp.dump());
        }
        callLoc = bits.randomW();
        if (direction) {
            pckPtp = new packPptp();
            pckPtp.fillOutReq(callLoc, called);
            pckPtp.createOutReq(pckBin);
            pckPtp.createControl(pckBin);
            pckPtp.sendPack(conn, pckBin);
            if (debugger.clntPptpTraf) {
                logger.debug("tx " + pckPtp.dump());
            }
            pckPtp = new packPptp();
            if (pckPtp.recvPack(conn, pckBin)) {
                return;
            }
            if (pckPtp.parseControl(pckBin)) {
                return;
            }
            if (pckPtp.parseOutRep(pckBin)) {
                return;
            }
            if (debugger.clntPptpTraf) {
                logger.debug("rx " + pckPtp.dump());
            }
            callRem = pckPtp.callLoc;
        } else {
            pckPtp = new packPptp();
            pckPtp.fillInReq(callLoc, called, called);
            pckPtp.createInReq(pckBin);
            pckPtp.createControl(pckBin);
            pckPtp.sendPack(conn, pckBin);
            if (debugger.clntPptpTraf) {
                logger.debug("tx " + pckPtp.dump());
            }
            pckPtp = new packPptp();
            if (pckPtp.recvPack(conn, pckBin)) {
                return;
            }
            if (pckPtp.parseControl(pckBin)) {
                return;
            }
            if (pckPtp.parseInRep(pckBin)) {
                return;
            }
            if (debugger.clntPptpTraf) {
                logger.debug("rx " + pckPtp.dump());
            }
            callRem = pckPtp.callLoc;
            pckPtp = new packPptp();
            pckPtp.fillInCon(callRem);
            pckPtp.createInCon(pckBin);
            pckPtp.createControl(pckBin);
            pckPtp.sendPack(conn, pckBin);
        }
        int tim = 0;
        for (;;) {
            if (conn.isClosed() != 0) {
                return;
            }
            if (tim > 30) {
                pckPtp = new packPptp();
                pckPtp.fillEcho(true, bits.randomD());
                pckPtp.createEcho(pckBin);
                pckPtp.createControl(pckBin);
                pckPtp.sendPack(conn, pckBin);
                if (debugger.clntPptpTraf) {
                    logger.debug("tx " + pckPtp.dump());
                }
                tim = 0;
            }
            if (conn.ready2rx() > 0) {
                pckPtp = new packPptp();
                if (pckPtp.recvPack(conn, pckBin)) {
                    return;
                }
                if (pckPtp.parseControl(pckBin)) {
                    return;
                }
                if (debugger.clntPptpTraf) {
                    logger.debug("rx " + pckPtp.dump());
                }
                switch (pckPtp.typ) {
                    case packPptp.msgEchoReq:
                        if (pckPtp.parseEcho(pckBin, true)) {
                            return;
                        }
                        pckPtp.fillEcho(false, pckPtp.callLoc);
                        pckPtp.createEcho(pckBin);
                        pckPtp.createControl(pckBin);
                        pckPtp.sendPack(conn, pckBin);
                        if (debugger.clntPptpTraf) {
                            logger.debug("tx " + pckPtp.dump());
                        }
                        break;
                    case packPptp.msgClrReq:
                    case packPptp.msgDscNot:
                        return;
                    case packPptp.msgStopReq:
                    case packPptp.msgStopRep:
                        return;
                }
            }
            bits.sleep(1000);
            tim++;
        }
    }

    private void clearState() {
        if (conn != null) {
            conn.setClose();
        }
        if (fwdIfc != null) {
            fwdCor.protoDel(this, fwdIfc, fwdTrg);
            fwdIfc = null;
        }
        callLoc = 0;
        callRem = 0;
        seqRx = 0;
        seqTx = 0;
        nedRx = false;
        nedTx = false;
    }

    /**
     * get protocol number
     *
     * @return number
     */
    public int getProtoNum() {
        return prtGre.protoNum;
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
     * @param pckBin packet
     */
    public void recvPack(ipFwdIface rxIfc, packHolder pckBin) {
        packPptp pckRx = new packPptp();
        if (pckRx.parseData(pckBin)) {
            cntr.drop(pckBin, counter.reasons.badHdr);
            return;
        }
        if (pckRx.callRem != callLoc) {
            cntr.drop(pckBin, counter.reasons.badID);
            return;
        }
        if (pckBin.dataSize() < 1) {
            return;
        }
        nedTx = pckRx.seqTx >= 0;
        nedRx = pckRx.seqRx >= 0;
        if (nedTx) {
            seqRx = pckRx.seqTx;
        }
        cntr.rx(pckBin);
        upper.recvPack(pckBin);
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
     * @param err error reason
     * @param rtr address
     * @param rxIfc interface
     * @param pck packet
     */
    public void errorPack(counter.reasons err, addrIP rtr, ipFwdIface rxIfc, packHolder pck) {
    }

}
