package org.freertr.rtr;

import org.freertr.addr.addrIP;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.pack.packHolder;
import org.freertr.pack.packLdp;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtTcp;
import org.freertr.prt.prtUdp;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * label distribution protocol (rfc5036) targeted
 *
 * @author matecsaba
 */
public class rtrLdpTrgtd implements Runnable, Comparable<rtrLdpTrgtd> {

    /**
     * peer address
     */
    public final addrIP peer;

    /**
     * source interface
     */
    public ipFwdIface ifc;

    /**
     * ldp interface
     */
    public rtrLdpIface ldp;

    /**
     * ip layer
     */
    public ipFwd ip;

    /**
     * udp layer
     */
    public prtUdp udp;

    /**
     * tcp layer
     */
    public prtTcp tcp;

    /**
     * connection
     */
    private pipeSide conn;

    private int need2run;

    /**
     * create ldp targeted
     *
     * @param adr address of peer
     */
    public rtrLdpTrgtd(addrIP adr) {
        peer = adr.copyBytes();
    }

    public int compareTo(rtrLdpTrgtd o) {
        return peer.compareTo(o.peer);
    }

    public String toString() {
        return "ldp to " + peer;
    }

    /**
     * start connection
     */
    public void workStart() {
        if (debugger.rtrLdpEvnt) {
            logger.debug("starting targeted hello with " + peer);
        }
        keepWorking();
        new Thread(this).start();
    }

    /**
     * stop connection
     */
    public void workStop() {
        if (debugger.rtrLdpEvnt) {
            logger.debug("stopping targeted hello with " + peer);
        }
        if (conn != null) {
            conn.setClose();
        }
        rtrLdpNeigh ntry = ip.ldpNeighFind(peer, false);
        if (ntry == null) {
            return;
        }
        ntry.helloTrg = false;
        if (ntry.helloIfc) {
            return;
        }
        ip.ldpNeighDel(ntry);
    }

    /**
     * keep working
     */
    public void keepWorking() {
        need2run = ldp.trgtHelloIntrvl / 1000;
    }

    public void run() {
        try {
            conn = udp.streamConnect(new pipeLine(65536, true), ifc, packLdp.port, peer, packLdp.port, "ldp", -1, null, -1, -1);
            if (conn == null) {
                ip.ldpTargetDel(this);
                return;
            }
            conn.setTime(ldp.trgtHelloHldtm);
            int run = ldp.trgtHelloHldtm;
            for (;;) {
                bits.sleep(1000);
                run += 1000;
                if (conn == null) {
                    break;
                }
                if (conn.isClosed() != 0) {
                    break;
                }
                need2run--;
                if (need2run < 0) {
                    break;
                }
                if (run > ldp.trgtHelloIntrvl) {
                    if (debugger.rtrLdpEvnt) {
                        logger.debug("tx hello " + peer);
                    }
                    packLdp pk = new packLdp();
                    pk.conn = conn;
                    pk.cntr = new counter();
                    pk.lsrID = ifc.addr.toIPv4();
                    pk.transAddr = ifc.addr.copyBytes();
                    pk.msgTyp = packLdp.msgThello;
                    pk.holdTime = ldp.trgtHelloHldtm / 1000;
                    pk.targeted = true;
                    pk.putHelloParam();
                    pk.putTransAddr();
                    pk.createLDPheader();
                    pk.sendPack();
                    run = 0;
                }
                packLdp pk = new packLdp();
                pk.conn = conn;
                pk.pack.pipeRecv(conn, 0, packHolder.maxData, 142);
                if (pk.parseLDPheader()) {
                    continue;
                }
                if (pk.parseMSGheader()) {
                    continue;
                }
                if (pk.getHelloParam()) {
                    continue;
                }
                pk.transAddr = peer.copyBytes();
                pk.getTransAddr();
                if (debugger.rtrLdpEvnt) {
                    logger.debug("rx hello " + peer);
                }
                rtrLdpNeigh ntry = ip.ldpNeighFind(peer, true);
                ntry.helloTrg = true;
                if (ntry.udp != null) {
                    continue;
                }
                ntry.ifc = ifc;
                ntry.udp = udp;
                ntry.tcp = tcp;
                ntry.trans = pk.transAddr;
                ntry.lsrID = pk.lsrID;
                ntry.sessHelloHldtm = ldp.sessHelloHldtm;
                ntry.sessHelloIntrvl = ldp.sessHelloIntrvl;
                ntry.sessionTTL = ldp.sessionTTL;
                ntry.sessionTOS = ldp.sessionTOS;
                ntry.filterIn = ldp.filterIn;
                ntry.filterOut = ldp.filterOut;
                ntry.labelPop = ldp.labelPop;
                ntry.startPeer();
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        if (debugger.rtrLdpEvnt) {
            logger.debug("stopping targeted hello with " + peer);
        }
        workStop();
        ip.ldpTargetDel(this);
    }

}
