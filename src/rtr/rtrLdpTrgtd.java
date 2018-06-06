package rtr;

import ip.ipFwd;
import ip.ipFwdIface;

import java.util.Comparator;

import pack.packHolder;
import pack.packLdp;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtTcp;
import prt.prtUdp;
import util.bits;
import util.debugger;
import util.logger;
import addr.addrIP;
import util.counter;

/**
 * label distribution protocol (rfc5036) targeted
 *
 * @author matecsaba
 */
public class rtrLdpTrgtd implements Runnable, Comparator<rtrLdpTrgtd> {

    /**
     * peer address
     */
    public final addrIP peer;

    /**
     * source interface
     */
    public ipFwdIface ifc;

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

    public int compare(rtrLdpTrgtd o1, rtrLdpTrgtd o2) {
        return o1.peer.compare(o1.peer, o2.peer);
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
        rtrLdpNeigh ntry = ip.ldpNeighFind(null, peer, false);
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
        need2run = rtrLdpIface.trgtHelloIntrvl / 1000;
    }

    public void run() {
        try {
            conn = udp.streamConnect(new pipeLine(65536, true), ifc, packLdp.port, peer, packLdp.port, "ldp", null, -1);
            if (conn == null) {
                ip.ldpTargetDel(this);
                return;
            }
            conn.timeout = rtrLdpIface.trgtHelloHldtm;
            int run = rtrLdpIface.trgtHelloHldtm;
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
                if (run > rtrLdpIface.trgtHelloIntrvl) {
                    if (debugger.rtrLdpEvnt) {
                        logger.debug("tx hello " + peer);
                    }
                    packLdp pk = new packLdp();
                    pk.conn = conn;
                    pk.cntr = new counter();
                    pk.lsrID = ifc.addr.toIPv4();
                    pk.transAddr = ifc.addr.copyBytes();
                    pk.msgTyp = packLdp.msgThello;
                    pk.holdTime = rtrLdpIface.trgtHelloHldtm / 1000;
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
                rtrLdpNeigh ntry = ip.ldpNeighFind(null, peer, true);
                if (ntry == null) {
                    continue;
                }
                ntry.helloTrg = true;
                if (ntry.ifc != null) {
                    continue;
                }
                ntry.ifc = ifc;
                ntry.udp = udp;
                ntry.tcp = tcp;
                ntry.trans = pk.transAddr;
                ntry.lsrID = pk.lsrID;
                ntry.sessHelloHldtm = rtrLdpIface.sessHelloHldtm;
                ntry.sessHelloIntrvl = rtrLdpIface.sessHelloIntrvl;
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
