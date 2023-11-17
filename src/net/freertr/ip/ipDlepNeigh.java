package net.freertr.ip;

import java.util.Comparator;
import net.freertr.addr.addrIP;
import net.freertr.enc.encTlv;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtAccept;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * dynamic link exchange neighbor
 *
 * @author matecsaba
 */
public class ipDlepNeigh implements Runnable, Comparator<ipDlepNeigh> {

    /**
     * session initialization request
     */
    public final static int msgInitReq = 1;

    /**
     * session initialization response
     */
    public final static int msgInitRep = 2;

    /**
     * update request
     */
    public final static int msgUpdateReq = 3;

    /**
     * update response
     */
    public final static int msgUpdateRep = 4;

    /**
     * termination request
     */
    public final static int msgTermReq = 5;

    /**
     * termination response
     */
    public final static int msgTermRep = 6;

    /**
     * destination up request
     */
    public final static int msgUpReq = 7;

    /**
     * destination up response
     */
    public final static int msgUpRep = 8;

    /**
     * destination announce request
     */
    public final static int msgAnnoReq = 9;

    /**
     * destination announce response
     */
    public final static int msgAnnoRep = 10;

    /**
     * destination down request
     */
    public final static int msgDownReq = 11;

    /**
     * destination down response
     */
    public final static int msgDownRep = 12;

    /**
     * destination update
     */
    public final static int msgDestUpd = 13;

    /**
     * link characteristics request
     */
    public final static int msgLinkReq = 14;

    /**
     * link characteristics response
     */
    public final static int msgLinkRep = 15;
    
    /**
     * heartbeat
     */
    public final static int msgHertBet = 16;

    /**
     * status
     */
    public final static int typStatus = 1;

    /**
     * ipv4 connection point
     */
    public final static int typConnV4 = 2;

    /**
     * ipv6 connection point
     */
    public final static int typConnV6 = 3;

    /**
     * peer type
     */
    public final static int typPeerTyp = 4;

    /**
     * heartbeat
     */
    public final static int typHertBet = 5;

    /**
     * extensions supported
     */
    public final static int typExtSupp = 6;

    /**
     * mac address
     */
    public final static int typMacAddr = 7;

    /**
     * ipv4 address
     */
    public final static int typIpv4addr = 8;

    /**
     * ipv6 address
     */
    public final static int typIpv6addr = 9;

    /**
     * ipv4 attached subnet
     */
    public final static int typIpv4subnet = 10;

    /**
     * ipv6 attached subnet
     */
    public final static int typIpv6subnet = 11;

    /**
     * maximum data rate receive
     */
    public final static int typMaxRateRx = 12;

    /**
     * maximum data rate transmit
     */
    public final static int typMaxRateTx = 13;

    /**
     * current data rate receive
     */
    public final static int typCurRateRx = 14;

    /**
     * current data rate transmit
     */
    public final static int typCurRateTx = 15;

    /**
     * latency
     */
    public final static int typLatency = 16;

    /**
     * resources
     */
    public final static int typResources = 17;

    /**
     * relative link quality receive
     */
    public final static int typRelQualRx = 18;

    /**
     * relative link quality transmit
     */
    public final static int typRelQualTx = 19;

    /**
     * maximum transmission unit
     */
    public final static int typMaxXmitUnit = 20;

    private final ipDlepIface iface;

    private final addrIP peer;

    private final int port;

    private pipeSide conn;

    /**
     * create one instance
     *
     * @param ifc interface to use
     * @param id connection to use
     */
    public ipDlepNeigh(ipDlepIface ifc, addrIP per, int prt) {
        iface = ifc;
        port = prt;
        peer = per.copyBytes();
    }

    public int compare(ipDlepNeigh o1, ipDlepNeigh o2) {
        return o1.peer.compare(o1.peer, o2.peer);
    }

    /**
     * convert type to string
     *
     * @param i type to convert
     * @return string
     */
    public static final String type2string(int i) {
        switch (i) {
            case msgInitReq:
                return "initReq";
            case msgInitRep:
                return "initRep";
            case msgUpdateReq:
                return "updateReq";
            case msgUpdateRep:
                return "updateRep";
            case msgTermReq:
                return "termReq";
            case msgTermRep:
                return "termRep";
            case msgUpReq:
                return "upReq";
            case msgUpRep:
                return "upRep";
            case msgAnnoReq:
                return "annoReq";
            case msgAnnoRep:
                return "annoRep";
            case msgDownReq:
                return "downReq";
            case msgDownRep:
                return "downRep";
            case msgDestUpd:
                return "destUpd";
            case msgLinkReq:
                return "linkReq";
            case msgLinkRep:
                return "linkRep";
            case msgHertBet:
                return "hrtBet";
            default:
                return "unkonwn=" + i;
        }
    }

    private static encTlv getTlv() {
        return new encTlv(0, 16, 16, 16, 1, 0, 4, 1, 0, 512, true);
    }

    private synchronized int readUpPack(packHolder pck) {
        pck.clear();
        int i = pck.pipeRecv(conn, 0, 4, 143);
        if (i != 4) {
            return -1;
        }
        int o = pck.msbGetW(0); // type
        i = pck.msbGetW(2); // length
        pck.clear();
        if (i != pck.pipeRecv(conn, 0, i, 143)) {
            return -1;
        }
        if (debugger.ipDlepEvnt) {
            logger.debug("rx " + type2string(i));
        }
        return o;
    }

    private void doConn() {
        if (iface.client) {
            if (debugger.ipDlepEvnt) {
                logger.debug("accepting " + peer);
            }
            prtAccept ac = new prtAccept(iface.tcpCore, new pipeLine(65536, false), iface.ifcFwd, ipDlepIface.portNum, peer, 0, "dlep", -1, null, -1, -1);
            ac.wait4conn(30000);
            conn = ac.getConn(true);
        } else {
            if (debugger.ipDlepEvnt) {
                logger.debug("connecting " + peer);
            }
            conn = iface.tcpCore.streamConnect(new pipeLine(32768, false), iface.ifcFwd, 0, peer, ipDlepIface.portNum, "dlep", -1, null, -1, -1);
        }
    }

    private void doInit() {
        packHolder pck = new packHolder(true, true);
        encTlv tlv = getTlv();
        if (iface.client) {
            int i = readUpPack(pck);
            if (i != msgInitReq) {
                conn.setClose();
                return;
            }
            for (;;) {
                if (tlv.getBytes(pck)) {
                    break;
                }
                logger.debug(tlv.dump());///////////
            }
            pck.clear();
        } else {
            //////////
        }
    }

    public void run() {
        try {
            doConn();
        } catch (Exception e) {
            logger.traceback(e);
            iface.neighs.del(this);
            return;
        }
        if (conn == null) {
            if (debugger.ipDlepEvnt) {
                logger.debug("failed " + peer);
            }
            iface.neighs.del(this);
            return;
        }
        try {
            doInit();
        } catch (Exception e) {
            logger.traceback(e);
            conn.setClose();
            iface.neighs.del(this);
            return;
        }
        logger.warn("neighbor " + peer + " up");
        ///////////
        logger.warn("neighbor " + peer + " down");
        conn.setClose();
        iface.neighs.del(this);
    }

}
