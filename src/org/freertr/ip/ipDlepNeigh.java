package org.freertr.ip;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrMac;
import org.freertr.enc.encTlv;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtAccept;
import org.freertr.tab.tabGen;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * dynamic link exchange neighbor
 *
 * @author matecsaba
 */
public class ipDlepNeigh implements Runnable, Comparable<ipDlepNeigh> {

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

    private final int port;

    private pipeSide conn;

    /**
     * peer address
     */
    protected final addrIP peer;

    /**
     * clients
     */
    protected tabGen<ipDlepNeighEntry> found = new tabGen<ipDlepNeighEntry>();

    /**
     * create one instance
     *
     * @param ifc interface to use
     * @param per peer
     * @param prt port
     */
    public ipDlepNeigh(ipDlepIface ifc, addrIP per, int prt) {
        iface = ifc;
        port = prt;
        peer = per.copyBytes();
    }

    public int compareTo(ipDlepNeigh o) {
        return peer.compareTo(o.peer);
    }

    /**
     * convert type to string
     *
     * @param i type to convert
     * @return string
     */
    public static String type2string(int i) {
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

    private synchronized void sendPack(packHolder pck, int typ) {
        if (debugger.ipDlepEvnt) {
            logger.debug("tx " + type2string(typ));
        }
        pck.merge2beg();
        pck.msbPutW(0, typ); // type
        pck.msbPutW(2, pck.dataSize()); // length
        pck.putSkip(4);
        pck.merge2beg();
        pck.pipeSend(conn, 0, pck.dataSize(), 3);
    }

    private synchronized int readPack(packHolder pck) {
        pck.clear();
        int i = pck.pipeRecv(conn, 0, 4, 143);
        if (i != 4) {
            return -1;
        }
        int o = pck.msbGetW(0); // type
        i = pck.msbGetW(2); // length
        pck.clear();
        if (i > 0) {
            if (i != pck.pipeRecv(conn, 0, i, 143)) {
                return -1;
            }
        }
        if (debugger.ipDlepEvnt) {
            logger.debug("rx " + type2string(o));
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

    private void putMetrics(packHolder pck, encTlv tlv) {
        bits.msbPutQ(tlv.valDat, 0, iface.ifcIp.getBandwidth());
        tlv.putBytes(pck, typMaxRateRx, 8, tlv.valDat);
        tlv.putBytes(pck, typMaxRateTx, 8, tlv.valDat);
        tlv.putBytes(pck, typCurRateRx, 8, tlv.valDat);
        tlv.putBytes(pck, typCurRateTx, 8, tlv.valDat);
        bits.msbPutQ(tlv.valDat, 0, 10000);
        tlv.putBytes(pck, typLatency, 8, tlv.valDat);
        tlv.valDat[0] = 100;
        tlv.putBytes(pck, typResources, 1, tlv.valDat);
        tlv.putBytes(pck, typRelQualRx, 1, tlv.valDat);
        tlv.putBytes(pck, typRelQualTx, 1, tlv.valDat);
        bits.msbPutW(tlv.valDat, 0, iface.ifcIp.getMTUsize());
        tlv.putBytes(pck, typMaxXmitUnit, 2, tlv.valDat);
    }

    private boolean doInit() {
        packHolder pck = new packHolder(true, true);
        encTlv tlv = getTlv();
        if (iface.client) {
            int i = readPack(pck);
            if (i != msgInitReq) {
                return true;
            }
            pck.clear();
            tlv.valDat[0] = 0; // code
            tlv.putBytes(pck, typStatus, 1, tlv.valDat);
            bits.msbPutD(tlv.valDat, 0, iface.heartBeat);
            tlv.putBytes(pck, typHertBet, 4, tlv.valDat);
            tlv.valDat[0] = 0; // flags
            tlv.putBytes(pck, typPeerTyp, 1, tlv.valDat);
            putMetrics(pck, tlv);
            sendPack(pck, msgInitRep);
        } else {
            pck.clear();
            bits.msbPutD(tlv.valDat, 0, iface.heartBeat);
            tlv.putBytes(pck, typHertBet, 4, tlv.valDat);
            tlv.valDat[0] = 0; // flags
            tlv.putBytes(pck, typPeerTyp, 1, tlv.valDat);
            sendPack(pck, msgInitReq);
            int i = readPack(pck);
            if (i != msgInitRep) {
                return true;
            }
        }
        return false;
    }

    private boolean doWork() {
        packHolder pck = new packHolder(true, true);
        encTlv tlv = getTlv();
        if (conn.isClosed() != 0) {
            return true;
        }
        sendPack(pck, msgHertBet);
        if (iface.client) {
            tabGen<ipDlepNeighEntry> fresh = new tabGen<ipDlepNeighEntry>();
            for (int i = 0;; i++) {
                ipDlepNeighEntry cur = new ipDlepNeighEntry();
                addrIP adr = new addrIP();
                if (iface.ifcIp.getL2info(i, adr, cur.mac)) {
                    break;
                }
                fresh.add(cur);
            }
            for (int i = 0; i < fresh.size(); i++) {
                ipDlepNeighEntry cur = fresh.get(i);
                if (found.find(cur) != null) {
                    continue;
                }
                found.put(cur);
                pck.clear();
                tlv.putAddr(pck, typMacAddr, cur.mac);
                putMetrics(pck, tlv);
                sendPack(pck, msgUpReq);
            }
            for (int i = found.size() - 1; i >= 0; i--) {
                ipDlepNeighEntry cur = found.get(i);
                if (fresh.find(cur) != null) {
                    continue;
                }
                found.del(cur);
                pck.clear();
                tlv.putAddr(pck, typMacAddr, cur.mac);
                sendPack(pck, msgDownReq);
            }
        }
        if (conn.ready2rx() < 1) {
            return false;
        }
        int i = readPack(pck);
        addrMac adr = null;
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            if (tlv.valTyp != typMacAddr) {
                continue;
            }
            adr = new addrMac();
            adr.fromBuf(tlv.valDat, 0);
        }
        if (adr == null) {
            return false;
        }
        if (iface.client) {
            return false;
        }
        ipDlepNeighEntry cur = new ipDlepNeighEntry();
        cur.mac.setAddr(adr);
        switch (i) {
            case msgUpReq:
                i = msgUpRep;
                found.add(cur);
                break;
            case msgDownReq:
                i = msgDownRep;
                found.del(cur);
                break;
            case msgAnnoReq:
                i = msgAnnoRep;
                found.add(cur);
                break;
            case msgLinkReq:
                i = msgLinkRep;
                found.add(cur);
                break;
            default:
                return false;
        }
        pck.clear();
        tlv.valDat[0] = 0; // code
        tlv.putBytes(pck, typStatus, 1, tlv.valDat);
        tlv.putAddr(pck, typMacAddr, adr);
        sendPack(pck, i);
        return false;
    }

    /**
     * start working
     */
    protected void start() {
        logger.startThread(this);
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
            if (doInit()) {
                conn.setClose();
                return;
            }
        } catch (Exception e) {
            logger.traceback(e);
            conn.setClose();
            iface.neighs.del(this);
            return;
        }
        logger.warn("neighbor " + peer + " up");
        for (;;) {
            bits.sleep(iface.heartBeat);
            try {
                if (doWork()) {
                    break;
                }
            } catch (Exception e) {
                logger.traceback(e);
                break;
            }
        }
        logger.warn("neighbor " + peer + " down");
        conn.setClose();
        iface.neighs.del(this);
    }

    /**
     * get show
     *
     * @param l list to append
     */
    public void getShClnts(userFormat l) {
        for (int i = 0; i < found.size(); i++) {
            ipDlepNeighEntry ntry = found.get(i);
            l.add(peer + "|" + ntry.mac);
        }
    }

}

class ipDlepNeighEntry implements Comparable<ipDlepNeighEntry> {

    public addrMac mac = new addrMac();

    public int compareTo(ipDlepNeighEntry o) {
        return mac.compareTo(o.mac);
    }

    public String toString() {
        return "" + mac;
    }

}
