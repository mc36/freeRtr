package ip;

import addr.addrIP;
import addr.addrMac;
import clnt.clntProxy;
import ifc.ifcDn;
import ifc.ifcNull;
import ifc.ifcUp;
import java.util.Comparator;
import java.util.Timer;
import java.util.TimerTask;
import pack.packHolder;
import pipe.pipeSide;
import prt.prtTcp;
import prt.prtUdp;
import serv.servGeneric;
import tab.tabGen;
import user.userFormat;
import util.bits;
import util.counter;
import util.logger;
import util.state;

/**
 * transparent proxy
 *
 * @author matecsaba
 */
public class ipProxy implements ifcUp {

    /**
     * proxy to use
     */
    public final clntProxy upper;

    private ifcDn lower = new ifcNull();

    private addrMac hwaddr = new addrMac();

    private addrMac lastadr = new addrMac();

    private counter cntr = new counter();

    private Timer timer;

    private ipCor4 ip4;

    private ipCor6 ip6;

    private tabGen<ipProxyConn> tcp;

    private tabGen<ipProxyConn> udp;

    /**
     * create proxy handler
     *
     * @param parent parent of me
     */
    public ipProxy(clntProxy parent) {
        upper = parent;
        resetTimer(true);
        ip4 = new ipCor4();
        ip6 = new ipCor6();
        tcp = new tabGen<ipProxyConn>();
        udp = new tabGen<ipProxyConn>();
    }

    /**
     * get list of connections
     *
     * @return list of entries
     */
    public userFormat getShConn() {
        userFormat lst = new userFormat("|", "prt|src|trg|idle");
        getShConn(lst, tcp, "tcp");
        getShConn(lst, udp, "udp");
        return lst;
    }

    private void getShConn(userFormat lst, tabGen<ipProxyConn> tab, String str) {
        for (int i = 0; i < tab.size(); i++) {
            ipProxyConn ntry = tab.get(i);
            lst.add(str + "|" + ntry);
        }
    }

    private void resetTimer(boolean needRun) {
        try {
            timer.cancel();
        } catch (Exception e) {
        }
        if (!needRun) {
            return;
        }
        timer = new Timer();
        ipProxyTimer task = new ipProxyTimer(this);
        timer.schedule(task, 500, 1000);
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
     * set parent
     *
     * @param parent parent
     */
    public void setParent(ifcDn parent) {
        lower = parent;
        try {
            hwaddr = (addrMac) lower.getHwAddr();
        } catch (Exception e) {
        }
    }

    /**
     * set state
     *
     * @param stat state
     */
    public void setState(state.states stat) {
        cntr.stateChange(stat);
        resetTimer(state.toUsable(stat) == state.states.up);
    }

    /**
     * close interface
     */
    public void closeUp() {
        resetTimer(false);
    }

    public String toString() {
        return "transproxy on " + lower;
    }

    /**
     * purge tables
     */
    protected void doTablePurge() {
        byte[] buf = new byte[1024];
        packHolder pck = new packHolder(true, true);
        long tim = bits.getTime();
        for (int i = udp.size() - 1; i >= 0; i--) {
            ipProxyConn ntry = udp.get(i);
            if (ntry.pipe.isClosed() != 0) {
                udp.del(ntry);
                continue;
            }
            for (;;) {
                int siz = ntry.pipe.nonBlockGet(buf, 0, buf.length);
                if (siz < 1) {
                    break;
                }
                ntry.tim = tim;
                pck.clear();
                ntry.toPack(pck);
                pck.putCopy(buf, 0, 0, siz);
                pck.putSkip(siz);
                pck.merge2beg();
                prtUdp.createUDPheader(pck);
                sendPack(pck);
            }
        }
        for (int i = tcp.size() - 1; i >= 0; i--) {
            ipProxyConn ntry = tcp.get(i);
            if (ntry.pipe.isClosed() != 0) {
                tcp.del(ntry);
                pck.clear();
                ntry.toPack(pck);
                sendTcp(pck, prtTcp.flagRST);
                continue;
            }
            if (ntry.buf == null) {
                int siz = ntry.pipe.nonBlockGet(buf, 0, buf.length);
                if (siz < 1) {
                    continue;
                }
                ntry.buf = new byte[siz];
                bits.byteCopy(buf, 0, ntry.buf, 0, siz);
            }
            pck.clear();
            pck.putCopy(ntry.buf, 0, 0, ntry.buf.length);
            pck.putSkip(ntry.buf.length);
            ntry.toPack(pck);
            sendTcp(pck, prtTcp.flagPshAck);
        }
    }

    private void sendPack(packHolder pck) {
        int typ;
        if (pck.IPtrg.isIPv4()) {
            ip4.createIPheader(pck);
            typ = ipIfc4.type;
        } else {
            ip6.createIPheader(pck);
            typ = ipIfc6.type;
        }
        pck.msbPutW(0, typ);
        pck.putSkip(2);
        pck.merge2beg();
        pck.ETHsrc.setAddr(hwaddr);
        pck.ETHtrg.setAddr(lastadr);
        lower.sendPack(pck);
    }

    private void sendTcp(packHolder pck, int flg) {
        pck.merge2beg();
        pck.TCPflg = flg;
        pck.TCPwin = 32768;
        prtTcp.createTCPheader(pck, null);
        pck.merge2beg();
        sendPack(pck);
    }

    /**
     * received packet
     *
     * @param pck packet
     */
    public void recvPack(packHolder pck) {
        cntr.rx(pck);
        lastadr.setAddr(pck.ETHsrc);
        int typ = pck.msbGetW(0);
        pck.getSkip(2);
        switch (typ) {
            case ipIfc4.type:
                if (ip4.parseIPheader(pck, true)) {
                    return;
                }
                break;
            case ipIfc6.type:
                if (ip6.parseIPheader(pck, true)) {
                    return;
                }
                break;
            default:
                return;
        }
        pck.getSkip(pck.IPsiz);
        if (pck.IPtrg.isMulticast()) {
            return;
        }
        ipProxyConn conn;
        byte[] buf;
        switch (pck.IPprt) {
            case prtUdp.protoNum:
                if (prtUdp.parseUDPheader(pck)) {
                    return;
                }
                buf = pck.getCopy();
                pck.getSkip(buf.length);
                conn = udp.find(ipProxyConn.fromPack(pck));
                if (conn != null) {
                    conn.tim = bits.getTime();
                    conn.pipe.nonBlockPut(buf, 0, buf.length);
                    return;
                }
                conn = ipProxyConn.fromPack(pck);
                conn.pipe = upper.doConnect(servGeneric.protoUdp, conn.trgA, conn.trgP, "transproxy");
                if (conn.pipe == null) {
                    return;
                }
                conn.pipe.setReady();
                conn.tim = bits.getTime();
                udp.put(conn);
                conn.pipe.nonBlockPut(buf, 0, buf.length);
                break;
            case prtTcp.protoNum:
                if (prtTcp.parseTCPheader(pck)) {
                    return;
                }
                buf = pck.getCopy();
                pck.getSkip(buf.length);
                conn = tcp.find(ipProxyConn.fromPack(pck));
                if (conn == null) {
                    if ((pck.TCPflg & prtTcp.flagSYN) == 0) {
                        conn = ipProxyConn.fromPack(pck);
                        conn.toPack(pck);
                        pck.putStart();
                        sendTcp(pck, prtTcp.flagRST);
                        return;
                    }
                    conn = ipProxyConn.fromPack(pck);
                    conn.pipe = upper.doConnect(servGeneric.protoTcp, conn.trgA, conn.trgP, "transproxy");
                    if (conn.pipe == null) {
                        conn.toPack(pck);
                        pck.putStart();
                        sendTcp(pck, prtTcp.flagRST);
                        return;
                    }
                    conn.seqR = pck.TCPseq + 1;
                    conn.seqS = bits.randomD();
                    conn.pipe.setReady();
                    tcp.put(conn);
                }
                conn.tim = bits.getTime();
                if ((pck.TCPflg & prtTcp.flagSYN) != 0) {
                    conn.toPack(pck);
                    pck.putStart();
                    pck.TCPmss = 1024;
                    pck.TCPseq--;
                    sendTcp(pck, prtTcp.flagSynAck);
                    return;
                }
                if ((pck.TCPflg & prtTcp.flagFIN) != 0) {
                    conn.pipe.setClose();
                    return;
                }
                if ((pck.TCPflg & prtTcp.flagRST) != 0) {
                    conn.pipe.setClose();
                    return;
                }
                if (conn.buf != null) {
                    if (pck.TCPack == (conn.seqS + conn.buf.length)) {
                        conn.seqS += conn.buf.length;
                        conn.buf = null;
                    }
                }
                if (pck.TCPseq != conn.seqR) {
                    conn.toPack(pck);
                    pck.putStart();
                    sendTcp(pck, prtTcp.flagACK);
                    return;
                }
                if (conn.pipe.nonBlockPut(buf, 0, buf.length) != buf.length) {
                    return;
                }
                conn.seqR += buf.length;
                conn.toPack(pck);
                pck.putStart();
                sendTcp(pck, prtTcp.flagACK);
                break;
            default:
                return;
        }
    }

}

class ipProxyConn implements Comparator<ipProxyConn> {

    public addrIP srcA;

    public addrIP trgA;

    public int srcP;

    public int trgP;

    public int seqR;

    public int seqS;

    public long tim;

    public pipeSide pipe;

    public byte[] buf;

    public static ipProxyConn fromPack(packHolder pck) {
        ipProxyConn conn = new ipProxyConn();
        conn.srcA = pck.IPsrc.copyBytes();
        conn.trgA = pck.IPtrg.copyBytes();
        conn.srcP = pck.UDPsrc;
        conn.trgP = pck.UDPtrg;
        return conn;
    }

    public void toPack(packHolder pck) {
        pck.UDPsrc = trgP;
        pck.UDPtrg = srcP;
        pck.IPsrc.setAddr(trgA);
        pck.IPtrg.setAddr(srcA);
        pck.TCPack = seqR;
        pck.TCPseq = seqS;
    }

    public String toString() {
        return srcA + " " + srcP + "|" + trgA + " " + trgP + "|" + bits.timePast(tim);
    }

    public int compare(ipProxyConn o1, ipProxyConn o2) {
        if (o1.srcP < o2.srcP) {
            return -1;
        }
        if (o1.srcP > o2.srcP) {
            return +1;
        }
        if (o1.trgP < o2.trgP) {
            return -1;
        }
        if (o1.trgP > o2.trgP) {
            return +1;
        }
        int i = o1.srcA.compare(o1.srcA, o2.srcA);
        if (i != 0) {
            return i;
        }
        return o1.srcA.compare(o1.trgA, o2.trgA);
    }

}

class ipProxyTimer extends TimerTask {

    private ipProxy parent;

    public ipProxyTimer(ipProxy prnt) {
        parent = prnt;
    }

    public void run() {
        try {
            parent.doTablePurge();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
