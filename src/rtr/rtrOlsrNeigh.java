package rtr;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrPrefix;
import java.util.Comparator;
import pack.packHolder;
import prt.prtGenConn;
import tab.tabRoute;
import tab.tabRouteEntry;
import util.bits;
import util.debugger;
import util.logger;

/**
 * olsr neighbor
 *
 * @author matecsaba
 */
public class rtrOlsrNeigh implements rtrBfdClnt, Comparator<rtrOlsrNeigh> {

    /**
     * prefixes learned from this neighbor
     */
    public final tabRoute<addrIP> learned;

    /**
     * olsr interface this neighbor belongs to
     */
    protected rtrOlsrIface iface;

    /**
     * udp connection for this neighbor
     */
    protected prtGenConn conn;

    /**
     * uptime
     */
    protected long upTime;

    /**
     * create one neighbor
     *
     * @param id current connection
     */
    public rtrOlsrNeigh(prtGenConn id) {
        conn = id;
        learned = new tabRoute<addrIP>("olsr");
        upTime = bits.getTime();
    }

    public String toString() {
        return "olsr with " + conn;
    }

    /**
     * unregister from udp
     */
    public void unregister2udp() {
        conn.setClosing();
    }

    public int compare(rtrOlsrNeigh o1, rtrOlsrNeigh o2) {
        if (o1.conn.iface.ifwNum < o2.conn.iface.ifwNum) {
            return -1;
        }
        if (o1.conn.iface.ifwNum > o2.conn.iface.ifwNum) {
            return +1;
        }
        return o1.conn.peerAddr.compare(o1.conn.peerAddr, o2.conn.peerAddr);
    }

    /**
     * got one packet from neighbor
     *
     * @param pck packet received
     * @return false if successfully parsed, true if error happened
     */
    public synchronized boolean gotPack(packHolder pck) {
        if (debugger.rtrOlsrTraf) {
            logger.debug("rx " + conn);
        }
        int i = pck.msbGetW(0);
        if (pck.dataSize() < i) {
            logger.info("truncated " + conn);
            return true;
        }
        pck.setDataSize(i);
        int pckSeq = pck.msbGetW(2);
        pck.getSkip(4);
        tabRoute<addrIP> oldTab = new tabRoute<addrIP>("copy");
        oldTab.mergeFrom(tabRoute.addType.better, learned, null, true, tabRouteEntry.distanLim);
        long tim = bits.getTime();
        boolean ipv4 = conn.peerAddr.isIPv4();
        for (;;) {
            if (pck.dataSize() < 1) {
                break;
            }
            int msgTyp = pck.getByte(0);
            int vldTim = rtrOlsr.mant2tim(pck.getByte(1));
            i = pck.msbGetW(2) - 8;
            pck.getSkip(4);
            addrIP orig = new addrIP();
            if (ipv4) {
                addrIPv4 a4 = new addrIPv4();
                pck.getAddr(a4, 0);
                pck.getSkip(addrIPv4.size);
                i -= addrIPv4.size;
                orig.fromIPv4addr(a4);
            } else {
                addrIPv6 a6 = new addrIPv6();
                pck.getAddr(a6, 0);
                pck.getSkip(addrIPv6.size);
                i -= addrIPv6.size;
                orig.fromIPv6addr(a6);
            }
            int ttl = pck.getByte(0);
            int hop = pck.getByte(1);
            int msgSeq = pck.msbGetW(2);
            pck.getSkip(4);
            if (i < 0) {
                logger.info("bad length " + conn);
                break;
            }
            if (pck.dataSize() < i) {
                logger.info("truncated " + conn);
                return true;
            }
            byte[] buf = new byte[i];
            pck.getCopy(buf, 0, 0, buf.length);
            pck.getSkip(i);
            switch (msgTyp) {
                case rtrOlsr.typLqHello:
                    break;
                case rtrOlsr.typLqTc:
                    break;
                case rtrOlsr.typHello:
                    break;
                case rtrOlsr.typTc:
                    break;
                case rtrOlsr.typMid:
                    break;
                case rtrOlsr.typHna:
                    tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
                    ntry.iface = iface.iface;
                    ntry.srcRtr = conn.peerAddr.copyBytes();
                    ntry.distance = iface.distance;
                    ntry.nextHop = conn.peerAddr.copyBytes();
                    ntry.aggrRtr = orig.copyBytes();
                    ntry.time = tim;
                    ntry.accIgp = vldTim;
                    ntry.metric = iface.metricIn + hop;
                    if (ipv4) {
                        addrIPv4 a4 = new addrIPv4();
                        addrIPv4 m4 = new addrIPv4();
                        a4.fromBuf(buf, 0);
                        m4.fromBuf(buf, addrIPv4.size);
                        ntry.prefix = addrPrefix.ip4toIP(new addrPrefix<addrIPv4>(a4, m4.toNetmask()));
                    } else {
                        addrIPv6 a6 = new addrIPv6();
                        addrIPv6 m6 = new addrIPv6();
                        a6.fromBuf(buf, 0);
                        m6.fromBuf(buf, addrIPv6.size);
                        ntry.prefix = addrPrefix.ip6toIP(new addrPrefix<addrIPv6>(a6, m6.toNetmask()));
                    }
                    if (debugger.rtrOlsrTraf) {
                        logger.debug("rxnet " + ntry);
                    }
                    tabRoute.addUpdatedEntry(tabRoute.addType.always, learned, rtrBgpUtil.safiUnicast, ntry, iface.roumapIn, iface.roupolIn, iface.prflstIn);
                    break;
                default:
                    logger.info("invalid msg " + msgTyp);
                    break;
            }
        }
        return learned.differs(oldTab);
    }

    /**
     * do one work round
     *
     * @return true if table touched, false otherwise
     */
    public synchronized boolean doWork() {
        long curTim = bits.getTime();
        conn.timeout = iface.helloHold;
        conn.workInterval = iface.helloTimer;
        int done = 0;
        for (int i = learned.size() - 1; i >= 0; i--) {
            tabRouteEntry<addrIP> ntry = learned.get(i);
            if (ntry == null) {
                continue;
            }
            if ((curTim - ntry.time) < ntry.accIgp) {
                continue;
            }
            if (debugger.rtrOlsrEvnt) {
                logger.debug("netdel " + ntry);
            }
            learned.del(ntry.prefix);
            done++;
        }
        return (done > 0);
    }

    /**
     * stop work
     */
    public void bfdPeerDown() {
        iface.lower.datagramClosed(conn);
    }

}
