package org.freertr.rtr;

import org.freertr.addr.addrEui;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrPrefix;
import org.freertr.pack.packHolder;
import org.freertr.prt.prtGenConn;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteEntry;
import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.enc.encTlv;

/**
 * babel2 neighbor
 *
 * @author matecsaba
 */
public class rtrBabelNeigh implements rtrBfdClnt, Comparable<rtrBabelNeigh> {

    /**
     * prefixes learned from this neighbor
     */
    public final tabRoute<addrIP> learned;

    /**
     * babel interface this neighbor belongs to
     */
    protected final rtrBabelIface iface;

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
     * @param ifc current interface
     */
    public rtrBabelNeigh(prtGenConn id, rtrBabelIface ifc) {
        conn = id;
        iface = ifc;
        learned = new tabRoute<addrIP>("babel");
        upTime = bits.getTime();
    }

    public String toString() {
        return "babel with " + conn;
    }

    /**
     * unregister from udp
     */
    public void unregister2udp() {
        conn.setClosing();
    }

    public int compareTo(rtrBabelNeigh o) {
        if (conn.iface.ifwNum < o.conn.iface.ifwNum) {
            return -1;
        }
        if (conn.iface.ifwNum > o.conn.iface.ifwNum) {
            return +1;
        }
        return conn.peerAddr.compareTo(o.conn.peerAddr);
    }

    private addrPrefix<addrIP> getPrefix(encTlv tlv, int ofs, int ae, int len) {
        switch (ae) {
            case 1: // ipv4
                addrIPv4 a4 = new addrIPv4();
                a4.fromBuf(tlv.valDat, ofs);
                return addrPrefix.ip4toIP(new addrPrefix<addrIPv4>(a4, len));
            case 2: // ipv6
                addrIPv6 a6 = new addrIPv6();
                a6.fromBuf(tlv.valDat, ofs);
                return addrPrefix.ip6toIP(new addrPrefix<addrIPv6>(a6, len));
            default:
                return null;
        }
    }

    /**
     * got one packet from neighbor
     *
     * @param pck packet received
     * @return false if successfully parsed, true if error happened
     */
    public synchronized boolean gotPack(packHolder pck) {
        if (debugger.rtrBabelTraf) {
            logger.debug("rx " + conn);
        }
        if (pck.getByte(0) != rtrBabel.magic) {
            logger.info("bad magic " + conn);
            return true;
        }
        if (pck.getByte(1) != rtrBabel.version) {
            logger.info("bad version " + conn);
            return true;
        }
        int i = pck.msbGetW(2); // size
        pck.getSkip(rtrBabel.size);
        if (pck.dataSize() < i) {
            logger.info("truncated " + conn);
            return true;
        }
        pck.setDataSize(i);
        encTlv tlv = rtrBabel.getTlv();
        addrIP rtrid = new addrIP();
        long tim = bits.getTime();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case rtrBabel.tlvHello:
                    break;
                case rtrBabel.tlvIhu:
                    break;
                case rtrBabel.tlvNxtHop:
                    break;
                case rtrBabel.tlvRtrId:
                    addrEui ae = new addrEui();
                    ae.fromBuf(tlv.valDat, 2); // router id
                    rtrid.fromIPv6addr(ae.toIPv6(null));
                    break;
                case rtrBabel.tlvUpdate:
                    tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
                    ntry.best.srcRtr = conn.peerAddr.copyBytes();
                    ntry.best.iface = iface.iface;
                    ntry.best.distance = iface.distance;
                    ntry.best.nextHop = conn.peerAddr.copyBytes();
                    ntry.best.aggrRtr = rtrid.copyBytes();
                    ntry.best.time = tim;
                    ntry.best.accIgp = bits.msbGetW(tlv.valDat, 4) * 22; // interval
                    ntry.best.aggrAs = bits.msbGetW(tlv.valDat, 6); // seqno
                    ntry.best.metric = iface.metricIn + bits.msbGetW(tlv.valDat, 8); // metric
                    ntry.prefix = getPrefix(tlv, 10, bits.getByte(tlv.valDat, 0), bits.getByte(tlv.valDat, 2));
                    if (ntry.prefix == null) {
                        break;
                    }
                    if (debugger.rtrBabelTraf) {
                        logger.debug("rxnet " + ntry);
                    }
                    if (ntry.best.metric >= 0xffff) {
                        tabRoute.delUpdatedEntry(learned, rtrBgpUtil.sfiUnicast, 0, ntry, iface.roumapIn, iface.roupolIn, iface.prflstIn);
                    } else {
                        tabRoute.addUpdatedEntry(tabRoute.addType.always, learned, rtrBgpUtil.sfiUnicast, 0, ntry, true, iface.roumapIn, iface.roupolIn, iface.prflstIn);
                    }
                    break;
                case rtrBabel.tlvRouReq:
                    ntry = new tabRouteEntry<addrIP>();
                    ntry.prefix = getPrefix(tlv, 2, bits.getByte(tlv.valDat, 0), bits.getByte(tlv.valDat, 1));
                    if (ntry.prefix == null) {
                        break;
                    }
                    if (debugger.rtrBabelTraf) {
                        logger.debug("reqnet " + ntry);
                    }
                    break;
                case rtrBabel.tlvSeqReq:
                    ae = new addrEui();
                    ae.fromBuf(tlv.valDat, 6);
                    if (ae.compareTo(iface.lower.routerID) != 0) {
                        break;
                    }
                    iface.lower.incSeq();
                    if (debugger.rtrBabelTraf) {
                        logger.debug("seqno");
                    }
                    break;
                default:
                    logger.info("invalid tlv " + tlv.valTyp);
                    break;
            }
        }
        return false;
    }

    /**
     * do one work round
     *
     * @return true if table touched, false otherwise
     */
    public synchronized boolean doWork() {
        long curTim = bits.getTime();
        conn.timeout = iface.updateTimer * 4;
        conn.workInterval = iface.updateTimer;
        int done = 0;
        for (int i = learned.size() - 1; i >= 0; i--) {
            tabRouteEntry<addrIP> ntry = learned.get(i);
            if (ntry == null) {
                continue;
            }
            if ((curTim - ntry.best.time) < ntry.best.accIgp) {
                continue;
            }
            if (debugger.rtrBabelEvnt) {
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
