package rtr;

import addr.addrEui;
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
import util.typLenVal;

/**
 * babel2 neighbor
 *
 * @author matecsaba
 */
public class rtrBabelNeigh implements rtrBfdClnt, Comparator<rtrBabelNeigh> {

    /**
     * prefixes learned from this neighbor
     */
    public final tabRoute<addrIP> learned;

    /**
     * babel interface this neighbor belongs to
     */
    protected rtrBabelIface iface;

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
    public rtrBabelNeigh(prtGenConn id) {
        conn = id;
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

    public int compare(rtrBabelNeigh o1, rtrBabelNeigh o2) {
        if (o1.conn.iface.ifwNum < o2.conn.iface.ifwNum) {
            return -1;
        }
        if (o1.conn.iface.ifwNum > o2.conn.iface.ifwNum) {
            return +1;
        }
        return o1.conn.peerAddr.compare(o1.conn.peerAddr, o2.conn.peerAddr);
    }

    private addrPrefix<addrIP> getPrefix(typLenVal tlv, int ofs, int ae, int len) {
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
        typLenVal tlv = rtrBabel.getTlv();
        addrIP rtrid = new addrIP();
        tabRoute<addrIP> oldTab = new tabRoute<addrIP>("copy");
        oldTab.mergeFrom(tabRoute.addType.better, learned, null, true, tabRouteEntry.distanLim);
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
                    rtrid.fromIPv6addr(ae.toIPv6());
                    break;
                case rtrBabel.tlvUpdate:
                    tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
                    ntry.srcRtr = conn.peerAddr.copyBytes();
                    ntry.iface = iface.iface;
                    ntry.distance = iface.distance;
                    ntry.nextHop = conn.peerAddr.copyBytes();
                    ntry.aggrRtr = rtrid.copyBytes();
                    ntry.time = tim;
                    ntry.accIgp = bits.msbGetW(tlv.valDat, 4) * 22; // interval
                    ntry.aggrAs = bits.msbGetW(tlv.valDat, 6); // seqno
                    ntry.metric = iface.metricIn + bits.msbGetW(tlv.valDat, 8); // metric
                    ntry.prefix = getPrefix(tlv, 10, bits.getByte(tlv.valDat, 0), bits.getByte(tlv.valDat, 2));
                    if (ntry.prefix == null) {
                        break;
                    }
                    if (debugger.rtrBabelTraf) {
                        logger.debug("rxnet " + ntry);
                    }
                    if (ntry.metric >= 0xffff) {
                        tabRoute.delUpdatedEntry(learned, rtrBgpUtil.safiUnicast, ntry, iface.roumapIn, iface.roupolIn, iface.prflstIn);
                    } else {
                        tabRoute.addUpdatedEntry(tabRoute.addType.always, learned, rtrBgpUtil.safiUnicast, ntry, iface.roumapIn, iface.roupolIn, iface.prflstIn);
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
                    if (ae.compare(ae, iface.lower.routerID) != 0) {
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
        return learned.differs(oldTab);
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
            if ((curTim - ntry.time) < ntry.accIgp) {
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
