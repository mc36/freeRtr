package rtr;

import addr.addrIP;
import addr.addrIPv4;
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
 * ripv2 neighbor
 *
 * @author matecsaba
 */
public class rtrRip4neigh implements rtrBfdClnt, Comparator<rtrRip4neigh> {

    /**
     * prefixes learned from this neighbor
     */
    public final tabRoute<addrIP> learned;

    /**
     * rip interface this neighbor belongs to
     */
    protected rtrRip4iface iface;

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
    public rtrRip4neigh(prtGenConn id) {
        conn = id;
        learned = new tabRoute<addrIP>("rip");
        upTime = bits.getTime();
    }

    public String toString() {
        return "rip with " + conn;
    }

    /**
     * unregister from udp
     */
    public void unregister2udp() {
        conn.setClosing();
    }

    public int compare(rtrRip4neigh o1, rtrRip4neigh o2) {
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
        if (debugger.rtrRip4traf) {
            logger.debug("rx " + conn);
        }
        if (pck.getByte(1) != rtrRip4.version) {
            logger.info("bad version " + conn);
            return false;
        }
        int cmd = pck.getByte(0); // command
        pck.getSkip(rtrRip4.sizeHead);
        if ((iface.authentication == null) && (pck.msbGetW(0) == 0xffff)) {
            logger.info("got authed " + conn);
            return false;
        }
        byte buf[] = iface.getAuthData();
        for (int i = 0; i < buf.length; i++) {
            if (pck.getByte(i) != (buf[i] & 0xff)) {
                logger.info("bad auth " + conn);
                return false;
            }
        }
        pck.getSkip(buf.length);
        if (cmd == 1) { // request
            iface.sendOutUpdates(conn);
            return false;
        }
        if (cmd != 2) { // response
            logger.info("bad command " + conn);
            return false;
        }
        tabRoute<addrIP> oldTab = new tabRoute<addrIP>("copy");
        oldTab.mergeFrom(tabRoute.addType.better, learned, null, true, tabRouteEntry.distanLim);
        for (; pck.dataSize() >= rtrRip4.sizeNtry; pck.getSkip(rtrRip4.sizeNtry)) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.rouTyp = tabRouteEntry.routeType.rip4;
            ntry.srcRtr = conn.peerAddr.copyBytes();
            ntry.iface = iface.iface;
            ntry.distance = iface.distance;
            ntry.nextHop = conn.peerAddr.copyBytes();
            if (pck.msbGetW(0) != rtrRip4.afiID) {
                logger.info("bad afi id " + conn);
                continue;
            }
            ntry.tag = pck.msbGetW(2); // route tag
            ntry.metric = pck.msbGetD(16) + iface.metricIn; // metric
            if (ntry.metric < rtrRip4.metricMin) {
                ntry.metric = rtrRip4.metricMin;
            }
            if (ntry.metric >= rtrRip4.metricMax) {
                ntry.distance = tabRouteEntry.distanMax;
                ntry.metric = rtrRip4.metricMax;
            }
            addrIPv4 net4 = new addrIPv4();
            addrIPv4 msk4 = new addrIPv4();
            addrIPv4 hop4 = new addrIPv4();
            pck.getAddr(net4, 4); // network
            pck.getAddr(msk4, 8); // netmask
            pck.getAddr(hop4, 12); // nexthop
            addrIP netI = new addrIP();
            addrIP mskI = new addrIP();
            addrIP hopI = new addrIP();
            netI.fromIPv4addr(net4);
            mskI.fromIPv4mask(msk4);
            hopI.fromIPv4addr(hop4);
            ntry.prefix = new addrPrefix<addrIP>(netI, mskI.toNetmask());
            if (!iface.iface.lower.checkConnected(hopI)) {
                hopI = conn.peerAddr.copyBytes();
            }
            if (iface.iface.lower.checkMyAddress(hopI)) {
                hopI = conn.peerAddr.copyBytes();
            }
            ntry.nextHop = hopI;
            if (debugger.rtrRip4traf) {
                logger.debug("rxnet " + ntry);
            }
            if (ntry.metric >= rtrRip4.metricMax) {
                tabRouteEntry<addrIP> old = learned.find(ntry);
                if (old == null) {
                    continue;
                }
                if (old.metric >= rtrRip4.metricMax) {
                    continue;
                }
            }
            tabRoute.addUpdatedEntry(tabRoute.addType.always, learned, rtrBgpUtil.safiUnicast, ntry, iface.roumapIn, iface.roupolIn, iface.prflstIn);
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
        conn.timeout = iface.flushTimer * 2;
        conn.workInterval = iface.updateTimer;
        int done = 0;
        for (int i = learned.size() - 1; i >= 0; i--) {
            tabRouteEntry<addrIP> ntry = learned.get(i);
            if (ntry == null) {
                continue;
            }
            if ((curTim - ntry.time) < iface.holdTimer) {
                continue;
            }
            ntry.metric = rtrRip4.metricMax;
            if (ntry.distance < tabRouteEntry.distanMax) {
                if (debugger.rtrRip4evnt) {
                    logger.debug("netdown " + ntry);
                }
                ntry.distance = tabRouteEntry.distanMax;
                done++;
            }
            if ((curTim - ntry.time) < iface.flushTimer) {
                continue;
            }
            if (debugger.rtrRip4evnt) {
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
