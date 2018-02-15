package rtr;

import addr.addrIP;
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
 * ripv6 neighbor
 *
 * @author matecsaba
 */
public class rtrRip6neigh implements rtrBfdClnt, Comparator<rtrRip6neigh> {

    /**
     * prefixes learned from this neighbor
     */
    public final tabRoute<addrIP> learned;

    /**
     * rip interface this neighbor belongs to
     */
    protected rtrRip6iface iface;

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
     * @param id connection handler
     */
    public rtrRip6neigh(prtGenConn id) {
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

    public int compare(rtrRip6neigh o1, rtrRip6neigh o2) {
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
        if (debugger.rtrRip6traf) {
            logger.debug("rx " + conn);
        }
        if (pck.getByte(1) != rtrRip6.version) {
            logger.info("bad version " + conn);
            return false;
        }
        int cmd = pck.getByte(0); // command
        pck.getSkip(rtrRip6.sizeHead);
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
        for (; pck.dataSize() >= rtrRip6.sizeNtry; pck.getSkip(rtrRip6.sizeNtry)) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.rouTyp = tabRouteEntry.routeType.rip6;
            ntry.srcRtr = conn.peerAddr.copyBytes();
            ntry.iface = iface.iface;
            ntry.distance = iface.distance;
            ntry.nextHop = conn.peerAddr.copyBytes();
            ntry.tag = pck.msbGetW(16); // route tag
            ntry.metric = pck.getByte(19) + iface.metricIn; // metric
            if (ntry.metric < rtrRip6.metricMin) {
                ntry.metric = rtrRip6.metricMin;
            }
            if (ntry.metric >= rtrRip6.metricMax) {
                ntry.distance = tabRouteEntry.distanMax;
                ntry.metric = rtrRip6.metricMax;
            }
            addrIPv6 net6 = new addrIPv6();
            addrIPv6 msk6 = new addrIPv6();
            pck.getAddr(net6, 0); // network
            msk6.fromNetmask(pck.getByte(18)); // netmask
            addrIP netI = new addrIP();
            addrIP mskI = new addrIP();
            netI.fromIPv6addr(net6);
            mskI.fromIPv6mask(msk6);
            ntry.prefix = new addrPrefix<addrIP>(netI, mskI.toNetmask());
            ntry.nextHop = conn.peerAddr.copyBytes();
            if (debugger.rtrRip6traf) {
                logger.debug("rxnet " + ntry);
            }
            if (ntry.metric >= rtrRip6.metricMax) {
                tabRouteEntry<addrIP> old = learned.find(ntry);
                if (old == null) {
                    continue;
                }
                if (old.metric >= rtrRip6.metricMax) {
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
            ntry.metric = rtrRip6.metricMax;
            if (ntry.distance < tabRouteEntry.distanMax) {
                if (debugger.rtrRip6evnt) {
                    logger.debug("netdown " + ntry);
                }
                ntry.distance = tabRouteEntry.distanMax;
                done++;
            }
            if ((curTim - ntry.time) < iface.flushTimer) {
                continue;
            }
            if (debugger.rtrRip6evnt) {
                logger.debug("netdel " + ntry);
            }
            learned.del(ntry.prefix);
            done++;
        }
        return (done > 0);
    }

    public void bfdPeerDown() {
        iface.lower.datagramClosed(conn);
    }

}
