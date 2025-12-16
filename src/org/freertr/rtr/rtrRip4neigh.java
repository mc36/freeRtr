package org.freertr.rtr;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrPrefix;
import org.freertr.pack.packHolder;
import org.freertr.prt.prtGenConn;
import org.freertr.sec.secInfoWrk;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * ripv2 neighbor
 *
 * @author matecsaba
 */
public class rtrRip4neigh implements rtrBfdClnt, Comparable<rtrRip4neigh> {

    /**
     * ipinfo result
     */
    public secInfoWrk ipInfoRes;

    /**
     * prefixes learned from this neighbor
     */
    public final tabRoute<addrIP> learned;

    /**
     * rip interface this neighbor belongs to
     */
    protected final rtrRip4iface iface;

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
    public rtrRip4neigh(prtGenConn id, rtrRip4iface ifc) {
        conn = id;
        iface = ifc;
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

    public int compareTo(rtrRip4neigh o) {
        if (conn.iface.ifwNum < o.conn.iface.ifwNum) {
            return -1;
        }
        if (conn.iface.ifwNum > o.conn.iface.ifwNum) {
            return +1;
        }
        return conn.peerAddr.compareTo(o.conn.peerAddr);
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
            return true;
        }
        int cmd = pck.getByte(0); // command
        pck.getSkip(rtrRip4.sizeHead);
        if ((iface.authentication == null) && (pck.msbGetW(0) == 0xffff)) {
            logger.info("got authed " + conn);
            return true;
        }
        byte[] buf = iface.getAuthData();
        for (int i = 0; i < buf.length; i++) {
            if (pck.getByte(i) != (buf[i] & 0xff)) {
                logger.info("bad auth " + conn);
                return true;
            }
        }
        pck.getSkip(buf.length);
        if (cmd == 1) { // request
            iface.sendOutUpdates(conn);
            return true;
        }
        if (cmd != 2) { // response
            logger.info("bad command " + conn);
            return true;
        }
        for (; pck.dataSize() >= rtrRip4.sizeNtry; pck.getSkip(rtrRip4.sizeNtry)) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.best.rouTyp = tabRouteAttr.routeType.rip4;
            ntry.best.srcRtr = conn.peerAddr.copyBytes();
            ntry.best.iface = iface.iface;
            ntry.best.distance = iface.distance;
            ntry.best.nextHop = conn.peerAddr.copyBytes();
            if (pck.msbGetW(0) != rtrRip4.afiID) {
                logger.info("bad afi id " + conn);
                continue;
            }
            ntry.best.tag = pck.msbGetW(2); // route tag
            ntry.best.metric = pck.msbGetD(16) + iface.metricIn; // metric
            if (ntry.best.metric < rtrRip4.metricMin) {
                ntry.best.metric = rtrRip4.metricMin;
            }
            if (ntry.best.metric >= rtrRip4.metricMax) {
                ntry.best.distance = tabRouteAttr.distanMax;
                ntry.best.metric = rtrRip4.metricMax;
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
            ntry.best.nextHop = hopI;
            if (debugger.rtrRip4traf) {
                logger.debug("rxnet " + ntry);
            }
            if (ntry.best.metric >= rtrRip4.metricMax) {
                tabRouteEntry<addrIP> old = learned.find(ntry);
                if (old == null) {
                    continue;
                }
                if (old.best.metric >= rtrRip4.metricMax) {
                    continue;
                }
            }
            tabRoute.addUpdatedEntry(tabRoute.addType.always, learned, rtrBgpUtil.sfiUnicast, 0, ntry, true, iface.roumapIn, iface.roupolIn, iface.prflstIn);
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
        conn.timeout = iface.flushTimer * 2;
        conn.workInterval = iface.updateTimer;
        int done = 0;
        for (int i = learned.size() - 1; i >= 0; i--) {
            tabRouteEntry<addrIP> ntry = learned.get(i);
            if (ntry == null) {
                continue;
            }
            if ((curTim - ntry.best.time) < iface.holdTimer) {
                continue;
            }
            ntry.best.metric = rtrRip4.metricMax;
            if (ntry.best.distance < tabRouteAttr.distanMax) {
                if (debugger.rtrRip4evnt) {
                    logger.debug("netdown " + ntry);
                }
                ntry.best.distance = tabRouteAttr.distanMax;
                done++;
            }
            if ((curTim - ntry.best.time) < iface.flushTimer) {
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
