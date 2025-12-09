package org.freertr.rtr;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipRtr;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;

/**
 * route logger
 *
 * @author matecsaba
 */
public class rtrLogger extends ipRtr {

    /**
     * the forwarder protocol
     */
    public final ipFwd fwdCore;

    /**
     * route type
     */
    protected final tabRouteAttr.routeType rouTyp;

    /**
     * router number
     */
    protected final int rtrNum;

    /**
     * old unicast
     */
    protected tabRoute<addrIP> oldU;

    /**
     * old multicast
     */
    protected tabRoute<addrIP> oldM;

    /**
     * old flowspec
     */
    protected tabRoute<addrIP> oldF;

    /**
     * flaps
     */
    protected tabGen<rtrLoggerFlap> flaps;

    /**
     * logging
     */
    protected boolean logging;

    /**
     * address family
     */
    protected int afi;

    /**
     * create logger process
     *
     * @param forwarder forwarder to update
     * @param id process id
     */
    public rtrLogger(ipFwd forwarder, int id) {
        fwdCore = forwarder;
        rtrNum = id;
        switch (fwdCore.ipVersion) {
            case ipCor4.protocolVersion:
                rouTyp = tabRouteAttr.routeType.logger4;
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteAttr.routeType.logger6;
                break;
            default:
                rouTyp = null;
                break;
        }
        afi = 1;
        routerComputedU = new tabRoute<addrIP>("rx");
        routerComputedM = new tabRoute<addrIP>("rx");
        routerComputedF = new tabRoute<addrIP>("rx");
        routerComputedI = new tabGen<tabIndex<addrIP>>();
        oldU = new tabRoute<addrIP>("rx");
        oldM = new tabRoute<addrIP>("rx");
        oldF = new tabRoute<addrIP>("rx");
        routerCreateComputed();
        fwdCore.routerAdd(this, rouTyp, id);
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "logger on " + fwdCore;
    }

    /**
     * count prefix lengths
     *
     * @param ver ip version
     * @param tab routing table
     * @return prefix length report
     */
    public static userFormat prefixLengths(tabRoute<addrIP> tab, int ver) {
        int[] res = new int[(addrIP.size * 8) + 1];
        for (int i = 0; i < tab.size(); i++) {
            tabRouteEntry<addrIP> ntry = tab.get(i);
            res[ntry.prefix.maskLen] += ntry.alts.size();
        }
        userFormat lst = new userFormat("|", "len|count");
        if (ver == ipCor4.protocolVersion) {
            ver = (addrIP.size - addrIPv4.size) * 8;
        } else {
            ver = 0;
        }
        for (int i = ver; i < res.length; i++) {
            lst.add((i - ver) + "|" + res[i]);
        }
        return lst;
    }

    /**
     * count outgoing interfaces
     *
     * @param tab routing table
     * @return prefix length report
     */
    public static userFormat nexthopDistribution(tabRoute<addrIP> tab) {
        tabGen<rtrLoggerAdr> res = new tabGen<rtrLoggerAdr>();
        for (int o = 0; o < tab.size(); o++) {
            tabRouteEntry<addrIP> ntry = tab.get(o);
            for (int i = 0; i < ntry.alts.size(); i++) {
                tabRouteAttr<addrIP> attr = ntry.alts.get(i);
                addrIP hop = attr.oldHop;
                if (hop == null) {
                    hop = attr.nextHop;
                }
                if (hop == null) {
                    continue;
                }
                rtrLoggerAdr adr = new rtrLoggerAdr(hop);
                rtrLoggerAdr old = res.add(adr);
                if (old != null) {
                    adr = old;
                }
                adr.count++;
            }
        }
        userFormat lst = new userFormat("|", "nexthop|count");
        for (int i = 0; i < res.size(); i++) {
            lst.add("" + res.get(i));
        }
        return lst;
    }

    /**
     * count outgoing interfaces
     *
     * @param tab routing table
     * @return prefix length report
     */
    public static userFormat outgointInterfaces(tabRoute<addrIP> tab) {
        tabGen<rtrLoggerIfc> res = new tabGen<rtrLoggerIfc>();
        for (int o = 0; o < tab.size(); o++) {
            tabRouteEntry<addrIP> ntry = tab.get(o);
            for (int i = 0; i < ntry.alts.size(); i++) {
                tabRouteAttr<addrIP> attr = ntry.alts.get(i);
                rtrLoggerIfc ifc = new rtrLoggerIfc((ipFwdIface) attr.iface);
                rtrLoggerIfc old = res.add(ifc);
                if (old != null) {
                    ifc = old;
                }
                ifc.count++;
            }
        }
        userFormat lst = new userFormat("|", "iface|count");
        for (int i = 0; i < res.size(); i++) {
            lst.add("" + res.get(i));
        }
        return lst;
    }

    /**
     * count prefix lengths
     *
     * @return prefix length report
     */
    public userFormat prefixLengths() {
        return prefixLengths(oldU, fwdCore.ipVersion);
    }

    /**
     * count outgoing interfaces
     *
     * @return interface report
     */
    public userFormat outgoingInterfaces() {
        return outgointInterfaces(oldU);
    }

    /**
     * afi to string
     *
     * @param i afi
     * @return string
     */
    public static String afi2str(int i) {
        switch (i) {
            case 1:
                return "unicast";
            case 2:
                return "multicast";
            case 3:
                return "flowspec";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * string to afi
     *
     * @param s string
     * @return afi, -1 on error
     */
    public static int str2afi(String s) {
        if (s.equals("unicast")) {
            return 1;
        }
        if (s.equals("multicast")) {
            return 2;
        }
        if (s.equals("flowspec")) {
            return 3;
        }
        return -1;
    }

    /**
     * prefix to string
     *
     * @param i afi
     * @param p prefix
     * @return string
     */
    public static String prf2str(int i, addrPrefix<addrIP> p) {
        switch (i) {
            case 1:
                return addrPrefix.ip2str(p);
            case 2:
                return addrPrefix.ip2str(p);
            case 3:
                return addrPrefix.ip2evpn(p);
            default:
                return null;
        }
    }

    /**
     * clear flap stats
     */
    public void clearFlapstat() {
        flaps = new tabGen<rtrLoggerFlap>();
    }

    /**
     * get flap stats
     *
     * @param cnt minimum counter
     * @return list of statistics
     */
    public userFormat getFlapstat(int cnt) {
        userFormat l = new userFormat("|", "afi|prefix|count|ago|last");
        if (flaps == null) {
            return l;
        }
        for (int i = 0; i < flaps.size(); i++) {
            rtrLoggerFlap ntry = flaps.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.count < cnt) {
                continue;
            }
            l.add(flaps.get(i) + "");
        }
        return l;
    }

    /**
     * get routes
     *
     * @return routes
     */
    public tabRoute<addrIP> getRoutes() {
        switch (afi) {
            case 1:
                return oldU;
            case 2:
                return oldM;
            case 3:
                return oldF;
            default:
                return null;
        }
    }

    /**
     * get display mode
     *
     * @return mode
     */
    public int getDispMod() {
        switch (afi) {
            case 1:
                return 1;
            case 2:
                return 1;
            case 3:
                return 5;
            default:
                return 0;
        }
    }

    private void doChgd(int afi, tabRouteEntry<addrIP> ntry, String act) {
        if (logging) {
            logger.info(act + " " + afi2str(afi) + " " + prf2str(afi, ntry.prefix));
        }
        if (flaps == null) {
            return;
        }
        rtrLoggerFlap stat = new rtrLoggerFlap(afi, ntry.prefix);
        rtrLoggerFlap old = flaps.add(stat);
        if (old != null) {
            stat = old;
        }
        stat.count++;
        stat.last = bits.getTime();
    }

    private void doDiff(int afi, tabRoute<addrIP> o, tabRoute<addrIP> n) {
        for (int i = 0; i < o.size(); i++) {
            tabRouteEntry<addrIP> ntry = o.get(i);
            if (ntry == null) {
                continue;
            }
            if (n.find(ntry) != null) {
                continue;
            }
            doChgd(afi, ntry, "withdrawn");
        }
        for (int i = 0; i < n.size(); i++) {
            tabRouteEntry<addrIP> ntry = n.get(i);
            if (ntry == null) {
                continue;
            }
            tabRouteEntry<addrIP> old = o.find(ntry);
            if (old == null) {
                doChgd(afi, ntry, "reachable");
                continue;
            }
            if (ntry.differs(tabRoute.addType.alters, old) == 0) {
                continue;
            }
            doChgd(afi, ntry, "changed");
        }
    }

    /**
     * create computed
     */
    public synchronized void routerCreateComputed() {
        switch (afi) {
            case 1:
                doDiff(1, oldU, routerRedistedU);
                break;
            case 2:
                doDiff(2, oldM, routerRedistedM);
                break;
            case 3:
                doDiff(3, oldF, routerRedistedF);
                break;
        }
        oldU = routerRedistedU;
        oldM = routerRedistedM;
        oldF = routerRedistedF;
    }

    /**
     * redistribution changed
     */
    public void routerRedistChanged() {
        routerCreateComputed();
    }

    /**
     * others changed
     */
    public void routerOthersChanged() {
    }

    /**
     * get help
     *
     * @param l list
     */
    public void routerGetHelp(userHelp l) {
        l.add(null, false, 1, new int[]{-1}, "flapstat", "count flap statistics");
        l.add(null, false, 1, new int[]{-1}, "logging", "log events");
        l.add(null, false, 1, new int[]{2}, "afi", "set address family");
        l.add(null, false, 2, new int[]{-1}, "unicast", "select unicast");
        l.add(null, false, 2, new int[]{-1}, "multicast", "select multicast");
        l.add(null, false, 2, new int[]{-1}, "flowspec", "select flowspec");
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void routerGetConfig(List<String> l, String beg, int filter) {
        l.add(beg + "afi " + afi2str(afi));
        cmds.cfgLine(l, flaps == null, beg, "flapstat", "");
        cmds.cfgLine(l, !logging, beg, "logging", "");
    }

    /**
     * configure
     *
     * @param cmd command
     * @return false if success, true if error
     */
    public boolean routerConfigure(cmds cmd) {
        String s = cmd.word();
        boolean negated = false;
        if (s.equals(cmds.negated)) {
            s = cmd.word();
            negated = true;
        }
        if (s.equals("afi")) {
            if (negated) {
                afi = 1;
                return false;
            }
            afi = str2afi(cmd.word());
            return false;
        }
        if (s.equals("logging")) {
            logging = !negated;
            return false;
        }
        if (s.equals("flapstat")) {
            if (negated) {
                flaps = null;
            } else {
                flaps = new tabGen<rtrLoggerFlap>();
            }
            return false;
        }
        return true;
    }

    /**
     * stop work
     */
    public void routerCloseNow() {
    }

    /**
     * get neighbor count
     *
     * @return count
     */
    public int routerNeighCount() {
        return 0;
    }

    /**
     * list neighbors
     *
     * @param tab list
     */
    public void routerNeighList(tabRoute<addrIP> tab) {
    }

    /**
     * get interface count
     *
     * @return count
     */
    public int routerIfaceCount() {
        return 0;
    }

    /**
     * maximum recursion depth
     *
     * @return allowed number
     */
    public int routerRecursions() {
        return 1;
    }

    /**
     * get list of link states
     *
     * @param tab table to update
     * @param par parameter
     * @param asn asn
     * @param adv advertiser
     */
    public void routerLinkStates(tabRoute<addrIP> tab, int par, int asn, addrIPv4 adv) {
    }

    /**
     * get state information
     *
     * @param lst list to append
     */
    public void routerStateGet(List<String> lst) {
    }

    /**
     * set state information
     *
     * @param cmd string to append
     * @return true on error, false on success
     */
    public boolean routerStateSet(cmds cmd) {
        return true;
    }

}

class rtrLoggerFlap implements Comparable<rtrLoggerFlap> {

    public final int afi;

    public final addrPrefix<addrIP> prefix;

    public long count;

    public long last;

    public rtrLoggerFlap(int a, addrPrefix<addrIP> p) {
        afi = a;
        prefix = p.copyBytes();
    }

    public int compareTo(rtrLoggerFlap o) {
        if (afi < o.afi) {
            return -1;
        }
        if (afi > o.afi) {
            return +1;
        }
        return prefix.compareTo(o.prefix);
    }

    public String toString() {
        return rtrLogger.afi2str(afi) + "|" + rtrLogger.prf2str(afi, prefix) + "|" + count + "|" + bits.timePast(last) + "|" + bits.time2str(cfgAll.timeZoneName, last + cfgAll.timeServerOffset, 3);
    }

}

class rtrLoggerIfc implements Comparable<rtrLoggerIfc> {

    public final ipFwdIface iface;

    public int count;

    public rtrLoggerIfc(ipFwdIface ifc) {
        iface = ifc;
    }

    public int compareTo(rtrLoggerIfc o) {
        if (iface == null) {
            if (o.iface != null) {
                return -1;
            }
            return 0;
        }
        if (o.iface == null) {
            if (iface != null) {
                return +1;
            }
            return 0;
        }
        return iface.compareTo(o.iface);
    }

    public String toString() {
        return iface + "|" + count;
    }

}

class rtrLoggerAdr implements Comparable<rtrLoggerAdr> {

    public final addrIP addr;

    public int count;

    public rtrLoggerAdr(addrIP ifc) {
        addr = ifc;
    }

    public int compareTo(rtrLoggerAdr o) {
        return addr.compareTo(o.addr);
    }

    public String toString() {
        return addr + "|" + count;
    }

}
