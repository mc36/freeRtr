package net.freertr.rtr;

import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAll;
import net.freertr.ip.ipCor4;
import net.freertr.ip.ipCor6;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipRtr;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabIndex;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logger;

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
     * @param tab routing table
     * @return prefix length report
     */
    public static userFormat prefixLengths(tabRoute<addrIP> tab) {
        int[] res = new int[(addrIP.size * 8) + 1];
        for (int i = 0; i < tab.size(); i++) {
            tabRouteEntry<addrIP> ntry = tab.get(i);
            res[ntry.prefix.maskLen] += ntry.alts.size();
        }
        userFormat lst = new userFormat("|", "len|count");
        for (int i = 0; i < res.length; i++) {
            lst.add(i + "|" + res[i]);
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
        return prefixLengths(oldU);
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
     * @param afi afi
     * @return routes
     */
    public tabRoute<addrIP> getRoutes(int afi) {
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
        doDiff(1, oldU, routerRedistedU);
        doDiff(2, oldM, routerRedistedM);
        doDiff(3, oldF, routerRedistedF);
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
    public void routerGetHelp(userHelping l) {
        l.add(null, "1 .   flapstat                    count flap statistics");
        l.add(null, "1 .   logging                     log events");
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void routerGetConfig(List<String> l, String beg, int filter) {
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
        if (s.equals("no")) {
            s = cmd.word();
            negated = true;
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

}

class rtrLoggerFlap implements Comparator<rtrLoggerFlap> {

    public final int afi;

    public final addrPrefix<addrIP> prefix;

    public long count;

    public long last;

    public rtrLoggerFlap(int a, addrPrefix<addrIP> p) {
        afi = a;
        prefix = p.copyBytes();
    }

    public int compare(rtrLoggerFlap o1, rtrLoggerFlap o2) {
        if (o1.afi < o2.afi) {
            return -1;
        }
        if (o1.afi > o2.afi) {
            return +1;
        }
        return o1.prefix.compare(o1.prefix, o2.prefix);
    }

    public String toString() {
        return rtrLogger.afi2str(afi) + "|" + rtrLogger.prf2str(afi, prefix) + "|" + count + "|" + bits.timePast(last) + "|" + bits.time2str(cfgAll.timeZoneName, last + cfgAll.timeServerOffset, 3);
    }

}

class rtrLoggerIfc implements Comparator<rtrLoggerIfc> {

    public final ipFwdIface iface;

    public int count;

    public rtrLoggerIfc(ipFwdIface ifc) {
        iface = ifc;
    }

    public int compare(rtrLoggerIfc o1, rtrLoggerIfc o2) {
        if (o1.iface == null) {
            if (o2.iface != null) {
                return -1;
            }
            return 0;
        }
        if (o2.iface == null) {
            if (o1.iface != null) {
                return +1;
            }
            return 0;
        }
        return o1.iface.compare(o1.iface, o2.iface);
    }

    public String toString() {
        return iface + "|" + count;
    }

}
