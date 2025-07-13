package org.freertr.rtr;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrPrefix;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipRtr;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * deaggregation creator
 *
 * @author matecsaba
 */
public class rtrDeaggr extends ipRtr {

    /**
     * lower half distance
     */
    public int distance1;

    /**
     * upper half distance
     */
    public int distance2;

    /**
     * address family
     */
    protected int afi;

    /**
     * lower half nexthop
     */
    public addrIP nextHop1;

    /**
     * upper half nexthop
     */
    public addrIP nextHop2;

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
     * create deaggregator process
     *
     * @param forwarder forwarder to update
     * @param id process id
     */
    public rtrDeaggr(ipFwd forwarder, int id) {
        fwdCore = forwarder;
        rtrNum = id;
        switch (fwdCore.ipVersion) {
            case ipCor4.protocolVersion:
                rouTyp = tabRouteAttr.routeType.deaggr4;
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteAttr.routeType.deaggr6;
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
        distance1 = 254;
        distance2 = 254;
        nextHop1 = new addrIP();
        nextHop2 = new addrIP();
        routerCreateComputed();
        fwdCore.routerAdd(this, rouTyp, id);
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "deaggr on " + fwdCore;
    }

    private void doPrefix(tabRouteEntry<addrIP> ntry, tabRoute<addrIP> tab) {
        if (ntry == null) {
            return;
        }
        if (ntry.prefix.maskLen >= ntry.prefix.network.maxBits()) {
            return;
        }
        ntry = ntry.copyBytes(tabRoute.addType.notyet);
        ntry.best.rouTyp = rouTyp;
        ntry.best.protoNum = rtrNum;
        ntry.prefix.setMask(ntry.prefix.maskLen + 1);
        if (distance1 > 0) {
            ntry.best.distance = distance1;
        }
        if (!nextHop1.isEmpty()) {
            ntry.best.nextHop = nextHop1.copyBytes();
        }
        tab.add(tabRoute.addType.better, ntry, true, false);
        addrIP adr = new addrIP();
        adr.bitSet(ntry.prefix.maskLen - 1);
        adr.setOr(adr, ntry.prefix.network);
        ntry.prefix = new addrPrefix<addrIP>(adr, ntry.prefix.maskLen);
        if (distance2 > 0) {
            ntry.best.distance = distance2;
        }
        if (!nextHop2.isEmpty()) {
            ntry.best.nextHop = nextHop2.copyBytes();
        }
        tab.add(tabRoute.addType.better, ntry, true, false);
    }

    /**
     * create computed
     */
    public synchronized void routerCreateComputed() {
        tabRoute<addrIP> resU = new tabRoute<addrIP>("computed");
        tabRoute<addrIP> resM = new tabRoute<addrIP>("computed");
        switch (afi) {
            case 1:
                for (int i = 0; i < routerRedistedU.size(); i++) {
                    doPrefix(routerRedistedU.get(i), resU);
                }
                break;
            case 2:
                for (int i = 0; i < routerRedistedM.size(); i++) {
                    doPrefix(routerRedistedM.get(i), resM);
                }
                break;
        }
        routerDoAggregates(rtrBgpUtil.sfiUnicast, resU, resU, fwdCore.commonLabel, null, 0);
        routerDoAggregates(rtrBgpUtil.sfiMulticast, resM, resM, fwdCore.commonLabel, null, 0);
        boolean same = resU.preserveTime(routerComputedU);
        same &= resM.preserveTime(routerComputedM);
        if (same) {
            return;
        }
        routerComputedU = resU;
        routerComputedM = resM;
        fwdCore.routerChg(this, false);
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
        l.add(null, false, 1, new int[]{2}, "distance", "specify default distance");
        l.add(null, false, 2, new int[]{3}, "<num>", "lower half distance");
        l.add(null, false, 3, new int[]{-1}, "<num>", "upper half distance");
        l.add(null, false, 1, new int[]{2}, "nexthop", "specify default nexthop");
        l.add(null, false, 2, new int[]{3}, "<addr>", "lower half nexthop");
        l.add(null, false, 3, new int[]{-1}, "<addr>", "upper half nexthop");
        l.add(null, false, 1, new int[]{2}, "afi", "set address family");
        l.add(null, false, 2, new int[]{-1}, "unicast", "select unicast");
        l.add(null, false, 2, new int[]{-1}, "multicast", "select multicast");
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void routerGetConfig(List<String> l, String beg, int filter) {
        l.add(beg + "afi " + rtrLogger.afi2str(afi));
        l.add(beg + "distance " + distance1 + " " + distance2);
        l.add(beg + "nexthop " + nextHop1 + " " + nextHop2);
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
            afi = rtrLogger.str2afi(cmd.word());
            return false;
        }
        if (s.equals("distance")) {
            distance1 = bits.str2num(cmd.word());
            distance2 = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("nexthop")) {
            nextHop1 = new addrIP();
            nextHop2 = new addrIP();
            if (negated) {
                return false;
            }
            nextHop1.fromString(cmd.word());
            nextHop2.fromString(cmd.word());
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
     * get neighbor list
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
