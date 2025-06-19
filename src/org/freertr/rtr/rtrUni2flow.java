package org.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipRtr;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRouteUtil;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * unicast to flowspec
 *
 * @author matecsaba
 */
public class rtrUni2flow extends ipRtr {

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
     * route afi
     */
    protected final boolean ipv6;

    /**
     * distance to give
     */
    protected int distance;

    /**
     * direction: 1=trg, 2=src
     */
    protected int direction;

    /**
     * as to give
     */
    protected int trgAs;

    /**
     * rate to give
     */
    protected long trgRate;

    /**
     * create unicast to flowspec process
     *
     * @param forwarder forwarder to update
     * @param id process id
     */
    public rtrUni2flow(ipFwd forwarder, int id) {
        fwdCore = forwarder;
        rtrNum = id;
        switch (fwdCore.ipVersion) {
            case ipCor4.protocolVersion:
                rouTyp = tabRouteAttr.routeType.uni2flow4;
                ipv6 = false;
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteAttr.routeType.uni2flow6;
                ipv6 = true;
                break;
            default:
                ipv6 = false;
                rouTyp = null;
                break;
        }
        distance = 254;
        direction = 1;
        routerComputedU = new tabRoute<addrIP>("rx");
        routerComputedM = new tabRoute<addrIP>("rx");
        routerComputedF = new tabRoute<addrIP>("rx");
        routerComputedI = new tabGen<tabIndex<addrIP>>();
        routerCreateComputed();
        fwdCore.routerAdd(this, rouTyp, id);
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "uni2flow on " + fwdCore;
    }

    /**
     * create computed
     */
    public synchronized void routerCreateComputed() {
        tabRoute<addrIP> res = new tabRoute<addrIP>("computed");
        for (int i = 0; i < routerRedistedU.size(); i++) {
            tabRouteEntry<addrIP> ntry = routerRedistedU.get(i);
            if (ntry == null) {
                continue;
            }
            ntry = ntry.copyBytes(tabRoute.addType.notyet);
            tabRouteEntry<addrIP> attr = new tabRouteEntry<addrIP>();
            attr.best.rouTyp = rouTyp;
            attr.best.protoNum = rtrNum;
            if (distance > 0) {
                attr.best.distance = distance;
            }
            attr.best.nextHop = ntry.best.nextHop;
            if (attr.best.nextHop == null) {
                attr.best.nextHop = new addrIP();
            }
            attr.best.stdComm = ntry.best.stdComm;
            attr.best.extComm = ntry.best.extComm;
            attr.best.lrgComm = ntry.best.lrgComm;
            attr.best.metric = ntry.best.metric;
            attr.best.tag = ntry.best.tag;
            if (trgRate >= 0) {
                if (attr.best.extComm == null) {
                    attr.best.extComm = new ArrayList<Long>();
                }
                attr.best.extComm.add(tabRouteUtil.rate2comm(trgAs, trgRate));
            }
            attr = rtrBgpFlow.advertNetwork(ntry.prefix, ipv6, direction, attr);
            if (attr != null) {
                res.add(tabRoute.addType.better, attr, true, true);
            }
        }
        if (res.preserveTime(routerComputedF)) {
            return;
        }
        routerComputedF = res;
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
        l.add(null, false, 2, new int[]{-1}, "<num>", "distance");
        l.add(null, false, 1, new int[]{2}, "direction", "specify direction of rule");
        l.add(null, false, 2, new int[]{-1}, "source", "match source address");
        l.add(null, false, 2, new int[]{-1}, "target", "match target address");
        l.add(null, false, 1, new int[]{2}, "as", "specify target as");
        l.add(null, false, 2, new int[]{-1}, "<num>", "as");
        l.add(null, false, 1, new int[]{2}, "rate", "specify target rate");
        l.add(null, false, 2, new int[]{-1}, "<num>", "bytes/sec");
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void routerGetConfig(List<String> l, String beg, int filter) {
        l.add(beg + "distance " + distance);
        l.add(beg + "direction " + ((direction == 1) ? "target" : "source"));
        l.add(beg + "as " + trgAs);
        l.add(beg + "rate " + trgRate);
    }

    /**
     * configure
     *
     * @param cmd command
     * @return false on success, true on error
     */
    public boolean routerConfigure(cmds cmd) {
        String s = cmd.word();
        if (s.equals("distance")) {
            distance = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("direction")) {
            s = cmd.word();
            if (s.equals("source")) {
                direction = 2;
            } else {
                direction = 1;
            }
            return false;
        }
        if (s.equals("as")) {
            trgAs = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("rate")) {
            trgRate = bits.str2long(cmd.word());
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
