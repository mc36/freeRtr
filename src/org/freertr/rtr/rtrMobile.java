package org.freertr.rtr;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrPrefix;
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
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;

/**
 * mobile route creator
 *
 * @author matecsaba
 */
public class rtrMobile extends ipRtr implements Runnable {

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
     * distance to give
     */
    protected int distance;

    /**
     * need to run
     */
    protected boolean need2run;

    /**
     * create mobile process
     *
     * @param forwarder forwarder to update
     * @param id process id
     */
    public rtrMobile(ipFwd forwarder, int id) {
        fwdCore = forwarder;
        rtrNum = id;
        switch (fwdCore.ipVersion) {
            case ipCor4.protocolVersion:
                rouTyp = tabRouteAttr.routeType.mobile4;
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteAttr.routeType.mobile6;
                break;
            default:
                rouTyp = null;
                break;
        }
        distance = 254;
        need2run = true;
        routerComputedU = new tabRoute<addrIP>("rx");
        routerComputedM = new tabRoute<addrIP>("rx");
        routerComputedF = new tabRoute<addrIP>("rx");
        routerComputedI = new tabGen<tabIndex<addrIP>>();
        routerCreateComputed();
        fwdCore.routerAdd(this, rouTyp, id);
        logger.startThread(this);
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "mobile on " + fwdCore;
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
            if (ntry.best.rouTyp != tabRouteAttr.routeType.conn) {
                continue;
            }
            if (ntry.best.iface == null) {
                continue;
            }
            ipFwdIface ifc = fwdCore.ifaces.find((ipFwdIface) ntry.best.iface);
            if (ifc == null) {
                continue;
            }
            long tim = bits.getTime();
            for (int o = 0;; o++) {
                addrIP adr = new addrIP();
                addrMac mac = new addrMac();
                if (ifc.lower.getL2info(o, adr, mac)) {
                    break;
                }
                if (!ntry.prefix.matches(adr)) {
                    continue;
                }
                addrPrefix<addrIP> prf = new addrPrefix<addrIP>(adr.copyBytes(), adr.maxBits());
                tabRouteEntry<addrIP> rou = new tabRouteEntry<addrIP>();
                rou.prefix = prf;
                rou.best.iface = ntry.best.iface;
                rou.best.nextHop = adr.copyBytes();
                rou.best.rouTyp = rouTyp;
                rou.best.protoNum = rtrNum;
                rou.best.distance = distance;
                rou.best.time = tim;
                res.add(tabRoute.addType.better, rou, false, false);
            }
        }
        routerDoAggregates(rtrBgpUtil.sfiUnicast, res, res, fwdCore.commonLabel, null, 0);
        if (res.preserveTime(routerComputedU)) {
            return;
        }
        routerComputedU = res;
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
    }

    /**
     * configure
     *
     * @param cmd command
     * @return false if success, true if error
     */
    public boolean routerConfigure(cmds cmd) {
        String s = cmd.word();
        if (s.equals("distance")) {
            distance = bits.str2num(cmd.word());
            return false;
        }
        return true;
    }

    /**
     * stop work
     */
    public void routerCloseNow() {
        need2run = false;
        fwdCore.routerDel(this);
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

    public void run() {
        for (;;) {
            bits.sleep(5000);
            if (!need2run) {
                break;
            }
            try {
                routerCreateComputed();
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
    }

}
