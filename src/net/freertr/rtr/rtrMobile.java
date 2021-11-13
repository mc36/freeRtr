package net.freertr.rtr;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrMac;
import net.freertr.addr.addrPrefix;
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
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logger;

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
        new Thread(this).start();
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
        res.preserveTime(routerComputedU);
        routerComputedU = res;
        fwdCore.routerChg(this);
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
        l.add(null, "1 2   distance                    specify default distance");
        l.add(null, "2 .     <num>                     distance");
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
        boolean negated = false;
        if (s.equals("no")) {
            s = cmd.word();
            negated = true;
        }
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
