package rtr;

import addr.addrIP;
import addr.addrPrefix;
import ip.ipCor4;
import ip.ipCor6;
import ip.ipFwd;
import ip.ipRtr;
import java.util.List;
import tab.tabRoute;
import tab.tabRouteAttr;
import tab.tabRouteEntry;
import user.userHelping;
import util.bits;
import util.cmds;

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
        routerComputedU = new tabRoute<addrIP>("rx");
        routerComputedM = new tabRoute<addrIP>("rx");
        routerComputedF = new tabRoute<addrIP>("rx");
        distance1 = 254;
        distance2 = 254;
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
        ntry = ntry.copyBytes();
        ntry.best.rouTyp = rouTyp;
        ntry.best.protoNum = rtrNum;
        ntry.prefix.setMask(ntry.prefix.maskLen + 1);
        if (distance1 > 0) {
            ntry.best.distance = distance1;
        }
        tab.add(tabRoute.addType.better, ntry.copyBytes(), false, false);
        addrIP adr = new addrIP();
        adr.bitSet(ntry.prefix.maskLen - 1);
        adr.setOr(adr, ntry.prefix.network);
        ntry.prefix = new addrPrefix<addrIP>(adr, ntry.prefix.maskLen);
        if (distance2 > 0) {
            ntry.best.distance = distance2;
        }
        tab.add(tabRoute.addType.better, ntry.copyBytes(), false, false);
    }

    /**
     * create computed
     */
    public synchronized void routerCreateComputed() {
        tabRoute<addrIP> resU = new tabRoute<addrIP>("computed");
        tabRoute<addrIP> resM = new tabRoute<addrIP>("computed");
        for (int i = 0; i < routerRedistedU.size(); i++) {
            doPrefix(routerRedistedU.get(i), resU);
        }
        for (int i = 0; i < routerRedistedM.size(); i++) {
            doPrefix(routerRedistedM.get(i), resM);
        }
        routerDoAggregates(rtrBgpUtil.safiUnicast, resU, null, fwdCore.commonLabel, 0, null, 0);
        routerDoAggregates(rtrBgpUtil.safiMulticast, resM, null, fwdCore.commonLabel, 0, null, 0);
        resU.preserveTime(routerComputedU);
        resM.preserveTime(routerComputedM);
        routerComputedU = resU;
        routerComputedM = resM;
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
        l.add("1 2   distance                    specify default distance");
        l.add("2 3     <num>                     lower half distance");
        l.add("3 .       <num>                   upper half distance");
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void routerGetConfig(List<String> l, String beg, boolean filter) {
        l.add(beg + "distance " + distance1 + " " + distance2);
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
            distance1 = bits.str2num(cmd.word());
            distance2 = bits.str2num(cmd.word());
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

}
