package rtr;

import addr.addrIP;
import addr.addrPrefix;
import ip.ipCor4;
import ip.ipCor6;
import ip.ipFwd;
import ip.ipRtr;
import java.util.List;
import tab.tabRoute;
import tab.tabRouteEntry;
import user.userHelping;
import util.cmds;

/**
 * deaggregation creator
 *
 * @author matecsaba
 */
public class rtrDeaggr extends ipRtr {

    /**
     * the forwarder protocol
     */
    public final ipFwd fwdCore;

    /**
     * route type
     */
    protected final tabRouteEntry.routeType rouTyp;

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
                rouTyp = tabRouteEntry.routeType.deaggr4;
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteEntry.routeType.deaggr6;
                break;
            default:
                rouTyp = null;
                break;
        }
        routerComputedU = new tabRoute<addrIP>("rx");
        routerComputedM = new tabRoute<addrIP>("rx");
        routerComputedF = new tabRoute<addrIP>("rx");
        routerCreateComputed();
        fwdCore.routerAdd(this, rouTyp, id);
    }

    public String toString() {
        return "deaggr on " + fwdCore;
    }

    public synchronized void routerCreateComputed() {
        tabRoute<addrIP> res = new tabRoute<addrIP>("computed");
        for (int i = 0; i < routerRedistedU.size(); i++) {
            tabRouteEntry<addrIP> ntry = routerRedistedU.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.prefix.maskLen >= ntry.prefix.network.maxBits()) {
                continue;
            }
            ntry = ntry.copyBytes();
            ntry.rouTyp = rouTyp;
            ntry.protoNum = rtrNum;
            ntry.prefix.setMask(ntry.prefix.maskLen + 1);
            res.add(tabRoute.addType.better, ntry.copyBytes(), false, false);
            addrIP adr = new addrIP();
            adr.bitSet(ntry.prefix.maskLen - 1);
            adr.setOr(adr, ntry.prefix.network);
            ntry.prefix = new addrPrefix<addrIP>(adr, ntry.prefix.maskLen);
            res.add(tabRoute.addType.better, ntry.copyBytes(), false, false);
        }
        routerComputedU = res;
        fwdCore.routerChg(this);
    }

    public void routerRedistChanged() {
        routerCreateComputed();
    }

    public void routerOthersChanged() {
    }

    public void routerGetHelp(userHelping l) {
    }

    public void routerGetConfig(List<String> l, String beg, boolean filter) {
    }

    public boolean routerConfigure(cmds cmd) {
        return true;
    }

    public void routerCloseNow() {
    }

    public int routerNeighCount() {
        return 0;
    }

    public void routerNeighList(tabRoute<addrIP> tab) {
    }

    public int routerIfaceCount() {
        return 0;
    }

}
