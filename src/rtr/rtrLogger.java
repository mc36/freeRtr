package rtr;

import addr.addrIP;
import ip.ipCor4;
import ip.ipCor6;
import ip.ipFwd;
import ip.ipRtr;
import java.util.List;
import tab.tabRoute;
import tab.tabRouteEntry;
import user.userHelping;
import util.cmds;
import util.logger;

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
    protected final tabRouteEntry.routeType rouTyp;

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
                rouTyp = tabRouteEntry.routeType.logger4;
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteEntry.routeType.logger6;
                break;
            default:
                rouTyp = null;
                break;
        }
        routerComputedU = new tabRoute<addrIP>("rx");
        routerComputedM = new tabRoute<addrIP>("rx");
        routerComputedF = new tabRoute<addrIP>("rx");
        oldU = new tabRoute<addrIP>("rx");
        oldM = new tabRoute<addrIP>("rx");
        oldF = new tabRoute<addrIP>("rx");
        routerCreateComputed();
        fwdCore.routerAdd(this, rouTyp, id);
    }

    public String toString() {
        return "logger on " + fwdCore;
    }

    private void doDiff(String afi, tabRoute<addrIP> o, tabRoute<addrIP> n) {
        for (int i = 0; i < o.size(); i++) {
            tabRouteEntry<addrIP> ntry = o.get(i);
            if (ntry == null) {
                continue;
            }
            if (n.find(ntry) != null) {
                continue;
            }
            logger.info("withdrawn " + afi + " " + ntry.prefix);
        }
        for (int i = 0; i < n.size(); i++) {
            tabRouteEntry<addrIP> ntry = n.get(i);
            if (ntry == null) {
                continue;
            }
            tabRouteEntry<addrIP> old = o.find(ntry);
            if (old == null) {
                logger.info("reachable " + afi + " " + ntry.prefix);
                continue;
            }
            if (!ntry.differs(old)) {
                continue;
            }
            logger.info("changed " + afi + " " + ntry.prefix);
        }
    }

    public synchronized void routerCreateComputed() {
        doDiff("unicast", oldU, routerRedistedU);
        doDiff("multicast", oldM, routerRedistedM);
        doDiff("flowspec", oldF, routerRedistedF);
        oldU = routerRedistedU;
        oldM = routerRedistedM;
        oldF = routerRedistedF;
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
