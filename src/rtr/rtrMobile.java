package rtr;

import addr.addrIP;
import addr.addrMac;
import addr.addrPrefix;
import ip.ipCor4;
import ip.ipCor6;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipRtr;
import java.util.List;
import tab.tabRoute;
import tab.tabRouteEntry;
import user.userHelping;
import util.bits;
import util.cmds;

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
    protected final tabRouteEntry.routeType rouTyp;

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
                rouTyp = tabRouteEntry.routeType.mobile4;
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteEntry.routeType.mobile6;
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
            if (ntry.rouTyp != tabRouteEntry.routeType.conn) {
                continue;
            }
            if (ntry.iface == null) {
                continue;
            }
            ipFwdIface ifc = fwdCore.ifaces.find((ipFwdIface) ntry.iface);
            if (ifc == null) {
                continue;
            }
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
                rou.iface = ntry.iface;
                rou.nextHop = adr.copyBytes();
                rou.rouTyp = rouTyp;
                rou.protoNum = rtrNum;
                rou.distance = distance;
                rou.time = bits.getTime();
                res.add(tabRoute.addType.better, rou, false, false);
            }
        }
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
        l.add("1 2   distance                    specify default distance");
        l.add("2 .     <num>                     distance");
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void routerGetConfig(List<String> l, String beg, boolean filter) {
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

    public void run() {
        for (;;) {
            bits.sleep(5000);
            if (!need2run) {
                break;
            }
            try {
                routerCreateComputed();
            } catch (Exception e) {
            }
        }
    }

}
