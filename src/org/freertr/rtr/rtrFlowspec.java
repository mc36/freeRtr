package org.freertr.rtr;

import java.util.ArrayList;
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
 * flowspec rewriter
 *
 * @author matecsaba
 */
public class rtrFlowspec extends ipRtr {

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
     * bytes to add
     */
    protected byte[] add;

    /**
     * create flowspec rewriter process
     *
     * @param forwarder forwarder to update
     * @param id process id
     */
    public rtrFlowspec(ipFwd forwarder, int id) {
        fwdCore = forwarder;
        rtrNum = id;
        switch (fwdCore.ipVersion) {
            case ipCor4.protocolVersion:
                rouTyp = tabRouteAttr.routeType.flwspc4;
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteAttr.routeType.flwspc6;
                break;
            default:
                rouTyp = null;
                break;
        }
        distance = 254;
        add = new byte[0];
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
        return "flowspec on " + fwdCore;
    }

    /**
     * create computed
     */
    public synchronized void routerCreateComputed() {
        tabRoute<addrIP> res = new tabRoute<addrIP>("computed");
        byte[] buf = new byte[128];
        byte[] adr = new byte[addrIP.size];
        for (int i = 0; i < routerRedistedF.size(); i++) {
            tabRouteEntry<addrIP> ntry = routerRedistedF.get(i);
            if (ntry == null) {
                continue;
            }
            ntry = ntry.copyBytes(tabRoute.addType.notyet);
            ntry.best.rouTyp = rouTyp;
            ntry.best.protoNum = rtrNum;
            if (distance > 0) {
                ntry.best.distance = distance;
            }
            bits.byteCopy(ntry.prefix.network.getBytes(), 1, buf, 0, 15);
            bits.byteCopy(ntry.prefix.broadcast.getBytes(), 0, buf, 15, 16);
            bits.byteCopy(ntry.prefix.wildcard.getBytes(), 0, buf, 31, 16);
            bits.byteCopy(ntry.prefix.mask.getBytes(), 0, buf, 47, 16);
            int o = ntry.prefix.network.getBytes()[0];
            bits.byteCopy(add, 0, buf, o, add.length);
            o += add.length;
            ntry.prefix = new addrPrefix<addrIP>(new addrIP(), adr.length * 8);
            adr[0] = (byte) o;
            bits.byteCopy(buf, 0, adr, 1, 15);
            ntry.prefix.network.fromBuf(adr, 0);
            bits.byteCopy(buf, 15, adr, 0, 16);
            ntry.prefix.broadcast.fromBuf(adr, 0);
            bits.byteCopy(buf, 31, adr, 0, 16);
            ntry.prefix.wildcard.fromBuf(adr, 0);
            bits.byteCopy(buf, 47, adr, 0, 16);
            ntry.prefix.mask.fromBuf(adr, 0);
            res.add(tabRoute.addType.better, ntry, false, false);
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
        l.add(null, false, 1, new int[]{2}, "add", "specify bytes to add");
        l.add(null, false, 2, new int[]{2, -1}, "<num>", "byte");
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
        String a = "";
        for (int i = 0; i < add.length; i++) {
            a += " " + (add[i] & 0xff);
        }
        l.add(beg + "add" + a);
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
        if (s.equals("distance")) {
            distance = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("add")) {
            if (negated) {
                add = new byte[0];
                return false;
            }
            List<Integer> lst = new ArrayList<Integer>();
            for (;;) {
                s = cmd.word();
                if (s.length() < 1) {
                    break;
                }
                lst.add(bits.str2num(s));
            }
            byte[] res = new byte[lst.size()];
            for (int i = 0; i < res.length; i++) {
                res[i] = (byte) ((int) lst.get(i));
            }
            add = res;
            return false;
        }
        return true;
    }

    /**
     * stop work
     */
    public void routerCloseNow() {
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
