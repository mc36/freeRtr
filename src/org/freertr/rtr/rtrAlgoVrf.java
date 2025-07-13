package org.freertr.rtr;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.ip.ipRtr;
import org.freertr.tab.tabRoute;
import org.freertr.user.userHelp;
import org.freertr.util.cmds;

/**
 * one flexible algorithm
 *
 * @author matecsaba
 */
public class rtrAlgoVrf extends ipRtr {

    private final rtrAlgo parent;

    /**
     * unregister from ip
     */
    public void unregister2ip() {
        parent.fwd.routerDel(this);
    }

    /**
     * register to ip
     */
    public void register2ip() {
        parent.fwd.routerAdd(this, parent.typ, parent.prc);
    }

    /**
     * update routes
     *
     * @param rou route table
     */
    public void update2ip(tabRoute<addrIP> rou) {
        rou.setProto(routerProtoTyp, routerProcNum);
        if (rou.preserveTime(routerComputedU)) {
            return;
        }
        routerComputedU = rou;
        routerComputedM = rou;
        parent.fwd.routerChg(this, false);
    }

    /**
     * create new instance
     *
     * @param p parent to use
     */
    public rtrAlgoVrf(rtrAlgo p) {
        parent = p;
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "flexalgo on " + parent.fwd;
    }

    /**
     * create computed
     */
    public synchronized void routerCreateComputed() {
    }

    /**
     * redistribution changed
     */
    public void routerRedistChanged() {
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
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void routerGetConfig(List<String> l, String beg, int filter) {
    }

    /**
     * configure
     *
     * @param cmd command
     * @return false if success, true if error
     */
    public boolean routerConfigure(cmds cmd) {
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
