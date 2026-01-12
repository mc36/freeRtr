package org.freertr.rtr;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.cfg.cfgRtr;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipRtr;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.tab.tabRoute;
import org.freertr.user.userHelp;
import org.freertr.util.cmds;

/**
 * pvrp other router
 *
 * @author matecsaba
 */
public class rtrPvrpOther extends ipRtr {

    /**
     * enabled
     */
    public boolean enabled;

    /**
     * stub flag
     */
    public boolean stub = false;

    /**
     * advertise labels
     */
    public boolean labels = false;

    /**
     * segment routing index
     */
    public int segrouIdx = 0;

    /**
     * bier index
     */
    public int bierIdx = 0;

    /**
     * bier subdomain
     */
    public int bierSub = 0;

    /**
     * suppress interface addresses
     */
    public boolean suppressAddr = false;

    /**
     * forwarding core
     */
    public final ipFwd fwd;

    private final rtrPvrp parent;

    /**
     * unregister from ip
     */
    public void unregister2ip() {
        if (!enabled) {
            return;
        }
        enabled = false;
        fwd.routerDel(this);
    }

    /**
     * register to ip
     */
    public void register2ip() {
        if (enabled) {
            return;
        }
        enabled = true;
        fwd.routerAdd(this, parent.rouTyp, parent.rtrNum);
    }

    /**
     * create instance
     *
     * @param p parent
     * @param f forwarder
     */
    public rtrPvrpOther(rtrPvrp p, ipFwd f) {
        fwd = f;
        parent = p;
        routerComputedU = new tabRoute<addrIP>("rx");
        routerComputedM = new tabRoute<addrIP>("rx");
        routerComputedF = new tabRoute<addrIP>("rx");
        routerComputedI = new tabGen<tabIndex<addrIP>>();
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "pvrp on " + parent.fwdCore;
    }

    /**
     * create computed table
     */
    public synchronized void routerCreateComputed() {
    }

    /**
     * redistribution changed
     */
    public void routerRedistChanged() {
        parent.routerRedistChanged();
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
     * get config
     *
     * @param l list to append
     * @param beg beginning
     * @param afi afi name
     */
    public void getConfig(List<String> l, String beg, String afi) {
        cmds.cfgLine(l, !enabled, beg, afi + "enable", "");
        cmds.cfgLine(l, !stub, beg, afi + "stub", "");
        cmds.cfgLine(l, !labels, beg, afi + "labels", "");
        cmds.cfgLine(l, !suppressAddr, beg, afi + "suppress-prefix", "");
        cmds.cfgLine(l, parent.segrouMax < 1, beg, afi + "segrout", "" + segrouIdx);
        cmds.cfgLine(l, parent.bierMax < 1, beg, afi + "bier", bierIdx + " " + bierSub);
        cfgRtr.getShRedist(l, beg + afi, this);
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
