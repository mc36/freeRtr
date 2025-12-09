package org.freertr.spf;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.tab.tabRouteIface;

/**
 * spf result
 *
 * @param <Ta> type of nodes
 * @author matecsaba
 */
public class spfResult<Ta extends addrType> implements Comparable<spfResult<Ta>> {

    /**
     * node handle
     */
    protected spfNode<Ta> nodeH;

    /**
     * node address
     */
    public Ta nodeA;

    /**
     * hop count
     */
    public int hops;

    /**
     * nexthop address
     */
    public addrIP nxtHop;

    /**
     * other nexthop address
     */
    public addrIP othHop;

    /**
     * forwarding interface
     */
    public tabRouteIface iface;

    /**
     * other forwarding interface
     */
    public tabRouteIface oface;

    /**
     * segrou base
     */
    public int srBeg;

    /**
     * bier base
     */
    public int brBeg;

    /**
     * create instance
     *
     * @param nam node
     * @param hp hops
     */
    public spfResult(spfNode<Ta> nam, int hp) {
        nodeH = nam;
        nodeA = nam.name;
        hops = hp;
    }

    public int compareTo(spfResult<Ta> o) {
        if (hops < o.hops) {
            return -1;
        }
        if (hops > o.hops) {
            return +1;
        }
        return nodeH.compareTo(o.nodeH);
    }

    public String toString() {
        return "" + nodeH;
    }

}
