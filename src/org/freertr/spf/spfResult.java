package org.freertr.spf;

import java.util.Comparator;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrType;
import org.freertr.tab.tabRouteIface;

/**
 * spf result
 *
 * @param <Ta> type of nodes
 * @author matecsaba
 */
public class spfResult<Ta extends addrType> implements Comparator<spfResult<Ta>> {

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

    public int compare(spfResult<Ta> o1, spfResult<Ta> o2) {
        if (o1.hops < o2.hops) {
            return -1;
        }
        if (o1.hops > o2.hops) {
            return +1;
        }
        return o1.nodeH.compare(o1.nodeH, o2.nodeH);
    }

    public String toString() {
        return "" + nodeH;
    }

}
