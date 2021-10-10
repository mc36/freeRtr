package net.freertr.util;

import java.util.Comparator;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrType;
import net.freertr.tab.tabRouteIface;

/**
 * spf result
 *
 * @param <Ta> type of nodes
 * @author matecsaba
 */
public class shrtPthFrstRes<Ta extends addrType> implements Comparator<shrtPthFrstRes<Ta>> {

    /**
     * node handle
     */
    protected shrtPthFrstNode<Ta> nodeH;

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
    public shrtPthFrstRes(shrtPthFrstNode<Ta> nam, int hp) {
        nodeH = nam;
        nodeA = nam.name;
        hops = hp;
    }

    public int compare(shrtPthFrstRes<Ta> o1, shrtPthFrstRes<Ta> o2) {
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
