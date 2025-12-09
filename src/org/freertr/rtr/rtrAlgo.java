package org.freertr.rtr;

import org.freertr.ip.ipFwd;
import org.freertr.tab.tabRouteAttr;

/**
 * one flexible algorithm
 *
 * @author matecsaba
 */
public class rtrAlgo implements Comparable<rtrAlgo> {

    /**
     * algorithm id
     */
    public final int num;

    /**
     * forwarder to use
     */
    public final ipFwd fwd;

    /**
     * router type
     */
    public final tabRouteAttr.routeType typ;

    /**
     * router id
     */
    public final int prc;

    /**
     * worker
     */
    public final rtrAlgoVrf vrf;

    /**
     * create instance
     *
     * @param n algorithm id
     * @param f forwarder
     * @param t router type
     * @param p router id
     */
    public rtrAlgo(int n, ipFwd f, tabRouteAttr.routeType t, int p) {
        num = n;
        fwd = f;
        typ = t;
        prc = p;
        vrf = new rtrAlgoVrf(this);
    }

    public String toString() {
        return num + " " + fwd.cfgName;
    }

    public int compareTo(rtrAlgo o) {
        if (num < o.num) {
            return -1;
        }
        if (num > o.num) {
            return +1;
        }
        return 0;
    }

}
