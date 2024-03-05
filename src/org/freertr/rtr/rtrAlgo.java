package org.freertr.rtr;

import java.util.Comparator;
import org.freertr.ip.ipFwd;
import org.freertr.tab.tabRouteAttr;

/**
 * one flexible algorithm
 *
 * @author matecsaba
 */
public class rtrAlgo implements Comparator<rtrAlgo> {

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

    public int compare(rtrAlgo o1, rtrAlgo o2) {
        if (o1.num < o2.num) {
            return -1;
        }
        if (o1.num > o2.num) {
            return +1;
        }
        return 0;
    }

}
