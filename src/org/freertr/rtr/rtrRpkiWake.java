package org.freertr.rtr;

import org.freertr.tab.tabRouteAttr;

/**
 * wake up remote
 *
 * @author matecsaba
 */
public class rtrRpkiWake implements Comparable<rtrRpkiWake> {

    /**
     * remote type
     */
    protected final tabRouteAttr.routeType remT;

    /**
     * remote number
     */
    protected final int remN;

    /**
     * create instance
     *
     * @param t type
     * @param n number
     */
    public rtrRpkiWake(tabRouteAttr.routeType t, int n) {
        remT = t;
        remN = n;
    }

    public int compareTo(rtrRpkiWake o) {
        int i = remT.compareTo(o.remT);
        if (i != 0) {
            return i;
        }
        if (remN < o.remN) {
            return -1;
        }
        if (remN > o.remN) {
            return +1;
        }
        return 0;
    }

}
