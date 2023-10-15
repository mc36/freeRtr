package net.freertr.rtr;

import java.util.Comparator;
import net.freertr.cfg.cfgRtr;
import net.freertr.tab.tabRouteAttr;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * wake up remote
 *
 * @author matecsaba
 */
public class rtrRpkiWake implements Comparator<rtrRpkiWake> {

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

    public int compare(rtrRpkiWake o1, rtrRpkiWake o2) {
        int i = o1.remT.compareTo(o2.remT);
        if (i != 0) {
            return i;
        }
        if (o1.remN < o2.remN) {
            return -1;
        }
        if (o1.remN > o2.remN) {
            return +1;
        }
        return 0;
    }

}
