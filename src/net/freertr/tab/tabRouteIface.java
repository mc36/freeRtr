package net.freertr.tab;

import java.util.Comparator;

/**
 * represents one route interface
 *
 * @author matecsaba
 */
public class tabRouteIface implements Comparator<tabRouteIface> {

    /**
     * create instance
     */
    public tabRouteIface() {
    }

    /**
     * interface number
     */
    public int ifwNum;

    public int compare(tabRouteIface o1, tabRouteIface o2) {
        if (o1.ifwNum < o2.ifwNum) {
            return -1;
        }
        if (o1.ifwNum > o2.ifwNum) {
            return +1;
        }
        return 0;
    }

    public String toString() {
        return "#" + ifwNum;
    }
}
