package org.freertr.tab;

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
     * interface type
     */
    public enum ifaceType {

        /**
         * null interface
         */
        nul,
        /**
         * loopback interface
         */
        loopback,
        /**
         * template interface
         */
        template,
        /**
         * dialer interface
         */
        dialer,
        /**
         * sdn interface
         */
        sdn,
        /**
         * pw headend interface
         */
        pweth,
        /**
         * virtual ppp interface
         */
        virtppp,
        /**
         * ethernet interface
         */
        ether,
        /**
         * serial interface
         */
        serial,
        /**
         * arcnet interface
         */
        arcnet,
        /**
         * infiniband interface
         */
        infiniband,
        /**
         * atm interface
         */
        atm,
        /**
         * bridged head interface
         */
        bridge,
        /**
         * bundle head interface
         */
        bundle,
        /**
         * hairpin interface
         */
        hairpin,
        /**
         * tunnel interface
         */
        tunnel

    }

    /**
     * interface number
     */
    public int ifwNum;

    /**
     * interface type
     */
    public ifaceType ifwTyp;

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
