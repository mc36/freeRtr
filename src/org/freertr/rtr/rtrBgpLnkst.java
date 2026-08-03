package org.freertr.rtr;

import org.freertr.ip.ipRtr;

/**
 * bgp4 link state
 *
 * @author matecsaba
 */
public class rtrBgpLnkst implements Comparable<rtrBgpLnkst> {

    /**
     * create instance
     */
    public rtrBgpLnkst() {
    }

    /**
     * router
     */
    public ipRtr rtr;

    /**
     * parameter
     */
    public int par;

    public int compareTo(rtrBgpLnkst o) {
        if (par < o.par) {
            return -1;
        }
        if (par > o.par) {
            return +1;
        }
        return rtr.compareTo(o.rtr);
    }

}
