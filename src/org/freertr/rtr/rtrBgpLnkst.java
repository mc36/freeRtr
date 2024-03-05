package org.freertr.rtr;

import java.util.Comparator;
import org.freertr.ip.ipRtr;

/**
 * bgp4 link state
 *
 * @author matecsaba
 */
public class rtrBgpLnkst implements Comparator<rtrBgpLnkst> {

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

    public int compare(rtrBgpLnkst o1, rtrBgpLnkst o2) {
        if (o1.par < o2.par) {
            return -1;
        }
        if (o1.par > o2.par) {
            return +1;
        }
        return o1.rtr.compare(o1.rtr, o2.rtr);
    }

}
