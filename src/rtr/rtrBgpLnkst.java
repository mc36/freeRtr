package rtr;

import ip.ipRtr;
import java.util.Comparator;

/**
 * bgp4 link state
 *
 * @author matecsaba
 */
public class rtrBgpLnkst implements Comparator<rtrBgpLnkst> {

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
