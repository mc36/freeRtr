package rtr;

import cfg.cfgVrf;
import java.util.Comparator;

/**
 * bgp4 vrf
 *
 * @author matecsaba
 */
public class rtrBgpVrf implements Comparator<rtrBgpVrf> {

    /**
     * vrf to use
     */
    protected final String vrf;

    /**
     * handler
     */
    protected final rtrBgpVrfRtr doer;

    /**
     * create new instance
     *
     * @param p parent to use
     * @param v vrf to use
     * @param o other afi
     */
    public rtrBgpVrf(rtrBgp p, cfgVrf v, boolean o) {
        vrf = "" + v.name;
        doer = new rtrBgpVrfRtr(p, v, o);
    }

    public int compare(rtrBgpVrf o1, rtrBgpVrf o2) {
        return o1.vrf.compareTo(o2.vrf);
    }

}
