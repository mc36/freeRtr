package org.freertr.rtr;

import org.freertr.cfg.cfgVrf;

/**
 * bgp4 vrf
 *
 * @author matecsaba
 */
public class rtrBgpVrf implements Comparable<rtrBgpVrf> {

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

    public int compareTo(rtrBgpVrf o) {
        return vrf.compareTo(o.vrf);
    }

}
