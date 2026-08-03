package org.freertr.rtr;

import org.freertr.clnt.clntWhois;
import org.freertr.util.bits;

/**
 * bgp4 flap asn
 *
 * @author matecsaba
 */
public class rtrBgpFlapAsn implements Comparable<rtrBgpFlapAsn> {

    /**
     * as number
     */
    public final int asn;

    /**
     * previous as number
     */
    public final int prev;

    /**
     * counter
     */
    public int count;

    /**
     * create instance
     *
     * @param pr previous asn
     * @param as asn
     */
    public rtrBgpFlapAsn(int pr, int as) {
        asn = as;
        prev = pr;
    }

    public int compareTo(rtrBgpFlapAsn o) {
        if (prev < o.prev) {
            return -1;
        }
        if (prev > o.prev) {
            return +1;
        }
        if (asn < o.asn) {
            return -1;
        }
        if (asn > o.asn) {
            return +1;
        }
        return 0;
    }

    public String toString() {
        return bits.num2str(asn) + "|" + clntWhois.asn2name(asn, true) + "|" + count + "|" + clntWhois.asn2infos(asn);
    }

}
