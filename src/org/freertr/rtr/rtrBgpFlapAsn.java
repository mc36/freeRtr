package org.freertr.rtr;

import java.util.Comparator;
import org.freertr.clnt.clntWhois;
import org.freertr.util.bits;

/**
 * bgp4 flap statistic
 *
 * @author matecsaba
 */
public class rtrBgpFlapAsn implements Comparator<rtrBgpFlapAsn> {

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

    public int compare(rtrBgpFlapAsn o1, rtrBgpFlapAsn o2) {
        if (o1.prev < o2.prev) {
            return -1;
        }
        if (o1.prev > o2.prev) {
            return +1;
        }
        if (o1.asn < o2.asn) {
            return -1;
        }
        if (o1.asn > o2.asn) {
            return +1;
        }
        return 0;
    }

    public String toString() {
        return bits.num2str(asn) + "|" + clntWhois.asn2name(asn, true) + "|" + count + "|" + clntWhois.asn2infos(asn);
    }

}
