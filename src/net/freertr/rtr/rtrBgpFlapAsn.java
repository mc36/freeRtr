package net.freertr.rtr;

import java.util.Comparator;
import net.freertr.clnt.clntWhois;
import net.freertr.util.bits;

/**
 * bgp4 flap statistic
 *
 * @author matecsaba
 */
public class rtrBgpFlapAsn implements Comparator<rtrBgpFlapAsn> {

    /**
     * path
     */
    public final int asn;

    /**
     * counter
     */
    public int count;

    /**
     * create instance
     *
     * @param as asn
     */
    public rtrBgpFlapAsn(int as) {
        asn = as;
    }

    public int compare(rtrBgpFlapAsn o1, rtrBgpFlapAsn o2) {
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
