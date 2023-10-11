package net.freertr.rtr;

import java.util.Comparator;
import net.freertr.clnt.clntWhois;
import net.freertr.util.bits;

/**
 * bgp4 flap statistic
 *
 * @author matecsaba
 */
public class rtrBgpFlapasn implements Comparator<rtrBgpFlapasn> {

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
    public rtrBgpFlapasn(int as) {
        asn = as;
    }

    public int compare(rtrBgpFlapasn o1, rtrBgpFlapasn o2) {
        if (o1.asn < o2.asn) {
            return -1;
        }
        if (o1.asn > o2.asn) {
            return +1;
        }
        return 0;
    }

    public String toString() {
        return bits.num2str(asn) + "|" + count + "|" + clntWhois.asn2name(asn, true) + "|" + clntWhois.asn2infos(asn);
    }

}
