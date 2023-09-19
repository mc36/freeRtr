package net.freertr.clnt;

import java.util.Comparator;
import net.freertr.cfg.cfgAll;
import net.freertr.util.bits;

/**
 * whois asn cache
 *
 * @author matecsaba
 */
public class clntWhoisAsn implements Comparator<clntWhoisAsn> {

    /**
     * as number
     */
    public final int asn;

    /**
     * time created
     */
    public final long created;

    /**
     * hit count
     */
    public int hits = 1;

    /**
     * name cached
     */
    public String name;

    /**
     * create entry
     *
     * @param i asn
     */
    public clntWhoisAsn(int i) {
        asn = i;
        created = bits.getTime();
    }

    public int compare(clntWhoisAsn o1, clntWhoisAsn o2) {
        if (o1.asn < o2.asn) {
            return -1;
        }
        if (o1.asn > o2.asn) {
            return +1;
        }
        return 0;
    }

    public String toString() {
        return bits.num2str(asn) + "|" + name + "|" + bits.timePast(created) + "|" + bits.time2str(cfgAll.timeZoneName, created + cfgAll.timeServerOffset, 3) + "|" + hits;
    }
}
