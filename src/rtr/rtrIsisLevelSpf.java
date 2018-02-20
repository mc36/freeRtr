package rtr;

import addr.addrIsis;
import java.util.Comparator;
import util.bits;

/**
 * isis spf address
 *
 * @author matecsaba
 */
public class rtrIsisLevelSpf implements Comparator<rtrIsisLevelSpf> {

    /**
     * node address
     */
    public addrIsis adr;

    /**
     * pseudonode id
     */
    public int psn;

    /**
     * create instance
     *
     * @param a node
     * @param p pseudonode
     */
    public rtrIsisLevelSpf(addrIsis a, int p) {
        adr = a;
        psn = p;
    }

    public int compare(rtrIsisLevelSpf o1, rtrIsisLevelSpf o2) {
        if (o1.psn < o2.psn) {
            return -1;
        }
        if (o1.psn > o2.psn) {
            return +1;
        }
        return o1.adr.compare(o1.adr, o2.adr);
    }

    public String toString() {
        return adr + "." + bits.toHexB(psn);
    }

    /**
     * convert from string
     *
     * @param s string to convert
     * @return true on error, false on success
     */
    public boolean fromString(String s) {
        int i = s.lastIndexOf(".");
        if (i < 0) {
            return true;
        }
        if (adr.fromString(s.substring(0, i))) {
            return true;
        }
        psn = bits.fromHex(s.substring(i + 1, s.length()));
        return false;
    }

}
