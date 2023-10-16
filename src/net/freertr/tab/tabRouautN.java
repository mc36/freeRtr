package net.freertr.tab;

import java.util.Comparator;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * route authorization entry
 *
 * @author matecsaba
 */
public class tabRouautN implements Comparator<tabRouautN> {

    /**
     * create instance
     */
    public tabRouautN() {
    }

    /**
     * prefix base
     */
    public addrPrefix<addrIP> pref;

    /**
     * max length
     */
    public int max;

    /**
     * allowed asn
     */
    public int asn;

    public int compare(tabRouautN o1, tabRouautN o2) {
        if (o1.max < o2.max) {
            return -1;
        }
        if (o1.max > o2.max) {
            return +1;
        }
        return o1.pref.compare(o1.pref, o2.pref);
    }

    /**
     * convert from string
     *
     * @param cmd commands to read
     * @return true on error, false on success
     */
    public boolean fromString(cmds cmd) {
        pref = addrPrefix.str2ip(cmd.word());
        if (pref == null) {
            return true;
        }
        max = bits.str2num(cmd.word());
        asn = bits.str2num(cmd.word());
        return false;
    }

    public String toString() {
        return pref + " " + max + " " + asn;
    }

}
