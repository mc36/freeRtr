package rtr;

import addr.addrIPv4;
import java.util.Comparator;
import util.bits;

/**
 * ospfv3 spf address
 *
 * @author matecsaba
 */
public class rtrOspf6areaSpf implements Comparator<rtrOspf6areaSpf> {

    /**
     * node address
     */
    public addrIPv4 adr;

    /**
     * link id
     */
    public int lnk;

    /**
     * create instance
     *
     * @param a node
     * @param l link
     */
    public rtrOspf6areaSpf(addrIPv4 a, int l) {
        adr = a;
        lnk = l;
    }

    public int compare(rtrOspf6areaSpf o1, rtrOspf6areaSpf o2) {
        if (o1.lnk < o2.lnk) {
            return -1;
        }
        if (o1.lnk > o2.lnk) {
            return +1;
        }
        return o1.adr.compare(o1.adr, o2.adr);
    }

    public String toString() {
        return adr + "/" + bits.toHexD(lnk);
    }

    /**
     * convert from string
     *
     * @param s string to convert
     * @return true on error, false on success
     */
    public boolean fromString(String s) {
        int i = s.lastIndexOf("/");
        if (i < 0) {
            return true;
        }
        if (adr.fromString(s.substring(0, i))) {
            return true;
        }
        lnk = bits.fromHex(s.substring(i + 1, s.length()));
        return false;
    }

}
