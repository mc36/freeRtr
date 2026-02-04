package org.freertr.tab;

import org.freertr.addr.addrIPv6;
import org.freertr.util.bits;

/**
 * ipv6 community
 *
 * @author matecsaba
 */
public class tabIpv6comm implements Comparable<tabIpv6comm> {

    /**
     * create instance
     */
    public tabIpv6comm() {
    }

    /**
     * type
     */
    public int typ;

    /**
     * address
     */
    public addrIPv6 adr = new addrIPv6();

    /**
     * local
     */
    public int loc;

    public String toString() {
        return typ + "-" + adr + "-" + loc;
    }

    /**
     * convert from string
     *
     * @param a string to read
     * @return false on success, true on error
     */
    public boolean fromString(String a) {
        int i = a.indexOf("-");
        if (i < 0) {
            return true;
        }
        typ = bits.str2num(a.substring(0, i));
        a = a.substring(i + 1, a.length());
        i = a.indexOf("-");
        if (i < 0) {
            return true;
        }
        loc = bits.str2num(a.substring(i + 1, a.length()));
        return adr.fromString(a.substring(0, i));
    }

    /**
     * copy data
     *
     * @return copy
     */
    public tabIpv6comm copyBytes() {
        tabIpv6comm d = new tabIpv6comm();
        d.adr.setAddr(adr);
        d.typ = typ;
        d.loc = loc;
        return d;
    }

    public int compareTo(tabIpv6comm o) {
        if (typ < o.typ) {
            return -1;
        }
        if (typ > o.typ) {
            return +1;
        }
        if (loc < o.loc) {
            return -1;
        }
        if (loc > o.loc) {
            return +1;
        }
        return adr.compareTo(o.adr);
    }

}
