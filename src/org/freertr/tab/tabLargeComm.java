package org.freertr.tab;

import org.freertr.util.bits;

/**
 * large community
 *
 * @author matecsaba
 */
public class tabLargeComm implements Comparable<tabLargeComm> {

    /**
     * create instance
     */
    public tabLargeComm() {
    }

    /**
     * as number
     */
    public int as;

    /**
     * data #1
     */
    public int d1;

    /**
     * data #2
     */
    public int d2;

    public String toString() {
        return bits.num2str(as) + ":" + d1 + ":" + d2;
    }

    /**
     * convert from string
     *
     * @param a string to read
     * @return false on success, true on error
     */
    public boolean fromString(String a) {
        int i = a.indexOf(":");
        if (i < 0) {
            return true;
        }
        as = bits.str2num(a.substring(0, i));
        a = a.substring(i + 1, a.length());
        i = a.indexOf(":");
        if (i < 0) {
            return true;
        }
        d1 = bits.str2num(a.substring(0, i));
        d2 = bits.str2num(a.substring(i + 1, a.length()));
        return false;
    }

    /**
     * copy data
     *
     * @return copy
     */
    public tabLargeComm copyBytes() {
        tabLargeComm d = new tabLargeComm();
        d.as = as;
        d.d1 = d1;
        d.d2 = d2;
        return d;
    }

    public int compareTo(tabLargeComm o) {
        if (as < o.as) {
            return -1;
        }
        if (as > o.as) {
            return +1;
        }
        if (d1 < o.d1) {
            return -1;
        }
        if (d1 > o.d1) {
            return +1;
        }
        if (d2 < o.d2) {
            return -1;
        }
        if (d2 > o.d2) {
            return +1;
        }
        return 0;
    }

}
