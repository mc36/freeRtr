package tab;

import java.util.Comparator;
import util.bits;

/**
 * large community
 *
 * @author matecsaba
 */
public class tabLargeComm implements Comparator<tabLargeComm> {

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

    public int compare(tabLargeComm o1, tabLargeComm o2) {
        if (o1.as < o2.as) {
            return -1;
        }
        if (o1.as > o2.as) {
            return +1;
        }
        if (o1.d1 < o2.d1) {
            return -1;
        }
        if (o1.d1 > o2.d1) {
            return +1;
        }
        if (o1.d2 < o2.d2) {
            return -1;
        }
        if (o1.d2 > o2.d2) {
            return +1;
        }
        return 0;
    }
}
