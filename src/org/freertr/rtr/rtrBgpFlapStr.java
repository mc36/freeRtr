package org.freertr.rtr;

import java.util.Comparator;
import org.freertr.util.bits;

/**
 * bgp4 flap statistic
 *
 * @author matecsaba
 */
public class rtrBgpFlapStr implements Comparator<rtrBgpFlapStr> {

    /**
     * path
     */
    public final String info;

    /**
     * counter
     */
    public int count;

    /**
     * last
     */
    public long last;

    /**
     * create instance
     *
     * @param a as path
     */
    public rtrBgpFlapStr(String a) {
        info = a;
    }

    public int compare(rtrBgpFlapStr o1, rtrBgpFlapStr o2) {
        return o1.info.compareTo(o2.info);
    }

    public String toString() {
        return count + " " + bits.timePast(last) + " " + info;
    }

}
