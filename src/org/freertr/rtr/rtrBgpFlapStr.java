package org.freertr.rtr;

import org.freertr.util.bits;

/**
 * bgp4 flap statistic
 *
 * @author matecsaba
 */
public class rtrBgpFlapStr implements Comparable<rtrBgpFlapStr> {

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

    public int compareTo(rtrBgpFlapStr o) {
        return info.compareTo(o.info);
    }

    public String toString() {
        return count + " " + bits.timePast(last) + " " + info;
    }

}
