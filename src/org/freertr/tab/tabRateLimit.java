package org.freertr.tab;

import org.freertr.util.bits;

/**
 * rate limiter
 *
 * @author matecsaba
 */
public class tabRateLimit {

    /**
     * last cleared
     */
    private long last = 0;

    /**
     * times in this period
     */
    private int done = 0;

    /**
     * size of interval
     */
    private int intrvl = 0;

    /**
     * times per interval
     */
    private int allowd = 0;

    /**
     * create instance
     *
     * @param r allowed rate
     * @param i interval
     */
    public tabRateLimit(int r, int i) {
        allowd = r;
        intrvl = i;
    }

    public String toString() {
        return allowd + " " + intrvl;
    }

    /**
     * check unreachable interval
     *
     * @param inc increment
     * @return false if good, true if error
     */
    public boolean check(int inc) {
        long tim = bits.getTime();
        if ((tim - last) > intrvl) {
            done = 0;
        }
        if (done >= allowd) {
            return true;
        }
        last = tim;
        done += inc;
        return false;
    }

    /**
     * get interval in ms
     *
     * @return interval
     */
    public int getInterval() {
        return intrvl;
    }

    /**
     * get bytes/interval
     *
     * @return bytes/int
     */
    public int getBytePerInt() {
        return allowd;
    }

}
