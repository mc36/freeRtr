package net.freertr.tab;

import net.freertr.util.bits;

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
     * @return false if good, true if error
     */
    public boolean check() {
        long tim = bits.getTime();
        if ((tim - last) > intrvl) {
            done = 0;
        }
        if (done >= allowd) {
            return true;
        }
        last = tim;
        done++;
        return false;
    }

}
