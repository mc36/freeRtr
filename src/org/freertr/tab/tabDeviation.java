package org.freertr.tab;

import java.util.List;
import org.freertr.util.bits;

/**
 * rolling statistics
 *
 * @author matecsaba
 */
public class tabDeviation {

    /**
     * create instance
     */
    public tabDeviation() {
        min = Long.MAX_VALUE;
        max = Long.MIN_VALUE;
        lst = 0;
        cnt = 0;
        ak = 0;
        qk = 0;
        qr = 0;
    }

    private long min;

    private long max;

    private long lst;

    private long cnt;

    private float ak;

    private float qk;

    private float qr;

    /**
     * got value
     *
     * @param v value
     */
    public void addVal(long v) {
        lst = v;
        cnt++;
        if (v < min) {
            min = v;
        }
        if (v > max) {
            max = v;
        }
        float x = v;
        float ak1 = ak;
        float qk1 = qk;
        ak = ak1 + ((x - ak1) / cnt);
        qk = qk1 + ((x - ak1) * (x - ak));
        qr = qk / cnt;
    }

    /**
     * get results
     *
     * @return results
     */
    public String toString() {
        if (cnt < 1) {
            return "n/a";
        }
        return min + "/" + bits.toPrecise(ak) + "/" + max + "/" + bits.toPrecise(qr);
    }

    /**
     * get minimum
     *
     * @return value
     */
    public long getMin() {
        return min;
    }

    /**
     * get maximum
     *
     * @return value
     */
    public long getMax() {
        return max;
    }

    /**
     * get last
     *
     * @return value
     */
    public long getLst() {
        return lst;
    }

    /**
     * get count
     *
     * @return value
     */
    public long getCnt() {
        return cnt;
    }

    /**
     * get average
     *
     * @return value
     */
    public float getAvg() {
        return ak;
    }

    /**
     * get deviation
     *
     * @return value
     */
    public float getDev() {
        return qr;
    }

    /**
     * get full info
     *
     * @param l list to append
     */
    public void getFull(List<String> l) {
        l.add("last|" + lst);
        l.add("count|" + cnt);
        l.add("minimum|" + min);
        l.add("average|" + bits.toPrecise(ak));
        l.add("maximum|" + max);
        l.add("deviate|" + bits.toPrecise(qr));
    }

}
