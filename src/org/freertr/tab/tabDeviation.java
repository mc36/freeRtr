package org.freertr.tab;

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
    }

    private long min;

    private long max;

    private long lst;

    private long cnt;

    private float ak;

    private float qk;

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
        return min + "/" + bits.toPrecise(ak) + "/" + max + "/" + bits.toPrecise(qk / (float) cnt);
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
        if (cnt < 1) {
            return 0;
        }
        return qk / (float) cnt;
    }

}
