package net.freertr.tab;

import java.util.ArrayList;
import java.util.List;

/**
 * average calculator
 *
 * @author matecsaba
 */
public class tabAverage {

    /**
     * maximum size
     */
    public int max;

    private List<Integer> vals;

    /**
     * create instance
     */
    public tabAverage() {
        vals = new ArrayList<Integer>();
    }

    public String toString() {
        String a = "";
        for (int i = 0; i < vals.size(); i++) {
            a += " " + vals.get(i);
        }
        return a;
    }

    /**
     * add value
     *
     * @param val value
     */
    public void addValue(int val) {
        vals.add(val);
        for (; vals.size() > max;) {
            vals.remove(0);
        }
    }

    /**
     * get average
     *
     * @return result
     */
    public int getAverage() {
        if (vals.size() < 1) {
            return Integer.MAX_VALUE / 2;
        }
        int res = 0;
        for (int i = 0; i < vals.size(); i++) {
            int cur = vals.get(i);
            res += cur;
        }
        return res / vals.size();
    }

    /**
     * get minimum
     *
     * @return result
     */
    public int getMinimum() {
        int res = Integer.MAX_VALUE;
        for (int i = 0; i < vals.size(); i++) {
            int cur = vals.get(i);
            if (cur < res) {
                res = cur;
            }
        }
        return res;
    }

    /**
     * get maximum
     *
     * @return result
     */
    public int getMaximum() {
        int res = Integer.MIN_VALUE;
        for (int i = 0; i < vals.size(); i++) {
            int cur = vals.get(i);
            if (cur > res) {
                res = cur;
            }
        }
        return res;
    }

}
