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
     * mode, 0=none, 1=min, 2=avg, 3=max, 4=diff
     */
    public int algorithm = 1;

    /**
     * number of bucket
     */
    public int buckets = 5;

    /**
     * minimum result
     */
    public int minimum;

    /**
     * maximum result
     */
    public int maximum;

    /**
     * divide result
     */
    public int divisor = 1;

    /**
     * multiply result
     */
    public int multiply = 1;

    /**
     * relaxer changes
     */
    public int ignorer = 0;

    /**
     * discard smallest values
     */
    public int discardLo = 0;

    /**
     * discard highest values
     */
    public int discardHi = 0;

    private int lastReported;

    private final List<Integer> pastValues;

    /**
     * create instance
     *
     * @param min minimum
     * @param max maximum
     */
    public tabAverage(int min, int max) {
        pastValues = new ArrayList<Integer>();
        minimum = min;
        maximum = max;
        lastReported = max;
    }

    public String toString() {
        String a = "";
        for (int i = 0; i < pastValues.size(); i++) {
            a += " " + pastValues.get(i);
        }
        return a;
    }

    /**
     * get algorithm name
     *
     * @return name
     */
    public String getAlgoName() {
        switch (algorithm) {
            case 0:
                return "none";
            case 1:
                return "minimum";
            case 2:
                return "average";
            case 3:
                return "maximum";
            case 4:
                return "differs";
            default:
                return "unknown=" + algorithm;
        }
    }

    /**
     * set algorithm name
     *
     * @param a name
     */
    public void string2algo(String a) {
        if (a.equals("none")) {
            algorithm = 0;
            return;
        }
        if (a.equals("minimum")) {
            algorithm = 1;
            return;
        }
        if (a.equals("average")) {
            algorithm = 2;
            return;
        }
        if (a.equals("maximum")) {
            algorithm = 3;
            return;
        }
        if (a.equals("differs")) {
            algorithm = 4;
            return;
        }
        algorithm = 0;
    }

    /**
     * add value
     *
     * @param val value
     */
    public void addValue(int val) {
        synchronized (pastValues) {
            pastValues.add(val);
            for (; pastValues.size() > buckets;) {
                pastValues.remove(0);
            }
        }
    }

    /**
     * update parameters
     *
     * @param src source
     */
    public void updateFrom(tabAverage src) {
        algorithm = src.algorithm;
        buckets = src.buckets;
        minimum = src.minimum;
        maximum = src.maximum;
        divisor = src.divisor;
        multiply = src.multiply;
        ignorer = src.ignorer;
    }

    /**
     * get result
     *
     * @param met default value
     * @return result
     */
    public int getResult(int met) {
        if (pastValues.size() < 1) {
            return met;
        }
        List<Integer> vals = pastValues;
        for (int o = 0; o < discardLo; o++) {
            List<Integer> v = new ArrayList<Integer>();
            synchronized (vals) {
                for (int i = 0; i < vals.size(); i++) {
                    v.add(vals.get(i));
                }
            }
            int val = Integer.MAX_VALUE;
            int seq = -1;
            for (int i = 0; i < v.size(); i++) {
                int cur = v.get(i);
                if (cur < val) {
                    val = cur;
                    seq = i;
                }
            }
            if (seq < 0) {
                return met;
            }
            v.remove(seq);
            vals = v;
        }
        for (int o = 0; o < discardHi; o++) {
            List<Integer> v = new ArrayList<Integer>();
            synchronized (vals) {
                for (int i = 0; i < vals.size(); i++) {
                    v.add(vals.get(i));
                }
            }
            int val = Integer.MIN_VALUE;
            int seq = -1;
            for (int i = 0; i < v.size(); i++) {
                int cur = v.get(i);
                if (cur > val) {
                    val = cur;
                    seq = i;
                }
            }
            if (seq < 0) {
                return met;
            }
            v.remove(seq);
            vals = v;
        }
        switch (algorithm) {
            case 1:
                met = Integer.MAX_VALUE;
                synchronized (vals) {
                    for (int i = 0; i < vals.size(); i++) {
                        int cur = vals.get(i);
                        if (cur < met) {
                            met = cur;
                        }
                    }
                }
                break;
            case 2:
                met = 0;
                synchronized (vals) {
                    for (int i = 0; i < vals.size(); i++) {
                        int cur = vals.get(i);
                        met += cur;
                    }
                    met /= vals.size();
                }
                break;
            case 3:
                met = Integer.MIN_VALUE;
                synchronized (vals) {
                    for (int i = 0; i < vals.size(); i++) {
                        int cur = vals.get(i);
                        if (cur > met) {
                            met = cur;
                        }
                    }
                }
                break;
            case 4:
                synchronized (vals) {
                    met = 0;
                    int prv = vals.get(0);
                    for (int i = 1; i < vals.size(); i++) {
                        int cur = vals.get(i);
                        int dif = prv - cur;
                        prv = cur;
                        if (dif < 0) {
                            dif = -dif;
                        }
                        met += dif;
                    }
                }
                break;
            default:
                return met;
        }
        met *= multiply;
        met /= divisor;
        if (met < minimum) {
            met = minimum;
        }
        if (met > maximum) {
            met = maximum;
        }
        int i = met - lastReported;
        if (i < 0) {
            i = -i;
        }
        if (i < ignorer) {
            return lastReported;
        }
        lastReported = met;
        return met;
    }

}
