package org.freertr.spf;

/**
 * spf index
 *
 * @author matecsaba
 */
public class spfIndex implements Comparable<spfIndex> {

    /**
     * value
     */
    protected final int val;

    /**
     * create instance
     *
     * @param i index
     */
    public spfIndex(int i) {
        val = i;
    }

    public String toString() {
        return "" + val;
    }

    /**
     * get value
     *
     * @return value
     */
    public int get() {
        return val;
    }

    public int compareTo(spfIndex o) {
        if (val < o.val) {
            return -1;
        }
        if (val > o.val) {
            return +1;
        }
        return 0;
    }

}
