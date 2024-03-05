package org.freertr.spf;

import java.util.Comparator;

/**
 * spf index
 *
 * @author matecsaba
 */
public class spfIndex implements Comparator<spfIndex> {

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

    public int compare(spfIndex o1, spfIndex o2) {
        if (o1.val < o2.val) {
            return -1;
        }
        if (o1.val > o2.val) {
            return +1;
        }
        return 0;
    }

}
