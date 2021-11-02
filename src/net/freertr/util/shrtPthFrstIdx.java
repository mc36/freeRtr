package net.freertr.util;

import java.util.Comparator;

/**
 * spf index
 *
 * @author matecsaba
 */
public class shrtPthFrstIdx implements Comparator<shrtPthFrstIdx> {

    /**
     * value
     */
    protected final int val;

    /**
     * create instance
     *
     * @param i index
     */
    public shrtPthFrstIdx(int i) {
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

    public int compare(shrtPthFrstIdx o1, shrtPthFrstIdx o2) {
        if (o1.val < o2.val) {
            return -1;
        }
        if (o1.val > o2.val) {
            return +1;
        }
        return 0;
    }

}
