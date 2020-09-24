package util;

import java.util.Comparator;

/**
 * spf index
 *
 * @author matecsaba
 */
public class shrtPthFrstIdx implements Comparator<shrtPthFrstIdx> {

    protected final int val;

    public shrtPthFrstIdx(int i) {
        val = i;
    }

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
