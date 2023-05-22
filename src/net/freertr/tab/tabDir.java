package net.freertr.tab;

import java.io.File;
import java.util.Comparator;

/**
 * one virtual machine directory
 *
 * @author matecsaba
 */
public class tabDir implements Comparator<tabDir> {

    /**
     * the number of the file
     */
    public final int num;

    /**
     * position
     */
    public final int pos;

    /**
     * the constructor
     *
     * @param i index number
     * @param p position number
     */
    public tabDir(int i, int p) {
        num = i;
        pos = p;
    }

    /**
     * compare two directories
     *
     * @param o1 first one
     * @param o2 second one
     * @return -1 if smaller, +1 if bigger, 0 if equals
     */
    public int compare(tabDir o1, tabDir o2) {
        if (o1.num < o2.num) {
            return -1;
        }
        if (o1.num > o2.num) {
            return +1;
        }
        return 0;
    }

    /**
     * close the file
     */
    public final static void doClose() {
    }

}
