package org.freertr.tab;

import java.util.Comparator;

/**
 * one virtual machine file
 *
 * @author matecsaba
 */
public class tabFile implements Comparator<tabFile> {

    /**
     * the number of the file
     */
    public final int num;

    /**
     * position
     */
    public int pos;

    /**
     * the constructor
     *
     * @param i index number
     * @param p position number
     */
    public tabFile(int i, int p) {
        num = i;
        pos = p;
    }

    /**
     * the constructor
     *
     * @param i the id number
     */
    public tabFile(int i) {
        num = i;
        pos = 0;
    }

    /**
     * compare two directories
     *
     * @param o1 first one
     * @param o2 second one
     * @return -1 if smaller, +1 if bigger, 0 if equals
     */
    public int compare(tabFile o1, tabFile o2) {
        if (o1.num < o2.num) {
            return -1;
        }
        if (o1.num > o2.num) {
            return +1;
        }
        if (o1.pos < o2.pos) {
            return -1;
        }
        if (o1.pos > o2.pos) {
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
