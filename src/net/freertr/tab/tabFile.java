package net.freertr.tab;

import java.io.File;
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
    public final int pos;

    /**
     * the constructor
     *
     * @param i the id number
     * @param p position number
     */
    public tabFile(int i, int p) {
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
    public int compare(tabFile o1, tabFile o2) {
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
