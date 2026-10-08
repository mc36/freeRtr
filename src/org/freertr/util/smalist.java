package org.freertr.util;

import java.util.ArrayList;
import java.util.Collections;

/**
 * small list of ints
 *
 * @author matecsaba
 */
public class smalist {

    private ArrayList<Integer> lst;

    /**
     * size of list
     *
     * @return size
     */
    public int size() {
        return lst.size();
    }

    /**
     * add entry
     *
     * @param v value
     */
    public void add(int v) {
        lst.add(v);
    }

    /**
     * add entry
     *
     * @param i index
     * @param v value
     */
    public void set(int i, int v) {
        lst.set(i, v);
    }

    /**
     * index of value
     *
     * @param v value
     * @return index, -1 if not found
     */
    public int indexOf(int v) {
        for (int i = 0; i < lst.size(); i++) {
            if (lst.get(i) == v) {
                return i;
            }
        }
        return -1;
    }

    /**
     * get entry
     *
     * @param i index
     * @return value
     */
    public int get(int i) {
        return lst.get(i);
    }

    /**
     * delete entry
     *
     * @param i index
     * @return value
     */
    public int remove(int i) {
        return lst.remove(i);
    }

    /**
     * sort list
     */
    public void sort() {
        Collections.sort(lst);
    }

    /**
     * create instance
     */
    public smalist() {
        lst = new ArrayList<Integer>();
    }

    /**
     * copy labels
     *
     * @param src labels to copy
     * @return copyed labels
     */
    public static smalist copyLabels(smalist src) {
        if (src == null) {
            return null;
        }
        smalist res = new smalist();
        for (int i = 0; i < src.lst.size(); i++) {
            res.lst.add(i, src.lst.get(i));
        }
        return res;
    }

    /**
     * null empty list
     *
     * @param <E> type of list
     * @param l list
     * @return maybe nulled list
     */
    public static smalist nullEmptyList(smalist l) {
        if (l == null) {
            return null;
        }
        if (l.lst.size() < 1) {
            return null;
        }
        return l;
    }

    /**
     * convert integer to list
     *
     * @param val value to convert
     * @return converted
     */
    public static smalist int2labels(int val) {
        smalist res = new smalist();
        res.lst.add(val);
        return res;
    }

    /**
     * prepend one label
     *
     * @param trg where to prepend
     * @param val label to prepend
     * @return updated target list
     */
    public static smalist prependLabel(smalist trg, int val) {
        return prependLabels(trg, smalist.int2labels(val));
    }

    /**
     * prepend some labels
     *
     * @param trg where to prepend
     * @param src labels to prepend
     * @return updated target list
     */
    public static smalist prependLabels(smalist trg, smalist src) {
        if (src == null) {
            return trg;
        }
        if (src == trg) {
            return trg;
        }
        if (trg == null) {
            trg = new smalist();
        }
        for (int i = 0; i < src.lst.size(); i++) {
            trg.lst.add(i, src.lst.get(i));
        }
        return trg;
    }

}
