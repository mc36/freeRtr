package org.freertr.util;

import java.util.Arrays;

/**
 * small list of ints
 *
 * @author matecsaba
 */
public class smalist {

    private int[] lst;

    /**
     * create instance
     */
    public smalist() {
        lst = new int[0];
    }

    /**
     * create instance
     *
     * @param s size
     */
    protected smalist(int s) {
        lst = new int[s];
    }

    /**
     * size of list
     *
     * @return size
     */
    public int size() {
        return lst.length;
    }

    /**
     * add entry
     *
     * @param v value
     */
    public void add(int v) {
        int[] res = new int[lst.length + 1];
        System.arraycopy(lst, 0, res, 0, lst.length);
        res[lst.length] = v;
        lst = res;
    }

    /**
     * add values, if not already
     *
     * @param src source of values
     */
    public void appendIfNot(smalist src) {
        if (src == null) {
            return;
        }
        for (int i = 0; i < src.lst.length; i++) {
            int o = src.lst[i];
            if (indexOf(o) >= 0) {
                continue;
            }
            add(o);
        }
    }

    /**
     * index of value
     *
     * @param v value
     * @return index, -1 if not found
     */
    public int indexOf(int v) {
        for (int i = 0; i < lst.length; i++) {
            if (lst[i] == v) {
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
        return lst[i];
    }

    /**
     * delete entry
     *
     * @param i index
     */
    public void remove(int i) {
        int[] res = new int[lst.length - 1];
        System.arraycopy(lst, 0, res, 0, i);
        System.arraycopy(lst, i + 1, res, i, res.length - i);
        lst = res;
    }

    /**
     * sort list
     */
    public void sort() {
        Arrays.sort(lst);
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
        smalist res = new smalist(src.lst.length);
        System.arraycopy(src.lst, 0, res.lst, 0, src.lst.length);
        return res;
    }

    /**
     * null empty list
     *
     * @param l list
     * @return maybe nulled list
     */
    public static smalist nullEmptyList(smalist l) {
        if (l == null) {
            return null;
        }
        if (l.lst.length < 1) {
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
        smalist res = new smalist(1);
        res.lst[0] = val;
        return res;
    }

    /**
     * prepend one label
     *
     * @param src where to prepend
     * @param val label to prepend
     * @return updated target list
     */
    public static smalist prependLabel(smalist src, int val) {
        if (src == null) {
            smalist res = new smalist(1);
            res.lst[0] = val;
            return res;
        }
        smalist res = new smalist(src.lst.length + 1);
        System.arraycopy(src.lst, 0, res.lst, 1, src.lst.length);
        res.lst[0] = val;
        return res;
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
            smalist res = new smalist(src.lst.length);
            System.arraycopy(src.lst, 0, res.lst, 0, src.lst.length);
            return res;
        }
        smalist res = new smalist(src.lst.length + trg.lst.length);
        System.arraycopy(src.lst, 0, res.lst, 0, src.lst.length);
        System.arraycopy(trg.lst, 0, res.lst, src.lst.length, trg.lst.length);
        return res;
    }

    /**
     * find on integer list
     *
     * @param lst list to use
     * @param val value to find
     * @return position, -1 if not found
     */
    public static int findIntList(smalist lst, int val) {
        if (lst == null) {
            return -1;
        }
        return lst.indexOf(val);
    }

    /**
     * replace on integer list
     *
     * @param lst list to use
     * @param src source to replace
     * @param trg target to replace
     */
    public static void replaceIntList(smalist lst, int src, int trg) {
        if (lst == null) {
            return;
        }
        for (int i = 0; i < lst.lst.length; i++) {
            if (lst.lst[i] == src) {
                lst.lst[i] = trg;
            }
        }
    }

}
