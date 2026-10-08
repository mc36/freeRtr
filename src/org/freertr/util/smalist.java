package org.freertr.util;

import java.util.ArrayList;

/**
 * small list of ints
 *
 * @author matecsaba
 */
public class smalist extends ArrayList<Integer> {

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
        res.addAll(src);
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
        if (l.size() < 1) {
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
        res.add(val);
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
        for (int i = 0; i < src.size(); i++) {
            trg.add(i, src.get(i));
        }
        return trg;
    }

    /**
     * create instance
     */
    public smalist() {
        super();
    }

}
