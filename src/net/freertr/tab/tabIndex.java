package net.freertr.tab;

import java.util.Comparator;
import net.freertr.addr.addrPrefix;
import net.freertr.addr.addrType;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.counter;

/**
 * one index to prefix entry
 *
 * @param <T> type of address
 * @author matecsaba
 */
public class tabIndex<T extends addrType> implements Comparator<tabIndex<T>> {

    /**
     * index
     */
    public final int index;

    /**
     * prefix
     */
    public final addrPrefix<T> prefix;

    /**
     * neighbors
     */
    public tabGen<tabIndex<T>> neighs;

    /**
     * bitmap
     */
    public int bitmap;

    /**
     * connected
     */
    public boolean conned;

    /**
     * counter
     */
    public counter cntr = new counter();

    /**
     * hardware counter
     */
    public counter hwCntr;

    /**
     * create entry
     *
     * @param idx index
     * @param prf prefix
     */
    public tabIndex(int idx, addrPrefix<T> prf) {
        index = idx;
        prefix = prf;
    }

    public String toString() {
        return index + " " + prefix;
    }

    /**
     * copy entry
     *
     * @return copy
     */
    public tabIndex<T> copyBytes() {
        tabIndex<T> n = new tabIndex<T>(index, prefix.copyBytes());
        n.conned = conned;
        n.bitmap = bitmap;
        if (neighs != null) {
            n.neighs = new tabGen<tabIndex<T>>(neighs);
        }
        return n;
    }

    /**
     * compare to entry
     *
     * @param o other entry
     * @return true if differs, false if equals
     */
    public boolean differs(tabIndex<T> o) {
        if (bitmap != o.bitmap) {
            return true;
        }
        if (conned != o.conned) {
            return true;
        }
        if (prefix == null) {
            if (o.prefix != null) {
                return true;
            }
        } else {
            if (o.prefix == null) {
                return true;
            }
            if (prefix.compare(prefix, o.prefix) != 0) {
                return true;
            }
        }
        if (neighs == null) {
            if (o.neighs != null) {
                return true;
            }
        } else {
            if (o.neighs == null) {
                return true;
            }
            if (neighs.size() != o.neighs.size()) {
                return true;
            }
            for (int i = 0; i < neighs.size(); i++) {
                if (neighs.get(i) != o.neighs.get(i)) {
                    return true;
                }
            }
        }
        return false;
    }

    public int compare(tabIndex<T> o1, tabIndex<T> o2) {
        if (o1.index < o2.index) {
            return -1;
        }
        if (o1.index > o2.index) {
            return +1;
        }
        return 0;
    }

    /**
     * is better than other
     *
     * @param o other entry
     * @return true if better, false if not
     */
    public boolean isBetter(tabIndex<T> o) {
        if (prefix.maskLen < o.prefix.maskLen) {
            return false;
        }
        if (prefix.maskLen > o.prefix.maskLen) {
            return true;
        }
        return prefix.compare(prefix, o.prefix) < 0;
    }

    /**
     * merge tables
     *
     * @param <T> type of address
     * @param trg target table
     * @param src source entry
     * @return true if added, false if not
     */
    public static <T extends addrType> boolean add2table(tabGen<tabIndex<T>> trg, tabIndex<T> src) {
        tabIndex<T> old = trg.add(src);
        if (old == null) {
            return true;
        }
        if (!src.isBetter(old)) {
            return false;
        }
        trg.put(src);
        return true;
    }

    /**
     * merge tables
     *
     * @param <T> type of address
     * @param trg target table
     * @param src source table
     * @return entries added
     */
    public static <T extends addrType> int mergeTable(tabGen<tabIndex<T>> trg, tabGen<tabIndex<T>> src) {
        if (src == null) {
            return -1;
        }
        int cnt = 0;
        for (int i = 0; i < src.size(); i++) {
            tabIndex<T> ntry = src.get(i);
            if (add2table(trg, ntry)) {
                cnt++;
            }
        }
        return cnt;
    }

    /**
     * merge tables
     *
     * @param <T> type of address
     * @param src source list
     * @return converted
     */
    public static <T extends addrType> String convertTable(tabGen<tabIndex<T>> src) {
        if (src == null) {
            return "null";
        }
        String a = "";
        for (int i = 0; i < src.size(); i++) {
            a += " " + src.get(i).index;
        }
        if (src.size() > 0) {
            a = a.substring(1, a.length());
        }
        return a;
    }

    /**
     * merge tables
     *
     * @param <T> type of address
     * @param cmd source list
     * @return converted
     */
    public static <T extends addrType> tabGen<tabIndex<T>> convertTable(cmds cmd) {
        tabGen<tabIndex<T>> res = new tabGen<tabIndex<T>>();
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            tabIndex<T> ntry = new tabIndex<T>(bits.str2num(a), null);
            res.put(ntry);
        }
        return res;
    }

}
