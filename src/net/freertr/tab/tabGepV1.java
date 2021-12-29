package net.freertr.tab;

import java.util.Map;
import java.util.TreeMap;
import net.freertr.addr.addrPrefix;
import net.freertr.addr.addrType;

/**
 * one prefix tree
 *
 * @param <T> type of elements in the list
 * @author matecsaba
 */
public class tabGepV1<T extends addrType> {

    private TreeMap<tabGepV1nod<T>, tabRouteEntry<T>> tree;

    /**
     * create one generic tree
     */
    public tabGepV1() {
        tree = new TreeMap<tabGepV1nod<T>, tabRouteEntry<T>>();
    }

    /**
     * add one entry
     *
     * @param val value to add
     * @return old value, null if freshly added
     */
    public tabRouteEntry<T> add(tabRouteEntry<T> val) {
        return tree.put(new tabGepV1nod<T>(val), val);
    }

    /**
     * add one entry
     *
     * @param val value to add
     * @return removed value, null if not found
     */
    public tabRouteEntry<T> del(tabRouteEntry<T> val) {
        return tree.remove(new tabGepV1nod<T>(val));
    }

    /**
     * find one container entry
     *
     * @param val value to look up
     * @return found value
     */
    public tabRouteEntry<T> search(tabRouteEntry<T> val) {
        Map.Entry<tabGepV1nod<T>, tabRouteEntry<T>> res = tree.floorEntry(new tabGepV1nod<T>(val));
        for (;;) {
            if (res == null) {
                return null;
            }
            tabRouteEntry<T> v = res.getValue();
            if (v.prefix.supernet(val.prefix, true)) {
                return v;
            }
            res = tree.lowerEntry(res.getKey());
        }
    }

}

class tabGepV1nod<T extends addrType> implements Comparable<tabGepV1nod<T>> {

    public addrPrefix<T> pfx;

    public tabGepV1nod(tabRouteEntry<T> val) {
        pfx = val.prefix;
    }

    public int compareTo(tabGepV1nod<T> o) {
        return pfx.compare(pfx, o.pfx);
    }

}
