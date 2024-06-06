package org.freertr.tab;

import org.freertr.addr.addrType;

/**
 * one prefix tree
 *
 * @param <T> type of elements in the list
 * @author matecsaba
 */
public class tabGepV2<T extends addrType> {

    private tabGepV2nod<T> root;

    /**
     * create one generic tree
     */
    public tabGepV2() {
        root = new tabGepV2nod<T>();
    }

    /**
     * add one entry
     *
     * @param val value to add
     */
    public void add(tabRouteEntry<T> val) {
        tabGepV2nod<T> cur = root;
        for (int p = 0;; p++) {
            if (p >= val.prefix.maskLen) {
                cur.val = val;
                return;
            }
            if (val.prefix.network.bitValue(p)) {
                if (cur.one == null) {
                    cur.one = new tabGepV2nod<T>();
                }
                cur = cur.one;
            } else {
                if (cur.zero == null) {
                    cur.zero = new tabGepV2nod<T>();
                }
                cur = cur.zero;
            }
        }
    }

    /**
     * add one entry
     *
     * @param val value to add
     */
    public void del(tabRouteEntry<T> val) {
        tabGepV2nod<T> cur = root;
        for (int p = 0;; p++) {
            if (p >= val.prefix.maskLen) {
                cur.val = null;
                return;
            }
            if (val.prefix.network.bitValue(p)) {
                cur = cur.one;
            } else {
                cur = cur.zero;
            }
            if (cur == null) {
                return;
            }
        }
    }

    /**
     * find one container entry
     *
     * @param val value to look up
     * @return found value
     */
    public tabRouteEntry<T> search(tabRouteEntry<T> val) {
        tabGepV2nod<T> cur = root;
        tabRouteEntry<T> lst = null;
        for (int p = 0;; p++) {
            if (cur.val != null) {
                lst = cur.val;
            }
            if (p >= val.prefix.maskLen) {
                return lst;
            }
            if (val.prefix.network.bitValue(p)) {
                cur = cur.one;
            } else {
                cur = cur.zero;
            }
            if (cur == null) {
                return lst;
            }
        }
    }

}

class tabGepV2nod<T extends addrType> {

    public tabGepV2nod<T> zero;

    public tabGepV2nod<T> one;

    public tabRouteEntry<T> val;

    public tabGepV2nod() {
        val = null;
        zero = null;
        one = null;
    }

}
