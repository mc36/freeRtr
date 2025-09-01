package org.freertr.tab;

import org.freertr.addr.addrType;

/**
 * one cached prefix tree
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
        cache(root);
    }

    /**
     * add one entry
     *
     * @param val value to add
     */
    public void add(tabRouteEntry<T> val) {
        tabGepV2nod<T> cur = root;
        tabGepV2nod<T> bas = root;
        for (int p = 0;; p++) {
            if (p >= val.prefix.maskLen) {
                cur.val = val;
                cache(bas);
                return;
            }
            if ((p & 7) == 0) {
                cache(bas);
                bas = cur;
            }
            if (cur.zero == null) {
                cur.zero = new tabGepV2nod<T>();
            }
            if (cur.one == null) {
                cur.one = new tabGepV2nod<T>();
            }
            if (val.prefix.network.bitValue(p)) {
                cur = cur.one;
            } else {
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
        tabGepV2nod<T> bas = root;
        for (int p = 0;; p++) {
            if (p >= val.prefix.maskLen) {
                cur.val = null;
                cache(bas);
                return;
            }
            if ((p & 7) == 0) {
                bas = cur;
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

    @SuppressWarnings({"unchecked", "rawtypes"})
    private void cache(tabGepV2nod<T> bas) {
        tabGepV2nod<T> res[] = new tabGepV2nod[256];
        cache(res, bas, null, 0, res.length);
        bas.cache = res;
    }

    private void cache(tabGepV2nod<T> res[], tabGepV2nod<T> cur, tabRouteEntry<T> lst, int beg, int end) {
        if (cur == null) {
            return;
        }
        if (cur.val != null) {
            lst = cur.val;
        }
        int mid = end - beg;
        if (mid < 256) {
            cur.res = lst;
        }
        for (int i = beg; i < end; i++) {
            res[i] = cur;
        }
        if (mid <= 1) {
            return;
        }
        mid >>>= 1;
        mid += beg;
        cache(res, cur.zero, lst, beg, mid);
        cache(res, cur.one, lst, mid, end);
    }

    private tabRouteEntry<T> search_uncached(tabRouteEntry<T> val) {
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

    /**
     * find one container entry
     *
     * @param val value to look up
     * @return found value
     */
    public tabRouteEntry<T> search(tabRouteEntry<T> val) {
        tabGepV2nod<T> cur = root;
        tabRouteEntry<T> lst = null;
        for (int p = 0;; p += 8) {
            if (cur.res != null) {
                lst = cur.res;
            }
            if (cur.cache == null) {
                return lst;
            }
            if (p >= val.prefix.maskLen) {
                return lst;
            }
            int i = val.prefix.network.getBytes()[p >>> 3] & 0xff;
            cur = cur.cache[i];
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

    public tabRouteEntry<T> res;

    public tabGepV2nod<T> cache[];

    public tabGepV2nod() {
        val = null;
        zero = null;
        one = null;
    }

}
