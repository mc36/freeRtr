package tab;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import user.userFormat;

/**
 * one sorted, synchronized list
 *
 * @param <T> type of elements in the list
 * @author matecsaba
 */
public class tabGen<T extends Comparator<? super T>> {

    private final List<T> lst;

    /**
     * create one generic table
     */
    public tabGen() {
        lst = new ArrayList<T>();
    }

    /**
     * delete all values
     */
    public void clear() {
        synchronized (lst) {
            lst.clear();
        }
    }

    /**
     * get size of list
     *
     * @return size of list
     */
    public int size() {
        synchronized (lst) {
            return lst.size();
        }
    }

    /**
     * sort values
     */
    public void reSort() {
        synchronized (lst) {
            if (lst.size() < 1) {
                return;
            }
            Collections.sort(lst, lst.get(0));
        }
    }

    /**
     * get value
     *
     * @param idx sequence number
     * @return read value, null if not found
     */
    public T get(int idx) {
        synchronized (lst) {
            if (idx < 0) {
                return null;
            }
            if (idx >= lst.size()) {
                return null;
            }
            return lst.get(idx);
        }
    }

    private int doFind(T val) {
        int lower = 0;
        int upper = lst.size() - 1;
        while (lower <= upper) {
            int mid = (lower + upper) >>> 1;
            int cmp = val.compare(lst.get(mid), val);
            if (cmp < 0) {
                lower = mid + 1;
                continue;
            }
            if (cmp > 0) {
                upper = mid - 1;
                continue;
            }
            return mid;
        }
        return -lower - 1;
    }

    /**
     * find one value
     *
     * @param val value to find
     * @return value in list, null if not found
     */
    public T find(T val) {
        synchronized (lst) {
            int i = doFind(val);
            if (i < 0) {
                return null;
            }
            return lst.get(i);
        }
    }

    /**
     * return index of value
     *
     * @param val value to find
     * @return index in list, negated insertion point if not
     */
    public int indexOf(T val) {
        return doFind(val);
    }

    /**
     * delete value
     *
     * @param val value to delete
     * @return value deleted, null if not found
     */
    public T del(T val) {
        synchronized (lst) {
            int i = doFind(val);
            if (i < 0) {
                return null;
            }
            return lst.remove(i);
        }
    }

    /**
     * delete index
     *
     * @param idx sequence number
     * @return value deleted, null if not found
     */
    public T del(int idx) {
        synchronized (lst) {
            if (idx < 0) {
                return null;
            }
            if (idx >= lst.size()) {
                return null;
            }
            return lst.remove(idx);
        }
    }

    /**
     * add one value, do not update if already
     *
     * @param val value to add
     * @return null on addition, old value if already exists
     */
    public T add(T val) {
        synchronized (lst) {
            int i = doFind(val);
            if (i >= 0) {
                return lst.get(i);
            }
            lst.add(-i - 1, val);
            return null;
        }
    }

    /**
     * update one value, do not add if not in list
     *
     * @param val value to update
     * @return old value, null if in list
     */
    public T set(T val) {
        synchronized (lst) {
            int i = doFind(val);
            if (i < 0) {
                return null;
            }
            T old = lst.get(i);
            lst.set(i, val);
            return old;
        }
    }

    /**
     * put to list, update if already
     *
     * @param val value to put
     * @return old value if updated, null if added
     */
    public T put(T val) {
        synchronized (lst) {
            int i = doFind(val);
            if (i < 0) {
                lst.add(-i - 1, val);
                return null;
            }
            T old = lst.get(i);
            lst.set(i, val);
            return old;
        }
    }

    /**
     * dump list
     *
     * @param l list to update
     */
    public void dump(userFormat l) {
        for (int i = 0; i < size(); i++) {
            l.add("" + get(i));
        }
    }

    /**
     * get range of entries, including boundaries
     *
     * @param first first entry, null if none
     * @param last last entry, null if none
     * @return list of entries
     */
    public tabGen<T> range(T first, T last) {
        tabGen<T> l = new tabGen<T>();
        synchronized (lst) {
            for (int i = 0; i < lst.size(); i++) {
                T ntry = lst.get(i);
                if (first != null) {
                    if (ntry.compare(ntry, first) < 0) {
                        continue;
                    }
                }
                if (last != null) {
                    if (ntry.compare(ntry, last) > 0) {
                        continue;
                    }
                }
                l.add(ntry);
            }
        }
        return l;
    }

    /**
     * get range of entries, including boundaries
     *
     * @param first first entry
     * @param last last entry
     * @return list of entries
     */
    public tabGen<T> range(int first, int last) {
        tabGen<T> l = new tabGen<T>();
        synchronized (lst) {
            if (last >= lst.size()) {
                last = lst.size() - 1;
            }
            for (int i = first; i <= last; i++) {
                T ntry = lst.get(i);
                l.add(ntry);
            }
        }
        return l;
    }

    /**
     * convert to list
     *
     * @return list
     */
    public List<T> toList() {
        List<T> res = new ArrayList<T>();
        synchronized (lst) {
            for (int i = 0; i < lst.size(); i++) {
                res.add(lst.get(i));
            }
        }
        return res;
    }

    /**
     * collect entries from list
     *
     * @param old list of entries to find
     * @return list of found entries
     */
    public tabGen<T> collect(tabGen<T> old) {
        tabGen<T> l = new tabGen<T>();
        for (int i = 0; i < old.size(); i++) {
            T ntry = old.get(i);
            if (ntry == null) {
                continue;
            }
            ntry = find(ntry);
            if (ntry == null) {
                continue;
            }
            l.add(ntry);
        }
        return l;
    }

}
