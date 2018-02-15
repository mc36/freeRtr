package tab;

import java.util.ArrayList;
import java.util.Comparator;

/**
 * one sorted, synchronized list
 *
 * @param <T> type of elements in the list
 * @author matecsaba
 */
public class tabGenV1<T extends Comparator<? super T>> {

    private final ArrayList<T> lst;

    /**
     * create one generic table
     */
    public tabGenV1() {
        lst = new ArrayList<T>();
    }

    /**
     * clone one generic table
     *
     * @param old where from copy
     */
    public tabGenV1(tabGenV1<T> old) {
        lst = new ArrayList<T>();
        synchronized (old.lst) {
            for (int i = 0; i < old.lst.size(); i++) {
                lst.add(old.lst.get(i));
            }
        }
    }

    /**
     * optimize for lookup
     */
    public void optimize4lookup() {
    }

    /**
     * get table info
     *
     * @return info
     */
    public String getTableInfo() {
        synchronized (lst) {
            return "len=" + lst.size();
        }
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

}
