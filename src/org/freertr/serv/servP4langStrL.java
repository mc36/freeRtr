package org.freertr.serv;

import org.freertr.tab.tabGen;

/**
 * one p4lang storage extension
 *
 * @param <T> type to compare
 * @param <U> type to list
 * @author matecsaba
 */
public class servP4langStrL<T extends Comparable<T>, U extends Comparable<? super U>> implements Comparable<servP4langStrL<T, U>> {

    /**
     * the data
     */
    protected final T data;

    /**
     * the list
     */
    protected tabGen<U> list;

    /**
     * create instance
     *
     * @param dat data to store
     */
    protected servP4langStrL(T dat) {
        data = dat;
        list = new tabGen<U>();
    }

    public int compareTo(servP4langStrL<T, U> o) {
        return data.compareTo(o.data);
    }

    /**
     * check if storage changed
     *
     * @param o other to check
     * @return true if differs, false if identical
     */
    protected boolean differs(servP4langStrL<T, U> o) {
        if (o == null) {
            return true;
        }
        if (list.size() != o.list.size()) {
            return true;
        }
        for (int i = 0; i < list.size(); i++) {
            U d1 = list.get(i);
            U d2 = o.list.get(i);
            if (d1.compareTo(d2) != 0) {
                return true;
            }
        }
        return false;
    }

}
