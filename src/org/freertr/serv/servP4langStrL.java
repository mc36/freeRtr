package org.freertr.serv;

import java.util.Comparator;
import org.freertr.tab.tabGen;

/**
 * one p4lang storage extension
 *
 * @param <T> type to compare
 * @param <U> type to list
 * @author matecsaba
 */
public class servP4langStrL<T extends Comparator<T>, U extends Comparator<? super U>> implements Comparator<servP4langStrL<T, U>> {

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

    public int compare(servP4langStrL<T, U> o1, servP4langStrL<T, U> o2) {
        return o1.data.compare(o1.data, o2.data);
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
            if (d1.compare(d1, d2) != 0) {
                return true;
            }
        }
        return false;
    }

}
