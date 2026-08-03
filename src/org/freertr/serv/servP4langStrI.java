package org.freertr.serv;

import org.freertr.tab.tabGen;

/**
 * one p4lang storage extension
 *
 * @param <T> type to compare
 * @author matecsaba
 */
public class servP4langStrI<T extends Comparable<T>> implements Comparable<servP4langStrI<T>> {

    /**
     * the data
     */
    protected final T data;

    /**
     * first storage
     */
    protected int stor1;

    /**
     * second storage
     */
    protected int stor2;

    /**
     * create instance
     *
     * @param dat data to store
     */
    protected servP4langStrI(T dat) {
        data = dat;
    }

    public int compareTo(servP4langStrI<T> o) {
        return data.compareTo(o.data);
    }

    /**
     * check if storage changed
     *
     * @param l list to check
     * @return true if differs, false if identical
     */
    protected boolean differs(tabGen<servP4langStrI<T>> l) {
        servP4langStrI<T> o = l.find(this);
        if (o == null) {
            return true;
        }
        return (stor1 != o.stor1) || (stor2 != o.stor2);
    }

}
