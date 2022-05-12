package net.freertr.serv;

import java.util.Comparator;
import net.freertr.tab.tabGen;

/**
 * one p4lang storage extension
 *
 * @author matecsaba
 */
public class servP4langStr<T extends Comparator<T>> implements Comparator<servP4langStr<T>> {

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
    protected servP4langStr(T dat) {
        data = dat;
    }

    public int compare(servP4langStr<T> o1, servP4langStr<T> o2) {
        return o1.data.compare(o1.data, o2.data);
    }

    /**
     * check if storage changed
     *
     * @param l list to check
     * @return true if differs, false if identical
     */
    protected boolean differs(tabGen<servP4langStr<T>> l) {
        servP4langStr<T> o = l.find(this);
        if (o == null) {
            return true;
        }
        return (stor1 != o.stor1) || (stor2 != o.stor2);
    }

}
