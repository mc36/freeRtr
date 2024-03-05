package org.freertr.serv;

import java.util.Comparator;

/**
 * one p4lang magic by id
 *
 * @author matecsaba
 */
public class servP4langMgcI implements Comparator<servP4langMgcI> {

    /**
     * interface id
     */
    protected final int id;

    /**
     * name provided
     */
    protected final String nam;

    /**
     * create instance
     *
     * @param i id
     * @param n name
     */
    protected servP4langMgcI(int i, String n) {
        id = i;
        nam = n;
    }

    public int compare(servP4langMgcI o1, servP4langMgcI o2) {
        if (o1.id < o2.id) {
            return -1;
        }
        if (o1.id > o2.id) {
            return +1;
        }
        return 0;
    }

}
