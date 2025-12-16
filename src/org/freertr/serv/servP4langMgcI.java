package org.freertr.serv;

/**
 * one p4lang magic by id
 *
 * @author matecsaba
 */
public class servP4langMgcI implements Comparable<servP4langMgcI> {

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

    public int compareTo(servP4langMgcI o) {
        if (id < o.id) {
            return -1;
        }
        if (id > o.id) {
            return +1;
        }
        return 0;
    }

}
