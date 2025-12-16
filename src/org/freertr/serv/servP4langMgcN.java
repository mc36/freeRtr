package org.freertr.serv;

/**
 * one p4lang magic by name
 *
 * @author matecsaba
 */
public class servP4langMgcN implements Comparable<servP4langMgcN> {

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
    protected servP4langMgcN(int i, String n) {
        id = i;
        nam = n;
    }

    public int compareTo(servP4langMgcN o) {
        return nam.compareTo(o.nam);
    }

}
