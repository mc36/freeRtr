package net.freertr.serv;

import java.util.Comparator;

/**
 * one p4lang magic by name
 *
 * @author matecsaba
 */
public class servP4langMgcN implements Comparator<servP4langMgcN> {

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

    public int compare(servP4langMgcN o1, servP4langMgcN o2) {
        return o1.nam.compareTo(o2.nam);
    }

}
