package net.freertr.serv;

import java.util.Comparator;

/**
 * one p4lang frontpanel port
 *
 * @author matecsaba
 */
public class servP4langFrnt implements Comparator<servP4langFrnt> {

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
    protected servP4langFrnt(int i, String n) {
        id = i;
        nam = n;
    }

    public int compare(servP4langFrnt o1, servP4langFrnt o2) {
        return o1.nam.compareTo(o2.nam);
    }

}
