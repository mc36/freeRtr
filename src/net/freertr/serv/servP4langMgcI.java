package net.freertr.serv;

import java.util.Comparator;
import net.freertr.tab.tabGen;

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

    /**
     * convert table
     *
     * @param src source
     * @param ned needed
     * @return converted
     */
    public static tabGen<servP4langMgcI> convTab(tabGen<servP4langMgcN> src, boolean ned) {
        tabGen<servP4langMgcI> res = new tabGen<servP4langMgcI>();
        if (!ned) {
            return res;
        }
        for (int i = 0; i < src.size(); i++) {
            servP4langMgcN ntry = src.get(i);
            res.add(new servP4langMgcI(ntry.id, ntry.nam));
        }
        return res;
    }

    /**
     * convert id
     *
     * @param id identifier
     * @param src source list
     * @return converted string
     */
    public static String convId(int id, tabGen<servP4langMgcI> src) {
        servP4langMgcI ntry = new servP4langMgcI(id, null);
        ntry = src.find(ntry);
        if (ntry == null) {
            return "" + id;
        }
        return ntry.nam;
    }

}
