package net.freertr.serv;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.tab.tabGen;
import net.freertr.user.userFormat;
import net.freertr.util.bits;

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

    /**
     * convert to number
     *
     * @param src source list
     * @param num number or name to find
     * @param inv invalid value
     * @return id, inv if error
     */
    public static int toNum(tabGen<servP4langMgcN> src, String num, int inv) {
        int i = bits.str2num(num);
        if (num.equals("" + i)) {
            return i;
        }
        servP4langMgcN ntry = new servP4langMgcN(inv, num);
        ntry = src.find(ntry);
        if (ntry == null) {
            return inv;
        }
        return ntry.id;
    }

    /**
     * convert to help
     *
     * @param src source list
     * @return converted text
     */
    public static List<String> toHelp(tabGen<servP4langMgcN> src) {
        List<String> res = new ArrayList<String>();
        for (int i = 0; i < src.size(); i++) {
            res.add(src.get(i).nam);
        }
        return res;
    }

    /**
     * convert to show
     *
     * @param beg beginning
     * @param src source list
     * @param trg target list
     */
    public static void toShow(String beg, tabGen<servP4langMgcN> src, userFormat trg) {
        for (int i = 0; i < src.size(); i++) {
            servP4langMgcN ntry = src.get(i);
            trg.add(beg + ntry.id + "|" + ntry.nam);
        }
    }

}
