package net.freertr.rtr;

import java.util.Comparator;
import java.util.List;
import net.freertr.cfg.cfgAll;
import net.freertr.util.bits;

/**
 * bgp4 flap list
 *
 * @author matecsaba
 */
public class rtrBgpFlapLst implements Comparator<rtrBgpFlapLst> {

    /**
     * list of numbers
     */
    public final List<Integer> lst;

    /**
     * counter
     */
    public int count;

    /**
     * last
     */
    public long last;

    /**
     * create instance
     *
     * @param l list of numbers
     */
    public rtrBgpFlapLst(List<Integer> l) {
        lst = l;
    }

    public int compare(rtrBgpFlapLst o1, rtrBgpFlapLst o2) {
        int s1 = o1.lst.size();
        int s2 = o2.lst.size();
        if (s1 < s2) {
            return -1;
        }
        if (s1 > s2) {
            return +1;
        }
        for (int i = 0; i < s1; i++) {
            int v1 = o1.lst.get(i);
            int v2 = o2.lst.get(i);
            if (v1 < v2) {
                return -1;
            }
            if (v1 > v2) {
                return +1;
            }
        }
        return 0;
    }

    /**
     * dump entry
     *
     * @param rev reverse path
     * @return dumped
     */
    public String dump(boolean rev) {
        String p;
        if (rev) {
            p = "";
            for (int i = 0; i < lst.size(); i++) {
                p = lst.get(i) + " " + p;
            }
        } else {
            p = "";
            for (int i = 0; i < lst.size(); i++) {
                p += " " + lst.get(i);
            }
        }
        return count + "|" + bits.timePast(last) + "|" + bits.time2str(cfgAll.timeZoneName, last + cfgAll.timeServerOffset, 3) + "|" + p.trim();
    }

}
