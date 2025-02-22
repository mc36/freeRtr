package org.freertr.rtr;

import java.util.List;
import org.freertr.cfg.cfgAll;
import org.freertr.clnt.clntWhois;
import org.freertr.util.bits;

/**
 * bgp4 flap list
 *
 * @author matecsaba
 */
public class rtrBgpFlapLst implements Comparable<rtrBgpFlapLst> {

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

    public int compareTo(rtrBgpFlapLst o) {
        int s1 = lst.size();
        int s2 = o.lst.size();
        if (s1 < s2) {
            return -1;
        }
        if (s1 > s2) {
            return +1;
        }
        for (int i = 0; i < s1; i++) {
            int v1 = lst.get(i);
            int v2 = o.lst.get(i);
            if (v1 < v2) {
                return -1;
            }
            if (v1 > v2) {
                return +1;
            }
        }
        return 0;
    }

    private String dumpPathRev() {
        String p = "";
        for (int i = 0; i < lst.size(); i++) {
            p = clntWhois.asn2mixed(lst.get(i), true) + " " + p;
        }
        return p;
    }

    private String dumpPathFwd() {
        String p = "";
        for (int i = 0; i < lst.size(); i++) {
            p += " " + clntWhois.asn2mixed(lst.get(i), true);
        }
        return p;
    }

    /**
     * dump entry
     *
     * @param rev reverse path
     * @return dumped
     */
    public String dumpFlap(boolean rev) {
        String p;
        if (rev) {
            p = dumpPathRev();
        } else {
            p = dumpPathFwd();
        }
        return count + "|" + bits.timePast(last) + "|" + bits.time2str(cfgAll.timeZoneName, last + cfgAll.timeServerOffset, 3) + "|" + p.trim();
    }

    /**
     * dump entry
     *
     * @return dumped
     */
    public String dumpContain() {
        return count + "|" + dumpPathFwd();
    }

}
