package org.freertr.util;

import org.freertr.cfg.cfgAll;
import org.freertr.tab.tabGen;
import org.freertr.user.userFormat;

/**
 * one keyword statistic
 *
 * @author matecsaba
 */
public class keyword implements Comparable<keyword> {

    /**
     * message
     */
    public final String msg;

    /**
     * hit count
     */
    public int cnt;

    /**
     * last time
     */
    public long lst;

    /**
     * create instance
     *
     * @param m message
     */
    public keyword(String m) {
        msg = m;
    }

    public int compareTo(keyword o) {
        return msg.compareTo(o.msg);
    }

    public String toString() {
        return msg;
    }

    /**
     * dump statistics
     *
     * @param tab table to dump
     * @return dump
     */
    public final static userFormat dump(tabGen<keyword> tab) {
        if (tab == null) {
            return null;
        }
        userFormat res = new userFormat("|", "message|count|last|ago");
        for (int i = 0; i < tab.size(); i++) {
            keyword k = tab.get(i);
            if (k == null) {
                continue;
            }
            res.add(k.msg + "|" + k.cnt + "|" + bits.time2str(cfgAll.timeZoneName, k.lst + cfgAll.timeServerOffset, 3) + "|" + bits.timePast(k.lst));
        }
        return res;
    }

    /**
     * update api statistics
     *
     * @param l table to dump
     * @param a message
     */
    public final static void update(tabGen<keyword> l, String a) {
        int i = a.indexOf(" ");
        if (i > 0) {
            a = a.substring(0, i);
        }
        keyword m = new keyword(a);
        keyword o = l.add(m);
        if (o != null) {
            m = o;
        }
        m.cnt++;
        m.lst = bits.getTime();
    }

}
