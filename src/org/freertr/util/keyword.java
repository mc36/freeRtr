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
     * @param a message
     */
    public keyword(String a) {
        int i = a.indexOf(" ");
        if (i > 0) {
            a = a.substring(0, i);
        }
        msg = a;
    }

    public int compareTo(keyword o) {
        return msg.compareTo(o.msg);
    }

    public String toString() {
        return msg;
    }

    /**
     * update statistics
     *
     * @param l list to update
     * @param a message
     */
    public final static void update(tabGen<keyword> l, String a) {
        if (l == null) {
            return;
        }
        keyword m = new keyword(a);
        keyword o = l.add(m);
        if (o != null) {
            m = o;
        }
        m.cnt++;
        m.lst = bits.getTime();
    }

    /**
     * dump statistics
     *
     * @param l list to dump
     * @return dump
     */
    public final static userFormat dump(tabGen<keyword> l) {
        if (l == null) {
            return null;
        }
        userFormat r = new userFormat("|", "message|count|last|ago");
        for (int i = 0; i < l.size(); i++) {
            keyword k = l.get(i);
            if (k == null) {
                continue;
            }
            r.add(k.msg + "|" + k.cnt + "|" + bits.time2str(cfgAll.timeZoneName, k.lst + cfgAll.timeServerOffset, 3) + "|" + bits.timePast(k.lst));
        }
        return r;
    }

    /**
     * dump statistics
     *
     * @param r rx list to dump
     * @param t tx list to dump
     * @return dump
     */
    public final static userFormat dump(tabGen<keyword> r, tabGen<keyword> t) {
        if (r == null) {
            return null;
        }
        if (t == null) {
            return null;
        }
        userFormat l = new userFormat("|", "message|rx|tx|rx|tx|rx|tx", "1|2counts|2last|2ago");
        for (int i = 0; i < r.size(); i++) {
            keyword k = r.get(i);
            if (k == null) {
                continue;
            }
            if (t.find(k) != null) {
                continue;
            }
            l.add(k.msg + "|" + k.cnt + "|0|" + bits.time2str(cfgAll.timeZoneName, k.lst + cfgAll.timeServerOffset, 3) + "|-|" + bits.timePast(k.lst) + "|-");
        }
        for (int i = 0; i < t.size(); i++) {
            keyword k = t.get(i);
            if (k == null) {
                continue;
            }
            if (r.find(k) != null) {
                continue;
            }
            l.add(k.msg + "|0|" + k.cnt + "|-|" + bits.time2str(cfgAll.timeZoneName, k.lst + cfgAll.timeServerOffset, 3) + "|-|" + bits.timePast(k.lst));
        }
        for (int i = 0; i < t.size(); i++) {
            keyword k = t.get(i);
            if (k == null) {
                continue;
            }
            keyword p = r.find(k);
            if (p == null) {
                continue;
            }
            l.add(k.msg + "|" + p.cnt + "|" + k.cnt + "|" + bits.time2str(cfgAll.timeZoneName, p.lst + cfgAll.timeServerOffset, 3) + "|" + bits.time2str(cfgAll.timeZoneName, k.lst + cfgAll.timeServerOffset, 3) + "|" + bits.timePast(p.lst) + "|" + bits.timePast(k.lst));
        }
        return l;
    }

}
