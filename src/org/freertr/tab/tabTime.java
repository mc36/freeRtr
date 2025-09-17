package org.freertr.tab;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import org.freertr.util.bits;

/**
 * represents one time
 *
 * @author matecsaba
 */
public class tabTime implements Comparable<tabTime> {

    /**
     * sequence number
     */
    public int seq;

    /**
     * action
     */
    public tabListingEntry.actionType act;

    /**
     * year to match
     */
    public tabIntMatcher year;

    /**
     * month to match
     */
    public tabIntMatcher month;

    /**
     * day to match
     */
    public tabIntMatcher day;

    /**
     * day of week to match
     */
    public tabIntMatcher dow;

    /**
     * week of month to match
     */
    public tabIntMatcher wom;

    /**
     * week of year to match
     */
    public tabIntMatcher woy;

    /**
     * day of month to match
     */
    public tabIntMatcher dom;

    /**
     * day of year to match
     */
    public tabIntMatcher doy;

    /**
     * hour to match
     */
    public tabIntMatcher hour;

    /**
     * minute to match
     */
    public tabIntMatcher min;

    /**
     * second to match
     */
    public tabIntMatcher sec;

    /**
     * millisecond to match
     */
    public tabIntMatcher mil;

    /**
     * period length
     */
    public int perL;

    /**
     * period begin
     */
    public int perB;

    /**
     * period end
     */
    public int perE;

    /**
     * create new
     */
    public tabTime() {
        year = new tabIntMatcher();
        month = new tabIntMatcher();
        day = new tabIntMatcher();
        dow = new tabIntMatcher();
        wom = new tabIntMatcher();
        woy = new tabIntMatcher();
        dom = new tabIntMatcher();
        doy = new tabIntMatcher();
        hour = new tabIntMatcher();
        min = new tabIntMatcher();
        sec = new tabIntMatcher();
        mil = new tabIntMatcher();
        perL = 0;
        perB = 0;
        perE = 0;
    }

    /**
     * set calendar from time
     *
     * @param zoNam name
     * @param tim time
     * @return true if matched
     */
    public static Calendar time2calendar(String zoNam, long tim) {
        Calendar cal = bits.getCalendar(zoNam);
        cal.setTime(new Date(tim));
        return cal;
    }

    /**
     * patch file name
     *
     * @param fn filename to patch
     * @param zoNam name
     * @param tim time
     * @return patched file name
     */
    public static String patchFileName(String fn, String zoNam, long tim) {
        if (fn == null) {
            return null;
        }
        Calendar cal = time2calendar(zoNam, tim);
        fn = fn.replaceAll("%year%", "" + cal.get(Calendar.YEAR));
        fn = fn.replaceAll("%month%", "" + (cal.get(Calendar.MONTH) + 1));
        fn = fn.replaceAll("%day%", "" + cal.get(Calendar.DAY_OF_MONTH));
        fn = fn.replaceAll("%hour%", "" + cal.get(Calendar.HOUR_OF_DAY));
        fn = fn.replaceAll("%minute%", "" + cal.get(Calendar.MINUTE));
        fn = fn.replaceAll("%second%", "" + cal.get(Calendar.SECOND));
        fn = fn.replaceAll("%millis%", "" + cal.get(Calendar.MILLISECOND));
        return fn;
    }

    /**
     * matches a date or not
     *
     * @param cal calendar
     * @param tim time
     * @return true if matched
     */
    public boolean matches(Calendar cal, long tim) {
        if (!year.matches(cal.get(Calendar.YEAR))) {
            return false;
        }
        if (!month.matches(cal.get(Calendar.MONTH) + 1)) {
            return false;
        }
        if (!day.matches(cal.get(Calendar.DAY_OF_MONTH))) {
            return false;
        }
        if (!dow.matches(cal.get(Calendar.DAY_OF_WEEK))) {
            return false;
        }
        if (!wom.matches(cal.get(Calendar.WEEK_OF_MONTH))) {
            return false;
        }
        if (!woy.matches(cal.get(Calendar.WEEK_OF_YEAR))) {
            return false;
        }
        if (!dom.matches(cal.get(Calendar.DAY_OF_MONTH))) {
            return false;
        }
        if (!doy.matches(cal.get(Calendar.DAY_OF_YEAR))) {
            return false;
        }
        if (!hour.matches(cal.get(Calendar.HOUR_OF_DAY))) {
            return false;
        }
        if (!min.matches(cal.get(Calendar.MINUTE))) {
            return false;
        }
        if (!sec.matches(cal.get(Calendar.SECOND))) {
            return false;
        }
        if (!mil.matches(cal.get(Calendar.MILLISECOND))) {
            return false;
        }
        if (perL < 1) {
            return true;
        }
        tim %= perL;
        return (tim > perB) && (tim < perE);
    }

    public int compareTo(tabTime o) {
        if (seq < o.seq) {
            return -1;
        }
        if (seq > o.seq) {
            return +1;
        }
        return 0;
    }

    /**
     * dump out matchers
     *
     * @param beg beginning
     * @return text text
     */
    public List<String> dump(String beg) {
        beg = beg + "sequence " + seq + beg;
        List<String> l = new ArrayList<String>();
        l.add(beg + "action " + tabListingEntry.action2string(act));
        l.add(beg + "match year " + year);
        l.add(beg + "match month " + month);
        l.add(beg + "match day " + day);
        l.add(beg + "match dow " + dow);
        l.add(beg + "match wom " + wom);
        l.add(beg + "match woy " + woy);
        l.add(beg + "match dom " + dom);
        l.add(beg + "match doy " + doy);
        l.add(beg + "match hour " + hour);
        l.add(beg + "match minute " + min);
        l.add(beg + "match second " + sec);
        l.add(beg + "match milli " + mil);
        l.add(beg + "match periodic " + perL + " " + perB + " " + perE);
        return l;
    }

    /**
     * generate calendar
     *
     * @param year year
     * @param month month
     * @return text
     */
    public static List<String> getCalendar(int year, int month) {
        Calendar cal = Calendar.getInstance();
        cal.set(year, month - 1, 1);
        List<String> res = new ArrayList<String>();
        int dow = cal.get(Calendar.DAY_OF_WEEK) - 1;
        int max = cal.getActualMaximum(Calendar.DAY_OF_MONTH);
        res.add("       " + year + " " + bits.padBeg("" + month, 2, " "));
        res.add(" su mo tu we th fr sa");
        String a = bits.padBeg("", dow * 3, " ");
        for (int i = 1; i <= max; i++) {
            a += bits.padBeg("" + i, 3, " ");
            dow++;
            if (dow < 7) {
                continue;
            }
            res.add(a);
            a = "";
            dow = 0;
        }
        if (a.length() < 1) {
            return res;
        }
        res.add(a);
        return res;
    }

    /**
     * generate clock
     *
     * @param maxX max x
     * @param maxY max y
     * @param sec seconds too
     * @return the text
     */
    public static List<String> getClock(int maxX, int maxY, boolean sec) {
        if (maxX > (maxY * 2)) {
            maxX = maxY * 2;
        }
        if (maxY > (maxX / 2)) {
            maxY = maxX / 2;
        }
        int hlfY = maxY / 2;
        List<String> res = new ArrayList<String>();
        for (int i = 0; i < maxY; i++) {
            res.add(bits.padEnd("", maxX, " "));
        }
        for (int y = 0; y < maxY; y++) {
            for (int x = 0; x < maxX; x++) {
                int a = (y - hlfY);
                int b = (x - maxY);
                a = (a * a) + ((b * b) / 4);
                int d = (int) Math.sqrt(a);
                if (hlfY != d) {
                    continue;
                }
                getClock(res, x, y, "*");
            }
        }
        Calendar cal = Calendar.getInstance();
        getClock(res, maxY, hlfY, (hlfY * 2) / 3, cal.get(Calendar.HOUR_OF_DAY) / 24.0, "@");
        getClock(res, maxY, hlfY, (hlfY * 5) / 6, cal.get(Calendar.MINUTE) / 60.0, "#");
        if (!sec) {
            return res;
        }
        getClock(res, maxY, hlfY, hlfY, cal.get(Calendar.SECOND) / 60.0, "%");
        return res;
    }

    private static void getClock(List<String> res, int bx, int by, int len, double val, String ch) {
        val *= Math.PI * 2.0;
        val += Math.PI / 2.0;
        val += Math.PI;
        int ex = (int) (len * Math.cos(val) * 2.0);
        int ey = (int) (len * Math.sin(val));
        for (int i = 0; i < bx; i++) {
            int x = (ex * i) / bx;
            int y = (ey * i) / bx;
            getClock(res, bx + x, by + y, ch);
        }
    }

    private static void getClock(List<String> res, int x, int y, String ch) {
        String a = res.get(y);
        a = a.substring(0, x) + ch + a.substring(x + 1, a.length());
        res.set(y, a);
    }

}
