package net.freertr.tab;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import net.freertr.util.bits;

/**
 * represents one time
 *
 * @author matecsaba
 */
public class tabTime implements Comparator<tabTime> {

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

    public int compare(tabTime o1, tabTime o2) {
        if (o1.seq < o2.seq) {
            return -1;
        }
        if (o1.seq > o2.seq) {
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

}
