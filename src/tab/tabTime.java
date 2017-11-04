package tab;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Comparator;
import java.util.Date;
import java.util.List;
import util.bits;

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
    public tabPlcmapN.actionType act;

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
     * create new
     */
    public tabTime() {
        year = new tabIntMatcher();
        month = new tabIntMatcher();
        day = new tabIntMatcher();
        dow = new tabIntMatcher();
        hour = new tabIntMatcher();
        min = new tabIntMatcher();
        sec = new tabIntMatcher();
    }

    /**
     * matches a date or not
     *
     * @param zoNam name
     * @param tim time
     * @return true if matched
     */
    public boolean matches(String zoNam, long tim) {
        Calendar cal = bits.getCalendar(zoNam);
        cal.setTime(new Date(tim));
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
        if (!hour.matches(cal.get(Calendar.HOUR_OF_DAY))) {
            return false;
        }
        if (!min.matches(cal.get(Calendar.MINUTE))) {
            return false;
        }
        if (!sec.matches(cal.get(Calendar.SECOND))) {
            return false;
        }
        return true;
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
        l.add(beg + "match hour " + hour);
        l.add(beg + "match minute " + min);
        l.add(beg + "match second " + sec);
        return l;
    }

}
