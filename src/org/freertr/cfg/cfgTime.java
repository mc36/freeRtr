package org.freertr.cfg;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIntMatcher;
import org.freertr.tab.tabListingEntry;
import org.freertr.tab.tabTime;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * route map configuration
 *
 * @author matecsaba
 */
public class cfgTime implements Comparable<cfgTime>, cfgGeneric {

    /**
     * name of routemap
     */
    public String name;

    /**
     * description of access list
     */
    public String description;

    /**
     * time zone of access list
     */
    public String timeZone;

    /**
     * random offset of access list
     */
    public int randomMin;

    /**
     * random offset of access list
     */
    public int randomMax;

    /**
     * random offset of access list
     */
    public int randomCur;

    /**
     * current sequence number
     */
    public int seq;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("time-map .*", cmds.tabulator + cmds.negated + cmds.tabulator + "description", null),
        new userFilter("time-map .*", cmds.tabulator + cmds.negated + cmds.tabulator + "time-zone", null),
        new userFilter("time-map .*", cmds.tabulator + "random 0 0", null),
        new userFilter("time-map .*", cmds.tabulator + "sequence .* match year all", null),
        new userFilter("time-map .*", cmds.tabulator + "sequence .* match month all", null),
        new userFilter("time-map .*", cmds.tabulator + "sequence .* match day all", null),
        new userFilter("time-map .*", cmds.tabulator + "sequence .* match dow all", null),
        new userFilter("time-map .*", cmds.tabulator + "sequence .* match wom all", null),
        new userFilter("time-map .*", cmds.tabulator + "sequence .* match woy all", null),
        new userFilter("time-map .*", cmds.tabulator + "sequence .* match dom all", null),
        new userFilter("time-map .*", cmds.tabulator + "sequence .* match doy all", null),
        new userFilter("time-map .*", cmds.tabulator + "sequence .* match hour all", null),
        new userFilter("time-map .*", cmds.tabulator + "sequence .* match minute all", null),
        new userFilter("time-map .*", cmds.tabulator + "sequence .* match second all", null),
        new userFilter("time-map .*", cmds.tabulator + "sequence .* match milli all", null),
        new userFilter("time-map .*", cmds.tabulator + "sequence .* match periodic 0 0 0", null)
    };

    /**
     * time list
     */
    public tabGen<tabTime> timemap;

    /**
     * create new route map
     *
     * @param s name
     */
    public cfgTime(String s) {
        timemap = new tabGen<tabTime>();
        seq = nextseq();
        name = s;
    }

    /**
     * get next sequence
     *
     * @return seq number
     */
    public int nextseq() {
        if (timemap.size() < 1) {
            return 10;
        }
        return timemap.get(timemap.size() - 1).seq + 10;
    }

    /**
     * get current entry
     *
     * @return current entry
     */
    public synchronized tabTime getCurr() {
        tabTime ntry = new tabTime();
        ntry.seq = seq;
        ntry = timemap.find(ntry);
        if (ntry != null) {
            return ntry;
        }
        ntry = new tabTime();
        ntry.seq = seq;
        ntry.act = tabListingEntry.actionType.actPermit;
        timemap.add(ntry);
        return ntry;
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("time-map " + name);
        cmds.cfgLine(l, description == null, cmds.tabulator, "description", "" + description);
        cmds.cfgLine(l, timeZone == null, cmds.tabulator, "time-zone", "" + timeZone);
        l.add(cmds.tabulator + "random " + randomMin + " " + randomMax);
        for (int i = 0; i < timemap.size(); i++) {
            l.addAll(timemap.get(i).dump(cmds.tabulator));
        }
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "sequence", "sequence number of an entry");
        l.add(null, false, 2, new int[]{1, -1}, "<num>", "sequence number");
        l.add(null, false, 1, new int[]{3, -1}, "description", "specify description");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "text");
        l.add(null, false, 1, new int[]{2}, "rename", "rename this time map");
        l.add(null, false, 2, new int[]{-1}, "<str>", "set new name");
        l.add(null, false, 1, new int[]{2, -1}, "reindex", "reindex route map");
        l.add(null, false, 2, new int[]{3, -1}, "[num]", "initial number to start with");
        l.add(null, false, 3, new int[]{-1}, "[num]", "increment number");
        l.add(null, false, 1, new int[]{2}, "action", "set action to do");
        l.add(null, false, 2, new int[]{-1}, "deny", "specify to forbid");
        l.add(null, false, 2, new int[]{-1}, "permit", "specify to allow");
        l.add(null, false, 1, new int[]{3}, "time-zone", "specify time zone");
        l.add(null, false, 3, new int[]{-1}, "<str>", "text");
        l.add(null, false, 1, new int[]{3}, "random", "specify random offset in ms");
        l.add(null, false, 3, new int[]{4}, "<num>", "minimum");
        l.add(null, false, 4, new int[]{-1}, "<num>", "maximum");
        l.add(null, false, 1, new int[]{2}, "match", "match values");
        l.add(null, false, 2, new int[]{3}, "year", "match year");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "month", "match month");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "day", "match day");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "dow", "match day of week");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "wom", "match week of month");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "woy", "match week of year");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "dom", "match day of month");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "doy", "match day of year");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "hour", "match hour");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "minute", "match minute");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "second", "match second");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "milli", "match millisecond");
        l.add(null, false, 3, new int[]{-1}, "<num>", "value");
        l.add(null, false, 3, new int[]{-1}, "all", "any value");
        l.add(null, false, 2, new int[]{3}, "periodic", "match periodically in ms");
        l.add(null, false, 3, new int[]{4}, "<num>", "length");
        l.add(null, false, 4, new int[]{5}, "<num>", "begin");
        l.add(null, false, 5, new int[]{-1}, "<num>", "end");
    }

    public synchronized void doCfgStr(cmds cmd) {
        String a = cmd.word();
        if (a.equals("description")) {
            description = cmd.getRemaining();
            return;
        }
        if (a.equals("time-zone")) {
            timeZone = cmd.word();
            return;
        }
        if (a.equals("random")) {
            randomMin = bits.str2num(cmd.word());
            randomMax = bits.str2num(cmd.word());
            randomCur = bits.random(randomMin, randomMax);
            return;
        }
        if (a.equals("rename")) {
            a = cmd.word();
            cfgTime v = cfgAll.timeFind(a, false);
            if (v != null) {
                cmd.error("already exists");
                return;
            }
            name = a;
            return;
        }
        if (a.equals("sequence")) {
            seq = bits.str2num(cmd.word());
            a = cmd.word();
            if (a.length() < 1) {
                return;
            }
        }
        if (a.equals("action")) {
            tabTime ntry = getCurr();
            ntry.act = tabListingEntry.string2action(cmd.word());
            return;
        }
        if (a.equals("match")) {
            a = cmd.word();
            tabTime ntry = getCurr();
            if (a.equals("year")) {
                ntry.year.fromString(cmd.getRemaining());
                return;
            }
            if (a.equals("month")) {
                ntry.month.fromString(cmd.getRemaining());
                return;
            }
            if (a.equals("day")) {
                ntry.day.fromString(cmd.getRemaining());
                return;
            }
            if (a.equals("dow")) {
                ntry.dow.fromString(cmd.getRemaining());
                return;
            }
            if (a.equals("wom")) {
                ntry.wom.fromString(cmd.getRemaining());
                return;
            }
            if (a.equals("woy")) {
                ntry.woy.fromString(cmd.getRemaining());
                return;
            }
            if (a.equals("dom")) {
                ntry.dom.fromString(cmd.getRemaining());
                return;
            }
            if (a.equals("doy")) {
                ntry.doy.fromString(cmd.getRemaining());
                return;
            }
            if (a.equals("hour")) {
                ntry.hour.fromString(cmd.getRemaining());
                return;
            }
            if (a.equals("minute")) {
                ntry.min.fromString(cmd.getRemaining());
                return;
            }
            if (a.equals("second")) {
                ntry.sec.fromString(cmd.getRemaining());
                return;
            }
            if (a.equals("milli")) {
                ntry.mil.fromString(cmd.getRemaining());
                return;
            }
            if (a.equals("periodic")) {
                ntry.perL = bits.str2num(cmd.word());
                ntry.perB = bits.str2num(cmd.word());
                ntry.perE = bits.str2num(cmd.word());
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("reindex")) {
            int o = bits.str2num(cmd.word());
            int p = bits.str2num(cmd.word());
            if (o < 1) {
                o = 10;
            }
            if (p < 1) {
                p = 10;
            }
            for (int i = 0; i < timemap.size(); i++) {
                tabTime t = timemap.get(i);
                t.seq = (p * i) + o;
            }
            return;
        }
        if (!a.equals(cmds.negated)) {
            cmd.badCmd();
            return;
        }
        a = cmd.word();
        if (a.equals("description")) {
            description = null;
            return;
        }
        if (a.equals("time-zone")) {
            timeZone = null;
            return;
        }
        if (a.equals("random")) {
            randomMin = 0;
            randomMax = 0;
            randomCur = 0;
            return;
        }
        if (a.equals("match")) {
            a = cmd.word();
            tabTime ntry = getCurr();
            if (a.equals("year")) {
                ntry.year = new tabIntMatcher();
                return;
            }
            if (a.equals("month")) {
                ntry.month = new tabIntMatcher();
                return;
            }
            if (a.equals("day")) {
                ntry.day = new tabIntMatcher();
                return;
            }
            if (a.equals("dow")) {
                ntry.dow = new tabIntMatcher();
                return;
            }
            if (a.equals("wom")) {
                ntry.wom = new tabIntMatcher();
                return;
            }
            if (a.equals("woy")) {
                ntry.woy = new tabIntMatcher();
                return;
            }
            if (a.equals("dom")) {
                ntry.dom = new tabIntMatcher();
                return;
            }
            if (a.equals("doy")) {
                ntry.doy = new tabIntMatcher();
                return;
            }
            if (a.equals("hour")) {
                ntry.hour = new tabIntMatcher();
                return;
            }
            if (a.equals("minute")) {
                ntry.min = new tabIntMatcher();
                return;
            }
            if (a.equals("second")) {
                ntry.sec = new tabIntMatcher();
                return;
            }
            if (a.equals("milli")) {
                ntry.mil = new tabIntMatcher();
                return;
            }
            if (a.equals("periodic")) {
                ntry.perL = 0;
                ntry.perB = 0;
                ntry.perE = 0;
                return;
            }
            cmd.badCmd();
            return;
        }
        if (a.equals("sequence")) {
            tabTime ntry = new tabTime();
            ntry.seq = bits.str2num(cmd.word());
            if (timemap.del(ntry) == null) {
                cmd.error("invalid sequence");
                return;
            }
            return;
        }
        cmd.badCmd();
    }

    public int compareTo(cfgTime o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
    }

    public String getPrompt() {
        return "time";
    }

    public String toString() {
        return name;
    }

    /**
     * check if time matches
     *
     * @param tim time to check
     * @return false if matches, true if not
     */
    public boolean matches(long tim) {
        Calendar cal;
        tim += randomCur;
        if (timeZone != null) {
            cal = tabTime.time2calendar(timeZone, tim);
        } else {
            cal = tabTime.time2calendar(cfgAll.timeZoneName, tim);
        }
        for (int i = 0; i < timemap.size(); i++) {
            tabTime ntry = timemap.get(i);
            if (!ntry.matches(cal, tim)) {
                continue;
            }
            return ntry.act != tabListingEntry.actionType.actPermit;
        }
        return true;
    }

}
