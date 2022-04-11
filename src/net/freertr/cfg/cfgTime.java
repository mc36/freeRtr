package net.freertr.cfg;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Comparator;
import java.util.List;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabIntMatcher;
import net.freertr.tab.tabListingEntry;
import net.freertr.tab.tabTime;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * route map configuration
 *
 * @author matecsaba
 */
public class cfgTime implements Comparator<cfgTime>, cfgGeneric {

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
    public final static String[] defaultL = {
        "time-map .*! no description",
        "time-map .*! no time-zone",
        "time-map .*! random 0 0",
        "time-map .*! sequence .* match year all",
        "time-map .*! sequence .* match month all",
        "time-map .*! sequence .* match day all",
        "time-map .*! sequence .* match dow all",
        "time-map .*! sequence .* match wom all",
        "time-map .*! sequence .* match woy all",
        "time-map .*! sequence .* match dom all",
        "time-map .*! sequence .* match doy all",
        "time-map .*! sequence .* match hour all",
        "time-map .*! sequence .* match minute all",
        "time-map .*! sequence .* match second all",
        "time-map .*! sequence .* match milli all",
        "time-map .*! sequence .* match periodic 0 0 0"};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * time list
     */
    public tabGen<tabTime> timemap;

    /**
     * create new route map
     */
    public cfgTime() {
        timemap = new tabGen<tabTime>();
        seq = nextseq();
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

    public void getHelp(userHelping l) {
        l.add(null, "1 2   sequence              sequence number of an entry");
        l.add(null, "2 1,.   <num>               sequence number");
        l.add(null, "1 3,. description           specify description");
        l.add(null, "3 3,.   <str>               text");
        l.add(null, "1 2   rename                rename this time map");
        l.add(null, "2 .     <str>               set new name");
        l.add(null, "1 2,. reindex               reindex route map");
        l.add(null, "2 3,.   [num]               initial number to start with");
        l.add(null, "3 4,.     [num]             increment number");
        l.add(null, "1 2   action                set action to do");
        l.add(null, "2 .     deny                specify to forbid");
        l.add(null, "2 .     permit              specify to allow");
        l.add(null, "1 3   time-zone             specify time zone");
        l.add(null, "3 .     <str>               text");
        l.add(null, "1 3   random                specify random offset in ms");
        l.add(null, "3 4     <num>               minimum");
        l.add(null, "4 .       <num>             maximum");
        l.add(null, "1 2   match                 match values");
        l.add(null, "2 3     year                match year");
        l.add(null, "3 .       <num>             value");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     month               match month");
        l.add(null, "3 .       <num>             value");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     day                 match day");
        l.add(null, "3 .       <num>             value");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     dow                 match day of week");
        l.add(null, "3 .       <num>             value");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     wom                 match week of month");
        l.add(null, "3 .       <num>             value");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     woy                 match week of year");
        l.add(null, "3 .       <num>             value");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     dom                 match day of month");
        l.add(null, "3 .       <num>             value");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     doy                 match day of year");
        l.add(null, "3 .       <num>             value");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     hour                match hour");
        l.add(null, "3 .       <num>             value");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     minute              match minute");
        l.add(null, "3 .       <num>             value");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     second              match second");
        l.add(null, "3 .       <num>             value");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     milli               match millisecond");
        l.add(null, "3 .       <num>             value");
        l.add(null, "3 .       all               any value");
        l.add(null, "2 3     periodic            match periodically in ms");
        l.add(null, "3 4       <num>             length");
        l.add(null, "4 5         <num>           begin");
        l.add(null, "5 .           <num>         end");
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
        if (!a.equals("no")) {
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

    public int compare(cfgTime o1, cfgTime o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
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
