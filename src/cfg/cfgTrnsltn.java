package cfg;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import tab.tabGen;
import user.userFilter;
import user.userHelping;
import util.bits;
import util.cmds;
import util.logger;

/**
 * one translation rule configuration
 *
 * @author matecsaba
 */
public class cfgTrnsltn implements Comparator<cfgTrnsltn>, cfgGeneric {

    /**
     * name of this translation
     */
    public final String name;

    /**
     * defaults text
     */
    public final static String defaultL[] = {
        "translation-rule .*! no track",
        "translation-rule .*! no time",
        "translation-rule .*! no log",
        "translation-rule .*! no last",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * log translation
     */
    public boolean log;

    /**
     * last translation
     */
    public boolean last;

    /**
     * remove pattern
     */
    public List<String> remove = new ArrayList<String>();

    /**
     * replace pattern
     */
    public List<String> replace = new ArrayList<String>();

    /**
     * match tracker
     */
    public cfgTrack track;

    /**
     * match time
     */
    public cfgTime time;

    /**
     * match pattern
     */
    public List<String> match = new ArrayList<String>();

    /**
     * match pattern
     */
    public List<String> action = new ArrayList<String>();

    public int compare(cfgTrnsltn o1, cfgTrnsltn o2) {
        return o1.name.toLowerCase().compareTo(o2.name.toLowerCase());
    }

    public String toString() {
        return "translate " + name;
    }

    /**
     * create new translation rule
     *
     * @param nam name of rule
     */
    public cfgTrnsltn(String nam) {
        name = "" + bits.str2num(nam);
    }

    public List<String> getShRun(boolean filter) {
        List<String> l = new ArrayList<String>();
        l.add("translation-rule " + name);
        for (int i = 0; i < remove.size(); i++) {
            l.add(cmds.tabulator + "remove " + remove.get(i));
        }
        for (int i = 0; i < replace.size(); i++) {
            l.add(cmds.tabulator + "replace " + replace.get(i));
        }
        cmds.cfgLine(l, track == null, cmds.tabulator, "track", "" + track);
        cmds.cfgLine(l, time == null, cmds.tabulator, "time", "" + time);
        for (int i = 0; i < match.size(); i++) {
            l.add(cmds.tabulator + "match " + match.get(i));
        }
        for (int i = 0; i < action.size(); i++) {
            String a = action.get(i);
            String b = a.substring(0, 1);
            a = a.substring(1, a.length());
            String s = "unknown";
            if (b.equals("1")) {
                s = "text";
            }
            if (b.equals("2")) {
                s = "variable";
            }
            if (b.equals("3")) {
                s = "character";
            }
            l.add(cmds.tabulator + s + " " + a);
        }
        cmds.cfgLine(l, !last, cmds.tabulator, "last", "");
        cmds.cfgLine(l, !log, cmds.tabulator, "log", "");
        l.add(cmds.tabulator + cmds.finish);
        l.add(cmds.comment);
        if (!filter) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public userHelping getHelp() {
        userHelping l = userHelping.getGenCfg();
        l.add("1 2    remove              remove string");
        l.add("2 2,.    <str>             regular expression");
        l.add("1 2    track               consider tracker");
        l.add("2 .      <name>            name of tracker");
        l.add("1 2    time                consider time");
        l.add("2 .      <name>            name of time map");
        l.add("1 2    match               match string");
        l.add("2 2,.    <str>             regular expression");
        l.add("1 2    replace             replace string");
        l.add("2 3      <str>             regular expression");
        l.add("3 .        <str>           new string");
        l.add("1 2    text                place text");
        l.add("2 2,.    <str>             text");
        l.add("1 2    variable            place variable from match");
        l.add("2 2,.    <str>             variable name");
        l.add("1 2    character           place character");
        l.add("2 2,.    <str>             variable name");
        l.add("1 .    log                 log translations");
        l.add("1 .    last                stop after translation");
        return l;
    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        boolean negated = a.equals("no");
        if (negated) {
            a = cmd.word();
        }
        if (a.equals("log")) {
            log = !negated;
            return;
        }
        if (a.equals("last")) {
            last = !negated;
            return;
        }
        if (a.equals("track")) {
            if (negated) {
                track = null;
                return;
            }
            track = cfgAll.trackFind(cmd.word(), false);
            if (track == null) {
                cmd.error("no such tracker");
            }
            return;
        }
        if (a.equals("time")) {
            if (negated) {
                time = null;
                return;
            }
            time = cfgAll.timeFind(cmd.word(), false);
            if (time == null) {
                cmd.error("no such time");
            }
            return;
        }
        if (a.equals("match")) {
            a = cmd.getRemaining();
            if (negated) {
                match.remove(a);
            } else {
                match.add(a);
            }
            return;
        }
        if (a.equals("remove")) {
            a = cmd.getRemaining();
            if (negated) {
                remove.remove(a);
            } else {
                remove.add(a);
            }
            return;
        }
        if (a.equals("replace")) {
            a = cmd.getRemaining();
            if (negated) {
                replace.remove(a);
            } else {
                replace.add(a);
            }
            return;
        }
        if (a.equals("text")) {
            a = "1" + cmd.getRemaining();
            if (negated) {
                action.remove(a);
            } else {
                action.add(a);
            }
            return;
        }
        if (a.equals("variable")) {
            a = "2" + cmd.getRemaining();
            if (negated) {
                action.remove(a);
            } else {
                action.add(a);
            }
            return;
        }
        if (a.equals("character")) {
            a = "3" + cmd.getRemaining();
            if (negated) {
                action.remove(a);
            } else {
                action.add(a);
            }
            return;
        }
        cmd.badCmd();
        return;
    }

    public String getPrompt() {
        return "trns";
    }

    /**
     * do translation
     *
     * @param orig what to translate
     * @return translated text, null if not matched
     */
    public String doTranslate(String orig) {
        String src = orig;
        if (time != null) {
            if (time.matches(bits.getTime() + cfgAll.timeServerOffset)) {
                return null;
            }
        }
        if (track != null) {
            if (!track.worker.getStatus()) {
                return null;
            }
        }
        for (int i = 0; i < remove.size(); i++) {
            src = src.replaceAll(remove.get(i), "");
        }
        for (int i = 0; i < replace.size(); i++) {
            String a = replace.get(i);
            int o = a.indexOf(" ");
            if (o < 0) {
                continue;
            }
            src = src.replaceAll(a.substring(0, o), a.substring(o + 1, a.length()));
        }
        Pattern pat = null;
        Matcher mat = null;
        boolean ok = false;
        for (int i = 0; i < match.size(); i++) {
            try {
                pat = Pattern.compile(match.get(i));
                mat = pat.matcher(src);
                if (!mat.find()) {
                    continue;
                }
            } catch (Exception e) {
                continue;
            }
            ok = true;
            break;
        }
        if (!ok) {
            return null;
        }
        String res = "";
        for (int i = 0; i < action.size(); i++) {
            String a = action.get(i);
            String c = a.substring(0, 1);
            a = a.substring(1, a.length());
            if (c.equals("1")) {
                res += a;
                continue;
            }
            if (c.equals("3")) {
                byte[] buf = new byte[1];
                buf[0] = (byte) (bits.str2num(a) & 0xff);
                res += new String(buf);
                continue;
            }
            if (!c.equals("2")) {
                continue;
            }
            try {
                c = mat.group(a);
            } catch (Exception e) {
                continue;
            }
            res += c;
        }
        if (log) {
            logger.info("translated " + orig + " to " + res);
        }
        return res;
    }

    /**
     * do translation
     *
     * @param lst list to use
     * @param str what to translate
     * @return translated string
     */
    public static String doTranslate(List<cfgTrnsltn> lst, String str) {
        String cur = str;
        for (int i = 0; i < lst.size(); i++) {
            cfgTrnsltn ntry = lst.get(i);
            String res = ntry.doTranslate(cur);
            if (res == null) {
                continue;
            }
            cur = res;
            if (ntry.last) {
                break;
            }
        }
        return cur;
    }

}
