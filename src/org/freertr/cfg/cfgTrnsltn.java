package org.freertr.cfg;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;

/**
 * one translation rule configuration
 *
 * @author matecsaba
 */
public class cfgTrnsltn implements Comparable<cfgTrnsltn>, cfgGeneric {

    /**
     * name of this translation
     */
    public final String name;

    /**
     * description
     */
    public String description;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("translation-rule .*", cmds.tabulator + cmds.negated + cmds.tabulator + "description", null),
        new userFilter("translation-rule .*", cmds.tabulator + cmds.negated + cmds.tabulator + "track", null),
        new userFilter("translation-rule .*", cmds.tabulator + cmds.negated + cmds.tabulator + "time", null),
        new userFilter("translation-rule .*", cmds.tabulator + cmds.negated + cmds.tabulator + "log", null),
        new userFilter("translation-rule .*", cmds.tabulator + cmds.negated + cmds.tabulator + "last", null)
    };

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
    public String track;

    /**
     * match time
     */
    public cfgTime time;

    /**
     * match pattern
     */
    public List<String> match = new ArrayList<String>();

    /**
     * forbid pattern
     */
    public List<String> forbid = new ArrayList<String>();

    /**
     * action pattern
     */
    public List<String> action = new ArrayList<String>();

    public int compareTo(cfgTrnsltn o) {
        return name.toLowerCase().compareTo(o.name.toLowerCase());
    }

    public String toString() {
        return name;
    }

    /**
     * create new translation rule
     *
     * @param nam name of rule
     */
    public cfgTrnsltn(String nam) {
        name = "" + bits.str2num(nam);
    }

    public List<String> getShRun(int filter) {
        List<String> l = new ArrayList<String>();
        l.add("translation-rule " + name);
        cmds.cfgLine(l, description == null, cmds.tabulator, "description", description);
        for (int i = 0; i < remove.size(); i++) {
            l.add(cmds.tabulator + "remove " + remove.get(i));
        }
        for (int i = 0; i < replace.size(); i++) {
            l.add(cmds.tabulator + "replace " + replace.get(i));
        }
        cmds.cfgLine(l, track == null, cmds.tabulator, "track", "" + track);
        cmds.cfgLine(l, time == null, cmds.tabulator, "time", "" + time);
        for (int i = 0; i < forbid.size(); i++) {
            l.add(cmds.tabulator + "forbid " + forbid.get(i));
        }
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
        if ((filter & 1) == 0) {
            return l;
        }
        return userFilter.filterText(l, defaultF);
    }

    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{3, -1}, "description", "specify description");
        l.add(null, false, 3, new int[]{3, -1}, "<str>", "text");
        l.add(null, false, 1, new int[]{2}, "remove", "remove string");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "regular expression");
        l.add(null, false, 1, new int[]{2}, "track", "consider tracker");
        l.add(null, false, 2, new int[]{-1}, "<name:trk>", "name of tracker");
        l.add(null, false, 1, new int[]{2}, "time", "consider time");
        l.add(null, false, 2, new int[]{-1}, "<name:tm>", "name of time map");
        l.add(null, false, 1, new int[]{2}, "match", "match string");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "regular expression");
        l.add(null, false, 1, new int[]{2}, "forbid", "forbid string");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "regular expression");
        l.add(null, false, 1, new int[]{2}, "replace", "replace string");
        l.add(null, false, 2, new int[]{3}, "<str>", "regular expression");
        l.add(null, false, 3, new int[]{-1}, "<str>", "new string");
        l.add(null, false, 1, new int[]{2}, "text", "place text");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "text");
        l.add(null, false, 1, new int[]{2}, "variable", "place variable from match");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "variable name");
        l.add(null, false, 1, new int[]{2}, "character", "place character");
        l.add(null, false, 2, new int[]{2, -1}, "<str>", "variable name");
        l.add(null, false, 1, new int[]{-1}, "log", "log translations");
        l.add(null, false, 1, new int[]{-1}, "last", "stop after translation");
    }

    public void doCfgStr(cmds cmd) {
        String a = cmd.word();
        boolean negated = a.equals(cmds.negated);
        if (negated) {
            a = cmd.word();
        }
        if (a.equals("description")) {
            if (negated) {
                description = null;
            } else {
                description = cmd.getRemaining();
            }
            return;
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
            track = cmd.word();
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
        if (a.equals("forbid")) {
            a = cmd.getRemaining();
            if (negated) {
                forbid.remove(a);
            } else {
                forbid.add(a);
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
            cfgTrack res = cfgAll.trackFind(track, false);
            if (res == null) {
                return null;
            }
            if (!res.worker.getStatus()) {
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
        for (int i = 0; i < forbid.size(); i++) {
            if (src.matches(forbid.get(i))) {
                return null;
            }
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
