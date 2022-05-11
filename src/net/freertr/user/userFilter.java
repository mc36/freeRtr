package net.freertr.user;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import net.freertr.tab.tabGen;
import net.freertr.util.cmds;
import net.freertr.util.extMrkLng;
import net.freertr.util.extMrkLngEntry;

/**
 * config filter entry
 *
 * @author matecsaba
 */
public class userFilter implements Comparator<userFilter> {

    /**
     * name of section
     */
    public final String section;

    /**
     * name of command
     */
    public final String command;

    /**
     * listing
     */
    public final List<String> listing;

    /**
     * used flag
     */
    public boolean used;

    private Pattern patSec;

    private Pattern patCmd;

    /**
     * create new filter
     *
     * @param sec section
     * @param cmd command
     * @param lst listing
     */
    public userFilter(String sec, String cmd, List<String> lst) {
        section = sec;
        command = cmd;
        listing = lst;
    }

    /**
     * check if this line need to be ignored
     *
     * @return true if yes
     */
    public boolean shouldIgnore() {
        String a = command.trim();
        if (a.length() < 1) {
            return true;
        }
        if (a.equals(cmds.comment)) {
            return true;
        }
        return false;
    }

    public String toString() {
        String s;
        if (listing != null) {
            s = "";
            for (int i = 0; i < listing.size(); i++) {
                s += listing.get(i) + "/";
            }
        } else {
            s = "<null>";
        }
        return "sec=" + section + " cmd=" + command + " lst=" + s;
    }

    public int compare(userFilter o1, userFilter o2) {
        int i = o1.section.compareTo(o2.section);
        if (i != 0) {
            return i;
        }
        return o1.command.compareTo(o2.command);
    }

    /**
     * check if other entry matches this entry
     *
     * @param ntry entry to test
     * @return true if matches, false if not
     */
    public boolean matches(userFilter ntry) {
        if (patCmd == null) {
            if (!ntry.section.matches(section)) {
                return false;
            }
            if (!ntry.command.matches(command)) {
                return false;
            }
            return true;
        }
        Matcher m = patSec.matcher(ntry.section);
        if (!m.matches()) {
            return false;
        }
        m = patCmd.matcher(ntry.command);
        if (!m.matches()) {
            return false;
        }
        return true;
    }

    /**
     * compile the regexps for matching
     */
    public void optimize4lookup() {
        patSec = Pattern.compile(section);
        patCmd = Pattern.compile(command);
    }

    /**
     * negate this entry
     *
     * @return negated entry, null if nothing
     */
    public userFilter negate() {
        String c = command;
        String b = "";
        for (; c.startsWith(" ");) {
            c = c.substring(1, c.length());
            b += " ";
        }
        if (c.equals(cmds.finish)) {
            return null;
        }
        if (c.startsWith("no ")) {
            return new userFilter(section, b + c.substring(3, c.length()), listing);
        } else {
            return new userFilter(section, b + "no " + c, listing);
        }
    }

    /**
     * convert text to sections
     *
     * @param txt text to convert
     * @return sectioned text
     */
    public static List<userFilter> text2section(List<String> txt) {
        List<userFilter> res = new ArrayList<userFilter>();
        String sec;
        String nextSec = "";
        for (int ln = 0; ln < txt.size(); ln++) {
            sec = nextSec;
            String a = txt.get(ln);
            if (a.length() <= 1) {
                res.add(new userFilter("", a, null));
                nextSec = "";
                continue;
            }
            if (!a.startsWith(" ")) {
                nextSec = a;
                sec = "";
            }
            res.add(new userFilter(sec, a, null));
        }
        return res;
    }

    /**
     * section to xml
     *
     * @param rep xml
     * @param beg begin
     * @param sec section
     */
    public static void section2xml(extMrkLng rep, String beg, List<userFilter> sec) {
        for (int i = 0; i < sec.size(); i++) {
            userFilter cur = sec.get(i);
            String a = cur.command.trim();
            if (a.length() < 1) {
                continue;
            }
            if (a.equals(cmds.finish)) {
                continue;
            }
            cmds cmd = new cmds("x", a);
            String s = beg;
            for (;;) {
                a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                a = "/" + extMrkLng.escId(a);
                s += a;
            }
            rep.data.add(new extMrkLngEntry(null, s, "", ""));
            rep.data.add(new extMrkLngEntry(null, beg, "", ""));
        }
    }

    /**
     * convert sections to text
     *
     * @param src section to convert
     * @param rep repair sections
     * @return sectioned text
     */
    public static List<String> section2text(List<userFilter> src, boolean rep) {
        if (rep) {
            src = normalizeSection(src);
        }
        List<String> txt = new ArrayList<String>();
        String prev = "";
        String beg = "";
        boolean fin = false;
        for (int i = 0; i < src.size(); i++) {
            userFilter ntry = src.get(i);
            String s = ntry.command.trim();
            if (prev.equals(ntry.section)) {
                fin |= s.equals(cmds.finish);
                txt.add(beg + s);
                continue;
            }
            if (rep && !fin && (beg.length() > 0)) {
                txt.add(beg + cmds.finish);
            }
            prev = ntry.section.trim();
            if (prev.length() > 0) {
                beg = cmds.tabulator;
                if (rep) {
                    txt.add(prev);
                }
            } else {
                beg = "";
            }
            txt.add(beg + s);
            fin = s.equals(cmds.finish);
        }
        if (rep && !fin && (beg.length() > 0)) {
            txt.add(beg + cmds.finish);
        }
        return txt;
    }

    /**
     * convert sections to text
     *
     * @param sec section to convert
     * @param sep format to use
     * @return sectioned text
     */
    public static List<String> sectionDump(List<userFilter> sec, userFormat.tableMode sep) {
        List<String> txt = new ArrayList<String>();
        for (int i = 0; i < sec.size(); i++) {
            userFilter ntry = sec.get(i);
            switch (sep) {
                case normal:
                    txt.add(ntry.section + "|" + ntry.command + "|");
                    break;
                case csv:
                    txt.add(ntry.section + ";" + ntry.command);
                    break;
                case html:
                    txt.add("<tr><td>" + ntry.section + "</td><td>" + ntry.command + "</td></tr>");
                    break;
                case setdel:
                    String a = ntry.command.trim();
                    String s = "set";
                    if (a.startsWith("no ")) {
                        s = "delete";
                        a = a.substring(3, a.length());
                    }
                    txt.add(s + " " + (ntry.section + " " + a).trim());
                    break;
                default:
                    break;
            }
        }
        return txt;
    }

    /**
     * negate section
     *
     * @param src section to negate
     * @return negated section
     */
    public static List<userFilter> negateSection(List<userFilter> src) {
        List<userFilter> res = new ArrayList<userFilter>();
        for (int i = 0; i < src.size(); i++) {
            userFilter ntry = src.get(i);
            if (ntry.shouldIgnore()) {
                continue;
            }
            ntry = ntry.negate();
            if (ntry == null) {
                continue;
            }
            res.add(ntry);
        }
        return res;
    }

    /**
     * find filter
     *
     * @param txt text to find
     * @param lst filter to find
     * @return filter found
     */
    public static userFilter findFilter(userFilter txt, tabGen<userFilter> lst) {
        for (int i = 0; i < lst.size(); i++) {
            userFilter ntry = lst.get(i);
            if (ntry.matches(txt)) {
                return ntry;
            }
        }
        return null;
    }

    /**
     * find text
     *
     * @param txt text to find
     * @param lst list to parse
     * @return index, null if not found
     */
    public static userFilter findText(userFilter txt, List<userFilter> lst) {
        for (int i = 0; i < lst.size(); i++) {
            userFilter ntry = lst.get(i);
            if (ntry.used) {
                continue;
            }
            if (txt.compare(ntry, txt) == 0) {
                return ntry;
            }
        }
        return null;
    }

    /**
     * filter text
     *
     * @param txt text to filter
     * @param flt filter to use
     * @return filtered text
     */
    public static List<String> filterText(List<String> txt, tabGen<userFilter> flt) {
        List<String> res = new ArrayList<String>();
        List<userFilter> src = text2section(txt);
        for (int i = 0; i < src.size(); i++) {
            userFilter l = src.get(i);
            if (findFilter(l, flt) != null) {
                continue;
            }
            res.add(l.command);
        }
        return res;
    }

    /**
     * filter text
     *
     * @param src text to filter
     * @param flt filter to use
     * @return filtered text
     */
    public static List<userFilter> filter2text(List<userFilter> src, tabGen<userFilter> flt) {
        List<userFilter> res = new ArrayList<userFilter>();
        for (int i = 0; i < src.size(); i++) {
            userFilter l = src.get(i);
            if (findFilter(l, flt) == null) {
                continue;
            }
            res.add(l);
        }
        return res;
    }

    /**
     * set usage flags
     *
     * @param lst list to update
     * @param val new value
     */
    public static void setUsed(List<userFilter> lst, boolean val) {
        for (int i = 0; i < lst.size(); i++) {
            lst.get(i).used = val;
        }
    }

    /**
     * del certain flag
     *
     * @param lst list to update
     * @param val value to remove
     */
    public static void delUsed(List<userFilter> lst, boolean val) {
        for (int i = lst.size() - 1; i >= 0; i--) {
            if (lst.get(i).used != val) {
                continue;
            }
            lst.remove(i);
        }
    }

    /**
     * get section
     *
     * @param src source
     * @param sec regular expression to find
     * @return lines
     */
    public static List<String> getSection(List<String> src, String sec) {
        return userFilter.section2text(userFilter.getSection(userFilter.text2section(src), sec, true, false, false), true);
    }

    /**
     * get section
     *
     * @param src source
     * @param sec section to find
     * @param reg match with regular expression
     * @param neg negate selection
     * @param mpty match empty section command too
     * @return lines
     */
    public static List<userFilter> getSection(List<userFilter> src, String sec, boolean reg, boolean neg, boolean mpty) {
        List<userFilter> res = new ArrayList<userFilter>();
        for (int i = 0; i < src.size(); i++) {
            userFilter ntry = src.get(i);
            boolean b;
            if (reg) {
                b = ntry.section.matches(sec);
            } else {
                b = ntry.section.equals(sec);
            }
            if (mpty && ntry.section.length() < 1) {
                if (reg) {
                    b |= ntry.command.matches(sec);
                } else {
                    b |= ntry.command.equals(sec);
                }
            }
            if (neg) {
                b = !b;
            }
            if (!b) {
                continue;
            }
            res.add(ntry);
        }
        return res;
    }

    /**
     * get list of sections
     *
     * @param src source
     * @param flt filter to use
     * @param fin section terminators
     * @return lines
     */
    public static List<String> getSecList(List<userFilter> src, String flt, String fin) {
        List<String> res = new ArrayList<String>();
        for (int i = 0; i < src.size(); i++) {
            userFilter ntry = src.get(i);
            if (ntry.section.length() > 0) {
                continue;
            }
            if (flt != null) {
                if (!ntry.command.matches(flt)) {
                    continue;
                }
            }
            res.add(ntry.command);
            if (fin != null) {
                res.add(fin);
            }
        }
        return res;
    }

    private static void diffSec(List<userFilter> res, List<userFilter> srcO, List<userFilter> trgO, String sec) {
        List<userFilter> srcS = getSection(srcO, sec, false, false, true);
        List<userFilter> trgS = getSection(trgO, sec, false, false, true);
        setUsed(trgS, false);
        for (int i = srcS.size() - 1; i >= 0; i--) {
            userFilter ntry = srcS.get(i);
            userFilter found = findText(ntry, trgS);
            if (found != null) {
                found.used = true;
                continue;
            }
            userFilter neg = ntry.negate();
            if (neg == null) {
                continue;
            }
            found = findText(neg, trgS);
            if (found != null) {
                found.used = true;
                res.add(neg);
                continue;
            }
            res.add(neg);
        }
        for (int i = 0; i < trgS.size(); i++) {
            userFilter ntry = trgS.get(i);
            if (ntry.used) {
                continue;
            }
            res.add(ntry);
        }
        setUsed(srcS, true);
        setUsed(trgS, true);
        if (sec.length() < 1) {
            return;
        }
        if (trgS.size() > 0) {
            return;
        }
        userFilter ntry = new userFilter("", sec, null).negate();
        if (ntry == null) {
            return;
        }
        res.add(ntry);
    }

    private static List<userFilter> normalizeSection(List<userFilter> src) {
        List<userFilter> res = new ArrayList<userFilter>();
        String prev = "";
        for (int i = 0; i < src.size(); i++) {
            userFilter ntry = src.get(i);
            if (ntry.shouldIgnore()) {
                continue;
            }
            String sec = ntry.section.trim();
            String cmd = ntry.command.trim();
            if (prev.length() > 0) {
                if (prev.equals(sec)) {
                    res.remove(res.size() - 1);
                }
            }
            res.add(new userFilter(sec, cmd, ntry.listing));
            prev = (sec + " " + cmd).trim();
        }
        return res;
    }

    /**
     * get configuration differences
     *
     * @param src source
     * @param trg target
     * @return result
     */
    public static List<userFilter> diffText(List<userFilter> src, List<userFilter> trg) {
        src = normalizeSection(src);
        trg = normalizeSection(trg);
        setUsed(src, false);
        setUsed(trg, false);
        List<userFilter> res = new ArrayList<userFilter>();
        for (int i = src.size() - 1; i >= 0; i--) {
            userFilter ntry = src.get(i);
            if (ntry.used) {
                continue;
            }
            diffSec(res, src, trg, ntry.section);
        }
        for (int i = 0; i < trg.size(); i++) {
            userFilter ntry = trg.get(i);
            if (ntry.used) {
                continue;
            }
            diffSec(res, src, trg, ntry.section);
        }
        return res;
    }

    /**
     * get configuration differences
     *
     * @param src source
     * @param trg target
     * @return result
     */
    public static List<String> getDiffs(List<String> src, List<String> trg) {
        if (src == null) {
            return null;
        }
        if (trg == null) {
            return null;
        }
        return section2text(diffText(text2section(src), text2section(trg)), true);
    }

    /**
     * check if texts differs
     *
     * @param t1 text one
     * @param t2 text two
     * @return number of differences
     */
    public static int doDiffer(List<String> t1, List<String> t2) {
        if ((t1 == null) && (t2 == null)) {
            return 0;
        }
        if ((t1 == null) || (t2 == null)) {
            return Integer.MAX_VALUE;
        }
        int res = 0;
        int i = t1.size();
        int o = t2.size();
        if (i < o) {
            res += o - i;
            o = i;
        }
        if (i > o) {
            res += i - o;
            i = o;
        }
        for (i = 0; i < o; i++) {
            if (!t1.get(i).equals(t2.get(i))) {
                res++;
            }
        }
        return res;
    }

}
