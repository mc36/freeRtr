package user;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import tab.tabGen;
import util.cmds;

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
        if (!ntry.section.matches(section)) {
            return false;
        }
        if (!ntry.command.matches(command)) {
            return false;
        }
        return true;
    }

    /**
     * check if negated
     *
     * @return false if no, true if yes
     */
    public boolean negated() {
        return command.startsWith("no ");
    }

    /**
     * negate this entry
     *
     * @return negated entry
     */
    public userFilter negate() {
        if (!negated()) {
            return new userFilter(section, "no " + command.trim(), listing);
        }
        return new userFilter(section, command.substring(3, command.length()), listing);
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
            String old = txt.get(ln);
            String s = old.trim();
            if (s.length() < 1) {
                res.add(new userFilter("", old, null));
                nextSec = "";
                continue;
            }
            int i = old.indexOf(" ");
            if (i > 0) {
                nextSec = s;
                sec = "";
            }
            res.add(new userFilter(sec, old, null));
        }
        return res;
    }

    /**
     * convert sections to text
     *
     * @param sec section to convert
     * @return sectioned text
     */
    public static List<String> section2text(List<userFilter> sec) {
        List<String> txt = new ArrayList<String>();
        String prev = "";
        String beg = "";
        for (int i = 0; i < sec.size(); i++) {
            userFilter ntry = sec.get(i);
            String s = ntry.command.trim();
            if (prev.equals(ntry.section)) {
                txt.add(beg + s);
                continue;
            }
            if (beg.length() > 0) {
                txt.add(beg + cmds.finish);
            }
            prev = ntry.section.trim();
            if (prev.length() > 0) {
                beg = cmds.tabulator;
                txt.add(prev);
            } else {
                beg = "";
            }
            txt.add(beg + s);
        }
        if (beg.length() > 0) {
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
            res.add(src.get(i).negate());
        }
        return res;
    }

    /**
     * normalize section
     *
     * @param src section to normalize
     * @return normalized section
     */
    public static List<userFilter> normalizeSection(List<userFilter> src) {
        List<userFilter> res = new ArrayList<userFilter>();
        String prev = "";
        for (int i = 0; i < src.size(); i++) {
            userFilter ntry = src.get(i);
            String sec = ntry.section.trim();
            String cmd = ntry.command.trim();
            if (cmd.equals(cmds.finish)) {
                continue;
            }
            if (cmd.startsWith(cmds.comment)) {
                continue;
            }
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
     * get section
     *
     * @param src source
     * @param sec regular expression to find
     * @return lines
     */
    public static List<String> getSection(List<String> src, String sec) {
        return userFilter.section2text(userFilter.normalizeSection(userFilter.getSection(userFilter.text2section(src), sec, true)));
    }

    /**
     * get section
     *
     * @param src source
     * @param sec section to find
     * @param reg match with regular expression
     * @return lines
     */
    public static List<userFilter> getSection(List<userFilter> src, String sec, boolean reg) {
        List<userFilter> res = new ArrayList<userFilter>();
        for (int i = 0; i < src.size(); i++) {
            userFilter ntry = src.get(i);
            boolean b;
            if (reg) {
                b = ntry.section.matches(sec);
            } else {
                b = ntry.section.equals(sec);
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
        List<userFilter> srcS = getSection(srcO, sec, false);
        List<userFilter> trgS = getSection(trgO, sec, false);
        setUsed(trgS, false);
        for (int i = srcS.size() - 1; i >= 0; i--) {
            userFilter ntry = srcS.get(i);
            userFilter found = findText(ntry, trgS);
            if (found != null) {
                found.used = true;
                continue;
            }
            userFilter neg = ntry.negate();
            found = findText(neg, trgS);
            if (found != null) {
                found.used = true;
                res.add(neg);
                continue;
            }
            if (ntry.negated()) {
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
        res.add(new userFilter("", "no " + sec, null));
    }

    /**
     * get configuration differences
     *
     * @param src source
     * @param trg target
     * @return result
     */
    public static List<userFilter> diffText(List<userFilter> src, List<userFilter> trg) {
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
        return section2text(diffText(normalizeSection(text2section(src)), normalizeSection(text2section(trg))));
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
