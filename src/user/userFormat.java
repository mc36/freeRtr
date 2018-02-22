package user;

import java.util.ArrayList;
import java.util.List;
import util.bits;
import util.cmds;

/**
 * formatter system
 *
 * @author matecsaba
 */
public class userFormat {

    /**
     * table modes
     */
    public enum tableMode {

        /**
         * raw mode
         */
        raw,
        /**
         * normal mode
         */
        normal,
        /**
         * table mode
         */
        table,
        /**
         * fancy mode
         */
        fancy,
        /**
         * csv mode
         */
        csv,
        /**
         * html mode
         */
        html

    }

    private String separator;

    private List<userFormatLine> lines;

    private List<Integer> size;

    /**
     * set separator
     *
     * @param sep source separator
     * @param head header line
     */
    public userFormat(String sep, String head) {
        separator = "" + sep;
        lines = new ArrayList<userFormatLine>();
        size = new ArrayList<Integer>();
        lines.add(string2line(head));
    }

    /**
     * format all lines
     *
     * @param sep separator to use
     * @return formatted table
     */
    public List<String> formatAll(tableMode sep) {
        List<String> res = new ArrayList<String>();
        formatLines(res, sep);
        return res;
    }

    /**
     * add body line
     *
     * @param s string to add
     */
    public void add(String s) {
        lines.add(string2line(s));
    }

    /**
     * add body lines
     *
     * @param lst lines to add
     */
    public void add(List<String> lst) {
        for (int i = 0; i < lst.size(); i++) {
            add(lst.get(i));
        }
    }

    private int getLineSize(userFormatLine dat) {
        int i = dat.cells.size();
        int o = size.size();
        if (i < o) {
            return i;
        } else {
            return o;
        }
    }

    private userFormatLine string2line(String s) {
        userFormatLine res = new userFormatLine();
        cmds cmd = new cmds("tab", s);
        for (;;) {
            if (cmd.size() < 1) {
                break;
            }
            s = cmd.word(separator);
            res.cells.add(s.trim());
        }
        for (int i = size.size(); i < res.cells.size(); i++) {
            size.add(0);
        }
        for (int i = 0; i < getLineSize(res); i++) {
            int o = res.cells.get(i).length();
            if (o < size.get(i)) {
                continue;
            }
            size.set(i, o);
        }
        return res;
    }

    private String formatSep(tableMode sep, String fil) {
        String res = "";
        if (sep == tableMode.fancy) {
            res += " |" + fil;
        }
        for (int i = 0; i < size.size(); i++) {
            res += bits.padEnd("", size.get(i), fil);
            if ((i + 1) >= size.size()) {
                continue;
            }
            switch (sep) {
                case fancy:
                    res += fil + "|" + fil;
                    break;
                case table:
                    res += " | ";
                    break;
                default:
                    break;
            }
        }
        if (sep == tableMode.fancy) {
            res += fil + "|";
        }
        return res;
    }

    private String formatLine(userFormatLine dat, tableMode sep) {
        String s = "";
        switch (sep) {
            case fancy:
                s += " | ";
                break;
            case html:
                s += "<tr>";
                break;
            default:
                break;
        }
        for (int i = 0; i < size.size(); i++) {
            String a;
            if (i < dat.cells.size()) {
                a = dat.cells.get(i);
            } else {
                a = "";
            }
            if (sep == tableMode.html) {
                s += "<td>";
            }
            if (sep == tableMode.raw) {
                s += a;
            } else {
                s += bits.padEnd(a, size.get(i), " ");
            }
            if (sep == tableMode.html) {
                s += "</td>";
            }
            if ((i + 1) >= size.size()) {
                continue;
            }
            switch (sep) {
                case normal:
                    s += "  ";
                    break;
                case fancy:
                    s += " | ";
                    break;
                case table:
                    s += " | ";
                    break;
                case csv:
                    s += ";";
                    break;
                case raw:
                    s += ";";
                    break;
                default:
                    break;
            }
        }
        switch (sep) {
            case fancy:
                s += " |";
                break;
            case html:
                s += "</tr>";
                break;
            default:
                break;
        }
        return bits.trimE(s);
    }

    private void formatLines(List<String> trg, tableMode sep) {
        if (lines.size() < 1) {
            return;
        }
        if (sep == tableMode.fancy) {
            trg.add(formatSep(sep, "~"));
        }
        trg.add(formatLine(lines.get(0), sep));
        if (sep == tableMode.fancy) {
            trg.add(formatSep(sep, "-"));
        }
        for (int i = 1; i < lines.size(); i++) {
            trg.add(formatLine(lines.get(i), sep));
        }
        if (sep == tableMode.fancy) {
            trg.add(formatSep(sep, "_"));
        }
    }

}

class userFormatLine {

    public List<String> cells = new ArrayList<String>();

}
