package org.freertr.enc;

import java.util.List;

/**
 * markdown handler
 *
 * @author matecsaba
 */
public class encMarkDown {

    private encMarkDown() {
    }

    /**
     * convert plain text to html
     *
     * @param txt source
     * @return converted
     */
    public static String txt2html(List<String> txt) {
        if (txt == null) {
            return "";
        }
        String res = "<pre>\n";
        for (int i = 0; i < txt.size(); i++) {
            res += htmlize(txt.get(i)) + "\n";
        }
        return res + "</pre>\n";
    }

    private static String htmlize(String s) {
        s = s.replaceAll("<", "&lt;");
        s = s.replaceAll(">", "&gt;");
        return s;
    }

    private static String doMark(String s, String sb, String se, String hb, String he) {
        for (;;) {
            int i = s.indexOf(sb);
            if (i < 0) {
                break;
            }
            String a = s.substring(0, i);
            String b = s.substring(i + sb.length(), s.length());
            i = b.indexOf(se);
            if (i < 0) {
                break;
            }
            s = b.substring(i + se.length(), b.length());
            b = b.substring(0, i);
            s = a + hb + b + he + s;
        }
        return s;
    }

    private static String doLink(String s, String sb, String se, boolean img) {
        for (;;) {
            int i = s.indexOf(sb);
            if (i < 0) {
                break;
            }
            String a = s.substring(0, i);
            String b = s.substring(i + sb.length(), s.length());
            i = b.indexOf(se);
            if (i < 0) {
                break;
            }
            s = b.substring(i + se.length(), b.length());
            b = b.substring(0, i);
            String c = "";
            String d = "";
            if (s.startsWith("(")) {
                i = s.indexOf(")");
                if (i < 0) {
                    s = a + b + s;
                    continue;
                }
                c = s.substring(1, i);
                s = s.substring(i + 1, s.length());
                i = c.indexOf(" ");
                if (i >= 0) {
                    d = " title=\"" + c.substring(i + 1, c.length()).replaceAll("\"", "") + "\"";
                    c = c.substring(0, i);
                }
            } else {
                c = d;
            }
            c = c.replaceAll("\"", "");
            if (img) {
                s = a + "<img src=\"" + c + "\" alt=\"" + b.replaceAll("\"", "") + "\"" + d + ">" + s;
            } else {
                s = a + "<a href=\"" + c + "\"" + d + ">" + b + "</a>" + s;
            }
        }
        return s;
    }

    private static String doStr(String s) {
        s = htmlize(s);
        s = doLink(s, "![", "]", true);
        s = doLink(s, "[", "]", false);
        s = doMark(s, "`", "`", "<code>", "</code>");
        s = doMark(s, "**", "**", "<b>", "</b>");
        s = doMark(s, "__", "__", "<b>", "</b>");
        s = doMark(s, "~~", "~~", "<del>", "</del>");
        s = doMark(s, "*", "*", "<i>", "</i>");
        s = doMark(s, "_", "_", "<i>", "</i>");
        return s;
    }

    private static String doTab(String s, String sb, String se) {
        s = s.substring(1, s.length());
        String r = "";
        for (;;) {
            int i = s.indexOf("|");
            if (i < 0) {
                break;
            }
            String a = s.substring(0, i);
            s = s.substring(i + 1, s.length());
            r += sb + doStr(a) + se;
        }
        return "<tr>" + r + s + "</tr>";
    }

    private static boolean isUnList(String s) {
        return s.startsWith("* ") || s.startsWith("- ") || s.startsWith("+ ");
    }

    private static boolean isOrdList(String s) {
        return s.startsWith("0.") || s.startsWith("1.") || s.startsWith("2.") || s.startsWith("3.") || s.startsWith("4.") || s.startsWith("5.") || s.startsWith("6.") || s.startsWith("7.") || s.startsWith("8.") || s.startsWith("9.");
    }

    private static boolean isQuote(String s) {
        return s.startsWith("> ");
    }

    private static boolean isCode(String s) {
        return s.startsWith("```");
    }

    private static boolean isTable(String s) {
        return s.startsWith("|") && s.endsWith("|");
    }

    /**
     * convert markdown to html
     *
     * @param md source
     * @return converted
     */
    public static String md2html(List<String> md) {
        if (md == null) {
            return "";
        }
        String res = "";
        for (int p = 0; p < md.size(); p++) {
            String s = md.get(p);
            if (s.startsWith("# ")) {
                res += "<h1>" + doStr(s.substring(1, s.length())) + "</h1>\n";
                continue;
            }
            if (s.startsWith("## ")) {
                res += "<h2>" + doStr(s.substring(2, s.length())) + "</h2>\n";
                continue;
            }
            if (s.startsWith("### ")) {
                res += "<h3>" + doStr(s.substring(3, s.length())) + "</h3>\n";
                continue;
            }
            if (s.startsWith("#### ")) {
                res += "<h4>" + doStr(s.substring(4, s.length())) + "</h4>\n";
                continue;
            }
            if (s.equals("---") || s.equals("***") || s.equals("___")) {
                res += "<hr/>\n";
                continue;
            }
            if (isUnList(s)) {
                res += "<ul>\n";
                for (; p < md.size(); p++) {
                    s = doStr(md.get(p));
                    if (!isUnList(s)) {
                        break;
                    }
                    res += "<li>" + s.substring(1, s.length()) + "</li>\n";
                }
                res += "</ul>\n";
                if (!isUnList(s)) {
                    p--;
                }
                continue;
            }
            if (isOrdList(s)) {
                res += "<ol>\n";
                for (; p < md.size(); p++) {
                    s = doStr(md.get(p));
                    if (!isOrdList(s)) {
                        break;
                    }
                    res += "<li>" + s.substring(2, s.length()) + "</li>\n";
                }
                res += "</ol>\n";
                if (!isOrdList(s)) {
                    p--;
                }
                continue;
            }
            if (isQuote(s)) {
                res += "<blockquote>\n";
                for (; p < md.size(); p++) {
                    s = md.get(p);
                    if (!isQuote(s)) {
                        break;
                    }
                    res += doStr(s.substring(2, s.length())) + "<br/>\n";
                }
                res += "</blockquote>\n";
                if (!isQuote(s)) {
                    p--;
                }
                continue;
            }
            if (isCode(s)) {
                res += "<pre><code>\n";
                for (p++; p < md.size(); p++) {
                    s = md.get(p);
                    if (isCode(s)) {
                        break;
                    }
                    res += htmlize(s) + "\n";
                }
                res += "</code></pre>\n";
                continue;
            }
            if (isTable(s)) {
                res += "<table><thead>\n";
                res += doTab(s, "<th>", "</th>") + "\n";
                res += "</thead><tbody>\n";
                for (p += 2; p < md.size(); p++) {
                    s = md.get(p);
                    if (!isTable(s)) {
                        break;
                    }
                    res += doTab(s, "<td>", "</td>") + "\n";
                }
                res += "</tbody></table>\n";
                if (!isTable(s)) {
                    p--;
                }
                continue;
            }
            res += doStr(s) + "<br/>\n";
        }
        return res + "<br/>\n";
    }

}
