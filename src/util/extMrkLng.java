package util;

import java.util.ArrayList;
import java.util.List;

/**
 * extensible markup language
 *
 * @author matecsaba
 */
public class extMrkLng {

    /**
     * original string
     */
    public String orig;

    /**
     * self closing tags
     */
    public String closed = "";

    /**
     * elements
     */
    public List<extMrkLngEntry> data;

    /**
     * clear new instance
     */
    public extMrkLng() {
        clear();
    }

    /**
     * clear all data
     */
    public void clear() {
        data = new ArrayList<extMrkLngEntry>();
        orig = "";
    }

    /**
     * copy from other xml
     *
     * @param src original xml to copy
     */
    public void copyBytes(extMrkLng src) {
        clear();
        orig = "" + src.orig;
        for (int i = 0; i < src.data.size(); i++) {
            data.add(src.data.get(i).copyBytes());
        }
    }

    /**
     * convert strings to xml
     *
     * @param l list to parse
     * @return false on success, true on error
     */
    public boolean fromString(List<String> l) {
        String s = "";
        for (int i = 0; i < l.size(); i++) {
            s += l.get(i);
        }
        return fromString(s);
    }

    /**
     * convert string to xml
     *
     * @param s string to parse
     * @return false on success, true on error
     */
    public boolean fromString(String s) {
        clear();
        orig = "" + s;
        s = bits.trimE(s);
        String path = "";
        for (;;) {
            s = bits.trimB(s);
            if (s.length() < 1) {
                break;
            }
            int i = s.indexOf("<");
            if (i < 0) {
                i = s.length();
            }
            if (i > 0) {
                String a = s.substring(0, i);
                s = s.substring(i, s.length());
                a = bits.trimE(a);
                if (a.length() < 1) {
                    continue;
                }
                if (data.size() > 0) {
                    extMrkLngEntry ntry = data.get(data.size() - 1);
                    if (ntry.name.equals(path)) {
                        ntry.value += a;
                        continue;
                    }
                }
                data.add(new extMrkLngEntry(path, "", a));
                continue;
            }
            i = s.indexOf(">");
            String a;
            if (i < 0) {
                a = s.substring(1, s.length());
                s = "";
            } else {
                a = s.substring(1, i);
                s = s.substring(i + 1, s.length());
            }
            a = bits.trimB(a);
            a = bits.trimE(a);
            boolean nb = a.endsWith("/");
            if (nb) {
                a = a.substring(0, a.length() - 1);
                a = bits.trimE(a);
            }
            i = a.indexOf(" ");
            String b = "";
            if (i > 0) {
                b = a.substring(i + 1, a.length());
                a = a.substring(0, i);
                b = bits.trimB(b);
            }
            if (a.startsWith("/")) {
                b = path + "/";
                i = b.lastIndexOf(a + "/");
                if (i < 0) {
                    continue;
                }
                path = path.substring(0, i);
                data.add(new extMrkLngEntry(path, "", ""));
                continue;
            }
            if (closed.indexOf("|" + a + "|") >= 0) {
                nb = true;
            }
            data.add(new extMrkLngEntry(path + "/" + a, b, ""));
            if (nb) {
                data.add(new extMrkLngEntry(path, "", ""));
                continue;
            }
            path += "/" + a;
        }
        data.add(new extMrkLngEntry("", "", ""));
        return false;
    }

    private String getPath(String tag, String add, boolean ord, String prm) {
        List<String> lst = new ArrayList<String>();
        for (;;) {
            int i = tag.indexOf("/");
            if (i < 0) {
                break;
            }
            lst.add(tag.substring(0, i));
            tag = tag.substring(i + 1, tag.length());
        }
        if (lst.size() > 0) {
            int i = lst.size() - 1;
            if (prm.length() > 0) {
                prm = " " + prm;
            }
            lst.set(i, lst.get(i) + prm);
        }
        tag = "";
        for (int i = 0; i < lst.size(); i++) {
            prm = "<" + add + lst.get(i) + ">";
            if (ord) {
                tag = prm + tag;
            } else {
                tag = tag + prm;
            }
        }
        return tag;
    }

    private String getMove(String src, String trg, String prm) {
        if (src.equals(trg)) {
            return "";
        }
        src += "/";
        trg += "/";
        for (;;) {
            int i = src.indexOf("/");
            int o = trg.indexOf("/");
            if (i != o) {
                break;
            }
            if (!src.substring(0, i).equals(trg.substring(0, i))) {
                break;
            }
            src = src.substring(i + 1, src.length());
            trg = trg.substring(i + 1, trg.length());
        }
        return getPath(src, "/", true, "") + getPath(trg, "", false, prm);
    }

    /**
     * convert to xml
     *
     * @return xml format
     */
    public List<String> toXMLlst() {
        List<String> lst = new ArrayList<String>();
        String path = "";
        for (int pos = 0; pos < data.size(); pos++) {
            extMrkLngEntry ntry = data.get(pos);
            lst.add(getMove(path, ntry.name, ntry.param) + ntry.value);
            path = ntry.name;
        }
        return lst;
    }

    /**
     * convert to xml
     *
     * @return xml format
     */
    public String toXMLstr() {
        List<String> lst = toXMLlst();
        String res = "";
        for (int i = 0; i < lst.size(); i++) {
            res += bits.trimB(lst.get(i));
        }
        return res;
    }

    /**
     * convert to string
     *
     * @return lines of string
     */
    public List<String> show() {
        List<String> l = new ArrayList<String>();
        for (int i = 0; i < data.size(); i++) {
            l.add("" + data.get(i));
        }
        return l;
    }

    /**
     * convert string to xml
     *
     * @param xml string to convert
     * @return converted xml
     */
    public static extMrkLng parseOne(String xml) {
        extMrkLng res = new extMrkLng();
        res.fromString(xml);
        return res;
    }

    /**
     * setup for html parsing
     */
    public void setup2html() {
        closed = "|br|p|img|dl|dt|dd|ul|ol|li|tr|td|table|input|button|link|meta|";
    }

    /**
     * format for html
     *
     * @param txts text
     * @param lnks links
     * @param width screen size
     * @return false on success, true on error
     */
    public boolean formatHtml(List<String> txts, List<List<Integer>> lnks, int width) {
        String txt = "";
        List<Integer> lnk = new ArrayList<Integer>();
        for (int num = 0; num < data.size(); num++) {
            extMrkLngEntry ntry = data.get(num);
            String str = ntry.value.trim();
            String tag = "/" + ntry.name.trim().toLowerCase().replaceAll(" ", "") + "/";
            if (tag.indexOf("/script/") > 0) {
                str = "";
            }
            if (tag.indexOf("/style/") > 0) {
                str = "";
            }
            tag = ntry.getTag().trim().toLowerCase();
            int pos = 0;
            boolean br = "|br|p|ul|ol|tr|dl|dt|dd".indexOf("|" + tag + "|") >= 0;
            if (tag.equals("a")) {
                if (str.length() < 1) {
                    str = "<link>";
                }
                pos = num;
            }
            if (tag.equals("link")) {
                if (str.length() < 1) {
                    str = "<link>";
                }
                pos = num;
            }
            if (tag.equals("object")) {
                if (str.length() < 1) {
                    str = "<obj>";
                }
                pos = num;
            }
            if (tag.equals("applet")) {
                if (str.length() < 1) {
                    str = "<app>";
                }
                pos = num;
            }
            if (tag.equals("title")) {
                str = "title: " + str;
                br = true;
            }
            if (tag.equals("td")) {
                str = str + " | ";
            }
            if (tag.equals("li")) {
                if (txt.length() > 0) {
                    txts.add(txt);
                    lnks.add(lnk);
                    txt = "";
                    lnk = new ArrayList<Integer>();
                }
                str = " * " + str;
            }
            if (tag.equals("img")) {
                str = "<image>";
                pos = num;
            }
            if (tag.equals("frame")) {
                str = "<frame>";
                pos = num;
            }
            if (tag.equals("iframe")) {
                str = "<iframe>";
                pos = num;
            }
            if (tag.equals("script")) {
                str = "<script>";
                pos = num;
            }
            if (tag.equals("meta")) {
                List<extMrkLngEntry> lst = decodeParams(ntry.param);
                int i = findParam(lst, "|http-equiv|");
                if (i < 0) {
                    continue;
                }
                if (!lst.get(i).value.trim().toLowerCase().equals("refresh")) {
                    continue;
                }
                str = "<refresh>";
                pos = num;
            }
            if (tag.equals("input") || tag.equals("button") || tag.equals("select")) {
                List<extMrkLngEntry> lst = decodeParams(ntry.param);
                int i = findParam(lst, "|type|");
                String a;
                if (i < 0) {
                    a = "unknown";
                } else {
                    a = lst.get(i).value.trim().toLowerCase();
                }
                if (a.equals("hidden")) {
                    continue;
                }
                if (a.equals("submit")) {
                    i = findParam(lst, "|value|");
                    if (i >= 0) {
                        str = "<" + lst.get(i).value + ">";
                    } else {
                        str = "<submit>";
                    }
                } else {
                    str = "<input>";
                }
                pos = num;
            }
            if (tag.equals("textarea")) {
                str = "<text>";
                pos = num;
            }
            if (!txt.endsWith(" ") && (txt.length() > 0)) {
                txt = txt + " ";
                padUp(lnk, txt.length(), 0);
            }
            str = decodeQuoted(str);
            for (;;) {
                if ((txt.length() + str.length()) < width) {
                    txt = txt + str;
                    padUp(lnk, txt.length(), pos);
                    if (!br) {
                        break;
                    }
                    txts.add(txt);
                    lnks.add(lnk);
                    txt = "";
                    lnk = new ArrayList<Integer>();
                    break;
                }
                String a = str.substring(0, width - txt.length());
                int i = a.lastIndexOf(" ");
                i = max(i, a.lastIndexOf(","));
                i = max(i, a.lastIndexOf("."));
                i = max(i, a.lastIndexOf(":"));
                i = max(i, a.lastIndexOf(";"));
                i = max(i, a.lastIndexOf("!"));
                i = max(i, a.lastIndexOf("?"));
                i = max(i, a.lastIndexOf("-"));
                i = max(i, a.lastIndexOf("+"));
                if (i < 0) {
                    i = a.length();
                }
                a = str.substring(0, i);
                str = str.substring(i, str.length()).trim();
                txt = txt + a;
                padUp(lnk, txt.length(), pos);
                txts.add(txt);
                lnks.add(lnk);
                txt = "";
                lnk = new ArrayList<Integer>();
            }
        }
        if (txt.length() < 1) {
            return false;
        }
        txts.add(txt);
        lnks.add(lnk);
        return false;
    }

    private void padUp(List<Integer> lst, int max, int val) {
        for (; lst.size() < max;) {
            lst.add(val);
        }
    }

    private int max(int a, int b) {
        if (a < b) {
            return b;
        } else {
            return a;
        }
    }

    /**
     * decode parameter list
     *
     * @param str string to parse
     * @return list of parameter values
     */
    public static List<extMrkLngEntry> decodeParams(String str) {
        str = str.trim();
        List<extMrkLngEntry> res = new ArrayList<extMrkLngEntry>();
        for (;;) {
            if (str.length() < 1) {
                break;
            }
            int i = str.indexOf(" ");
            int o = str.indexOf("=");
            if (i < 0) {
                i = str.length();
            }
            if (o < 0) {
                o = str.length();
            }
            if (o < i) {
                i = o;
            }
            String a = str.substring(0, i).trim();
            str = str.substring(i, str.length()).trim();
            if (!str.startsWith("=")) {
                res.add(new extMrkLngEntry(a, "", ""));
                continue;
            }
            str = str.substring(1, str.length()).trim();
            String b = "\"";
            if (str.startsWith(b)) {
                str = str.substring(1, str.length()).trim();
            } else {
                b = " ";
            }
            i = str.indexOf(b);
            if (i < 0) {
                i = str.length();
            }
            str = str + " ";
            b = str.substring(0, i);
            str = str.substring(i + 1, str.length()).trim();
            res.add(new extMrkLngEntry(a, "", b));
        }
        return res;
    }

    /**
     * encode parameters
     *
     * @param lst list of parameters
     * @return encoded string
     */
    public static String encodeParams(List<extMrkLngEntry> lst) {
        String str = "";
        for (int i = 0; i < lst.size(); i++) {
            extMrkLngEntry ntry = lst.get(i);
            str = str + " " + ntry.name + "=\"" + ntry.value + "\"";
        }
        return str;
    }

    /**
     * find parameter
     *
     * @param lst list to use
     * @param ned text to find
     * @return index, -1 if not found
     */
    public static int findParam(List<extMrkLngEntry> lst, String ned) {
        for (int i = 0; i < lst.size(); i++) {
            lst.get(i);
            if (ned.indexOf("|" + lst.get(i).name.trim().toLowerCase() + "|") >= 0) {
                return i;
            }
        }
        return -1;
    }

    /**
     * decode quoted text
     *
     * @param str string to decode
     * @return decoded
     */
    public static String decodeQuoted(String str) {
        String res = "";
        for (;;) {
            int i = str.indexOf("&");
            if (i < 0) {
                break;
            }
            res = res + str.substring(0, i);
            str = str.substring(i + 1, str.length());
            i = str.indexOf(";");
            if (i < 0) {
                break;
            }
            String a = str.substring(0, i).trim().toLowerCase();
            str = str.substring(i + 1, str.length());
            if (a.startsWith("#")) {
                a = a.substring(1, a.length());
                if (a.startsWith("x")) {
                    a = a.substring(1, a.length());
                    i = bits.fromHex(a);
                } else {
                    i = bits.str2num(a);
                }
                byte[] buf = new byte[1];
                buf[0] = (byte) (i & 0xff);
                res = res + new String(buf);
                continue;
            }
            String b = null;
            if (a.equals("quot")) {
                b = "\"";
            }
            if (a.equals("nbsp")) {
                b = " ";
            }
            if (a.equals("amp")) {
                b = "&";
            }
            if (a.equals("divide")) {
                b = "/";
            }
            if (a.equals("tilde")) {
                b = "~";
            }
            if (a.equals("times")) {
                b = "*";
            }
            if (a.equals("prod")) {
                b = "*";
            }
            if (a.equals("minus")) {
                b = "-";
            }
            if (a.equals("and")) {
                b = "&";
            }
            if (a.equals("or")) {
                b = "|";
            }
            if (a.equals("gt")) {
                b = ">";
            }
            if (a.equals("lt")) {
                b = "<";
            }
            if (a.equals("micro")) {
                b = "u";
            }
            if (a.equals("middot")) {
                b = ".";
            }
            if (a.equals("para")) {
                b = "P";
            }
            if (a.equals("plusmn")) {
                b = "+-";
            }
            if (a.equals("iquest")) {
                b = "?";
            }
            if (a.equals("euro")) {
                b = "E";
            }
            if (a.equals("pound")) {
                b = "L";
            }
            if (a.equals("reg")) {
                b = "R";
            }
            if (a.equals("copy")) {
                b = "C";
            }
            if (a.equals("not")) {
                b = "!";
            }
            if (a.equals("sect")) {
                b = "S";
            }
            if (a.equals("trade")) {
                b = "tm";
            }
            if (a.equals("yen")) {
                b = "Y";
            }
            if (a.equals("deg")) {
                b = "d";
            }
            if (a.equals("aacute")) {
                b = "a";
            }
            if (a.equals("agrave")) {
                b = "a";
            }
            if (a.equals("acirc")) {
                b = "a";
            }
            if (a.equals("aring")) {
                b = "a";
            }
            if (a.equals("atilde")) {
                b = "a";
            }
            if (a.equals("auml")) {
                b = "a";
            }
            if (a.equals("oelig")) {
                b = "oe";
            }
            if (a.equals("aelig")) {
                b = "ae";
            }
            if (a.equals("ccedil")) {
                b = "c";
            }
            if (a.equals("eacute")) {
                b = "e";
            }
            if (a.equals("egrave")) {
                b = "e";
            }
            if (a.equals("ecirc")) {
                b = "e";
            }
            if (a.equals("euml")) {
                b = "e";
            }
            if (a.equals("iacute")) {
                b = "i";
            }
            if (a.equals("igrave")) {
                b = "i";
            }
            if (a.equals("icirc")) {
                b = "i";
            }
            if (a.equals("iuml")) {
                b = "i";
            }
            if (a.equals("ntilde")) {
                b = "n";
            }
            if (a.equals("oacute")) {
                b = "o";
            }
            if (a.equals("ograve")) {
                b = "o";
            }
            if (a.equals("ocirc")) {
                b = "o";
            }
            if (a.equals("oslash")) {
                b = "o";
            }
            if (a.equals("otilde")) {
                b = "o";
            }
            if (a.equals("ouml")) {
                b = "o";
            }
            if (a.equals("szlig")) {
                b = "B";
            }
            if (a.equals("eth")) {
                b = "D";
            }
            if (a.equals("fnof")) {
                b = "f";
            }
            if (a.equals("thorn")) {
                b = "P";
            }
            if (a.equals("uacute")) {
                b = "u";
            }
            if (a.equals("ugrave")) {
                b = "u";
            }
            if (a.equals("ucirc")) {
                b = "u";
            }
            if (a.equals("uuml")) {
                b = "u";
            }
            if (a.equals("scaron")) {
                b = "s";
            }
            if (a.equals("yuml")) {
                b = "y";
            }
            if (b == null) {
                continue;
            }
            res = res + b;
        }
        return res + str;
    }

}
