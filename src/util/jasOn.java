package util;

import java.util.ArrayList;
import java.util.List;

/**
 * json handler
 *
 * @author matecsaba
 */
public class jasOn {

    /**
     * original string
     */
    public String orig;

    /**
     * elements
     */
    public List<jasOnEntry> data;

    /**
     * clear new instance
     */
    public jasOn() {
        clear();
    }

    /**
     * clear all data
     */
    public void clear() {
        data = new ArrayList<jasOnEntry>();
        orig = "";
    }

    /**
     * copy from other xml
     *
     * @param src original xml to copy
     */
    public void copyBytes(jasOn src) {
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
        orig = "" + s;
        int lev = 0;
        String cur = "";
        for (;;) {
            if (s.length() < 1) {
                break;
            }
            String a = s.substring(0, 1);
            s = s.substring(1, s.length());
            if (a.equals("\"")) {
                int i = s.indexOf("\"");
                if (i < 0) {
                    cur += a;
                    continue;
                }
                jasOnEntry ntry = new jasOnEntry();
                ntry.level = lev;
                ntry.value = s.substring(0, i);
                s = s.substring(i + 1, s.length());
                data.add(ntry);
                cur = "";
                continue;
            }
            if (a.equals("{")) {
                jasOnEntry ntry = new jasOnEntry();
                ntry.level = lev;
                ntry.value = "" + cur;
                if (cur.length() > 0) {
                    data.add(ntry);
                }
                cur = "";
                lev++;
                continue;
            }
            if (a.equals("}")) {
                jasOnEntry ntry = new jasOnEntry();
                ntry.level = lev;
                ntry.value = "" + cur;
                if (cur.length() > 0) {
                    data.add(ntry);
                }
                cur = "";
                lev--;
                continue;
            }
            cur += a;
        }
        jasOnEntry ntry = new jasOnEntry();
        ntry.level = lev;
        ntry.value = "" + cur;
        if (cur.length() > 0) {
            data.add(ntry);
        }
        return lev != 0;
    }

    /**
     * convert string to json
     *
     * @param js string to convert
     * @return converted json
     */
    public static jasOn parseOne(String js) {
        jasOn res = new jasOn();
        res.fromString(js);
        return res;
    }

    /**
     * convert to json
     *
     * @return json format
     */
    public String toJSONstr() {
        List<String> lst = toJSONlst();
        String res = "";
        jasOnEntry old = new jasOnEntry();
        for (int i = 0; i < lst.size(); i++) {
            jasOnEntry ntry = data.get(i);
            res += bits.padEnd("", ntry.level - old.level, "{");
            res += bits.padEnd("", old.level - ntry.level, "}");
            res += "\"" + ntry.value + "\"";
            old = ntry;
        }
        res += bits.padEnd("", old.level, "}");
        return res;
    }

    /**
     * convert to json
     *
     * @return json format
     */
    public List<String> toJSONlst() {
        List<String> lst = new ArrayList<String>();
        for (int pos = 0; pos < data.size(); pos++) {
            jasOnEntry ntry = data.get(pos);
            lst.add(ntry.level + "," + ntry.value);
        }
        return lst;
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

}
