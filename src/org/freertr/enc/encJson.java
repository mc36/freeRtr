package org.freertr.enc;

import java.util.ArrayList;
import java.util.List;
import org.freertr.util.bits;

/**
 * json handler
 *
 * @author matecsaba
 */
public class encJson {

    /**
     * original string
     */
    public String orig;

    /**
     * elements
     */
    public List<encJsonEntry> data;

    /**
     * clear new instance
     */
    public encJson() {
        clear();
    }

    /**
     * clear all data
     */
    public void clear() {
        data = new ArrayList<encJsonEntry>();
        orig = "";
    }

    /**
     * copy from other json
     *
     * @param src original json to copy
     */
    public void copyBytes(encJson src) {
        clear();
        orig = "" + src.orig;
        for (int i = 0; i < src.data.size(); i++) {
            data.add(src.data.get(i).copyBytes());
        }
    }

    /**
     * convert strings to json
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
     * convert string to json
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
            if (a.equals(" ")) {
                continue;
            }
            if (a.equals(":")) {
                encJsonEntry ntry = new encJsonEntry();
                ntry.level = lev;
                ntry.data = "" + cur;
                if (cur.length() > 0) {
                    data.add(ntry);
                }
                cur = "";
                continue;
            }
            if (a.equals(",")) {
                encJsonEntry ntry = new encJsonEntry();
                ntry.level = lev;
                ntry.data = "" + cur;
                if (cur.length() > 0) {
                    data.add(ntry);
                }
                cur = "";
                continue;
            }
            if (a.equals("\"")) {
                int i = s.indexOf("\"");
                if (i < 0) {
                    cur += a;
                    continue;
                }
                encJsonEntry ntry = new encJsonEntry();
                ntry.level = lev;
                ntry.data = s.substring(0, i);
                s = s.substring(i + 1, s.length());
                data.add(ntry);
                cur = "";
                continue;
            }
            if (a.equals("{")) {
                encJsonEntry ntry = new encJsonEntry();
                ntry.level = lev;
                ntry.data = "" + cur;
                if (cur.length() > 0) {
                    data.add(ntry);
                }
                cur = "";
                lev++;
                continue;
            }
            if (a.equals("}")) {
                encJsonEntry ntry = new encJsonEntry();
                ntry.level = lev;
                ntry.data = "" + cur;
                if (cur.length() > 0) {
                    data.add(ntry);
                }
                cur = "";
                lev--;
                continue;
            }
            if (a.equals("[")) {
                encJsonEntry ntry = new encJsonEntry();
                ntry.level = lev;
                ntry.data = "" + cur;
                if (cur.length() > 0) {
                    data.add(ntry);
                }
                cur = "";
                lev++;
                continue;
            }
            if (a.equals("]")) {
                encJsonEntry ntry = new encJsonEntry();
                ntry.level = lev;
                ntry.data = "" + cur;
                if (cur.length() > 0) {
                    data.add(ntry);
                }
                cur = "";
                lev--;
                continue;
            }
            cur += a;
        }
        encJsonEntry ntry = new encJsonEntry();
        ntry.level = lev;
        ntry.data = "" + cur;
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
    public static encJson parseOne(String js) {
        encJson res = new encJson();
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
        encJsonEntry old = new encJsonEntry();
        for (int i = 0; i < lst.size(); i++) {
            encJsonEntry ntry = data.get(i);
            res += bits.padEnd("", ntry.level - old.level, "{");
            res += bits.padEnd("", old.level - ntry.level, "}");
            res += "\"" + ntry.data + "\"";
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
            encJsonEntry ntry = data.get(pos);
            lst.add(ntry.level + "," + ntry.data);
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

    /**
     * find value
     *
     * @param s string
     * @return index, -1 if not found
     */
    public int findValue(String s) {
        for (int i = 0; i < data.size(); i++) {
            encJsonEntry ntry = data.get(i);
            if (!s.equals(ntry.data)) {
                continue;
            }
            return i;
        }
        return -1;
    }

    /**
     * get value
     *
     * @param i index to read
     * @return value, null if not found
     */
    public String getValue(int i) {
        if (i < 0) {
            return null;
        }
        if (i >= data.size()) {
            return null;
        }
        return data.get(i).data;
    }


    /**
     * get value
     *
     * @param i index to read
     * @return value, null if not found
     */
    public encJsonEntry getData(int i) {
        if (i < 0) {
            return null;
        }
        if (i >= data.size()) {
            return null;
        }
        return data.get(i);
    }

    /**
     * get one value
     *
     * @param a json
     * @param t tag
     * @return value, null if not found
     */
    public static String getValue(String a, String t) {
        int i = a.indexOf("\"" + t + "\":\"");
        if (i < 0) {
            return null;
        }
        a = a.substring(i + t.length() + 4, a.length());
        i = a.indexOf("\"");
        if (i < 0) {
            return null;
        }
        return a.substring(0, i);
    }

}
