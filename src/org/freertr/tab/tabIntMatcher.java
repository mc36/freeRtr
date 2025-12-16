package org.freertr.tab;

import org.freertr.util.bits;

/**
 * matches to an integer
 *
 * @author matecsaba
 */
public class tabIntMatcher {

    /**
     * create instance
     */
    public tabIntMatcher() {
    }

    /**
     * match criteria
     */
    public actionType action = actionType.always;

    /**
     * list of match criterias
     */
    public enum actionType {
        /**
         * always
         */
        always,
        /**
         * exact
         */
        xact,
        /**
         * range
         */
        range,
        /**
         * mask
         */
        mask,
        /**
         * list
         */
        list
    }

    /**
     * range lower value (inclusive)
     */
    public int rangeMin;

    /**
     * range upper value (inclusive)
     */
    public int rangeMax;

    /**
     * list of allowed values;
     */
    public tabGen<tabIntMatcherVal> allowed;

    /**
     * copy the matcher
     *
     * @return a copy
     */
    public tabIntMatcher copyBytes() {
        tabIntMatcher r = new tabIntMatcher();
        r.action = action;
        r.rangeMin = rangeMin;
        r.rangeMax = rangeMax;
        if (allowed != null) {
            r.allowed = new tabGen<tabIntMatcherVal>(allowed);
        }
        return r;
    }

    /**
     * test if a value matches to this matcher
     *
     * @param i value to test
     * @return true if matches, false if not
     */
    public boolean matches(int i) {
        switch (action) {
            case always:
                return true;
            case xact:
                return (i == rangeMin);
            case range:
                return ((i >= rangeMin) && (i <= rangeMax));
            case mask:
                return (i & rangeMax) == rangeMin;
            case list:
                return allowed.find(new tabIntMatcherVal(i)) != null;
            default:
                return false;
        }
    }

    /**
     * clear matching criteria to always
     */
    public void set2always() {
        action = actionType.always;
        rangeMin = 0;
        rangeMax = 0;
    }

    /**
     * convert string to address
     *
     * @param s string to convert
     * @return true if error happened
     */
    public boolean fromString(String s) {
        s = s.trim().toLowerCase();
        if (s.equals("all")) {
            set2always();
            return false;
        }
        int i = s.indexOf("&");
        if (i >= 0) {
            action = actionType.mask;
            rangeMin = bits.str2num(s.substring(0, i).trim());
            rangeMax = bits.str2num(s.substring(i + 1, s.length()).trim());
            rangeMin &= rangeMax;
            return false;
        }
        i = s.indexOf("-");
        if (i > 0) {
            action = actionType.range;
            rangeMin = bits.str2num(s.substring(0, i).trim());
            rangeMax = bits.str2num(s.substring(i + 1, s.length()).trim());
            return false;
        }
        i = s.indexOf(",");
        if (i >= 0) {
            allowed = new tabGen<tabIntMatcherVal>();
            for (;;) {
                i = s.indexOf(",");
                String a;
                if (i < 0) {
                    a = s;
                    s = "";
                } else {
                    a = s.substring(0, i);
                    s = s.substring(i + 1, s.length());
                }
                allowed.add(new tabIntMatcherVal(bits.str2num(a)));
                if (i < 0) {
                    break;
                }
            }
            action = actionType.list;
            rangeMin = allowed.get(0).val;
            rangeMax = allowed.get(allowed.size() - 1).val;
            return false;
        }
        action = actionType.xact;
        rangeMin = bits.str2num(s);
        rangeMax = rangeMin;
        return false;
    }

    /**
     * set match exact value
     *
     * @param val value
     */
    public void setExact(int val) {
        action = actionType.xact;
        rangeMin = val;
        rangeMax = rangeMin;
    }

    public String toString() {
        switch (action) {
            case always:
                return "all";
            case xact:
                return bits.num2str(rangeMin);
            case range:
                return bits.num2str(rangeMin) + "-" + bits.num2str(rangeMax);
            case mask:
                return bits.num2str(rangeMin) + "&" + bits.num2str(rangeMax);
            case list:
                String a = "";
                for (int i = 0; i < allowed.size(); i++) {
                    a += "," + bits.num2str(allowed.get(i).val);
                }
                if (a.length() > 0) {
                    a = a.substring(1, a.length());
                }
                return a;
            default:
                return "unknown";
        }
    }

}

class tabIntMatcherVal implements Comparable<tabIntMatcherVal> {

    public final int val;

    public tabIntMatcherVal(int i) {
        val = i;
    }

    public int compareTo(tabIntMatcherVal o) {
        if (val < o.val) {
            return -1;
        }
        if (val > o.val) {
            return +1;
        }
        return 0;
    }

}
