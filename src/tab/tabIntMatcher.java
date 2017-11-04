package tab;

import util.bits;

/**
 * matches to an integer
 *
 * @author matecsaba
 */
public class tabIntMatcher {

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
        range
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
        int i = s.indexOf("-");
        if (i >= 0) {
            action = actionType.range;
            rangeMin = bits.str2num(s.substring(0, i).trim());
            rangeMax = bits.str2num(s.substring(i + 1, s.length()).trim());
            return false;
        }
        action = actionType.xact;
        rangeMin = bits.str2num(s);
        rangeMax = rangeMin;
        return false;
    }

    public String toString() {
        switch (action) {
            case always:
                return "all";
            case xact:
                return "" + rangeMin;
            case range:
                return rangeMin + "-" + rangeMax;
            default:
                return "unknown";
        }
    }

}
