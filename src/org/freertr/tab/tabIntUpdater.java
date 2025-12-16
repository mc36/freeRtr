package org.freertr.tab;

import org.freertr.util.bits;

/**
 * manipulates an integer
 *
 * @author matecsaba
 */
public class tabIntUpdater {

    /**
     * create instance
     */
    public tabIntUpdater() {
    }

    /**
     * set action
     */
    public actionType action = actionType.nothing;

    /**
     * possible actions
     */
    public enum actionType {
        /**
         * do nothing
         */
        nothing,
        /**
         * constant
         */
        setter,
        /**
         * addition
         */
        adder,
        /**
         * subtract
         */
        suber,
        /**
         * and
         */
        ander,
        /**
         * or
         */
        orer,
        /**
         * xor
         */
        xorer,
    }

    /**
     * updater value
     */
    public int val = 0;

    /**
     * update value by this updater
     *
     * @param i value to update
     * @return updated value
     */
    public int update(int i) {
        switch (action) {
            case nothing:
                return i;
            case setter:
                return val;
            case adder:
                return i + val;
            case suber:
                return i - val;
            case ander:
                return i & val;
            case orer:
                return i | val;
            case xorer:
                return i ^ val;
            default:
                return i;
        }
    }

    /**
     * clear to unchange state
     */
    public void set2unchange() {
        action = actionType.nothing;
        val = 0;
    }

    /**
     * convert string to address
     *
     * @param s string to convert
     * @return true if error happened
     */
    public boolean fromString(String s) {
        s = s.trim().toLowerCase();
        if (s.equals("leave")) {
            set2unchange();
            return false;
        }
        s = s.trim();
        if (s.length() < 1) {
            return false;
        }
        val = bits.str2num(s.substring(1, s.length()));
        if (s.startsWith("=")) {
            action = actionType.setter;
            return false;
        }
        if (s.startsWith("+")) {
            action = actionType.adder;
            return false;
        }
        if (s.startsWith("-")) {
            action = actionType.suber;
            return false;
        }
        if (s.startsWith("&")) {
            action = actionType.ander;
            return false;
        }
        if (s.startsWith("|")) {
            action = actionType.orer;
            return false;
        }
        if (s.startsWith("^")) {
            action = actionType.xorer;
            return false;
        }
        val = bits.str2num(s);
        action = actionType.setter;
        return false;
    }

    public String toString() {
        switch (action) {
            case nothing:
                return "leave";
            case setter:
                return "" + val;
            case adder:
                return "+" + val;
            case suber:
                return "-" + val;
            case ander:
                return "&" + val;
            case orer:
                return "|" + val;
            case xorer:
                return "^" + val;
            default:
                return "unknown";
        }
    }

}
