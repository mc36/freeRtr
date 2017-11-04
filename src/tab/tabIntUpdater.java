package tab;

import util.bits;

/**
 * manipulates an integer
 *
 * @author matecsaba
 */
public class tabIntUpdater {

    /**
     * set action
     */
    public actionType action = actionType.nothing;

    private enum actionType {

        nothing,
        setter,
        adder,
        suber

    }

    /**
     * updater value
     */
    public int value = 0;

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
                return value;
            case adder:
                return i + value;
            case suber:
                return i - value;
            default:
                return i;
        }
    }

    /**
     * clear to unchange state
     */
    public void set2unchange() {
        action = actionType.nothing;
        value = 0;
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
        int i = s.indexOf(" ");
        if (i < 0) {
            return true;
        }
        String a = s.substring(0, i).trim().toLowerCase();
        i = bits.str2num(s.substring(i + 1, s.length()).trim());
        if (a.equals("set")) {
            action = actionType.setter;
            value = i;
            return false;
        }
        if (a.equals("add")) {
            action = actionType.adder;
            value = i;
            return false;
        }
        if (a.equals("sub")) {
            action = actionType.suber;
            value = i;
            return false;
        }
        return true;
    }

    public String toString() {
        switch (action) {
            case nothing:
                return "leave";
            case setter:
                return "set " + value;
            case adder:
                return "add " + value;
            case suber:
                return "sub " + value;
            default:
                return "unknown";
        }
    }

}
