package util;

/**
 * interface states
 *
 * @author matecsaba
 */
public class state {

    private state() {
    }

    /**
     * interface state
     */
    public enum states {

        /**
         * line protocol down
         */
        down,
        /**
         * line protocol up
         */
        up,
        /**
         * line protocol administratively down
         */
        admin,
        /**
         * interface closed
         */
        close

    }

    /**
     * convert state to usable state (up,down,admin)
     *
     * @param stat state to convert
     * @return state converted
     */
    public static states toUsable(states stat) {
        if (stat == states.up) {
            return states.up;
        }
        if (stat == states.admin) {
            return states.admin;
        }
        return states.down;
    }

    /**
     * convert state to forceable state (up,admin)
     *
     * @param stat state to convert
     * @return state converted
     */
    public static states toForceable(states stat) {
        if (stat == states.admin) {
            return states.admin;
        }
        if (stat == states.close) {
            return states.admin;
        }
        return states.up;
    }

    /**
     * convert state where it could be in best case (up,admin)
     *
     * @param stat state to convert
     * @return state converted
     */
    public static states toWilling(states stat) {
        if (stat == states.admin) {
            return states.admin;
        }
        if (stat == states.admin) {
            return states.admin;
        }
        if (stat == states.close) {
            return states.admin;
        }
        return states.up;
    }

    /**
     * convert state to string
     *
     * @param stat state to convert
     * @return state converted
     */
    public static String conv2string(states stat) {
        String s;
        switch (stat) {
            case down:
                s = "down";
                break;
            case up:
                s = "up";
                break;
            case admin:
                s = "admin";
                break;
            case close:
                s = "closed";
                break;
            default:
                s = "unknown #" + stat;
                break;
        }
        return s;
    }

}
