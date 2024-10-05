package org.freertr.user;

/**
 * one tester connection
 *
 * @author matecsaba
 */
public class userTesterCon implements Comparable<userTesterCon> {

    /**
     * local port
     */
    protected int locP;

    /**
     * remote port
     */
    protected int remP;

    /**
     * persistent process
     */
    protected userTesterPrc perP;

    /**
     * console of process
     */
    protected userTesterCon perC;

    /**
     * interface to use
     */
    protected String ifc;

    /**
     * create instance
     */
    protected userTesterCon() {
    }

    public int compareTo(userTesterCon o) {
        if (locP < o.locP) {
            return -1;
        }
        if (locP > o.locP) {
            return +1;
        }
        return 0;
    }

}
