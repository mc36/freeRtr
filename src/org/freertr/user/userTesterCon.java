package org.freertr.user;

import java.util.Comparator;

/**
 * one tester connection
 *
 * @author matecsaba
 */
public class userTesterCon implements Comparator<userTesterCon> {

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

    public int compare(userTesterCon o1, userTesterCon o2) {
        if (o1.locP < o2.locP) {
            return -1;
        }
        if (o1.locP > o2.locP) {
            return +1;
        }
        return 0;
    }

}
