package net.freertr.user;

import java.util.Comparator;

/**
 * one tester console
 *
 * @author matecsaba
 */
public class userTesterCon implements Comparator<userTesterCon> {

    public int locP;

    public int remP;

    public userTesterPrc perP;

    public userTesterCon perC;

    public String ifc;

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
