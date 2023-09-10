package net.freertr.user;

import java.util.Comparator;
import net.freertr.util.syncInt;

/**
 * one tester feature
 *
 * @author matecsaba
 */
public class userTesterFtr implements Comparator<userTesterFtr> {

    public final String fil;

    public syncInt lck = new syncInt(0);

    public int ret;

    public int ran;

    public boolean res;

    public String ftr;

    public String htm;

    public String csv;

    public userTesterFtr(String fn) {
        fil = fn;
    }

    public int compare(userTesterFtr o1, userTesterFtr o2) {
        return o1.fil.compareTo(o2.fil);
    }

    public String getter(int mod) {
        switch (mod) {
            case 1: // filename
                return fil;
            case 2: // feature
                return ftr;
            case 3: // html
                return htm;
            case 4: // csv
                return csv;
            case 5: // result
                return "" + res;
            case 6: // retries
                return "" + ret;
            default:
                return null;
        }
    }

}
