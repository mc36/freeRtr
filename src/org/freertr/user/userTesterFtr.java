package org.freertr.user;

import java.util.Comparator;
import org.freertr.util.syncInt;

/**
 * one tester feature
 *
 * @author matecsaba
 */
public class userTesterFtr implements Comparator<userTesterFtr> {

    /**
     * name of file
     */
    protected final String fil;

    /**
     * locker
     */
    protected final syncInt lck = new syncInt(0);

    /**
     * retries
     */
    protected int ret;

    /**
     * already run
     */
    protected int ran;

    /**
     * result
     */
    protected boolean res;

    /**
     * feature name
     */
    protected String ftr;

    /**
     * html line
     */
    protected String htm;

    /**
     * csv line
     */
    protected String csv;

    /**
     * create instance
     *
     * @param fn name of file
     */
    protected userTesterFtr(String fn) {
        fil = fn;
    }

    public int compare(userTesterFtr o1, userTesterFtr o2) {
        return o1.fil.compareTo(o2.fil);
    }

    /**
     * get one line
     *
     * @param mod mode
     * @return line
     */
    protected String getter(int mod) {
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
