package org.freertr.user;

import java.util.ArrayList;
import java.util.List;
import org.freertr.cfg.cfgAll;
import org.freertr.util.bits;

/**
 * one tester change
 *
 * @author matecsaba
 */
public class userTesterChg {

    /**
     * time in text format
     */
    protected final String str;

    /**
     * time in binary format
     */
    protected final long tim;

    /**
     * changes detected
     */
    protected final List<String> txt = new ArrayList<String>();

    /**
     * version information
     */
    protected String ver;

    /**
     * create instance
     *
     * @param a time
     */
    public userTesterChg(String a) {
        a = a.replaceAll(" ", "_");
        str = a;
        tim = bits.str2time(cfgAll.timeZoneName, a);
    }

}
