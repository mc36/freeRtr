package org.freertr.serv;

import org.freertr.cfg.cfgAll;
import org.freertr.util.bits;

/**
 * http directory statistic
 *
 * @author matecsaba
 */
public class servHttpDirs implements Comparable<servHttpDirs> {

    /**
     * name of entry
     */
    protected final String name;

    /**
     * number of entry
     */
    protected int count;

    /**
     * bytes in entries
     */
    protected long bytes;

    /**
     * lowest size
     */
    protected long sizMin;

    /**
     * highest size
     */
    protected long sizMax;

    /**
     * oldest time
     */
    protected long timMin;

    /**
     * newest time
     */
    protected long timMax;

    /**
     * create instance
     *
     * @param n name
     */
    public servHttpDirs(String n) {
        name = n;
        sizMin = Long.MAX_VALUE;
        sizMax = Long.MIN_VALUE;
        timMin = Long.MAX_VALUE;
        timMax = Long.MIN_VALUE;
    }

    public int compareTo(servHttpDirs o) {
        return name.compareTo(o.name);
    }

    public String toString() {
        String smi = "" + sizMin;
        String sma = "" + sizMax;
        String tmi = bits.time2str(cfgAll.timeZoneName, timMin, 3);
        String tma = bits.time2str(cfgAll.timeZoneName, timMax, 3);
        if (count < 1) {
            smi = "-";
            sma = "-";
            tmi = "-";
            tma = "-";
        }
        return "<tr><td>" + name + "</td><td>" + count + "</td><td>" + bytes + "</td><td>" + smi + "</td><td>" + sma + "</td><td>" + tmi + "</td><td>" + tma + "</td></tr>\n";
    }

    /**
     * update
     *
     * @param siz size
     * @param tim time
     */
    public void update(long siz, long tim) {
        count++;
        bytes += siz;
        if (siz < sizMin) {
            sizMin = siz;
        }
        if (siz > sizMax) {
            sizMax = siz;
        }
        if (tim < timMin) {
            timMin = tim;
        }
        if (tim > timMax) {
            timMax = tim;
        }
    }

}
