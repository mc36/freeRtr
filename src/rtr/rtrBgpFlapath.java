package rtr;

import cfg.cfgAll;
import java.util.Comparator;
import util.bits;

/**
 * bgp4 flap statistic
 *
 * @author matecsaba
 */
public class rtrBgpFlapath implements Comparator<rtrBgpFlapath> {

    /**
     * path
     */
    public String path;

    /**
     * counter
     */
    public int count;

    /**
     * last
     */
    public long last;

    public int compare(rtrBgpFlapath o1, rtrBgpFlapath o2) {
        return o1.path.compareTo(o2.path);
    }

    public String toString() {
        return count + "|" + bits.timePast(last) + "|" + bits.time2str(cfgAll.timeZoneName, last + cfgAll.timeServerOffset, 3) + "|" + path;
    }

}
