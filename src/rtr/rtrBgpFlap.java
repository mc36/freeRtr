package rtr;

import addr.addrIP;
import addr.addrPrefix;
import cfg.cfgAll;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import util.bits;

/**
 * bgp4 flap statistic
 *
 * @author matecsaba
 */
public class rtrBgpFlap implements Comparator<rtrBgpFlap> {

    /**
     * prefix
     */
    public addrPrefix<addrIP> prefix;

    /**
     * counter
     */
    public long count;

    /**
     * last
     */
    public long last;

    /**
     * paths seen
     */
    public List<String> paths = new ArrayList<String>();

    public int compare(rtrBgpFlap o1, rtrBgpFlap o2) {
        return o1.prefix.compare(o1.prefix, o2.prefix);
    }

    public String toString() {
        return addrPrefix.ip2str(prefix) + "|" + count + "|" + bits.timePast(last) + "|" + bits.time2str(cfgAll.timeZoneName, last + cfgAll.timeServerOffset, 3);
    }

}
