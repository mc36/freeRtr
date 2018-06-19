package rtr;

import addr.addrIP;
import addr.addrPrefix;
import cfg.cfgAll;
import java.util.Comparator;
import tab.tabGen;
import tab.tabRtrmapN;
import util.bits;

/**
 * bgp4 flap statistic
 *
 * @author matecsaba
 */
public class rtrBgpFlap implements Comparator<rtrBgpFlap> {

    /**
     * address family
     */
    public int afi;

    /**
     * route distinguisher
     */
    public long rd;

    /**
     * prefix
     */
    public addrPrefix<addrIP> prefix;

    /**
     * counter
     */
    public int count;

    /**
     * last
     */
    public long last;

    /**
     * paths seen
     */
    public tabGen<rtrBgpFlapath> paths = new tabGen<rtrBgpFlapath>();

    public int compare(rtrBgpFlap o1, rtrBgpFlap o2) {
        if (o1.afi < o2.afi) {
            return -1;
        }
        if (o1.afi > o2.afi) {
            return +1;
        }
        if (o1.rd < o2.rd) {
            return -1;
        }
        if (o1.rd > o2.rd) {
            return +1;
        }
        return o1.prefix.compare(o1.prefix, o2.prefix);
    }

    public String toString() {
        return addrPrefix.ip2str(prefix) + " " + tabRtrmapN.rd2string(rd) + "|" + count + "|" + paths.size() + "|" + bits.timePast(last) + "|" + bits.time2str(cfgAll.timeZoneName, last + cfgAll.timeServerOffset, 3);
    }

    /**
     * get all the paths
     *
     * @return paths
     */
    public String getPaths() {
        if (paths.size() < 1) {
            return "";
        }
        String s = "";
        for (int i = 0; i < paths.size(); i++) {
            s += " " + paths.get(i).path;
        }
        return s.substring(1, s.length());
    }

}
