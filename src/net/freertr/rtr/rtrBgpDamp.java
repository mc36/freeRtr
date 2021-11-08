package net.freertr.rtr;

import java.util.Comparator;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAll;
import net.freertr.tab.tabRtrmapN;
import net.freertr.util.bits;

/**
 * bgp4 dampening statistics
 *
 * @author matecsaba
 */
public class rtrBgpDamp implements Comparator<rtrBgpDamp> {

    /**
     * create instance
     */
    public rtrBgpDamp() {
    }

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
     * penalty
     */
    public int penalty;

    /**
     * dampened
     */
    public boolean dampened;

    /**
     * last
     */
    public long last;

    public int compare(rtrBgpDamp o1, rtrBgpDamp o2) {
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
        return rtrBgpUtil.safi2string(afi) + "|" + addrPrefix.ip2str(prefix) + " " + tabRtrmapN.rd2string(rd) + "|" + penalty + "|" + dampened + "|" + bits.timePast(last) + "|" + bits.time2str(cfgAll.timeZoneName, last + cfgAll.timeServerOffset, 3);
    }

}
