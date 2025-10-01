package org.freertr.rtr;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.tab.tabRouteUtil;
import org.freertr.util.bits;

/**
 * bgp4 dampening statistics
 *
 * @author matecsaba
 */
public class rtrBgpDamp implements Comparable<rtrBgpDamp> {
    
    /**
     * address family
     */
    public final long mask;

    /**
     * address family
     */
    public final int afi;

    /**
     * route distinguisher
     */
    public final long rd;

    /**
     * prefix
     */
    public final addrPrefix<addrIP> prefix;

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

    /**
     * create instance
     *
     * @param m afi
     * @param a afi
     * @param r rd
     * @param p prefix
     */
    public rtrBgpDamp(long m, int a, long r, addrPrefix<addrIP> p) {
        mask = m;
        afi = a;
        rd = r;
        prefix = p.copyBytes();
    }

    public int compareTo(rtrBgpDamp o) {
        if (mask < o.mask) {
            return -1;
        }
        if (mask > o.mask) {
            return +1;
        }
        if (rd < o.rd) {
            return -1;
        }
        if (rd > o.rd) {
            return +1;
        }
        return prefix.compareTo(o.prefix);
    }

    public String toString() {
        return rtrBgpUtil.safi2string(afi) + "|" + addrPrefix.ip2str(prefix) + " " + tabRouteUtil.rd2string(rd) + "|" + penalty + "|" + dampened + "|" + bits.timePast(last) + "|" + bits.time2str(cfgAll.timeZoneName, last + cfgAll.timeServerOffset, 3);
    }

}
