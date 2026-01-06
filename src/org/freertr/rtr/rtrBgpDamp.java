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
    public final int idx;

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
     * @param i afi
     * @param r rd
     * @param p prefix
     */
    public rtrBgpDamp(int i, long r, addrPrefix<addrIP> p) {
        idx = i;
        rd = r;
        prefix = p.copyBytes();
    }

    public int compareTo(rtrBgpDamp o) {
        if (idx < o.idx) {
            return -1;
        }
        if (idx > o.idx) {
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
        return rtrBgpParam.idx2string(idx) + "|" + addrPrefix.ip2str(prefix) + " " + tabRouteUtil.rd2string(rd) + "|" + penalty + "|" + dampened + "|" + bits.timePast(last) + "|" + bits.time2str(cfgAll.timeZoneName, last + cfgAll.timeServerOffset, 3);
    }

    /**
     * logger result
     *
     * @return string
     */
    public String toLogRes() {
        return rtrLogger.afi2str(idx) + "|" + rtrLogger.prf2str(idx, prefix) + "|" + penalty + "|" + bits.timePast(last) + "|" + bits.time2str(cfgAll.timeZoneName, last + cfgAll.timeServerOffset, 3);
    }

}
