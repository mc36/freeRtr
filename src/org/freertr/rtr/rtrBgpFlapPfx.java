package org.freertr.rtr;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.clnt.clntWhois;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabRouteUtil;
import org.freertr.util.bits;

/**
 * bgp4 flap prefix
 *
 * @param <T> type of elements in the list
 * @author matecsaba
 */
public class rtrBgpFlapPfx<T extends Comparable<? super T>> implements Comparable<rtrBgpFlapPfx<T>> {

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
    public tabGen<T> paths = new tabGen<T>();

    /**
     * create instance
     *
     * @param i afi
     * @param r rd
     * @param p prefix
     */
    public rtrBgpFlapPfx(int i, long r, addrPrefix<addrIP> p) {
        idx = i;
        rd = r;
        prefix = p.copyBytes();
    }

    /**
     * create instance
     *
     * @param i afi
     * @param r rd
     * @param p prefix
     */
    public rtrBgpFlapPfx(int i, long r, addrIP p) {
        idx = i;
        rd = r;
        prefix = new addrPrefix<addrIP>(p.copyBytes(), addrIP.size * 8);
    }

    public int compareTo(rtrBgpFlapPfx<T> o) {
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

    /**
     * get flap statistics
     *
     * @return string
     */
    public String toFlaps() {
        return addrPrefix.ip2str(prefix) + " " + tabRouteUtil.rd2string(rd) + "|" + count + "|" + paths.size() + "|" + bits.timePast(last) + "|" + bits.time2str(cfgAll.timeZoneName, last + cfgAll.timeServerOffset, 3);
    }

    /**
     * get inconsistency paths
     *
     * @param stat entry
     * @return paths
     */
    public static String toInconsStr(rtrBgpFlapPfx<addrIP> stat) {
        String s = "";
        for (int i = 0; i < stat.paths.size(); i++) {
            s += " " + stat.paths.get(i);
        }
        return addrPrefix.ip2str(stat.prefix) + " " + tabRouteUtil.rd2string(stat.rd) + "|" + s.trim();
    }

    /**
     * get inconsistency paths
     *
     * @param stat entry
     * @return paths
     */
    public static String toInconsPth(rtrBgpFlapPfx<rtrBgpFlapLst> stat) {
        String s = "";
        for (int i = 0; i < stat.paths.size(); i++) {
            rtrBgpFlapLst ntry = stat.paths.get(i);
            for (int o = 0; o < ntry.lst.size(); o++) {
                int p = ntry.lst.get(o);
                s += " " + clntWhois.asn2mixed(p, true);
            }
        }
        return addrPrefix.ip2str(stat.prefix) + " " + tabRouteUtil.rd2string(stat.rd) + "|" + s.trim();
    }

    /**
     * get usage of next hops
     *
     * @return paths
     */
    public String toNhPrfxes() {
        return prefix.network + "|" + count;
    }

    /**
     * get usage of next hops
     *
     * @param stat entry
     * @return paths
     */
    public static String toNhTrnsit(rtrBgpFlapPfx<rtrBgpFlapLst> stat) {
        String s = "";
        for (int i = 0; i < stat.paths.size(); i++) {
            rtrBgpFlapLst ntry = stat.paths.get(i);
            for (int o = 0; o < ntry.lst.size(); o++) {
                int p = ntry.lst.get(o);
                s += " " + clntWhois.asn2mixed(p, true);
            }
        }
        return stat.prefix.network + "|" + stat.paths.size() + "|" + s.trim();
    }

}
