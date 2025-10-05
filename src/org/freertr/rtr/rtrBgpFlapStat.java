package org.freertr.rtr;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.clnt.clntWhois;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabRouteUtil;
import org.freertr.util.bits;

/**
 * bgp4 flap statistic
 *
 * @author matecsaba
 */
public class rtrBgpFlapStat implements Comparable<rtrBgpFlapStat> {

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
    public tabGen<rtrBgpFlapLst> paths = new tabGen<rtrBgpFlapLst>();

    /**
     * paths seen
     */
    public tabGen<rtrBgpFlapStr> infos = new tabGen<rtrBgpFlapStr>();

    /**
     * create instance
     *
     * @param i afi
     * @param r rd
     * @param p prefix
     */
    public rtrBgpFlapStat(int i, long r, addrPrefix<addrIP> p) {
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
    public rtrBgpFlapStat(int i, long r, addrIP p) {
        idx = i;
        rd = r;
        prefix = new addrPrefix<addrIP>(p.copyBytes(), addrIP.size * 8);
    }

    public int compareTo(rtrBgpFlapStat o) {
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
     * @return paths
     */
    public String toInconsStr() {
        String s = "";
        for (int i = 0; i < infos.size(); i++) {
            s += " " + infos.get(i).info;
        }
        return addrPrefix.ip2str(prefix) + " " + tabRouteUtil.rd2string(rd) + "|" + s.trim();
    }

    /**
     * get inconsistency paths
     *
     * @return paths
     */
    public String toInconsPth() {
        String s = "";
        for (int i = 0; i < paths.size(); i++) {
            rtrBgpFlapLst ntry = paths.get(i);
            for (int o = 0; o < ntry.lst.size(); o++) {
                int p = ntry.lst.get(o);
                s += " " + clntWhois.asn2mixed(p, true);
            }
        }
        return addrPrefix.ip2str(prefix) + " " + tabRouteUtil.rd2string(rd) + "|" + s.trim();
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
     * @return paths
     */
    public String toNhTrnsit() {
        String s = "";
        for (int i = 0; i < paths.size(); i++) {
            rtrBgpFlapLst ntry = paths.get(i);
            for (int o = 0; o < ntry.lst.size(); o++) {
                int p = ntry.lst.get(o);
                s += " " + clntWhois.asn2mixed(p, true);
            }
        }
        return prefix.network + "|" + paths.size() + "|" + s.trim();
    }

}
