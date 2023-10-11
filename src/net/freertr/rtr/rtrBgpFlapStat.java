package net.freertr.rtr;

import java.util.Comparator;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAll;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabRouteUtil;
import net.freertr.util.bits;

/**
 * bgp4 flap statistic
 *
 * @author matecsaba
 */
public class rtrBgpFlapStat implements Comparator<rtrBgpFlapStat> {

    /**
     * create instance
     *
     * @param a afi
     * @param r rd
     * @param p prefix
     */
    public rtrBgpFlapStat(int a, long r, addrPrefix<addrIP> p) {
        afi = a;
        rd = r;
        prefix = p.copyBytes();
    }

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

    public int compare(rtrBgpFlapStat o1, rtrBgpFlapStat o2) {
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
    public String toIncons() {
        String s = "";
        for (int i = 0; i < infos.size(); i++) {
            s += " " + infos.get(i).info;
        }
        return addrPrefix.ip2str(prefix) + " " + tabRouteUtil.rd2string(rd) + "|" + s.trim();
    }

}
