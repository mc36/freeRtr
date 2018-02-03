package ip;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrPrefix;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import tab.tabLabelNtry;
import tab.tabListing;
import tab.tabPrfxlstN;
import tab.tabRoute;
import tab.tabRouteEntry;
import tab.tabRtrmapN;
import tab.tabRtrplcN;

/**
 * aggregate routes in routers
 *
 * @author matecsaba
 */
public class ipRtrAgr implements Comparator<ipRtrAgr> {

    /**
     * prefix to import
     */
    public final addrPrefix<addrIP> prefix;

    /**
     * prefix list
     */
    public tabListing<tabPrfxlstN, addrIP> prflst;

    /**
     * route map
     */
    public tabListing<tabRtrmapN, addrIP> roumap;

    /**
     * route policy
     */
    public tabListing<tabRtrplcN, addrIP> rouplc;

    /**
     * as path
     */
    public boolean aspath;

    /**
     * summary
     */
    public boolean summary;

    /**
     * create aggregation
     *
     * @param prf prefix to aggregate
     */
    public ipRtrAgr(addrPrefix<addrIP> prf) {
        prefix = prf.copyBytes();
    }

    public int compare(ipRtrAgr o1, ipRtrAgr o2) {
        return o1.prefix.compare(o1.prefix, o2.prefix);
    }

    private void addAll(List<Integer> trg, List<Integer> src) {
        if (src == null) {
            return;
        }
        for (int i = 0; i < src.size(); i++) {
            int o = src.get(i);
            if (trg.indexOf(o) >= 0) {
                return;
            }
            trg.add(o);
        }
    }

    /**
     * filter by this aggregation
     *
     * @param afi address family
     * @param tab table to update
     * @param hop next hop
     * @param lab label to use
     * @param src route source
     * @param agrR aggregator router
     * @param agrA aggregator as
     */
    public void filter(int afi, tabRoute<addrIP> tab, addrIP hop, tabLabelNtry lab, int src, addrIPv4 agrR, int agrA) {
        int cnt = 0;
        List<Integer> pathSet = new ArrayList<Integer>();
        List<Integer> confSet = new ArrayList<Integer>();
        for (int i = tab.size() - 1; i >= 0; i--) {
            tabRouteEntry<addrIP> ntry = tab.get(i);
            if (!prefix.matches(ntry.prefix.broadcast)) {
                continue;
            }
            if (prflst != null) {
                if (prflst.find(afi, ntry) == null) {
                    continue;
                }
            }
            if (aspath) {
                addAll(pathSet, ntry.pathSet);
                addAll(pathSet, ntry.pathSeq);
                addAll(confSet, ntry.confSet);
                addAll(confSet, ntry.confSeq);
            }
            if (summary) {
                tab.del(ntry);
            }
            cnt++;
        }
        if (cnt < 1) {
            return;
        }
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = prefix.copyBytes();
        ntry.aggrAs = agrA;
        if (agrR != null) {
            addrIP adr = new addrIP();
            adr.fromIPv4addr(agrR);
            ntry.aggrRtr = adr;
        }
        if (hop != null) {
            ntry.nextHop = hop.copyBytes();
        }
        ntry.pathSet = pathSet;
        ntry.confSet = confSet;
        ntry.atomicAggr = !aspath;
        ntry.labelLoc = lab;
        ntry.rouSrc = src;
        if (roumap != null) {
            ntry = roumap.update(afi, ntry, false);
        }
        if (ntry == null) {
            return;
        }
        tab.add(tabRoute.addType.better, ntry, false, true);
    }

}
