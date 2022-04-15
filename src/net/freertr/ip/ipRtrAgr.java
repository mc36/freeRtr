package net.freertr.ip;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrPrefix;
import net.freertr.tab.tabIntUpdater;
import net.freertr.tab.tabLabelEntry;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabPrfxlstN;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRtrmapN;
import net.freertr.tab.tabRtrplcN;

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
     * metric
     */
    public tabIntUpdater metric;

    /**
     * tag
     */
    public tabIntUpdater tag;

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
     * @param src source table
     * @param trg target table
     * @param lab label to use
     * @param agrR aggregator router
     * @param agrA aggregator as
     * @param rtrT router type
     * @param rtrN router number
     */
    public void filter(int afi, tabRoute<addrIP> src, tabRoute<addrIP> trg, tabLabelEntry lab, addrIPv4 agrR, int agrA, tabRouteAttr.routeType rtrT, int rtrN) {
        int cnt = 0;
        List<Integer> pathSet = new ArrayList<Integer>();
        List<Integer> confSet = new ArrayList<Integer>();
        for (int i = src.size() - 1; i >= 0; i--) {
            tabRouteEntry<addrIP> ntry = src.get(i);
            if (!prefix.supernet(ntry.prefix, true)) {
                continue;
            }
            if (prflst != null) {
                if (!prflst.matches(afi, 0, ntry)) {
                    continue;
                }
            }
            if (aspath) {
                addAll(pathSet, ntry.best.pathSet);
                addAll(pathSet, ntry.best.pathSeq);
                addAll(confSet, ntry.best.confSet);
                addAll(confSet, ntry.best.confSeq);
            }
            if (summary) {
                src.del(ntry);
            }
            cnt++;
        }
        if (cnt < 1) {
            return;
        }
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = prefix.copyBytes();
        ntry.best.aggrAs = agrA;
        if (agrR != null) {
            addrIP adr = new addrIP();
            adr.fromIPv4addr(agrR);
            ntry.best.aggrRtr = adr;
        }
        ntry.best.pathSet = pathSet;
        ntry.best.confSet = confSet;
        ntry.best.atomicAggr = !aspath;
        ntry.best.labelLoc = lab;
        ntry.best.rouTyp = rtrT;
        ntry.best.protoNum = rtrN;
        if (metric != null) {
            ntry.best.metric = metric.update(ntry.best.metric);
        }
        if (tag != null) {
            ntry.best.tag = tag.update(ntry.best.tag);
        }
        tabRoute.addUpdatedEntry(tabRoute.addType.better, trg, afi, 0, ntry, true, roumap, rouplc, null);
    }

}
