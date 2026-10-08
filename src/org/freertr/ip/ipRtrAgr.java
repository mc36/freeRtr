package org.freertr.ip;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrPrefix;
import org.freertr.tab.tabIntUpdater;
import org.freertr.tab.tabLabelEntry;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRtrmapN;
import org.freertr.tab.tabRtrplcN;
import org.freertr.util.smalist;

/**
 * aggregate routes in routers
 *
 * @author matecsaba
 */
public class ipRtrAgr implements Comparable<ipRtrAgr> {

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

    public int compareTo(ipRtrAgr o) {
        return prefix.compareTo(o.prefix);
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
        smalist pathSet = new smalist();
        smalist confSet = new smalist();
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
                pathSet.appendIfNot(ntry.best.pathSet);
                pathSet.appendIfNot(ntry.best.pathSeq);
                confSet.appendIfNot(ntry.best.confSet);
                confSet.appendIfNot(ntry.best.confSeq);
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
