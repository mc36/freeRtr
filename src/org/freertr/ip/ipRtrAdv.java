package org.freertr.ip;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.tab.tabIntUpdater;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRtrmapN;
import org.freertr.tab.tabRtrplcN;

/**
 * prefix import into routers
 *
 * @author matecsaba
 */
public class ipRtrAdv implements Comparable<ipRtrAdv> {

    /**
     * prefix to import
     */
    public final addrPrefix<addrIP> prefix;

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
     * ecmp mode
     */
    public boolean ecmp;

    /**
     * create advertisement
     *
     * @param prf prefix to redistribute
     */
    public ipRtrAdv(addrPrefix<addrIP> prf) {
        prefix = prf.copyBytes();
    }

    public int compareTo(ipRtrAdv o) {
        return prefix.compareTo(o.prefix);
    }

    /**
     * filter by this redistribution
     *
     * @param afi address family
     * @param trg target table to append
     * @param src source table to use
     * @param chk check if mine
     */
    public void filter(int afi, tabRoute<addrIP> trg, tabRoute<addrIP> src, boolean chk) {
        tabRouteEntry<addrIP> ntry = src.find(prefix);
        if (ntry == null) {
            return;
        }
        if (chk) {
            switch (ntry.best.rouTyp) {
                case conn:
                case staticRoute:
                case local:
                case defpref:
                    break;
                default:
                    return;
            }
        }
        if (prefix.compareTo(ntry.prefix) != 0) {
            return;
        }
        if (metric != null) {
            ntry = ntry.copyBytes(tabRoute.addType.notyet);
            ntry.best.metric = metric.update(ntry.best.metric);
        }
        if (tag != null) {
            ntry = ntry.copyBytes(tabRoute.addType.notyet);
            ntry.best.tag = tag.update(ntry.best.tag);
        }
        tabRoute.addUpdatedEntry(ecmp ? tabRoute.addType.altEcmp : tabRoute.addType.better, trg, afi, 0, ntry, true, roumap, rouplc, null);
    }

}
