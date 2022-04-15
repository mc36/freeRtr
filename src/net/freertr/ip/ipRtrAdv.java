package net.freertr.ip;

import java.util.Comparator;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.tab.tabIntUpdater;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRtrmapN;
import net.freertr.tab.tabRtrplcN;

/**
 * prefix import into routers
 *
 * @author matecsaba
 */
public class ipRtrAdv implements Comparator<ipRtrAdv> {

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

    public int compare(ipRtrAdv o1, ipRtrAdv o2) {
        return o1.prefix.compare(o1.prefix, o2.prefix);
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
        if (prefix.compare(prefix, ntry.prefix) != 0) {
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
