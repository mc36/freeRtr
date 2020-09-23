package ip;

import java.util.Comparator;

import tab.tabListing;
import tab.tabPrfxlstN;
import tab.tabRoute;
import tab.tabRtrmapN;
import addr.addrIP;
import tab.tabIntUpdater;
import tab.tabRouteAttr;
import tab.tabRouteEntry;
import tab.tabRtrplcN;

/**
 * protocol import into routers
 *
 * @author matecsaba
 */
public class ipRtrRed implements Comparator<ipRtrRed> {

    /**
     * type of protocol
     */
    public final tabRouteAttr.routeType typ;

    /**
     * number of process
     */
    public final int num;

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
     * create redistributor
     *
     * @param prot type of protocol
     * @param proc number of process
     */
    public ipRtrRed(tabRouteAttr.routeType prot, int proc) {
        typ = prot;
        num = proc;
    }

    public int compare(ipRtrRed o1, ipRtrRed o2) {
        int i = o1.typ.compareTo(o2.typ);
        if (i != 0) {
            return i;
        }
        if (o1.num < o2.num) {
            return -1;
        }
        if (o1.num > o2.num) {
            return +1;
        }
        return 0;
    }

    /**
     * filter by this redistribution
     *
     * @param afi address family
     * @param trg target table to append
     * @param src source table to use
     */
    public void filter(int afi, tabRoute<addrIP> trg, tabRoute<addrIP> src) {
        for (int i = 0; i < src.size(); i++) {
            tabRouteEntry<addrIP> ntry = src.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.best.rouTyp != typ) {
                continue;
            }
            if (ntry.best.protoNum != num) {
                continue;
            }
            if (metric != null) {
                ntry = ntry.copyBytes(tabRoute.addType.notyet);
                ntry.best.metric = metric.update(ntry.best.metric);
            }
            tabRoute.addUpdatedEntry(tabRoute.addType.better, trg, afi, ntry, true, roumap, rouplc, prflst);
        }
    }

}
