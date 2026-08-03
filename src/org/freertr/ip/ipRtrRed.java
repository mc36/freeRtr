package org.freertr.ip;

import org.freertr.addr.addrIP;
import org.freertr.tab.tabIntUpdater;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRtrmapN;
import org.freertr.tab.tabRtrplcN;

/**
 * protocol import into routers
 *
 * @author matecsaba
 */
public class ipRtrRed implements Comparable<ipRtrRed> {

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
     * tag
     */
    public tabIntUpdater tag;

    /**
     * ecmp mode
     */
    public boolean ecmp;

    /**
     * limit
     */
    public int limit;

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

    public int compareTo(ipRtrRed o) {
        int i = typ.compareTo(o.typ);
        if (i != 0) {
            return i;
        }
        if (num < o.num) {
            return -1;
        }
        if (num > o.num) {
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
        tabRoute.addType mod = ecmp ? tabRoute.addType.altEcmp : tabRoute.addType.better;
        int done = 0;
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
            if (tag != null) {
                ntry = ntry.copyBytes(tabRoute.addType.notyet);
                ntry.best.tag = tag.update(ntry.best.tag);
            }
            done += tabRoute.addUpdatedEntry(mod, trg, afi, 0, ntry, true, roumap, rouplc, prflst);
            if (limit < 1) {
                continue;
            }
            if (done >= limit) {
                break;
            }
        }
    }

}
