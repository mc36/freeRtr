package net.freertr.ip;

import java.util.Comparator;
import net.freertr.addr.addrIP;
import net.freertr.pack.packHolder;
import net.freertr.tab.tabAceslstN;
import net.freertr.tab.tabIntUpdater;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabPrfxlstN;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRtrmapN;
import net.freertr.tab.tabRtrplcN;

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
     * access list
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> acl;

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
        tabRoute.addType mod = ecmp ? tabRoute.addType.altEcmp : tabRoute.addType.better;
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
            if (acl == null) {
                tabRoute.addUpdatedEntry(mod, trg, afi, 0, ntry, true, roumap, rouplc, prflst);
                continue;
            }
            ntry = tabRoute.doUpdateEntry(afi, 0, ntry, roumap, rouplc, prflst);
            if (ntry == null) {
                continue;
            }
            packHolder pck = new packHolder(false, false);
            pck.IPsrc.setAddr(ntry.prefix.network);
            pck.IPtrg.setAddr(ntry.prefix.mask);
            if (!acl.matches(false, false, pck)) {
                continue;
            }
            trg.add(mod, ntry, false, true);
        }
    }

}
