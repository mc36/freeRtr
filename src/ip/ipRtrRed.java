package ip;

import java.util.Comparator;

import tab.tabListing;
import tab.tabPrfxlstN;
import tab.tabRoute;
import tab.tabRtrmapN;
import addr.addrIP;
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
    public final tabRouteEntry.routeType typ;

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
     * create redistributor
     *
     * @param prot type of protocol
     * @param proc number of process
     */
    public ipRtrRed(tabRouteEntry.routeType prot, int proc) {
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
        tabRoute<addrIP> lst = src.justProto(typ, num);
        tabRoute.addUpdatedTable(tabRoute.addType.better, afi, trg, lst, roumap, rouplc, prflst);
    }

}
