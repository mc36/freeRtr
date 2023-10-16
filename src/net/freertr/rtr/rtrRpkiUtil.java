package net.freertr.rtr;

import net.freertr.addr.addrIP;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;

/**
 * rpki utilities
 *
 * @author matecsaba
 */
public class rtrRpkiUtil {

    /**
     * create instance
     */
    private rtrRpkiUtil() {
    }

    /**
     * set validity
     *
     * @param tab table to update
     * @param roas route authorizations
     */
    public static void setValidityTable(tabRoute<addrIP> tab, tabRoute<addrIP> roas) {
        if (roas == null) {
            return;
        }
        for (int i = 0; i < tab.size(); i++) {
            setValidityRoute(tab.get(i), roas);
        }
    }

    /**
     * set validity
     *
     * @param ntry entry to update
     * @param roas route authorizations
     */
    public static void setValidityRoute(tabRouteEntry<addrIP> ntry, tabRoute<addrIP> roas) {
        if (roas == null) {
            return;
        }
        tabRouteEntry<addrIP> res = roas.route(ntry.prefix.broadcast);
        for (int o = 0; o < ntry.alts.size(); o++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(o);
            if (res == null) {
                attr.validity = 2;
                continue;
            }
            int i = attr.asPathEnd();
            if (i != res.best.rouSrc) {
                attr.validity = 3;
                continue;
            }
            attr.validity = 1;
        }
        ntry.selectBest();
    }

    /**
     * set validity
     *
     * @param ntry entry to update
     * @param val result code
     */
    public static void setValidityRoute(tabRouteEntry<addrIP> ntry, int val) {
        for (int o = 0; o < ntry.alts.size(); o++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(o);
            attr.validity = val;
        }
        ntry.selectBest();
    }

    /**
     * set validity
     *
     * @param tab table to update
     * @param val result code
     */
    public static void setValidityTable(tabRoute<addrIP> tab, int val) {
        for (int i = 0; i < tab.size(); i++) {
            setValidityRoute(tab.get(i), val);
        }
    }

}
