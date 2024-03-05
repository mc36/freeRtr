package org.freertr.sec;

import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgScrpt;
import org.freertr.clnt.clntTrack;
import org.freertr.ip.ipFwd;
import org.freertr.tab.tabAceslstN;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.tab.tabRateLimit;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRtrmapN;
import org.freertr.tab.tabRtrplcN;
import org.freertr.user.userFormat;

/**
 * generic information config
 *
 * @author matecsaba
 */
public class secInfoCfg {

    /**
     * create instance
     */
    public secInfoCfg() {
        secInfoUtl.doSanityChecks(this);
    }

    /**
     * script to run
     */
    public cfgScrpt script;

    /**
     * tracker to use
     */
    public clntTrack tracker;

    /**
     * accesses per interval
     */
    public tabRateLimit accessRate;

    /**
     * limit on startup
     */
    public int startupDelay;

    /**
     * access list to use
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> srvAccess;

    /**
     * access prefix list
     */
    public tabListing<tabPrfxlstN, addrIP> prefixList;

    /**
     * access route map
     */
    public tabListing<tabRtrmapN, addrIP> routeMap;

    /**
     * access route policy
     */
    public tabListing<tabRtrplcN, addrIP> routePolicy;

    /**
     * pretend a dumb server
     */
    public boolean tinyHttp;

    /**
     * allow to query others
     */
    public boolean others;

    /**
     * resolve addresses
     */
    public boolean resolve;

    /**
     * resolve ipv4 prefixes
     */
    public tabRouteAttr.routeType router4typ;

    /**
     * resolve ipv6 prefixes
     */
    public tabRouteAttr.routeType router6typ;

    /**
     * resolve ipv4 prefixes
     */
    public int router4num;

    /**
     * resolve ipv6 prefixes
     */
    public int router6num;

    /**
     * ipv4 resolver vrf
     */
    public ipFwd fwder4;

    /**
     * ipv6 resolver vrf
     */
    public ipFwd fwder6;

    /**
     * use route rd
     */
    public long rd;

    /**
     * add route details
     */
    public boolean details;

    /**
     * add route summary
     */
    public boolean single;

    /**
     * add crlf to summary
     */
    public boolean separate;

    /**
     * hack route details
     */
    public boolean hacked;

    /**
     * plain route details
     */
    public boolean plain;

    /**
     * ip only headline
     */
    public boolean justip;

    /**
     * style to send
     */
    public String style;

    /**
     * set table formatter
     */
    public userFormat.tableMode format = userFormat.tableMode.normal;

    /**
     * pmtud min
     */
    public int pmtudMin;

    /**
     * pmtud max
     */
    public int pmtudMax;

    /**
     * pmtud timeout
     */
    public int pmtudTim;

}
