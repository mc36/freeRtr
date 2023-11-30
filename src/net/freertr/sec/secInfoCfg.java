package net.freertr.sec;

import net.freertr.cfg.cfgScrpt;
import net.freertr.clnt.clntTrack;
import net.freertr.ip.ipFwd;
import net.freertr.tab.tabRouteAttr;
import net.freertr.user.userFormat;

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
