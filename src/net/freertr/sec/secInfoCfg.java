package net.freertr.sec;

import net.freertr.cfg.cfgScrpt;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipRtr;
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
    public ipRtr router4;

    /**
     * resolve ipv6 prefixes
     */
    public ipRtr router6;

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
