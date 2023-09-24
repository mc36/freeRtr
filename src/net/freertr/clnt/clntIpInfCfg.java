package net.freertr.clnt;

import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgRtr;
import net.freertr.cfg.cfgScrpt;
import net.freertr.cfg.cfgVrf;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipRtr;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteUtil;
import net.freertr.user.userFormat;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * generic ipinfo configuration
 *
 * @author matecsaba
 */
public class clntIpInfCfg {

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
     * style to send
     */
    public String style;

    /**
     * set table formatter
     */
    public userFormat.tableMode format = userFormat.tableMode.normal;

    /**
     * create instance
     */
    public clntIpInfCfg() {
        clntIpInfWrk.doSanityChecks(this);
    }

    /**
     * do config knob
     *
     * @param cmd command to interpret
     * @param negated command was negated
     * @return null if cleared, config on success
     */
    public static final clntIpInfCfg doCfgStr(clntIpInfCfg cfg, cmds cmd, boolean negated) {
        if (cfg == null) {
            cfg = new clntIpInfCfg();
        }
        String s = cmd.word();
        if (s.equals("script")) {
            if (negated) {
                cfg.script = null;
                clntIpInfWrk.doSanityChecks(cfg);
                return cfg;
            }
            cfg.script = cfgAll.scrptFind(cmd.word(), false);
            clntIpInfWrk.doSanityChecks(cfg);
            return cfg;
        }
        if (s.equals("resolve")) {
            cfg.resolve = !negated;
            clntIpInfWrk.doSanityChecks(cfg);
            return cfg;
        }
        if (s.equals("others")) {
            cfg.others = !negated;
            clntIpInfWrk.doSanityChecks(cfg);
            return cfg;
        }
        if (s.equals("tinyhttp")) {
            cfg.tinyHttp = !negated;
            clntIpInfWrk.doSanityChecks(cfg);
            return cfg;
        }
        if (s.equals("details")) {
            cfg.details = !negated;
            clntIpInfWrk.doSanityChecks(cfg);
            return cfg;
        }
        if (s.equals("single")) {
            cfg.single = !negated;
            clntIpInfWrk.doSanityChecks(cfg);
            return cfg;
        }
        if (s.equals("hacked")) {
            cfg.hacked = !negated;
            clntIpInfWrk.doSanityChecks(cfg);
            return cfg;
        }
        if (s.equals("plain")) {
            cfg.plain = !negated;
            clntIpInfWrk.doSanityChecks(cfg);
            return cfg;
        }
        if (s.equals("style")) {
            if (negated) {
                cfg.style = null;
                return cfg;
            }
            cfg.style = cmd.getRemaining();
            return cfg;
        }
        if (s.equals("format")) {
            if (negated) {
                cfg.format = userFormat.tableMode.normal;
                return cfg;
            }
            s = cmd.word();
            cfg.format = userFormat.str2tabmod(s);
            return cfg;
        }
        if (s.equals("router4")) {
            if (negated) {
                cfg.router4 = null;
                cfg.fwder4 = null;
                clntIpInfWrk.doSanityChecks(cfg);
                return cfg;
            }
            tabRouteAttr.routeType o = cfgRtr.name2num(cmd.word());
            int i = bits.str2num(cmd.word());
            cfgRtr rtr = cfgAll.rtrFind(o, i, false);
            if (rtr == null) {
                cmd.error("no such router process");
                return cfg;
            }
            if (rtr.fwd == null) {
                cmd.error("router have no vrf");
                return cfg;
            }
            cfg.router4 = rtr.getRouter();
            cfg.fwder4 = rtr.fwd;
            clntIpInfWrk.doSanityChecks(cfg);
            return cfg;
        }
        if (s.equals("router6")) {
            if (negated) {
                cfg.router6 = null;
                cfg.fwder6 = null;
                clntIpInfWrk.doSanityChecks(cfg);
                return cfg;
            }
            tabRouteAttr.routeType o = cfgRtr.name2num(cmd.word());
            int i = bits.str2num(cmd.word());
            cfgRtr rtr = cfgAll.rtrFind(o, i, false);
            if (rtr == null) {
                cmd.error("no such router process");
                return cfg;
            }
            if (rtr.fwd == null) {
                cmd.error("router have no vrf");
                return cfg;
            }
            cfg.router6 = rtr.getRouter();
            cfg.fwder6 = rtr.fwd;
            clntIpInfWrk.doSanityChecks(cfg);
            return cfg;
        }
        if (s.equals("rd")) {
            if (negated) {
                cfg.rd = 0;
                clntIpInfWrk.doSanityChecks(cfg);
                return cfg;
            }
            s = cmd.word();
            cfg.rd = tabRouteUtil.string2rd(s);
            clntIpInfWrk.doSanityChecks(cfg);
            return cfg;
        }
        if (s.equals("vrf")) {
            if (negated) {
                cfg.rd = 0;
                clntIpInfWrk.doSanityChecks(cfg);
                return cfg;
            }
            s = cmd.word();
            cfgVrf ntry = cfgAll.vrfFind(s, false);
            if (ntry == null) {
                cmd.error("no such vrf");
                return cfg;
            }
            cfg.fwder4 = ntry.fwd4;
            cfg.fwder6 = ntry.fwd6;
            cfg.rd = cfg.fwder4.rd;
            cfg.rd = cfg.fwder6.rd;
            clntIpInfWrk.doSanityChecks(cfg);
            return cfg;
        }
        cmd.badCmd();
        return cfg;
    }

}
