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
     * @return true on error, false on success
     */
    public boolean doCfgStr(cmds cmd, boolean negated) {
        String s = cmd.word();
        if (s.equals("script")) {
            if (negated) {
                script = null;
                clntIpInfWrk.doSanityChecks(this);
                return false;
            }
            script = cfgAll.scrptFind(cmd.word(), false);
            clntIpInfWrk.doSanityChecks(this);
            return false;
        }
        if (s.equals("resolve")) {
            resolve = !negated;
            clntIpInfWrk.doSanityChecks(this);
            return false;
        }
        if (s.equals("others")) {
            others = !negated;
            clntIpInfWrk.doSanityChecks(this);
            return false;
        }
        if (s.equals("tinyhttp")) {
            tinyHttp = !negated;
            clntIpInfWrk.doSanityChecks(this);
            return false;
        }
        if (s.equals("details")) {
            details = !negated;
            clntIpInfWrk.doSanityChecks(this);
            return false;
        }
        if (s.equals("single")) {
            single = !negated;
            clntIpInfWrk.doSanityChecks(this);
            return false;
        }
        if (s.equals("hacked")) {
            hacked = !negated;
            clntIpInfWrk.doSanityChecks(this);
            return false;
        }
        if (s.equals("plain")) {
            plain = !negated;
            clntIpInfWrk.doSanityChecks(this);
            return false;
        }
        if (s.equals("style")) {
            if (negated) {
                style = null;
                return false;
            }
            style = cmd.getRemaining();
            return false;
        }
        if (s.equals("format")) {
            if (negated) {
                format = userFormat.tableMode.normal;
                return false;
            }
            s = cmd.word();
            format = userFormat.str2tabmod(s);
            return false;
        }
        if (s.equals("router4")) {
            if (negated) {
                router4 = null;
                fwder4 = null;
                clntIpInfWrk.doSanityChecks(this);
                return false;
            }
            tabRouteAttr.routeType o = cfgRtr.name2num(cmd.word());
            int i = bits.str2num(cmd.word());
            cfgRtr rtr = cfgAll.rtrFind(o, i, false);
            if (rtr == null) {
                cmd.error("no such router process");
                return false;
            }
            if (rtr.fwd == null) {
                cmd.error("router have no vrf");
                return false;
            }
            router4 = rtr.getRouter();
            fwder4 = rtr.fwd;
            clntIpInfWrk.doSanityChecks(this);
            return false;
        }
        if (s.equals("router6")) {
            if (negated) {
                router6 = null;
                fwder6 = null;
                clntIpInfWrk.doSanityChecks(this);
                return false;
            }
            tabRouteAttr.routeType o = cfgRtr.name2num(cmd.word());
            int i = bits.str2num(cmd.word());
            cfgRtr rtr = cfgAll.rtrFind(o, i, false);
            if (rtr == null) {
                cmd.error("no such router process");
                return false;
            }
            if (rtr.fwd == null) {
                cmd.error("router have no vrf");
                return false;
            }
            router6 = rtr.getRouter();
            fwder6 = rtr.fwd;
            clntIpInfWrk.doSanityChecks(this);
            return false;
        }
        if (s.equals("rd")) {
            if (negated) {
                rd = 0;
                clntIpInfWrk.doSanityChecks(this);
                return false;
            }
            s = cmd.word();
            rd = tabRouteUtil.string2rd(s);
            clntIpInfWrk.doSanityChecks(this);
            return false;
        }
        if (s.equals("vrf")) {
            if (negated) {
                rd = 0;
                clntIpInfWrk.doSanityChecks(this);
                return false;
            }
            s = cmd.word();
            cfgVrf ntry = cfgAll.vrfFind(s, false);
            if (ntry == null) {
                cmd.error("no such vrf");
                return false;
            }
            fwder4 = ntry.fwd4;
            fwder6 = ntry.fwd6;
            if (fwder4 != null) {
                rd = fwder4.rd;
            }
            if (fwder6 != null) {
                rd = fwder6.rd;
            }
            clntIpInfWrk.doSanityChecks(this);
            return false;
        }
        cmd.badCmd();
        return false;
    }

}
