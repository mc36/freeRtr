package net.freertr.serv;

import java.util.List;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgRtr;
import net.freertr.cfg.cfgScrpt;
import net.freertr.cfg.cfgVrf;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipRtr;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteUtil;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * generic honeypot configuration
 *
 * @author matecsaba
 */
public class servHoneyPotCfg {

    /**
     * script to run
     */
    public cfgScrpt script;

    /**
     * pretend a dumb server
     */
    public boolean tinyHttp;

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
    public long routeDstngshr;

    /**
     * add route details
     */
    public boolean routeDetails;

    /**
     * hack route details
     */
    public boolean routeHacked;

    /**
     * create instance
     */
    public servHoneyPotCfg() {
        doSanityChecks();
    }

    /**
     * get help messages
     *
     * @param lst help text to update
     * @param tab base level
     * @param dng enable dangerous knobs
     */
    public static final void getHelp(userHelping lst, int tab, boolean dng) {
        lst.add(null, (tab + 1) + " " + (tab + 2) + "  router4                      lookup addresses");
        cfgRtr.getRouterList(lst, tab, "");
        lst.add(null, (tab + 3) + " .         <num:rtr>       process id");
        lst.add(null, (tab + 1) + " " + (tab + 2) + "  router6                      lookup addresses");
        cfgRtr.getRouterList(lst, tab, "");
        lst.add(null, (tab + 3) + " .         <num:rtr>       process id");
        lst.add(null, (tab + 1) + " .  route-details                print prefix details");
        lst.add(null, (tab + 1) + " .  route-hacked                 hackerize prefix details");
        lst.add(null, (tab + 1) + " " + (tab + 2) + "  route-distinguisher          rd to use");
        lst.add(null, (tab + 2) + " .    <rd>                       rd in ASnum:IDnum format");
        lst.add(null, (tab + 1) + " " + (tab + 2) + "  route-vrf                    vrf to use");
        lst.add(null, (tab + 2) + " .    <name:vrf>                 name of table");
        if (!dng) {
            return;
        }
        lst.add(null, (tab + 1) + " " + (tab + 2) + "  script                       script to execute");
        lst.add(null, (tab + 2) + " .    <name:scr>                 script name");
        lst.add(null, (tab + 1) + " .  resolve                      resolve addresses");
        lst.add(null, (tab + 1) + " .  tiny-http                    pretend http server");
    }

    /**
     * get configuration
     *
     * @param beg beginning
     * @param lst list to update
     * @param dng enable dangerous knobs
     */
    public void doGetCfg(String beg, List<String> lst, boolean dng) {
        if (router4 == null) {
            lst.add(beg + "no router4");
        } else {
            lst.add(beg + "router4 " + router4.routerGetName());
        }
        if (router6 == null) {
            lst.add(beg + "no router6");
        } else {
            lst.add(beg + "router6 " + router6.routerGetName());
        }
        cmds.cfgLine(lst, !routeDetails, beg, "route-details", "");
        cmds.cfgLine(lst, !routeHacked, beg, "route-hacked", "");
        cmds.cfgLine(lst, routeDstngshr == 0, beg, "route-distinguisher", "" + tabRouteUtil.rd2string(routeDstngshr));
        if (!dng) {
            return;
        }
        if (script == null) {
            lst.add(beg + "no script");
        } else {
            lst.add(beg + "script " + script.name);
        }
        cmds.cfgLine(lst, !resolve, beg, "resolve", "");
        cmds.cfgLine(lst, !tinyHttp, beg, "tiny-http", "");
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
                doSanityChecks();
                return false;
            }
            script = cfgAll.scrptFind(cmd.word(), false);
            doSanityChecks();
            return false;
        }
        if (s.equals("resolve")) {
            resolve = !negated;
            doSanityChecks();
            return false;
        }
        if (s.equals("tiny-http")) {
            tinyHttp = !negated;
            doSanityChecks();
            return false;
        }
        if (s.equals("router4")) {
            if (negated) {
                router4 = null;
                fwder4 = null;
                doSanityChecks();
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
            doSanityChecks();
            return false;
        }
        if (s.equals("router6")) {
            if (negated) {
                router6 = null;
                fwder6 = null;
                doSanityChecks();
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
            doSanityChecks();
            return false;
        }
        if (s.equals("route-details")) {
            routeDetails = !negated;
            doSanityChecks();
            return false;
        }
        if (s.equals("route-hacked")) {
            routeHacked = !negated;
            doSanityChecks();
            return false;
        }
        if (s.equals("route-distinguisher")) {
            if (negated) {
                routeDstngshr = 0;
                doSanityChecks();
                return false;
            }
            s = cmd.word();
            routeDstngshr = tabRouteUtil.string2rd(s);
            doSanityChecks();
            return false;
        }
        if (s.equals("route-vrf")) {
            if (negated) {
                routeDstngshr = 0;
                doSanityChecks();
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
                routeDstngshr = fwder4.rd;
            }
            if (fwder6 != null) {
                routeDstngshr = fwder6.rd;
            }
            doSanityChecks();
            return false;
        }
        cmd.badCmd();
        return false;
    }

    /**
     * perform sanity checks
     */
    public synchronized void doSanityChecks() {
        if (router4 == null) {
            fwder4 = null;
        }
        if (router6 == null) {
            fwder6 = null;
        }
        if ((router4 != null) && (router6 == null)) {
            routeDstngshr = 0;
        }
        resolve &= cfgAll.domainLookup;
    }

}
