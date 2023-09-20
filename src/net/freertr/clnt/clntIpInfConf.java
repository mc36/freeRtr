package net.freertr.clnt;

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
 * generic ipinfo configuration
 *
 * @author matecsaba
 */
public class clntIpInfConf {

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
    public boolean summary;

    /**
     * hack route details
     */
    public boolean hacked;

    /**
     * create instance
     */
    public clntIpInfConf() {
        clntIpInfUtil.doSanityChecks(this);
    }

    /**
     * get help messages
     *
     * @param lst help text to update
     * @param tab base level
     */
    public final static void getHelp(userHelping lst, int tab) {
        lst.add(null, (tab + 1) + " " + (tab + 2) + "  router4                      lookup addresses");
        cfgRtr.getRouterList(lst, tab, "");
        lst.add(null, (tab + 3) + " .         <num:rtr>       process id");
        lst.add(null, (tab + 1) + " " + (tab + 2) + "  router6                      lookup addresses");
        cfgRtr.getRouterList(lst, tab, "");
        lst.add(null, (tab + 3) + " .         <num:rtr>       process id");
        lst.add(null, (tab + 1) + " " + (tab + 2) + "  rd                           rd to use");
        lst.add(null, (tab + 2) + " .    <rd>                       rd in ASnum:IDnum format");
        lst.add(null, (tab + 1) + " " + (tab + 2) + "  vrf                          vrf to use");
        lst.add(null, (tab + 2) + " .    <name:vrf>                 name of table");
        lst.add(null, (tab + 1) + " " + (tab + 2) + "  script                       script to execute");
        lst.add(null, (tab + 2) + " .    <name:scr>                 script name");
        lst.add(null, (tab + 1) + " .  details                      print prefix details");
        lst.add(null, (tab + 1) + " .  summary                      print prefix summary");
        lst.add(null, (tab + 1) + " .  hacked                       hackerize prefix details");
        lst.add(null, (tab + 1) + " .  resolve                      resolve addresses");
        lst.add(null, (tab + 1) + " .  tinyhttp                     pretend http server");
        lst.add(null, (tab + 1) + " .  others                       allow any addresses");
    }

    /**
     * get configuration
     *
     * @param beg beginning
     * @param lst list to update
     */
    public void doGetCfg(String beg, List<String> lst) {
        if (router4 != null) {
            lst.add(beg + "router4 " + router4.routerGetName());
        }
        if (router6 != null) {
            lst.add(beg + "router6 " + router6.routerGetName());
        }
        if (details) {
            lst.add(beg + "details");
        }
        if (summary) {
            lst.add(beg + "summary");
        }
        if (hacked) {
            lst.add(beg + "hacked");
        }
        if (rd != 0) {
            lst.add(beg + "rd " + tabRouteUtil.rd2string(rd));
        }
        if (script != null) {
            lst.add(beg + "script " + script.name);
        }
        if (resolve) {
            lst.add(beg + "resolve");
        }
        if (tinyHttp) {
            lst.add(beg + "tinyhttp");
        }
        if (others) {
            lst.add(beg + "others");
        }
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
                clntIpInfUtil.doSanityChecks(this);
                return false;
            }
            script = cfgAll.scrptFind(cmd.word(), false);
            clntIpInfUtil.doSanityChecks(this);
            return false;
        }
        if (s.equals("resolve")) {
            resolve = !negated;
            clntIpInfUtil.doSanityChecks(this);
            return false;
        }
        if (s.equals("others")) {
            others = !negated;
            clntIpInfUtil.doSanityChecks(this);
            return false;
        }
        if (s.equals("tinyhttp")) {
            tinyHttp = !negated;
            clntIpInfUtil.doSanityChecks(this);
            return false;
        }
        if (s.equals("details")) {
            details = !negated;
            clntIpInfUtil.doSanityChecks(this);
            return false;
        }
        if (s.equals("summary")) {
            summary = !negated;
            clntIpInfUtil.doSanityChecks(this);
            return false;
        }
        if (s.equals("hacked")) {
            hacked = !negated;
            clntIpInfUtil.doSanityChecks(this);
            return false;
        }
        if (s.equals("router4")) {
            if (negated) {
                router4 = null;
                fwder4 = null;
                clntIpInfUtil.doSanityChecks(this);
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
            clntIpInfUtil.doSanityChecks(this);
            return false;
        }
        if (s.equals("router6")) {
            if (negated) {
                router6 = null;
                fwder6 = null;
                clntIpInfUtil.doSanityChecks(this);
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
            clntIpInfUtil.doSanityChecks(this);
            return false;
        }
        if (s.equals("rd")) {
            if (negated) {
                rd = 0;
                clntIpInfUtil.doSanityChecks(this);
                return false;
            }
            s = cmd.word();
            rd = tabRouteUtil.string2rd(s);
            clntIpInfUtil.doSanityChecks(this);
            return false;
        }
        if (s.equals("vrf")) {
            if (negated) {
                rd = 0;
                clntIpInfUtil.doSanityChecks(this);
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
            clntIpInfUtil.doSanityChecks(this);
            return false;
        }
        cmd.badCmd();
        return false;
    }

}
