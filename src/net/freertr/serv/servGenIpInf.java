package net.freertr.serv;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgRtr;
import net.freertr.cfg.cfgScrpt;
import net.freertr.cfg.cfgVrf;
import net.freertr.clnt.clntDns;
import net.freertr.enc.enc7bit;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipRtr;
import net.freertr.pack.packDnsRec;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRouteUtil;
import net.freertr.user.userFormat;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * the default ipinfo querier
 *
 * @author matecsaba
 */
public class servGenIpInf {

    /**
     * script to run
     */
    public cfgScrpt script;

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
     * create an instance
     */
    public servGenIpInf() {
        doSanityChecks();
    }

    public void doScript(addrIP addr) {
        if (script == null) {
            return;
        }
        script.doRound(bits.str2lst("set remote " + addr));
    }

    public List<String> getRouteDetails(addrIP addr, int port) {
        if (!routeDetails) {
            return new ArrayList<String>();
        }
        ipRtr rtr = findOneRtr(addr, router4, router6);
        ipFwd fwd = findOneFwd(addr, fwder4, fwder6);
        tabRouteEntry<addrIP> ntry = findOneRoute(0, addr, rtr, fwd);
        return getRouteDetails(fwd, ntry, userFormat.tableMode.fancy, routeHacked);
    }

    public String getRoute1liner(addrIP addr, int port) {
        String s = addr + " :" + port;
        if (resolve) {
            clntDns clnt = new clntDns();
            clnt.doResolvList(cfgAll.nameServerAddr, packDnsRec.generateReverse(addr), false, packDnsRec.typePTR);
            s += " - " + clnt.getPTR();
        }
        ipRtr rtr = findOneRtr(addr, router4, router6);
        ipFwd fwd = findOneFwd(addr, fwder4, fwder6);
        tabRouteEntry<addrIP> ntry = findOneRoute(0, addr, rtr, fwd);
        s += " - " + getRoute1liner(fwd, ntry);
        if (!routeHacked) {
            return s;
        }
        s = enc7bit.toHackedStr(s);
        return s;
    }

    public void doGetCfg(String beg, List<String> lst, int filter) {
        if (script == null) {
            lst.add(beg + "no script");
        } else {
            lst.add(beg + "script " + script.name);
        }
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
        cmds.cfgLine(lst, !resolve, beg, "resolve", "");
    }

    public boolean doCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("script")) {
            script = cfgAll.scrptFind(cmd.word(), false);
            doSanityChecks();
            return false;
        }
        if (s.equals("resolve")) {
            resolve = true;
            doSanityChecks();
            return false;
        }
        if (s.equals("router4")) {
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
            routeDetails = true;
            doSanityChecks();
            return false;
        }
        if (s.equals("route-hacked")) {
            routeHacked = true;
            doSanityChecks();
            return false;
        }
        if (s.equals("route-distinguisher")) {
            s = cmd.word();
            routeDstngshr = tabRouteUtil.string2rd(s);
            doSanityChecks();
            return false;
        }
        if (s.equals("route-vrf")) {
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
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("script")) {
            script = null;
            doSanityChecks();
            return false;
        }
        if (s.equals("resolve")) {
            resolve = false;
            doSanityChecks();
            return false;
        }
        if (s.equals("router4")) {
            router4 = null;
            fwder4 = null;
            doSanityChecks();
            return false;
        }
        if (s.equals("router6")) {
            router6 = null;
            fwder6 = null;
            doSanityChecks();
            return false;
        }
        if (s.equals("route-details")) {
            routeDetails = false;
            doSanityChecks();
            return false;
        }
        if (s.equals("route-hacked")) {
            routeHacked = false;
            doSanityChecks();
            return false;
        }
        if (s.equals("route-distinguisher")) {
            routeDstngshr = 0;
            doSanityChecks();
            return false;
        }
        if (s.equals("route-vrf")) {
            routeDstngshr = 0;
            doSanityChecks();
            return false;
        }
        return true;
    }

    /**
     * find one forwarder
     *
     * @param adr address to check
     * @param fwd4 ipv4 candidate
     * @param fwd6 ipv6 candidate
     * @return proper one, null if nothing
     */
    public final static ipFwd findOneFwd(addrIP adr, ipFwd fwd4, ipFwd fwd6) {
        if (adr == null) {
            return null;
        }
        if (adr.isIPv4()) {
            return fwd4;
        } else {
            return fwd6;
        }
    }

    /**
     * find one router
     *
     * @param adr address to check
     * @param rtr4 ipv4 candidate
     * @param rtr6 ipv6 candidate
     * @return proper one, null if nothing
     */
    public final static ipRtr findOneRtr(addrIP adr, ipRtr rtr4, ipRtr rtr6) {
        if (adr == null) {
            return null;
        }
        if (adr.isIPv4()) {
            return rtr4;
        } else {
            return rtr6;
        }
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

    /**
     * find one route
     *
     * @param rd route distinguisher, 0 for default
     * @param adr address to look up
     * @param rtr router to use
     * @param fwd forwarder to use
     * @return route entry, null if nothing
     */
    protected final static tabRouteEntry<addrIP> findOneRoute(long rd, addrIP adr, ipRtr rtr, ipFwd fwd) {
        if (adr == null) {
            return null;
        }
        if (rtr == null) {
            return null;
        }
        if (fwd == null) {
            return null;
        }
        tabRouteEntry<addrIP> ntry = rtr.routerComputedU.route(adr);
        if (ntry == null) {
            return null;
        }
        ntry = ntry.copyBytes(tabRoute.addType.alters);
        if (rd == 0) {
            return ntry;
        }
        ntry.rouDst = rd;
        return ntry;
    }

    private final static String noRoute = "route not found";

    /**
     * one liner of the route
     *
     * @param fwd forwarder to use
     * @param ntry route entry
     * @return one liner of the route
     */
    protected final static String getRoute1liner(ipFwd fwd, tabRouteEntry<addrIP> ntry) {
        if (ntry == null) {
            return noRoute;
        }
        String a;
        if (fwd != null) {
            a = "ipv" + fwd.ipVersion + "(" + fwd.vrfName + ") ";
        } else {
            a = "";
        }
        return addrPrefix.ip2str(ntry.prefix) + " " + tabRouteUtil.rd2string(ntry.rouDst) + " - " + ntry.best.asPathStr() + " - " + ntry.best.asInfoStr() + " - " + ntry.best.asNameStr();
    }

    /**
     * get route in details
     *
     * @param fwd forwarder to use
     * @param ntry route entry
     * @param tm table mode
     * @param hck hacker voiced
     * @return text representing the route
     */
    protected final static List<String> getRouteDetails(ipFwd fwd, tabRouteEntry<addrIP> ntry, userFormat.tableMode tm, boolean hck) {
        if (ntry == null) {
            return bits.str2lst(noRoute);
        }
        userFormat res = ntry.fullDump("", fwd);
        List<String> lst = res.formatAll(userFormat.tableMode.fancy);
        if (!hck) {
            return lst;
        }
        lst = enc7bit.toHackedLst(lst);
        return lst;
    }

    /**
     * get route as bytes
     *
     * @param lst list to convert
     * @return converted list
     */
    protected final static byte[] getRouteAscii(List<String> lst) {
        if (lst == null) {
            return new byte[0];
        }
        int lss = lst.size();
        List<Integer> res = new ArrayList<Integer>();
        byte[] buf = null;
        for (int o = 0; o < lss; o++) {
            String a = lst.get(o);
            if (a == null) {
                a = "";
            }
            a = enc7bit.decodeExtStr(a);
            buf = a.getBytes();
            for (int i = 0; i < buf.length; i++) {
                int p = (int) buf[i];
                res.add(p);
            }
            res.add(13);
            res.add(10);
        }
        buf = new byte[res.size()];
        for (int i = 0; i < buf.length; i++) {
            int o = res.get(i);
            buf[i] = (byte) o;
        }
        return buf;
    }

}
