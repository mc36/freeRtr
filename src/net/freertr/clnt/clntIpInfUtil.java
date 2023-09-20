package net.freertr.clnt;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAll;
import net.freertr.enc.enc7bit;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipRtr;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRouteUtil;
import net.freertr.user.userFormat;
import net.freertr.util.bits;
import net.freertr.util.logger;
import net.freertr.util.verCore;

/**
 * generic ipinfo utilities
 *
 * @author matecsaba
 */
public class clntIpInfUtil {

    /**
     * create instance
     */
    private clntIpInfUtil() {
    }

    /**
     * perform sanity checks
     *
     * @param cfg config to repair
     * @return changes made
     */
    public final static int doSanityChecks(clntIpInfConf cfg) {
        int chg = 0;
        if (cfg.router4 == null) {
            cfg.fwder4 = null;
            chg++;
        }
        if (cfg.router6 == null) {
            cfg.fwder6 = null;
            chg++;
        }
        if ((cfg.router4 != null) && (cfg.router6 == null)) {
            cfg.rd = 0;
            chg++;
        }
        if (!cfg.resolve) {
            return chg;
        }
        if (cfgAll.domainLookup) {
            return chg;
        }
        cfg.resolve = false;
        chg++;
        if (!verCore.release) {
            return chg;
        }
        cfg.script = null;
        chg++;
        cfg.others = false;
        chg++;
        return chg;
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
     * find one route
     *
     * @param adr address to look up
     * @param rtr router to use
     * @param fwd forwarder to use
     * @return route entry, null if nothing
     */
    public final static tabRouteEntry<addrIP> findOneRoute(addrIP adr, ipRtr rtr, ipFwd fwd) {
        if (adr == null) {
            logger.warn("no address");
            return null;
        }
        if (fwd == null) {
            logger.warn("no forwarder for " + adr);
            return null;
        }
        if (rtr == null) {
            logger.warn("no router for " + adr);
            return null;
        }
        tabRouteEntry<addrIP> ntry = rtr.routerComputedU.route(adr);
        if (ntry == null) {
            return null;
        }
        ntry = ntry.copyBytes(tabRoute.addType.alters);
        return ntry;
    }

    private final static String noRoute = "route not found";

    /**
     * one liner of the route
     *
     * @param fwd forwarder to use
     * @param rtr router to use
     * @param ntry route entry
     * @return one liner of the route
     */
    public final static String getRoute1liner(ipFwd fwd, ipRtr rtr, tabRouteEntry<addrIP> ntry) {
        if (ntry == null) {
            return noRoute;
        }
        return fwd.vrfName + " " + rtr.routerComputedU.size()
                + " " + addrPrefix.ip2str(ntry.prefix)
                + " " + tabRouteUtil.rd2string(ntry.rouDst)
                + " " + ntry.best.asPathStr()
                + " " + ntry.best.asInfoStr()
                + " " + ntry.best.asNameStr();
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
    public final static List<String> getRouteDetails(ipFwd fwd, tabRouteEntry<addrIP> ntry, userFormat.tableMode tm, boolean hck) {
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
    public final static byte[] getRouteAscii(List<String> lst) {
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
