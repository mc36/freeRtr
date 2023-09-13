package net.freertr.serv;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAll;
import net.freertr.clnt.clntDns;
import net.freertr.enc.enc7bit;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipRtr;
import net.freertr.pack.packDnsRec;
import net.freertr.pipe.pipeSide;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRouteUtil;
import net.freertr.user.userFormat;
import net.freertr.util.bits;

/**
 * generic honeypot worker
 *
 * @author matecsaba
 */
public class servHoneyPotWrk {

    private final servHoneyPotCfg cfg;

    private final pipeSide pipe;

    private final addrIP addr;

    private final int port;

    /**
     * create an instance
     *
     * @param c configuration to use
     * @param r pipeline to use
     * @param a address to check
     * @param p port number to check
     */
    public servHoneyPotWrk(servHoneyPotCfg c, pipeSide r, addrIP a, int p) {
        cfg = c;
        pipe = r;
        addr = a;
        port = p;
    }

    /**
     * do every work
     *
     * @param dng enable dangerous knobs
     */
    public void doWork(boolean dng) {
        //////
        doScript(addr);
        //////////////
    }

    /**
     * print out results
     *
     * @param pipe pipeline to use
     * @param frst print summary line
     */
    public void putResult(pipeSide pipe, boolean frst) {
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        if (frst) {
            String s = getRoute1liner(addr, port);
            pipe.linePut("you (" + s + ") have been logged!");
        }
        List<String> lst = getRouteDetails(addr);
        byte[] res = servHoneyPotWrk.getRouteAscii(lst);
        pipe.morePut(res, 0, res.length);
    }

    /**
     * print out results
     *
     * @param frst print summary line
     */
    public void putResult(boolean frst) {
        putResult(pipe, frst);
    }

    /**
     * execute the script
     *
     * @param addr address to pass
     */
    public void doScript(addrIP addr) {
        if (cfg.script == null) {
            return;
        }
        cfg.script.doRound(bits.str2lst("set remote " + addr));
    }

    /**
     * get route details
     *
     * @param addr address to check
     * @return route details or empty list
     */
    public List<String> getRouteDetails(addrIP addr) {
        if (!cfg.routeDetails) {
            return new ArrayList<String>();
        }
        ipRtr rtr = findOneRtr(addr, cfg.router4, cfg.router6);
        ipFwd fwd = findOneFwd(addr, cfg.fwder4, cfg.fwder6);
        tabRouteEntry<addrIP> ntry = findOneRoute(0, addr, rtr, fwd);
        return getRouteDetails(fwd, ntry, userFormat.tableMode.fancy, cfg.routeHacked);
    }

    /**
     * get one liner information
     *
     * @param addr address to check
     * @param port port to check
     * @return single line of information
     */
    public String getRoute1liner(addrIP addr, int port) {
        String s = addr + " :" + port;
        if (cfg.resolve) {
            clntDns clnt = new clntDns();
            clnt.doResolvList(cfgAll.nameServerAddr, packDnsRec.generateReverse(addr), false, packDnsRec.typePTR);
            s += " - " + clnt.getPTR();
        }
        ipRtr rtr = findOneRtr(addr, cfg.router4, cfg.router6);
        ipFwd fwd = findOneFwd(addr, cfg.fwder4, cfg.fwder6);
        tabRouteEntry<addrIP> ntry = findOneRoute(0, addr, rtr, fwd);
        s += " - " + getRoute1liner(fwd, ntry);
        if (!cfg.routeHacked) {
            return s;
        }
        s = enc7bit.toHackedStr(s);
        return s;
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
