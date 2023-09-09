package net.freertr.serv;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgRtr;
import net.freertr.cfg.cfgScrpt;
import net.freertr.clnt.clntDns;
import net.freertr.enc.enc7bit;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipRtr;
import net.freertr.pack.packDnsRec;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.user.userFilter;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.logger;

/**
 * honeypot server
 *
 */
public class servHoneyPot extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servHoneyPot() {
    }

    /**
     * port number
     */
    public static final int port = 22;

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
     * add route details
     */
    public boolean routeDetails;

    /**
     * hack route details
     */
    public boolean routeHacked;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server honeypot .*! port " + port,
        "server honeypot .*! protocol " + proto2string(protoAllStrm),
        "server honeypot .*! no script",
        "server honeypot .*! no router4",
        "server honeypot .*! no router6",
        "server honeypot .*! no route-details",
        "server honeypot .*! no route-hacked",
        "server honeypot .*! no resolve"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "honeypot";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(128 * 1024, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public void srvShRun(String beg, List<String> lst, int filter) {
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
        cmds.cfgLine(lst, !resolve, beg, "resolve", "");
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("script")) {
            script = cfgAll.scrptFind(cmd.word(), false);
            return false;
        }
        if (s.equals("resolve")) {
            resolve = true;
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
            return false;
        }
        if (s.equals("route-details")) {
            routeDetails = true;
            return false;
        }
        if (s.equals("route-hacked")) {
            routeHacked = true;
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("script")) {
            script = null;
            return false;
        }
        if (s.equals("resolve")) {
            resolve = false;
            return false;
        }
        if (s.equals("router4")) {
            router4 = null;
            fwder4 = null;
            return false;
        }
        if (s.equals("router6")) {
            router6 = null;
            fwder6 = null;
            return false;
        }
        if (s.equals("route-details")) {
            routeDetails = false;
            return false;
        }
        if (s.equals("route-hacked")) {
            routeHacked = false;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  script                       script to execute");
        l.add(null, "2 .    <name:scr>                 script name");
        l.add(null, "1 .  resolve                      resolve addresses");
        l.add(null, "1 2  router4                      lookup addresses");
        cfgRtr.getRouterList(l, 0, "");
        l.add(null, "3 .         <num:rtr>       process id");
        l.add(null, "1 2  router6                      lookup addresses");
        cfgRtr.getRouterList(l, 0, "");
        l.add(null, "3 .         <num:rtr>       process id");
        l.add(null, "1 .  route-details                print prefix details");
        l.add(null, "1 .  route-hacked                 hackerize prefix details");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(60000);
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        new servHoneyPotConn(this, pipe, id.peerAddr.copyBytes(), id.portRem);
        return false;
    }

    /**
     * find one forwarder
     *
     * @param adr address to check
     * @param fwd4 ipv4 candidate
     * @param fwd6 ipv6 candidate
     * @return proper one, null if nothing
     */
    protected final static ipFwd findOneFwd(addrIP adr, ipFwd fwd4, ipFwd fwd6) {
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
     * @param fwd4 ipv4 candidate
     * @param fwd6 ipv6 candidate
     * @return proper one, null if nothing
     */
    protected final static ipRtr findOneRtr(addrIP adr, ipRtr rtr4, ipRtr rtr6) {
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
     * @param ntry route entry
     * @return one liner of the route
     */
    protected static final String getRoute1liner(tabRouteEntry<addrIP> ntry) {
        if (ntry == null) {
            return noRoute;
        }
        return addrPrefix.ip2str(ntry.prefix) + " - " + ntry.best.asPathStr() + " - " + ntry.best.asInfoStr() + " - " + ntry.best.asNameStr();
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

class servHoneyPotConn implements Runnable {

    private servHoneyPot lower;

    private pipeSide pipe;

    private addrIP addr;

    private int port;

    public servHoneyPotConn(servHoneyPot parent, pipeSide conn, addrIP peer, int prt) {
        lower = parent;
        pipe = conn;
        addr = peer;
        port = prt;
        new Thread(this).start();
    }

    public void run() {
        String s = addr + " - " + port;
        if (lower.resolve) {
            clntDns clnt = new clntDns();
            clnt.doResolvList(cfgAll.nameServerAddr, packDnsRec.generateReverse(addr), false, packDnsRec.typePTR);
            s += " - " + clnt.getPTR();
        }
        ipRtr rtr = servHoneyPot.findOneRtr(addr, lower.router4, lower.router6);
        ipFwd fwd = servHoneyPot.findOneFwd(addr, lower.fwder4, lower.fwder6);
        tabRouteEntry<addrIP> ntry = servHoneyPot.findOneRoute(0, addr, rtr, fwd);
        s += " - " + servHoneyPot.getRoute1liner(ntry);
        pipe.linePut("you (" + s + ") have been logged!");
        List<String> lst = null;
        if (lower.routeDetails) {
            lst = servHoneyPot.getRouteDetails(fwd, ntry, userFormat.tableMode.fancy, lower.routeHacked);
        }
        if (lst != null) {
            byte[] res = servHoneyPot.getRouteAscii(lst);
            pipe.morePut(res, 0, res.length);
        }
        pipe.setClose();
        logger.info("honeypot hit from " + s);
        if (lower.script == null) {
            return;
        }
        lower.script.doRound(bits.str2lst("set remote " + addr));
    }

}
