package net.freertr.clnt;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAll;
import net.freertr.enc.enc7bit;
import net.freertr.enc.encUrl;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipRtr;
import net.freertr.pack.packDnsRec;
import net.freertr.pipe.pipeSide;
import net.freertr.serv.servHttp;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRouteUtil;
import net.freertr.user.userFormat;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.verCore;
import net.freertr.util.version;

/**
 * generic ipinfo worker
 *
 * @author matecsaba
 */
public class clntIpInfWork {

    private final clntIpInfConf cfg;

    private final pipeSide pipe;

    private final int port;

    private final boolean othrs;

    private final String style = "background-color: #000000; color: #00FFFF;"; ////

    private final addrIP addr = new addrIP();

    private boolean http;

    private boolean hack;

    private boolean detail;

    private boolean single;

    private String resolved = null;

    private ipRtr rtr = null;

    private ipFwd fwd = null;

    private tabRouteEntry<addrIP> ntry = null;

    /**
     * create an instance
     *
     * @param c configuration to use
     * @param r pipeline to use
     * @param a address to check
     * @param p port number to check
     */
    public clntIpInfWork(clntIpInfConf c, pipeSide r, addrIP a, int p) {
        cfg = c;
        pipe = r;
        port = p;
        hack = c.hacked;
        detail = c.details;
        single = c.single;
        http = c.tinyHttp;
        othrs = c.others;
        changeWorkAddr(a);
    }

    /**
     * do every work
     */
    public void doWork() {
        if (debugger.clntIpInfo) {
            logger.debug("working on " + addr + " " + port);
        }
        try {
            fwd = findOneFwd(addr, cfg.fwder4, cfg.fwder6);
            rtr = findOneRtr(addr, cfg.router4, cfg.router6);
            ntry = findOneRoute(addr, rtr, fwd);
            doResolve();
            doScript();
        } catch (Exception e) {
            logger.traceback(e, addr + " " + port);
        }
    }

    private void changeWorkAddr(addrIP adr) {
        addr.fromBuf(adr.getBytes(), 0);
    }

    /**
     * do minimal http exchange
     */
    public void doHttpRead() {
        if (!http) {
            return;
        }
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        String s = pipe.lineGet(1);
        cmds cmd = new cmds("api", s);
        cmd.word();
        encUrl gotUrl = new encUrl();
        gotUrl.fromString("tcp://" + cmd.word());
        doHttpUrl(gotUrl.toPathName());
    }

    /**
     * do minimal http exchange
     */
    public void doHttpWrite() {
        if (!http) {
            return;
        }
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        pipe.linePut("HTTP/1.1 200 ok");
        pipe.linePut("Server: " + version.usrAgnt);
        pipe.linePut("Content-Type: text/html");
        pipe.linePut("Connection: Close");
        pipe.linePut("");
        pipe.linePut(servHttp.htmlHead);
        pipe.linePut("<pre style=\" " + style + " \">");
    }

    /**
     * do minimal http exchange
     */
    public void doHttpFinish() {
        if (!http) {
            return;
        }
        pipe.linePut("</pre></body></html>");
    }

    /**
     * process url parameters
     *
     * @param a parameters
     */
    public void doHttpUrl(String a) {
        if (debugger.clntIpInfo) {
            logger.debug("api queried " + a + " from " + addr + " " + port);
        }
        cmds cmd = new cmds("url", a.replaceAll("/", " "));
        for (;;) {
            if (doOneCfg(cmd)) {
                break;
            }
        }
    }

    /**
     * set address
     *
     * @param a address to use
     */
    protected void setAddr(String a) {
        if (!othrs) {
            return;
        }
        addrIP adr = new addrIP();
        if (adr.fromString(a)) {
            return;
        }
        changeWorkAddr(adr);
    }

    /**
     * do one config command
     *
     * @param cmd command
     * @return true to terminate reading
     */
    public boolean doOneCfg(cmds cmd) {
        String a = cmd.word();
        if (a.length() < 1) {
            return true;
        }
        if (a.equals("addr")) {
            setAddr(cmd.word());
            return false;
        }
        if (a.equals("hacked")) {
            hack = true;
            return false;
        }
        if (a.equals("unhack")) {
            hack = false;
            return false;
        }
        if (a.equals("detail")) {
            detail = true;
            return false;
        }
        if (a.equals("undetail")) {
            detail = false;
            return false;
        }
        if (a.equals("single")) {
            single = true;
            return false;
        }
        if (a.equals("unsingle")) {
            single = false;
            return false;
        }
        if (a.equals("http")) {
            http = true;
            return false;
        }
        if (a.equals("unhttp")) {
            http = false;
            return false;
        }
        if (debugger.clntIpInfo) {
            logger.debug("bad api queried " + a + " from " + addr + " " + port);
        }
        return false;
    }

    /**
     * print out results
     *
     * @param pipe pipeline to use
     */
    public void putResult(pipeSide pipe) {
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        List<String> lst = getRouteInfos();
        byte[] res = getRouteAscii(lst);
        pipe.morePut(res, 0, res.length);
    }

    /**
     * print out results
     */
    public void putResult() {
        putResult(pipe);
    }

    /**
     * execute the script
     */
    public void doScript() {
        if (cfg.script == null) {
            return;
        }
        List<String> lst = new ArrayList<String>();
        lst.add("set remote " + addr);
        lst.add("set portnum " + port);
        cfg.script.doRound(lst);
    }

    /**
     * execute the script
     */
    public void doResolve() {
        if (resolved != null) {
            return;
        }
        if (!cfg.resolve) {
            resolved = "no dns allowed";
            return;
        }
        clntDns clnt = new clntDns();
        clnt.doResolvList(cfgAll.nameServerAddr, packDnsRec.generateReverse(addr), false, packDnsRec.typePTR);
        resolved = clnt.getPTR();
        if (resolved != null) {
            return;
        }
        logger.info("no reverse dns");
    }

    /**
     * get one liner information
     *
     * @return single line of information
     */
    public String getRoute1liner() {
        String s = addr + " :" + port + " - " + resolved;
        s += " - " + getRoute1liner(fwd, rtr, ntry);
        if (!hack) {
            return s;
        }
        s = enc7bit.toHackedStr(s);
        return s;
    }

    /**
     * get route details
     *
     * @return route details or empty list
     */
    public List<String> getRouteInfos() {
        if (!detail) {
            if (!single) {
                return new ArrayList<String>();
            }
            return bits.str2lst(getRoute1liner());
        }
        List<String> res = getRouteDetails(fwd, ntry, userFormat.tableMode.fancy, hack);
        if (!single) {
            return res;
        }
        String a = getRoute1liner();
        res.add(0, a);
        return res;
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
