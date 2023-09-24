package net.freertr.clnt;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgRtr;
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
import net.freertr.user.userHelping;
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
public class clntIpInfWrk {

    private final clntIpInfCfg cfg;

    private final pipeSide pipe;

    private final int port;

    private final boolean othrs;

    private final addrIP addr = new addrIP();

    private boolean http;

    private String style;

    private userFormat.tableMode format;

    private boolean hack;

    private boolean plain;

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
    public clntIpInfWrk(clntIpInfCfg c, pipeSide r, addrIP a, int p) {
        cfg = c;
        pipe = r;
        port = p;
        if (c == null) {
            changeWorkAddr(a);
            othrs = false;
            return;
        }
        hack = c.hacked;
        plain = c.plain;
        style = c.style;
        format = c.format;
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
     * get html boilerplate
     *
     * @param hdr true to preface, false fo closure
     * @return string, null if nothing
     */
    public String getHtmlLines(boolean hdr) {
        if (plain) {
            return null;
        }
        String tag;
        if (format == userFormat.tableMode.html) {
            tag = "table";
        } else {
            tag = "pre";
        }
        if (!hdr) {
            return "</" + tag + "></body></html>";
        }
        if (style == null) {
            return servHttp.htmlHead + "<" + tag + ">";
        }
        return servHttp.htmlHead + "<" + tag + " style=\"" + style + "\">";
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
        pipe.linePut("Content-Type: " + getContentType());
        pipe.linePut("Connection: Close");
        pipe.linePut("");
        String a = getHtmlLines(true);
        if (a == null) {
            return;
        }
        pipe.linePut(a);
    }

    /**
     * do minimal http exchange
     */
    public void doHttpFinish() {
        if (!http) {
            return;
        }
        String a = getHtmlLines(false);
        if (a == null) {
            return;
        }
        pipe.linePut(a);
    }

    /**
     * process url parameters
     *
     * @param a parameters
     */
    public void doHttpUrl(String a) {
        if (debugger.clntIpInfo) {
            logger.debug("api " + a + " queried " + addr + " " + port);
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
        if (a.equals("plain")) {
            plain = true;
            return false;
        }
        if (a.equals("unplain")) {
            plain = false;
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
        if (a.equals("style")) {
            style = doSanityStyle(cmd.word());
            return false;
        }
        if (a.equals("unstyle")) {
            style = null;
            return false;
        }
        if (a.equals("format")) {
            format = userFormat.str2tabmod(cmd.word());
            return false;
        }
        if (debugger.clntIpInfo) {
            logger.debug("bad api " + a + " queried " + addr + " " + port);
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
            resolved = null;
            return;
        }
        clntDns clnt = new clntDns();
        clnt.doResolvList(cfgAll.nameServerAddr, packDnsRec.generateReverse(addr), false, packDnsRec.typePTR);
        resolved = clnt.getPTR();
        if (resolved != null) {
            return;
        }
        logger.info("no reverse dns " + addr + " " + port);
    }

    /**
     * get one liner information
     *
     * @return single line of information
     */
    public String getRoute1liner() {
        String s = addr + " " + port + " " + getRoute1liner(fwd, rtr, ntry);
        if (resolved != null) {
            s += " " + resolved;
        }
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
        List<String> res = getRouteDetails(fwd, ntry, format, hack);
        if (!single) {
            return res;
        }
        String a = getRoute1liner();
        res.add(0, a);
        return res;
    }

    /**
     * get content type
     *
     * @return string
     */
    public String getContentType() {
        if (plain) {
            return "text/plain";
        } else {
            return "text/html";
        }
    }

    /**
     * preform style checks
     *
     * @param a string
     * @return null on error, style if ok
     */
    public final static String doSanityStyle(String a) {
        if (a == null) {
            return null;
        }
        if (a.indexOf("\"") >= 0) {
            return null;
        }
        if (a.indexOf("\\") >= 0) {
            return null;
        }
        if (a.indexOf(">") >= 0) {
            return null;
        }
        return a;
    }

    /**
     * perform sanity checks
     *
     * @param cfg config to repair
     * @return changes made
     */
    public final static int doSanityChecks(clntIpInfCfg cfg) {
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
        if (cfg.style != null) {
            String a = doSanityStyle(cfg.style);
            if (a.length() < 1) {
                cfg.style = null;
                chg++;
                a = null;
            }
            cfg.style = a;
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
                + " " + ntry.best.asPathStr().trim()
                + " " + ntry.best.asInfoStr().trim()
                + " " + ntry.best.asNameStr().trim();
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
        List<String> lst = res.formatAll(tm);
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
        lst.add(null, (tab + 1) + " " + (tab + 2) + "  style                        colorize prefix details");
        lst.add(null, (tab + 2) + " .    <str>                      string to send");
        lst.add(null, (tab + 1) + " " + (tab + 2) + "  format                       format prefix details");
        lst.add(null, (tab + 2) + " .    normal                     select normal mode");
        lst.add(null, (tab + 2) + " .    table                      select table mode");
        lst.add(null, (tab + 2) + " .    fancy                      select fancy mode");
        lst.add(null, (tab + 2) + " .    csv                        select csv mode");
        lst.add(null, (tab + 2) + " .    raw                        select raw mode");
        lst.add(null, (tab + 2) + " .    html                       select html mode");
        lst.add(null, (tab + 1) + " .  details                      print prefix details");
        lst.add(null, (tab + 1) + " .  single                       print prefix summary");
        lst.add(null, (tab + 1) + " .  hacked                       hackerize prefix details");
        lst.add(null, (tab + 1) + " .  plain                        plain prefix details");
        lst.add(null, (tab + 1) + " .  resolve                      resolve addresses");
        lst.add(null, (tab + 1) + " .  tinyhttp                     pretend http server");
        lst.add(null, (tab + 1) + " .  others                       allow any addresses");
    }

    public static final void getConfig(List<String> lst, clntIpInfCfg cfg, String beg) {
        if (cfg == null) {
            return;
        }
        if (cfg.router4 != null) {
            lst.add(beg + "router4 " + cfg.router4.routerGetName());
        }
        if (cfg.router6 != null) {
            lst.add(beg + "router6 " + cfg.router6.routerGetName());
        }
        if (cfg.details) {
            lst.add(beg + "details");
        }
        if (cfg.single) {
            lst.add(beg + "single");
        }
        if (cfg.hacked) {
            lst.add(beg + "hacked");
        }
        if (cfg.plain) {
            lst.add(beg + "plain");
        }
        if (cfg.style != null) {
            lst.add(beg + "style " + cfg.style);
        }
        if (cfg.format != userFormat.tableMode.normal) {
            lst.add(beg + "format " + userFormat.tabmod2str(cfg.format));
        }
        if (cfg.rd != 0) {
            lst.add(beg + "rd " + tabRouteUtil.rd2string(cfg.rd));
        }
        if (cfg.script != null) {
            lst.add(beg + "script " + cfg.script.name);
        }
        if (cfg.resolve) {
            lst.add(beg + "resolve");
        }
        if (cfg.tinyHttp) {
            lst.add(beg + "tinyhttp");
        }
        if (cfg.others) {
            lst.add(beg + "others");
        }
    }

}
