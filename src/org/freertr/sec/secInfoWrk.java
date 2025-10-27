package org.freertr.sec;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgInit;
import org.freertr.cfg.cfgRtr;
import org.freertr.clnt.clntDns;
import org.freertr.clnt.clntPmtud;
import org.freertr.enc.enc7bit;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipRtr;
import org.freertr.pack.packDnsRec;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeSide;
import org.freertr.rtr.rtrBgpUtil;
import org.freertr.serv.servHttp;
import org.freertr.tab.tabRpkiRoa;
import org.freertr.tab.tabRpkiUtil;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRtrplc;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * generic information worker
 *
 * @author matecsaba
 */
public class secInfoWrk implements Runnable {

    /**
     * configuration to use
     */
    protected final secInfoCfg config;

    /**
     * closer to use
     */
    protected final secInfoCls closer;

    /**
     * forwarder to use
     */
    protected final ipFwd connFwd;

    /**
     * protocol number
     */
    protected final int proto;

    /**
     * allow to query others
     */
    protected final boolean othrs;

    /**
     * address to query
     */
    protected final addrIP addr = new addrIP();

    /**
     * local address
     */
    protected final addrIP local;

    /**
     * tracker result
     */
    protected boolean tracker;

    /**
     * style to send
     */
    protected String style;

    /**
     * set table formatter
     */
    protected userFormat.tableMode format;

    /**
     * set box formatter
     */
    public userFormat.boxerMode boxed;

    /**
     * hack route details
     */
    protected boolean hack;

    /**
     * plain route details
     */
    protected boolean plain;

    /**
     * ip only headline
     */
    protected boolean justip;

    /**
     * add route details
     */
    protected boolean detail;

    /**
     * add route summary
     */
    protected boolean single;

    /**
     * add address summary
     */
    protected boolean client;

    /**
     * separate summary sections
     */
    protected boolean separate;

    /**
     * resolution status
     */
    protected String resolved = null;

    /**
     * router selected
     */
    protected cfgRtr rtrCfg = null;

    /**
     * router selected
     */
    protected ipRtr rtrIp = null;

    /**
     * valid selected
     */
    protected cfgRtr vldCfg = null;

    /**
     * valid selected
     */
    protected ipRtr vldIp = null;

    /**
     * forwarder selected
     */
    protected ipFwd fwd = null;

    /**
     * route entry found
     */
    protected tabRouteEntry<addrIP> ntry = null;

    /**
     * roa result got
     */
    protected int rpkiR = 0;

    /**
     * aspa result got
     */
    protected int rpkiA = 0;

    /**
     * pmtud result
     */
    protected clntPmtud pmtuD;

    /**
     * create an instance
     *
     * @param ned configuration to use
     * @param cls closer to use
     */
    public secInfoWrk(secInfoCfg ned, secInfoCls cls) {
        if (ned == null) {
            ned = new secInfoCfg();
        }
        closer = cls;
        connFwd = cls.fwder;
        config = ned;
        proto = cls.protNum;
        local = cls.local;
        hack = ned.hacked;
        plain = ned.plain;
        justip = ned.justip;
        style = ned.style;
        format = ned.format;
        boxed = ned.boxed;
        detail = ned.details;
        single = ned.single;
        client = ned.client;
        separate = ned.separate;
        othrs = ned.others;
        changeWorkAddr(cls.remote);
    }

    public void run() {
        doLongWork();
        doClosures();
    }

    public String toString() {
        return "" + addr;
    }

    private void changeWorkAddr(addrIP adr) {
        addr.fromBuf(adr.getBytes(), 0);
    }

    /**
     * get one liner information
     *
     * @return single line of information
     */
    public List<String> getRoute1liner() {
        if (justip) {
            return bits.str2lst("" + addr);
        }
        List<String> res = secInfoUtl.getRoute1liner(this);
        if (!hack) {
            return res;
        }
        res = enc7bit.toHackedLst(res);
        return res;
    }

    /**
     * get pmtud result
     */
    public void doPmtud() {
        pmtuD = secInfoUtl.doPmtud(config, connFwd, addr, local);
    }

    /**
     * do every work
     *
     * @param thrd start new thread if needed
     * @return true if a new thread started
     */
    public boolean doWork(boolean thrd) {
        doTrackers();
        doFindRoute();
        doCheckRpf();
        doCheckAcl();
        if (tracker) {
            return false;
        }
        thrd &= config.resolve || (config.pmtudTim > 0) || (config.script != null);
        if (thrd) {
            new Thread(this).start();
            return true;
        }
        doLongWork();
        return false;
    }

    /**
     * check if drop needed
     *
     * @return true if yes, false if no
     */
    public boolean need2drop() {
        if (tracker) {
            return true;
        }
        if (pmtuD == null) {
            return false;
        }
        if (pmtuD.last < 1) {
            return true;
        }
        return false;
    }

    /**
     * close the connections
     */
    protected void doClosures() {
        if (!need2drop()) {
            return;
        }
        try {
            if (closer.closeP != null) {
                closer.closeP.setClose();
            }
            if (closer.closeC != null) {
                closer.closeC.setClosing();
            }
        } catch (Exception e) {
            logger.traceback(e, addr + " " + proto);
        }
    }

    /**
     * update tracker
     */
    protected void doTrackers() {
        try {
            tracker = false;
            if (config.tracker != null) {
                tracker |= !config.tracker.getStatus();
            }
            if (config.startupDelay > 0) {
                tracker |= (bits.getTime() - cfgInit.started) < config.startupDelay;
            }
            if (config.accessRate != null) {
                tracker |= config.accessRate.check(1);
            }
        } catch (Exception e) {
            logger.traceback(e, addr + " " + proto);
        }
    }

    /**
     * check the route
     */
    protected void doCheckAcl() {
        try {
            if (config.accessList == null) {
                return;
            }
            if (closer.closeC != null) {
                tracker |= !config.accessList.matches(closer.closeC);
                return;
            }
            packHolder pck = new packHolder(true, true);
            pck.IPsrc.setAddr(addr);
            pck.IPtrg.setAddr(closer.local);
            pck.IPprt = proto;
            tracker |= !config.accessList.matches(false, false, pck);
        } catch (Exception e) {
            logger.traceback(e, addr + " " + proto);
        }
    }

    /**
     * check the route
     */
    protected void doCheckRpf() {
        try {
            if ((config.prefixList == null) && (config.routeMap == null) && (config.routePolicy == null)) {
                return;
            }
            tabRouteEntry<addrIP> n = closer.fwder.actualU.route(addr);
            if (n == null) {
                tracker = true;
                return;
            }
            if (config.prefixList != null) {
                tracker |= !config.prefixList.matches(rtrBgpUtil.sfiUnicast, 0, n.prefix);
            }
            if (config.routeMap != null) {
                tracker |= !config.routeMap.matches(rtrBgpUtil.sfiUnicast, 0, n);
            }
            if (config.routePolicy != null) {
                tracker |= tabRtrplc.doRpl(rtrBgpUtil.sfiUnicast, 0, n, config.routePolicy, true) == null;
            }
        } catch (Exception e) {
            logger.traceback(e, addr + " " + proto);
        }
    }

    /**
     * find the route
     */
    protected void doFindRoute() {
        try {
            fwd = secInfoUtl.findOneFwd(addr, config.fwder4, config.fwder6);
            rtrCfg = secInfoUtl.findOneRtr(addr, config.router4typ, config.router6typ, config.router4num, config.router6num);
            if (rtrCfg == null) {
                return;
            }
            rtrIp = rtrCfg.getRouter();
            if (fwd == null) {
                fwd = rtrCfg.fwd;
            }
            ntry = secInfoUtl.findOneRoute(addr, rtrIp, fwd);
            if (ntry == null) {
                return;
            }
            vldCfg = secInfoUtl.findOneRtr(addr, config.valid4typ, config.valid6typ, config.valid4num, config.valid6num);
            if (vldCfg == null) {
                return;
            }
            vldIp = vldCfg.getRouter();
            tabRpkiRoa rpkiV = secInfoUtl.findOneValidRoa(ntry, vldIp, fwd);
            rpkiR = tabRpkiUtil.calcValidityRoa(ntry.prefix, ntry.best, rpkiV);
            rpkiA = tabRpkiUtil.calcValidityAspa(ntry.best, secInfoUtl.findOneValidAspa(vldIp, fwd), 0);
        } catch (Exception e) {
            logger.traceback(e, addr + " " + proto);
        }
    }

    /**
     * do time consuming work
     */
    protected void doLongWork() {
        if (debugger.clntIpInfo) {
            logger.debug("working on " + addr + " " + proto);
        }
        try {
            doPmtud();
            doResolve();
            doScript();
        } catch (Exception e) {
            logger.traceback(e, addr + " " + proto);
        }
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
        String a = servHttp.htmlHead + "<title>ipinfo</title></head><body";
        if (style == null) {
            a += ">";
        } else {
            a += " style=\"" + style + "\">";
        }
        return a + "<" + tag + ">";
    }

    /**
     * get content type
     *
     * @return content type, null if no config
     */
    public String getContentType() {
        if (plain) {
            return "text/plain";
        } else {
            return "text/html";
        }
    }

    /**
     * process url parameters
     *
     * @param a parameters
     */
    public void doHttpUrl(String a) {
        if (debugger.clntIpInfo) {
            logger.debug("api " + a + " queried " + addr + " " + proto);
        }
        cmds cmd = new cmds("url", a.replaceAll("/", " "));
        for (;;) {
            if (secInfoUtl.doOneHttp(this, cmd)) {
                break;
            }
        }
    }

    /**
     * set address
     *
     * @param a address to use
     */
    public void setAddr(String a) {
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
     * get address
     *
     * @return address in use
     */
    public addrIP getAddr() {
        return addr.copyBytes();
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
        byte[] res = secInfoUtl.getRouteAscii(lst);
        pipe.morePut(res, 0, res.length);
    }

    /**
     * execute the script
     */
    protected void doScript() {
        if (config.script == null) {
            return;
        }
        List<String> lst = new ArrayList<String>();
        lst.add("set remote " + addr);
        lst.add("set proto " + proto);
        config.script.doRound(lst);
    }

    /**
     * execute the script
     */
    protected void doResolve() {
        if (resolved != null) {
            return;
        }
        if (!config.resolve) {
            resolved = null;
            return;
        }
        clntDns clnt = new clntDns();
        clnt.doResolvList(cfgAll.nameServerAddr, packDnsRec.generateReverse(addr), false, packDnsRec.typePTR);
        resolved = clnt.getPTR();
        if (resolved != null) {
            return;
        }
        logger.info("no reverse dns " + addr + " " + proto);
    }

    /**
     * get route details
     *
     * @return route details or empty list
     */
    public List<String> getRouteInfos() {
        List<String> res = new ArrayList<String>();
        if (single) {
            res.addAll(getRoute1liner());
        }
        if (!detail) {
            return res;
        }
        res.addAll(secInfoUtl.getRouteDetails(fwd, ntry, format, boxed, hack));
        return res;
    }

    /**
     * get route details
     *
     * @return route details or empty list
     */
    public List<String> getRouteHtml() {
        List<String> r = getRouteInfos();
        String a = getHtmlLines(true);
        if (a != null) {
            r.add(0, a);
        }
        a = getHtmlLines(false);
        if (a != null) {
            r.add(a);
        }
        return r;
    }

}
