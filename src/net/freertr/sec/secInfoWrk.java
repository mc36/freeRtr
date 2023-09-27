package net.freertr.sec;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.clnt.clntDns;
import net.freertr.clnt.clntPmtud;
import net.freertr.enc.enc7bit;
import net.freertr.enc.encUrl;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipRtr;
import net.freertr.pack.packDnsRec;
import net.freertr.pipe.pipeDiscard;
import net.freertr.pipe.pipeSide;
import net.freertr.serv.servHttp;
import net.freertr.tab.tabRouteEntry;
import net.freertr.user.userFormat;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.version;

/**
 * generic information worker
 *
 * @author matecsaba
 */
public class secInfoWrk implements Runnable {

    private final secInfoCfg cfg;

    private final ipFwd pmtuF;

    /**
     * reporter pipe
     */
    private final pipeSide rePip;

    private final int proto;

    private final boolean othrs;

    private final addrIP addr = new addrIP();

    private final addrIP local;

    protected boolean http;

    protected String style;

    protected userFormat.tableMode format;

    protected boolean hack;

    protected boolean plain;

    protected boolean justip;

    protected boolean detail;

    protected boolean single;

    protected String resolved = null;

    protected ipRtr rtr = null;

    protected ipFwd fwd = null;

    protected tabRouteEntry<addrIP> ntry = null;

    protected clntPmtud pmtuD;

    protected secInfoCls closer = new secInfoCls(null, null, null);

    /**
     * create an instance
     *
     * @param ned configuration to use
     * @param con pipeline to use for reports
     * @param fwd forwarder to use
     * @param adr address to check
     * @param prt protocol number to check
     * @param loc local address
     */
    public secInfoWrk(secInfoCfg ned, pipeSide con, ipFwd fwd, addrIP adr, int prt, addrIP loc) {
        pmtuF = fwd;
        cfg = ned;
        rePip = pipeDiscard.needAny(con);
        proto = prt;
        local = loc;
        if (ned == null) {
            changeWorkAddr(adr);
            othrs = false;
            return;
        }
        hack = ned.hacked;
        plain = ned.plain;
        justip = ned.justip;
        style = ned.style;
        format = ned.format;
        detail = ned.details;
        single = ned.single;
        http = ned.tinyHttp;
        othrs = ned.others;
        changeWorkAddr(adr);
    }

    public void run() {
        doTheWork();
    }

    public String toString() {
        return getRoute1liner();
    }

    private void changeWorkAddr(addrIP adr) {
        addr.fromBuf(adr.getBytes(), 0);
    }

    /**
     * get one liner information
     *
     * @return single line of information
     */
    public String getRoute1liner() {
        if (justip) {
            return "" + addr;
        }
        String s = addr + " prt=" + proto;
        if (pmtuD != null) {
            s += " pmtu=" + pmtuD;
        }
        if (resolved != null) {
            s += " dns=" + resolved;
        }
        s += secInfoUtl.getRoute1liner(fwd, rtr, ntry);
        if (!hack) {
            return s;
        }
        s = enc7bit.toHackedStr(s);
        return s;
    }

    /**
     * get pmtud result
     */
    public void doPmtud() {
        pmtuD = secInfoUtl.doPmtud(cfg, pmtuF, addr, local);
    }

    /**
     * do every work
     *
     * @param thrd start new thread if needed
     * @param cls closer to use
     * @return true if a new thread started
     */
    public boolean doWork(boolean thrd, secInfoCls cls) {
        if (cfg == null) {
            return false;
        }
        closer = cls;
        ////////////////////////
        thrd &= cfg.resolve;
        thrd &= cfg.pmtudTim > 0;
        if (!thrd) {
            doTheWork();
            return false;
        }
        new Thread(this).start();
        return true;
    }

    private void doTheWork() {
        if (debugger.clntIpInfo) {
            logger.debug("working on " + addr + " " + proto);
        }
        try {
            fwd = secInfoUtl.findOneFwd(addr, cfg.fwder4, cfg.fwder6);
            rtr = secInfoUtl.findOneRtr(addr, cfg.router4, cfg.router6);
            ntry = secInfoUtl.findOneRoute(addr, rtr, fwd);
            doPmtud();
            doResolve();
            doScript();
        } catch (Exception e) {
            logger.traceback(e, addr + " " + proto);
        }
    }

    /**
     * do minimal http exchange
     */
    public void doHttpRead() {
        if (!http) {
            return;
        }
        rePip.lineTx = pipeSide.modTyp.modeCRLF;
        rePip.lineRx = pipeSide.modTyp.modeCRorLF;
        String s = rePip.lineGet(1);
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
     * do minimal http exchange
     */
    public void doHttpWrite() {
        if (!http) {
            return;
        }
        rePip.lineTx = pipeSide.modTyp.modeCRLF;
        rePip.lineRx = pipeSide.modTyp.modeCRorLF;
        rePip.linePut("HTTP/1.1 200 ok");
        rePip.linePut("Server: " + version.usrAgnt);
        rePip.linePut("Content-Type: " + getContentType());
        rePip.linePut("Connection: Close");
        rePip.linePut("");
        String a = getHtmlLines(true);
        if (a == null) {
            return;
        }
        rePip.linePut(a);
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
        rePip.linePut(a);
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
     * print out results
     *
     * @param pipe pipeline to use
     */
    protected void putResult(pipeSide pipe) {
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        List<String> lst = getRouteInfos();
        byte[] res = secInfoUtl.getRouteAscii(lst);
        pipe.morePut(res, 0, res.length);
    }

    /**
     * print out results
     */
    public void putResult() {
        putResult(rePip);
    }

    /**
     * execute the script
     */
    protected void doScript() {
        if (cfg == null) {
            return;
        }
        if (cfg.script == null) {
            return;
        }
        List<String> lst = new ArrayList<String>();
        lst.add("set remote " + addr);
        lst.add("set proto " + proto);
        cfg.script.doRound(lst);
    }

    /**
     * execute the script
     */
    protected void doResolve() {
        if (cfg == null) {
            return;
        }
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
        logger.info("no reverse dns " + addr + " " + proto);
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
        List<String> res = secInfoUtl.getRouteDetails(fwd, ntry, format, hack);
        if (!single) {
            return res;
        }
        String a = getRoute1liner();
        res.add(0, a);
        return res;
    }

    /**
     * check if drop needed
     *
     * @return true if yes, false if no
     */
    public boolean need2drop() {
        if (cfg == null) {
            return false;
        }
        if (pmtuD == null) {
            return false;
        }
        if (pmtuD.last < 1) {
            return true;
        }
        return false;
    }

}
