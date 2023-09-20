package net.freertr.clnt;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgAll;
import net.freertr.enc.enc7bit;
import net.freertr.enc.encUrl;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipRtr;
import net.freertr.pack.packDnsRec;
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

    private final addrIP addr;

    private boolean http;

    private boolean hack;

    private boolean detail;

    private boolean sumary;

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
        addr = a.copyBytes();
        port = p;
        hack = c.hacked;
        detail = c.details;
        sumary = c.summary;
        http = c.tinyHttp;
        othrs = c.others;
    }

    /**
     * do every work
     */
    public void doWork() {
        if (debugger.clntIpInfo) {
            logger.debug("working on " + addr + " " + port);
        }
        fwd = clntIpInfUtil.findOneFwd(addr, cfg.fwder4, cfg.fwder6);
        rtr = clntIpInfUtil.findOneRtr(addr, cfg.router4, cfg.router6);
        ntry = clntIpInfUtil.findOneRoute(addr, rtr, fwd);
        doResolve();
        doScript();
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
            if (!othrs) {
                return false;
            }
            addrIP adr = new addrIP();
            if (adr.fromString(cmd.word())) {
                return false;
            }
            addr.fromBuf(adr.getBytes(), 0);
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
        if (a.equals("single")) {
            detail = false;
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
        if (sumary) {
            String s = getRoute1liner();
            pipe.linePut(s);
        }
        List<String> lst = getRouteDetails();
        byte[] res = clntIpInfUtil.getRouteAscii(lst);
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
    public synchronized void doScript() {
        if (cfg.script == null) {
            return;
        }
        cfg.script.doRound(bits.str2lst("set remote " + addr));
        cfg.script.doRound(bits.str2lst("set portnum " + port));
    }

    /**
     * execute the script
     */
    public synchronized void doResolve() {
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
     * get route details
     *
     * @return route details or empty list
     */
    public List<String> getRouteDetails() {
        if (!detail) {
            return bits.str2lst("");
        }
        return clntIpInfUtil.getRouteDetails(fwd, ntry, userFormat.tableMode.fancy, hack);
    }

    /**
     * get one liner information
     *
     * @return single line of information
     */
    public String getRoute1liner() {
        String s = addr + " :" + port + " - " + resolved;
        s += " - " + clntIpInfUtil.getRoute1liner(fwd, rtr, ntry);
        if (!hack) {
            return s;
        }
        s = enc7bit.toHackedStr(s);
        return s;
    }

}
