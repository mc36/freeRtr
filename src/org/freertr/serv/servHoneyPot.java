package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.enc.encUrl;
import org.freertr.ip.ipFwd;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.prt.prtTcp;
import org.freertr.sec.secInfoCfg;
import org.freertr.sec.secInfoCls;
import org.freertr.sec.secInfoUtl;
import org.freertr.sec.secInfoWrk;
import org.freertr.tab.tabGen;
import org.freertr.user.userFilter;
import org.freertr.user.userHelping;
import org.freertr.util.cmds;
import org.freertr.util.logger;
import org.freertr.util.version;

/**
 * honeypot server
 *
 * @author matecsaba
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
    public final static int port = 22;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server honeypot .*!" + cmds.tabulator + "port " + port,
        "server honeypot .*!" + cmds.tabulator + "protocol " + proto2string(protoAllStrm),
        "server honeypot .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "tiny-http",
        "server honeypot .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "blackhole",
        "server honeypot .*!" + cmds.tabulator + cmds.negated + cmds.tabulator + "closed"
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * ip information configuration
     */
    public secInfoCfg ipInfo;

    /**
     * pretend a http server
     */
    public boolean tinyHttp;

    /**
     * pretend closed port
     */
    public boolean closed;

    /**
     * blackhole immediately
     */
    public boolean blackhole;

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
        cmds.cfgLine(lst, !tinyHttp, beg, "tiny-http", "");
        cmds.cfgLine(lst, !closed, beg, "closed", "");
        cmds.cfgLine(lst, !blackhole, beg, "blackhole", "");
        secInfoUtl.getConfig(lst, ipInfo, beg + "info ");
    }

    public boolean srvCfgStr(cmds cmd) {
        String a = cmd.word();
        boolean neg = a.equals(cmds.negated);
        if (neg) {
            a = cmd.word();
        }
        if (a.equals("tiny-http")) {
            tinyHttp = !neg;
            return false;
        }
        if (a.equals("blackhole")) {
            blackhole = !neg;
            return false;
        }
        if (a.equals("closed")) {
            closed = !neg;
            return false;
        }
        if (!a.equals("info")) {
            cmd.badCmd();
            return false;
        }
        ipInfo = secInfoUtl.doCfgStr(ipInfo, cmd, neg);
        return false;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 .  tiny-http                     pretend http server");
        l.add(null, "1 .  closed                        pretend closed port");
        l.add(null, "1 .  blackhole                     blackhole immediately");
        secInfoUtl.getHelp(l, 1, "info            report parameters");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        logger.info("honeypot hit from " + id.peerAddr);
        if (blackhole) {
            srvBlackholePeer(id.peerAddr.isIPv4(), id.peerAddr);
        }
        if (closed) {
            return true;
        }
        pipe.setTime(60000);
        pipe.setReady();
        servHoneyPotConn ntry = new servHoneyPotConn(this, pipe, id.peerAddr, id.iface.addr);
        ntry.doStart();
        return false;
    }

}

class servHoneyPotConn implements Runnable {

    private final servHoneyPot lower;

    private final pipeSide pipe;

    private final secInfoCls cls;

    private final secInfoWrk ipi;

    private final addrIP remote;

    private final addrIP local;

    private final ipFwd fwdr;

    /**
     * create one connection
     *
     * @param parent lower
     * @param conn pipe
     * @param rem address
     * @param prt port
     */
    public servHoneyPotConn(servHoneyPot parent, pipeSide conn, addrIP rem, addrIP loc) {
        lower = parent;
        pipe = conn;
        remote = rem;
        local = loc;
        fwdr = lower.srvVrf.getFwd(rem);
        cls = new secInfoCls(null, null, null, lower.srvVrf.getFwd(remote), remote, prtTcp.protoNum, local);
        ipi = new secInfoWrk(lower.ipInfo, cls);
    }

    /**
     * do startup
     */
    public void doStart() {
        new Thread(this).start();
    }

    /**
     * do minimal http exchange
     */
    public void doHttpRead() {
        if (!lower.tinyHttp) {
            return;
        }
        if (pipe == null) {
            return;
        }
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        String s = pipe.lineGet(1);
        cmds cmd = new cmds("api", s);
        cmd.word();
        encUrl gotUrl = new encUrl();
        gotUrl.fromString("tcp://" + cmd.word());
        ipi.doHttpUrl(gotUrl.toPathName());
        for (;;) {
            s = pipe.lineGet(1);
            if (s.length() < 1) {
                break;
            }
        }
    }

    /**
     * do minimal http exchange
     */
    public void doHttpWrite() {
        if (!lower.tinyHttp) {
            return;
        }
        if (pipe == null) {
            return;
        }
        pipe.lineTx = pipeSide.modTyp.modeCRLF;
        pipe.lineRx = pipeSide.modTyp.modeCRorLF;
        pipe.linePut("HTTP/1.1 200 ok");
        pipe.linePut("Server: " + version.usrAgnt);
        pipe.linePut("Content-Type: " + ipi.getContentType());
        pipe.linePut("Connection: Close");
        pipe.linePut("");
        String a = ipi.getHtmlLines(true);
        if (a == null) {
            return;
        }
        pipe.linePut(a);
    }

    /**
     * do minimal http exchange
     */
    public void doHttpFinish() {
        if (!lower.tinyHttp) {
            return;
        }
        if (pipe == null) {
            return;
        }
        String a = ipi.getHtmlLines(false);
        if (a == null) {
            return;
        }
        pipe.linePut(a);
    }

    /**
     * print out results
     */
    public void putResult() {
        if (pipe == null) {
            return;
        }
        ipi.putResult(pipe);
    }

    public void run() {
        try {
            pipe.setReady();
            doHttpRead();
            ipi.doWork(false);
            ipi.need2drop();
            doHttpWrite();
            putResult();
            doHttpFinish();
            pipe.setClose();
        } catch (Exception e) {
            logger.traceback(e, "" + remote);
        }
    }

}
