package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgInit;
import org.freertr.enc.encUrl;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.prt.prtTcp;
import org.freertr.sec.secInfoCfg;
import org.freertr.sec.secInfoCls;
import org.freertr.sec.secInfoUtl;
import org.freertr.sec.secInfoWrk;
import org.freertr.user.userFilter;
import org.freertr.user.userHelp;
import org.freertr.util.cmds;
import org.freertr.util.logger;

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
    public final static int port = 2;

    /**
     * defaults text
     */
    public final static userFilter[] defaultF = {
        new userFilter("server honeypot .*", cmds.tabulator + "port " + port, null),
        new userFilter("server honeypot .*", cmds.tabulator + "protocol " + proto2string(protoAllStrm), null),
        new userFilter("server honeypot .*", cmds.tabulator + "logging", null),
        new userFilter("server honeypot .*", cmds.tabulator + cmds.negated + cmds.tabulator + "tiny-http", null),
        new userFilter("server honeypot .*", cmds.tabulator + cmds.negated + cmds.tabulator + "blackhole", null),
        new userFilter("server honeypot .*", cmds.tabulator + cmds.negated + cmds.tabulator + "closed", null)
    };

    /**
     * ip information configuration
     */
    public secInfoCfg ipInfo;

    /**
     * pretend a http server
     */
    public boolean tinyHttp;

    /**
     * enable logging
     */
    public boolean logging = true;

    /**
     * pretend closed port
     */
    public boolean closed;

    /**
     * blackhole immediately
     */
    public boolean blackhole;

    public userFilter[] srvDefFlt() {
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
        cmds.cfgLine(lst, !logging, beg, "logging", "");
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
        if (a.equals("logging")) {
            logging = !neg;
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

    public void srvHelp(userHelp l) {
        l.add(null, false, 1, new int[]{-1}, "tiny-http", "pretend http server");
        l.add(null, false, 1, new int[]{-1}, "logging", "log the hits");
        l.add(null, false, 1, new int[]{-1}, "closed", "pretend closed port");
        l.add(null, false, 1, new int[]{-1}, "blackhole", "blackhole immediately");
        secInfoUtl.getHelp(l, 1, "info", "report parameters");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        if (blackhole) {
            srvBlackholePeer(id.peerAddr.isIPv4(), id.peerAddr);
        }
        if (closed) {
            if (logging) {
                logger.info("honeypot hit from " + id.peerAddr);
            }
            pipe.setClose();
            return true;
        }
        pipe.setTime(60000);
        pipe.setReady();
        new servHoneyPotConn(this, pipe, id.peerAddr, id.iface.addr);
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

    /**
     * create one connection
     *
     * @param parent lower
     * @param conn pipe
     * @param rem address
     * @param loc address
     */
    public servHoneyPotConn(servHoneyPot parent, pipeSide conn, addrIP rem, addrIP loc) {
        lower = parent;
        pipe = conn;
        remote = rem;
        local = loc;
        cls = new secInfoCls(null, null, null, lower.srvVrf.getFwd(remote), remote, prtTcp.protoNum, local);
        ipi = new secInfoWrk(lower.ipInfo, cls);
        new Thread(this).start();
    }

    public void run() {
        try {
            pipe.lineTx = pipeSide.modTyp.modeCRLF;
            pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
            pipe.setReady();
            if (!lower.tinyHttp) {
                if (lower.logging) {
                    logger.info("honeypot hit from " + remote);
                }
                ipi.doWork(false);
                ipi.need2drop();
                ipi.putResult(pipe);
                pipe.setClose();
                return;
            }
            String a = pipe.lineGet(1);
            cmds cmd = new cmds("api", a);
            cmd.word();
            encUrl gotUrl = new encUrl();
            gotUrl.fromString("tcp://" + cmd.word());
            ipi.doHttpUrl(gotUrl.toPathName());
            for (;;) {
                a = pipe.lineGet(1);
                int i = a.indexOf(":");
                String s;
                if (i < 0) {
                    s = "";
                } else {
                    s = a.substring(i + 1, a.length());
                    a = a.substring(0, i);
                }
                a = a.trim().toLowerCase();
                s = s.trim();
                if (a.length() < 1) {
                    break;
                }
                if (!a.equals("x-forwarded-for")) {
                    continue;
                }
                i = s.indexOf(",");
                if (i >= 0) {
                    s = s.substring(0, i);
                }
                ipi.setAddr(s);
            }
            if (lower.logging) {
                logger.info("honeypot hit from " + ipi.getAddr() + " via " + remote);
            }
            ipi.doWork(false);
            ipi.need2drop();
            a = ipi.getContentType();
            List<String> r = ipi.getRouteHtml();
            byte[] b = secInfoUtl.getRouteAscii(r);
            pipe.lineTx = pipeSide.modTyp.modeCRLF;
            pipe.lineRx = pipeSide.modTyp.modeCRorLF;
            pipe.linePut("HTTP/1.1 200 ok");
            pipe.linePut("Server: " + cfgInit.versionAgent);
            pipe.linePut("Content-Length: " + b.length);
            pipe.linePut("Content-Type: " + a);
            pipe.linePut("Connection: Close");
            pipe.linePut("");
            pipe.morePut(b, 0, b.length);
            pipe.setClose();
        } catch (Exception e) {
            logger.traceback(e, "" + remote);
        }
    }

}
