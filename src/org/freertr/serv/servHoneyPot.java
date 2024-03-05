package org.freertr.serv;

import java.util.List;
import org.freertr.addr.addrIP;
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
        "server honeypot .*! port " + port,
        "server honeypot .*! protocol " + proto2string(protoAllStrm)
    };

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    /**
     * ip information configuration
     */
    public secInfoCfg ipInfo;

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
        secInfoUtl.getConfig(lst, ipInfo, beg + "info ");
    }

    public boolean srvCfgStr(cmds cmd) {
        String a = cmd.word();
        boolean neg = a.equals("no");
        if (neg) {
            a = cmd.word();
        }
        if (!a.equals("info")) {
            cmd.badCmd();
            return false;
        }
        ipInfo = secInfoUtl.doCfgStr(ipInfo, cmd, neg);
        return false;
    }

    public void srvHelp(userHelping l) {
        secInfoUtl.getHelp(l, 1, "info            report parameters");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
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
    }

    /**
     * do startup
     */
    public void doStart() {
        new Thread(this).start();
    }

    public void run() {
        try {
            logger.info("honeypot hit from " + remote);
            pipe.setReady();
            secInfoCls cls = new secInfoCls(null, null, null, lower.srvVrf.getFwd(remote), remote, prtTcp.protoNum, local);
            secInfoWrk ipi = new secInfoWrk(lower.ipInfo, cls, pipe);
            ipi.doHttpRead();
            ipi.doWork(false);
            ipi.need2drop();
            ipi.doHttpWrite();
            ipi.putResult();
            ipi.doHttpFinish();
            pipe.setClose();
        } catch (Exception e) {
            logger.traceback(e, "" + remote);
        }
    }

}
