package net.freertr.serv;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.clnt.clntIpInfCfg;
import net.freertr.clnt.clntIpInfWrk;
import net.freertr.clnt.clntPmtudCfg;
import net.freertr.clnt.clntPmtudWrk;
import net.freertr.ip.ipFwd;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.cmds;
import net.freertr.util.logger;

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
    public clntIpInfCfg ipInfo;

    /**
     * pmtu information configuration
     */
    public clntPmtudCfg pmtuD;

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
        clntIpInfWrk.getConfig(lst, ipInfo, beg + "info ");
        clntPmtudWrk.getConfig(lst, pmtuD, beg + "pmtud ");
    }

    public boolean srvCfgStr(cmds cmd) {
        String a = cmd.word();
        boolean neg = a.equals("no");
        if (neg) {
            a = cmd.word();
        }
        if (a.equals("info")) {
            ipInfo = clntIpInfCfg.doCfgStr(ipInfo, cmd, neg);
            return false;
        }
        if (a.equals("pmtud")) {
            pmtuD = clntPmtudCfg.doCfgStr(pmtuD, cmd, neg);
            return false;
        }
        cmd.badCmd();
        return false;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  info                      check visitors");
        clntIpInfWrk.getHelp(l, 1);
        l.add(null, "1 2  pmtud                     check pmtud");
        clntPmtudWrk.getHelp(l, 1);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(60000);
        pipe.setReady();
        servHoneyPotConn ntry = new servHoneyPotConn(this, pipe, id.peerAddr, id.iface.addr, id.portRem);
        ntry.doStart();
        return false;
    }

}

class servHoneyPotConn implements Runnable {

    private final servHoneyPot lower;

    private final pipeSide pipe;

    private final addrIP remote;

    private final addrIP local;

    private final int port;

    private final ipFwd fwdr;

    /**
     * create one connection
     *
     * @param parent lower
     * @param conn pipe
     * @param rem address
     * @param prt port
     */
    public servHoneyPotConn(servHoneyPot parent, pipeSide conn, addrIP rem, addrIP loc, int prt) {
        lower = parent;
        pipe = conn;
        remote = rem;
        local = loc;
        port = prt;
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
            logger.info("honeypot hit from " + remote + " " + port);
            pipe.setReady();
            clntPmtudWrk pmd = clntPmtudCfg.doWork(lower.pmtuD, fwdr, remote, local);
            clntIpInfWrk ipw = new clntIpInfWrk(lower.ipInfo, pipe, remote, port);
            ipw.doHttpRead();
            ipw.doWork();
            ipw.doHttpWrite();
            ipw.putResult();
            ipw.doHttpFinish();
            pipe.setClose();
        } catch (Exception e) {
            logger.traceback(e, remote + " " + port);
        }
    }

}
