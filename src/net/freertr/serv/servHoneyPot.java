package net.freertr.serv;

import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.clnt.clntIpInfCfg;
import net.freertr.clnt.clntIpInfWrk;
import net.freertr.clnt.clntPmtudCfg;
import net.freertr.clnt.clntPmtudWrk;
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
    public clntPmtudCfg pmtuD; //////////

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
        clntPmtudWrk.getConfig(lst, pmtuD, beg + "pmtu ");
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
        if (a.equals("pmtu")) {
            pmtuD = clntPmtudCfg.doCfgStr(pmtuD, cmd, neg);
            return false;
        }
        cmd.badCmd();
        return false;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  info                      check visitors");
        clntIpInfWrk.getHelp(l, 1);
        l.add(null, "1 2  pmtu                      check pmtu");
        clntPmtudWrk.getHelp(l, 1);
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(60000);
        pipe.setReady();
        servHoneyPotConn ntry = new servHoneyPotConn(this, pipe, id.peerAddr.copyBytes(), id.portRem);
        ntry.doStart();
        return false;
    }

}

class servHoneyPotConn implements Runnable {

    private final servHoneyPot lower;

    private final pipeSide pipe;

    private final addrIP addr;

    private final int port;

    /**
     * create one connection
     *
     * @param parent lower
     * @param conn pipe
     * @param peer address
     * @param prt port
     */
    public servHoneyPotConn(servHoneyPot parent, pipeSide conn, addrIP peer, int prt) {
        lower = parent;
        pipe = conn;
        addr = peer;
        port = prt;
    }

    /**
     * do startup
     */
    public void doStart() {
        new Thread(this).start();
    }

    public void run() {
        try {
            pipe.setReady();
            logger.info("honeypot hit from " + addr + " " + port);
            clntIpInfWrk w = new clntIpInfWrk(lower.ipInfo, pipe, addr, port);
            w.doHttpRead();
            w.doWork();
            w.doHttpWrite();
            w.putResult();
            w.doHttpFinish();
            pipe.setClose();
        } catch (Exception e) {
            logger.traceback(e, addr + " " + port);
        }
    }

}
