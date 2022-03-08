package net.freertr.serv;

import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.addr.addrPool;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgPool;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSetting;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.tab.tabGen;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * sdwan server
 *
 * @author matecsaba
 */
public class servSdwan extends servGeneric implements prtServS {

    /**
     * create instance
     */
    public servSdwan() {
    }

    /**
     * ipv4 pool
     */
    protected addrPool<addrIPv4> pool4;

    /**
     * ipv6 pool
     */
    protected addrPool<addrIPv6> pool6;

    /**
     * port number
     */
    public static final int port = 2554;

    /**
     * list of users
     */
    protected tabGen<servSdwanConn> conns = new tabGen<servSdwanConn>();

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server sdwan .*! port " + port,
        "server sdwan .*! protocol " + proto2string(protoAllStrm),
        "server sdwan .*! no pool4",
        "server sdwan .*! no pool6",};

    /**
     * defaults filter
     */
    public static tabGen<userFilter> defaultF;

    public tabGen<userFilter> srvDefFlt() {
        return defaultF;
    }

    public String srvName() {
        return "sdwan";
    }

    public int srvPort() {
        return port;
    }

    public int srvProto() {
        return protoAllStrm;
    }

    public boolean srvInit() {
        return genStrmStart(this, new pipeLine(65536, false), 0);
    }

    public boolean srvDeinit() {
        return genericStop(0);
    }

    public void srvShRun(String beg, List<String> l, int filter) {
        cmds.cfgLine(l, pool4 == null, beg, "pool4", "" + pool4);
        cmds.cfgLine(l, pool6 == null, beg, "pool6", "" + pool6);
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("pool4")) {
            cfgPool<addrIPv4> ntry = cfgAll.poolFind(cfgAll.ip4pool, cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such pool");
                return false;
            }
            pool4 = ntry.pool;
            return false;
        }
        if (s.equals("pool6")) {
            cfgPool<addrIPv6> ntry = cfgAll.poolFind(cfgAll.ip6pool, cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such pool");
                return false;
            }
            pool6 = ntry.pool;
            return false;
        }
        if (!s.equals("no")) {
            return true;
        }
        s = cmd.word();
        if (s.equals("pool4")) {
            pool4 = null;
            return false;
        }
        if (s.equals("pool6")) {
            pool6 = null;
            return false;
        }
        return true;
    }

    public void srvHelp(userHelping l) {
        l.add(null, "1 2  pool4                        ipv4 pool to use");
        l.add(null, "2 .    <name:pl4>                 name of pool");
        l.add(null, "1 2  pool6                        ipv6 pool to use");
        l.add(null, "2 .    <name:pl6>                 name of pool");
    }

    public boolean srvAccept(pipeSide pipe, prtGenConn id) {
        pipe.setTime(120000);
        new servSdwanConn(this, pipe, id.peerAddr, id.portRem);
        return false;
    }

    /**
     * send line to everyone
     *
     * @param excl peer to exclude
     * @param s line to send
     */
    protected void sendLn(servSdwanConn excl, String s) {
        for (int i = 0; i < conns.size(); i++) {
            servSdwanConn ntry = conns.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry == excl) {
                continue;
            }
            ntry.sendLn(s);
        }
    }

    /**
     * add one endpoint
     *
     * @param peer peer
     */
    protected synchronized void addEndpt(servSdwanConn peer) {
        sendLn(peer, "endpoint_add " + peer.getEndpt());
        servSdwanConn old = conns.put(peer);
        if (old == null) {
            return;
        }
        old.doClose();
    }

}

class servSdwanConn implements Runnable, Comparator<servSdwanConn> {

    public final servSdwan lower;

    public final addrIP connA;

    public final int connP;

    public pipeSide connS;

    public String authed;

    public String username;

    public String hostname;

    public String software;

    public String hardware;

    public String middleware;

    public String kernel;

    public int endptVer;

    public addrIP endptIp;

    public int endptPrt;

    public String endptPar;

    public addrIPv4 endptAdr4;

    public addrIPv6 endptAdr6;

    public int idNum;

    public long lastKeep;

    public int lastEcho;

    public servSdwanConn(servSdwan parent, pipeSide pipe, addrIP remA, int remP) {
        lower = parent;
        connS = pipe;
        connA = remA;
        connP = remP;
        new Thread(this).start();
    }

    public int compare(servSdwanConn o1, servSdwanConn o2) {
        if (o1.connP < o2.connP) {
            return -1;
        }
        if (o1.connP > o2.connP) {
            return +1;
        }
        return o1.connA.compare(o1.connA, o2.connA);
    }

    public void sendLn(String s) {
        if (debugger.servSdwanTraf) {
            logger.debug(connA + " tx " + s);
        }
        connS.linePut(s);
    }

    public String readLn() {
        String s = connS.lineGet(1);
        if (debugger.servSdwanTraf) {
            logger.debug(connA + " rx " + s);
        }
        return s;
    }

    public void run() {
        try {
            if (doInit()) {
                return;
            }
            for (;;) {
                if (doRound()) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        doClose();
    }

    public boolean doInit() {
        if (debugger.servSdwanTraf) {
            logger.debug("accepting " + connA);
        }
        connS = lower.negoSecSess(connS, servGeneric.protoSsh, new pipeLine(65536, false), lower.srvAuther);
        if (connS == null) {
            logger.error("unable to authenticate " + connA);
            return true;
        }
        connS.setTime(120000);
        connS.lineRx = pipeSide.modTyp.modeCRtryLF;
        connS.lineTx = pipeSide.modTyp.modeCRLF;
        authed = connS.settingsGet(pipeSetting.origin, "?");
        if (debugger.servSdwanTraf) {
            logger.debug("accepted " + connA);
        }
        idNum = bits.randomW();
        sendLn("hello");
        sendLn("yourid " + idNum);
        for (;;) {
            String a = readLn();
            cmds cmd = new cmds("sdw", a);
            a = cmd.word();
            if (a.length() < 1) {
                if (connS.isClosed() != 0) {
                    return true;
                }
                continue;
            }
            if (a.equals("nomore")) {
                break;
            }
            if (a.equals("username")) {
                username = cmd.getRemaining();
                continue;
            }
            if (a.equals("software")) {
                software = cmd.getRemaining();
                continue;
            }
            if (a.equals("hardware")) {
                hardware = cmd.getRemaining();
                continue;
            }
            if (a.equals("middleware")) {
                middleware = cmd.getRemaining();
                continue;
            }
            if (a.equals("kernel")) {
                kernel = cmd.getRemaining();
                continue;
            }
            if (a.equals("myendpoint")) {
                if (endptIp != null) {
                    continue;
                }
                endptVer = bits.str2num(cmd.word());
                endptIp = new addrIP();
                endptIp.fromString(cmd.word());
                endptPrt = bits.str2num(cmd.word());
                endptPar = cmd.getRemaining();
                if (lower.pool4 != null) {
                    endptAdr4 = lower.pool4.addrAlloc();
                }
                if (lower.pool6 != null) {
                    endptAdr6 = lower.pool6.addrAlloc();
                }
                continue;
            }
        }
        sendLn("youraddr " + endptAdr4 + " " + endptAdr6);
        sendLn("nomore");
        logger.info("neighbor " + connA + " up");
        lower.addEndpt(this);
        for (int i = 0; i < lower.conns.size(); i++) {
            servSdwanConn ntry = lower.conns.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry == this) {
                continue;
            }
            sendLn("endpoint_add " + ntry.getEndpt());
        }
        return false;
    }

    public void doClose() {
        logger.warn("neighbor " + connA + " down");
        lower.conns.del(this);
        connS.setClose();
        if (endptAdr4 != null) {
            lower.pool4.addrRelease(endptAdr4);
        }
        if (endptAdr6 != null) {
            lower.pool6.addrRelease(endptAdr6);
        }
        lower.sendLn(this, "endpoint_del " + getEndpt());
    }

    public String getEndpt() {
        return endptVer + " " + endptIp + " " + endptPrt + " " + idNum + " " + endptAdr4 + " " + endptAdr6 + " " + endptPar;
    }

    public boolean doRound() {
        if (connS.isClosed() != 0) {
            return true;
        }
        if (connS.ready2rx() > 2) {
            String a = readLn();
            if (a.equals("echoed")) {
                return false;
            }
        }
        long tim = bits.getTime();
        if ((tim - lastKeep) < 60000) {
            return false;
        }
        lastEcho = bits.randomD();
        sendLn("echo " + lastEcho);
        lastKeep = tim;
        return false;
    }

}
