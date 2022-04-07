package net.freertr.serv;

import java.util.Comparator;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;
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
import net.freertr.user.userFormat;
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
     * report natted addresses
     */
    public boolean natted = true;

    /**
     * list of hubs
     */
    public String hubs = null;

    /**
     * defaults text
     */
    public final static String[] defaultL = {
        "server sdwan .*! port " + port,
        "server sdwan .*! protocol " + proto2string(protoAllStrm),
        "server sdwan .*! no pool4",
        "server sdwan .*! no pool6",
        "server sdwan .*! no hubs",
        "server sdwan .*! natted",};

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
        cmds.cfgLine(l, hubs == null, beg, "hubs", "" + hubs);
        cmds.cfgLine(l, !natted, beg, "natted", "");
    }

    public boolean srvCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("natted")) {
            natted = true;
            return false;
        }
        if (s.equals("hubs")) {
            hubs = cmd.getRemaining();
            return false;
        }
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
        if (s.equals("natted")) {
            natted = false;
            return false;
        }
        if (s.equals("hubs")) {
            hubs = null;
            return false;
        }
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
        l.add(null, "1 .  natted                       use natted addresses");
        l.add(null, "1 2  hubs                         list of hubs");
        l.add(null, "2 2,.  <str>                      name of hub");
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
     * @param hub value of the sender
     * @param str line to send
     */
    protected void sendLn(servSdwanConn excl, boolean hub, String str) {
        for (int i = 0; i < conns.size(); i++) {
            servSdwanConn ntry = conns.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry == excl) {
                continue;
            }
            if ((ntry.hub == false) && (hub == false)) {
                continue;
            }
            ntry.sendLn(str);
        }
    }

    /**
     * add one endpoint
     *
     * @param peer peer
     */
    protected synchronized void addEndpt(servSdwanConn peer) {
        if (hubs == null) {
            peer.hub = true;
        } else {
            peer.hub = (" " + hubs + " ").indexOf(" " + peer.username + " ") >= 0;
        }
        servSdwanConn old = conns.put(peer);
        if (old != null) {
            old.doClose();
        }
        for (int i = 0; i < conns.size(); i++) {
            servSdwanConn ntry = conns.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry == peer) {
                continue;
            }
            if ((ntry.hub == false) && (peer.hub == false)) {
                continue;
            }
            peer.sendLn("endpoint_add " + ntry.getEndpt());
        }
        sendLn(peer, peer.hub, "endpoint_add " + peer.getEndpt());
    }

    /**
     * get show
     *
     * @return result
     */
    public userFormat getShow() {
        userFormat res = new userFormat("|", "addr|port|user|hub|id|prt|addr|port|prm|inner4|inner6|for|since");
        for (int i = 0; i < conns.size(); i++) {
            servSdwanConn ntry = conns.get(i);
            if (ntry == null) {
                continue;
            }
            res.add(ntry.connA + "|" + ntry.connP + "|" + ntry.username + "|" + ntry.hub + "|" + ntry.idNum + "|" + ntry.endptProto + "|" + ntry.endptIp + "|" + ntry.endptPort + "|" + ntry.endptPar + "|" + ntry.innerAdr4 + "|" + ntry.innerAdr6 + "|" + bits.timePast(ntry.created) + "|" + bits.time2str(cfgAll.timeZoneName, ntry.created + cfgAll.timeServerOffset, 3));
        }
        return res;
    }

}

class servSdwanConn implements Runnable, Comparator<servSdwanConn> {

    public final servSdwan lower;

    public final addrIP connA;

    public final int connP;

    public pipeSide connS;

    public String authed;

    public String username = "unknown";

    public String hostname = "unknown";

    public String software = "unknown";

    public String hardware = "unknown";

    public String middleware = "unknown";

    public String kernel = "unknown";

    public int endptProto;

    public addrIP endptIp = null;

    public int endptPort;

    public String endptPar;

    public addrIPv4 innerAdr4 = new addrIPv4();

    public addrIPv6 innerAdr6 = new addrIPv6();

    public boolean addrRel4 = false;

    public boolean addrRel6 = false;

    public boolean hub;

    public int idNum;

    public int lastEcho;

    public Timer keepTimer;

    public long created;

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
            restartTimer(false);
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
        created = bits.getTime();
        connS.setTime(120000);
        connS.lineRx = pipeSide.modTyp.modeCRtryLF;
        connS.lineTx = pipeSide.modTyp.modeCRLF;
        if (!readLn().equals("sdwan")) {
            logger.error("unable to validate " + connA);
            return true;
        }
        sendLn("okay");
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
        boolean needAdr4 = false;
        boolean needAdr6 = false;
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
            if (a.equals("hello")) {
                continue;
            }
            if (a.equals("username")) {
                username = cmd.getRemaining().replaceAll(" ", "_");
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
                endptProto = bits.str2num(cmd.word());
                endptIp = new addrIP();
                endptIp.fromString(cmd.word());
                endptPort = bits.str2num(cmd.word());
                endptPar = cmd.getRemaining();
                continue;
            }
            if (a.equals("needaddr")) {
                needAdr4 = cmd.word().equals("true");
                needAdr6 = cmd.word().equals("true");
                continue;
            }
            if (a.equals("myaddr")) {
                innerAdr4.fromString(cmd.word());
                innerAdr6.fromString(cmd.word());
                continue;
            }
            logger.warn("got unknown command: " + cmd.getOriginal());
        }
        if (endptIp == null) {
            sendLn("error no endpoint address sent");
            return true;
        }
        if ((needAdr4 == false) && (needAdr6 == false)) {
            sendLn("error no tunneled address asked");
            return true;
        }
        sendLn("hello");
        sendLn("yourid " + idNum);
        if (needAdr4 && (lower.pool4 != null)) {
            if (innerAdr4.isFilled(0)) {
                innerAdr4 = lower.pool4.addrAlloc();
                addrRel4 = innerAdr4 != null;
            }
        }
        if (needAdr6 && (lower.pool6 != null)) {
            if (innerAdr6.isFilled(0)) {
                innerAdr6 = lower.pool6.addrAlloc();
                addrRel6 = innerAdr6 != null;
            }
        }
        sendLn("youraddr " + innerAdr4 + " " + innerAdr6);
        sendLn("nomore");
        logger.info("neighbor " + connA + " up");
        lower.addEndpt(this);
        return false;
    }

    public void doClose() {
        restartTimer(true);
        logger.warn("neighbor " + connA + " down");
        lower.conns.del(this);
        connS.setClose();
        if (addrRel4 && (innerAdr4 != null)) {
            lower.pool4.addrRelease(innerAdr4);
        }
        if (addrRel6 && (innerAdr6 != null)) {
            lower.pool6.addrRelease(innerAdr6);
        }
        lower.sendLn(this, hub, "endpoint_del " + getEndpt());
    }

    public String getEndpt() {
        String a;
        if (lower.natted) {
            a = "" + connA;
        } else {
            a = "" + endptIp;
        }
        return endptProto + " " + a + " " + endptPort + " " + idNum + " " + innerAdr4 + " " + innerAdr6 + " " + username + " " + endptPar;
    }

    public boolean doRound() {
        String a = readLn();
        cmds cmd = new cmds("sdw", a);
        a = cmd.word();
        if (a.length() < 1) {
            return connS.isClosed() != 0;
        }
        if (a.equals("echo")) {
            sendLn("echoed " + cmd.getRemaining());
            return false;
        }
        if (a.equals("echoed")) {
            return false;
        }
        return false;
    }

    protected void doTimer() {
        lastEcho = bits.randomD();
        sendLn("echo " + lastEcho);
    }

    public void restartTimer(boolean shutdown) {
        try {
            keepTimer.cancel();
        } catch (Exception e) {
        }
        keepTimer = null;
        if (shutdown) {
            return;
        }
        keepTimer = new Timer();
        servSdwanTimer task = new servSdwanTimer(this);
        keepTimer.schedule(task, 500, 60000);
    }

}

class servSdwanTimer extends TimerTask {

    private final servSdwanConn lower;

    public servSdwanTimer(servSdwanConn parent) {
        lower = parent;
    }

    public void run() {
        try {
            lower.doTimer();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}
