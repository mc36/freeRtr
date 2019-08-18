package rtr;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrPrefix;
import auth.authConstant;
import cry.cryBase64;
import cry.cryHashSha2512;
import java.util.ArrayList;
import java.util.Comparator;
import cfg.cfgAll;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtAccept;
import sec.secClient;
import sec.secServer;
import serv.servGeneric;
import tab.tabRoute;
import tab.tabRouteEntry;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;
import util.notifier;

/**
 * pvrp neighbor
 *
 * @author matecsaba
 */
public class rtrPvrpNeigh implements Runnable, rtrBfdClnt, Comparator<rtrPvrpNeigh> {

    /**
     * transport address of peer
     */
    public addrIP peer;

    /**
     * hostname of peer
     */
    public String name;

    /**
     * router id of peer
     */
    public addrIPv4 rtrId;

    /**
     * learned routes
     */
    public tabRoute<addrIP> learned = new tabRoute<addrIP>("lrn");

    /**
     * advertised routes
     */
    public tabRoute<addrIP> adverted = new tabRoute<addrIP>("adv");

    /**
     * time last heard
     */
    public long lastHeard;

    /**
     * notified on route change
     */
    protected notifier notif = new notifier();

    /**
     * protocol handler
     */
    protected final rtrPvrp lower;

    /**
     * interface handler
     */
    protected final rtrPvrpIface iface;

    /**
     * uptime
     */
    protected long upTime;

    private pipeSide conn;

    private boolean need2run;

    /**
     * start one peer
     *
     * @param parent protocol handler
     * @param ifc interface handler
     * @param peer transport address
     */
    public rtrPvrpNeigh(rtrPvrp parent, rtrPvrpIface ifc, addrIPv4 peer) {
        lower = parent;
        iface = ifc;
        rtrId = peer.copyBytes();
        lastHeard = bits.getTime();
    }

    public int compare(rtrPvrpNeigh o1, rtrPvrpNeigh o2) {
        return o1.rtrId.compare(o1.rtrId, o2.rtrId);
    }

    public String toString() {
        return "pvrp with " + peer;
    }

    /**
     * stop work
     */
    public void bfdPeerDown() {
        stopWork();
    }

    /**
     * start work
     */
    public void startWork() {
        if (debugger.rtrPvrpEvnt) {
            logger.debug("starting peer " + rtrId + " (" + peer + ")");
        }
        lastHeard = bits.getTime();
        need2run = true;
        upTime = bits.getTime();
        new Thread(this).start();
    }

    /**
     * stop work
     */
    public void stopWork() {
        if (debugger.rtrPvrpEvnt) {
            logger.debug("stopping peer " + rtrId + " (" + peer + ")");
        }
        boolean oldrun = need2run;
        need2run = false;
        if (conn != null) {
            conn.setClose();
        }
        adverted.clear();
        learned.clear();
        if (oldrun) {
            iface.neighs.del(this);
        }
        lower.notif.wakeup();
        iface.iface.bfdDel(peer, this);
        notif.wakeup();
    }

    public void run() {
        try {
            for (;;) {
                if (!need2run) {
                    break;
                }
                doRun();
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        stopWork();
    }

    /**
     * receive line
     *
     * @return commands
     */
    protected cmds recvLn() {
        if (conn.isClosed() != 0) {
            return null;
        }
        String a = conn.lineGet(0x11);
        if (debugger.rtrPvrpTraf) {
            logger.debug(peer + " rx " + a);
        }
        return new cmds("rx", a);
    }

    /**
     * send one line
     *
     * @param s line to send
     */
    protected void sendLn(String s) {
        if (debugger.rtrPvrpTraf) {
            logger.debug(peer + " tx " + s);
        }
        conn.linePut(s);
    }

    private void doRun() {
        if (conn != null) {
            conn.setClose();
        }
        adverted.clear();
        learned.clear();
        bits.sleep(bits.random(1000, 5000));
        if (peer.compare(peer, iface.iface.addr) > 0) {
            if (debugger.rtrPvrpEvnt) {
                logger.debug("accepting " + peer);
            }
            prtAccept ac = new prtAccept(lower.tcpCore, new pipeLine(65536, false), iface.iface, rtrPvrp.port, peer, 0, 0, "pvrp", null, -1);
            ac.wait4conn(30000);
            conn = ac.getConn(true);
        } else {
            if (debugger.rtrPvrpEvnt) {
                logger.debug("connecting " + peer);
            }
            conn = lower.tcpCore.streamConnect(new pipeLine(65536, false), iface.iface, 0, peer, rtrPvrp.port, "pvrp", null, -1);
        }
        if (conn == null) {
            return;
        }
        conn.timeout = iface.deadTimer * 3;
        conn.lineRx = pipeSide.modTyp.modeCRtryLF;
        conn.lineTx = pipeSide.modTyp.modeCRLF;
        if (conn.wait4ready(0)) {
            return;
        }
        if (!need2run) {
            sendLn("error notNeeded");
            return;
        }
        if (iface.encryptionMethod > 0) {
            sendLn("startEncrypt " + servGeneric.proto2string(iface.encryptionMethod));
            cmds cmd = recvLn();
            if (cmd == null) {
                return;
            }
            String a = cmd.word();
            if (!a.equals("startEncrypt")) {
                sendLn("error startEncryptRequired");
                return;
            }
            if (peer.compare(peer, iface.iface.addr) > 0) {
                if (debugger.rtrPvrpEvnt) {
                    logger.debug("secure client " + peer);
                }
                conn = secClient.openSec(conn, iface.encryptionMethod, "", "");
            } else {
                if (debugger.rtrPvrpEvnt) {
                    logger.debug("secure server " + peer);
                }
                conn = secServer.openSec(conn, iface.encryptionMethod, new pipeLine(65536, false), new authConstant(true), iface.keyRsa.key, iface.keyDsa.key, iface.keyEcDsa.key, iface.certRsa.cert, iface.certDsa.cert, iface.certEcDsa.cert);
            }
            if (conn == null) {
                return;
            }
            conn.timeout = iface.deadTimer * 3;
            conn.lineRx = pipeSide.modTyp.modeCRtryLF;
            conn.lineTx = pipeSide.modTyp.modeCRLF;
            conn.wait4ready(0);
        }
        if (!need2run) {
            sendLn("error notNeeded");
            return;
        }
        sendLn("open rtrid=" + lower.routerID + " name=" + cfgAll.hostName);
        cmds cmd = recvLn();
        if (cmd == null) {
            return;
        }
        String a = cmd.word();
        if (!a.equals("open")) {
            sendLn("error openRequired");
            return;
        }
        name = "?" + rtrId;
        for (;;) {
            a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.startsWith("rtrid")) {
                continue;
            }
            if (a.startsWith("name")) {
                name = a.substring(5, a.length());
                continue;
            }
        }
        if (iface.authentication != null) {
            byte[] authenticate = new byte[128];
            for (int i = 0; i < authenticate.length; i++) {
                authenticate[i] = (byte) bits.randomB();
            }
            sendLn("password-request " + cryBase64.encodeBytes(authenticate));
            cmd = recvLn();
            if (cmd == null) {
                return;
            }
            a = cmd.word();
            if (!a.equals("password-request")) {
                sendLn("error passReqRequired");
                return;
            }
            a = cmd.word();
            byte[] chl = cryBase64.decodeBytes(a);
            cryHashSha2512 hsh = new cryHashSha2512();
            hsh.init();
            hsh.update(chl);
            hsh.update(iface.authentication.getBytes());
            hsh.update(chl);
            chl = hsh.finish();
            sendLn("password-reply " + cryBase64.encodeBytes(chl));
            cmd = recvLn();
            if (cmd == null) {
                return;
            }
            a = cmd.word();
            if (!a.equals("password-reply")) {
                sendLn("error passRepRequired");
                return;
            }
            hsh.init();
            hsh.update(authenticate);
            hsh.update(iface.authentication.getBytes());
            hsh.update(authenticate);
            chl = hsh.finish();
            a = cryBase64.encodeBytes(chl);
            if (!a.equals(cmd.word())) {
                sendLn("error badPassword");
                return;
            }
        }
        learned.clear();
        adverted.clear();
        logger.warn("neighbor " + name + " (" + peer + ") up");
        new rtrPvrpNeighRcvr(this).startWork();
        if (iface.bfdTrigger) {
            iface.iface.bfdAdd(peer, this, "pvrp");
        }
        for (;;) {
            if (!need2run) {
                break;
            }
            if (notif.misleep(iface.helloTimer) < 1) {
                sendLn("keepalive " + iface.deadTimer);
            }
            if (conn.isClosed() != 0) {
                break;
            }
            if (conn.ready2tx() > 1024) {
                doAdvert();
            }
        }
        stopWork();
        logger.error("neighbor " + name + " (" + peer + ") down");
    }

    private void doAdvert() {
        for (int i = 0; i < adverted.size(); i++) {
            tabRouteEntry<addrIP> ntry = adverted.get(i);
            if (ntry == null) {
                continue;
            }
            if (iface.need2adv.find(ntry) != null) {
                continue;
            }
            if (conn.ready2tx() < 1024) {
                return;
            }
            sendUpdate(ntry, false);
        }
        for (int i = 0; i < iface.need2adv.size(); i++) {
            tabRouteEntry<addrIP> ntry = iface.need2adv.get(i);
            if (ntry == null) {
                continue;
            }
            if (!ntry.differs(adverted.find(ntry))) {
                continue;
            }
            if (conn.ready2tx() < 1024) {
                return;
            }
            sendUpdate(ntry, true);
        }
    }

    private void sendUpdate(tabRouteEntry<addrIP> ntry, boolean reach) {
        String s;
        if (!reach) {
            s = "withdraw";
            adverted.del(ntry);
        } else {
            s = "reachable";
            adverted.add(tabRoute.addType.always, ntry, true, true);
        }
        String a = "";
        if (lower.labels) {
            if (ntry.labelLoc != null) {
                a = " label=" + ntry.labelLoc.getValue();
            }
        }
        sendLn(s + " prefix=" + addrPrefix.ip2str(ntry.prefix) + a + " metric=" + (ntry.metric + iface.metricOut) + " tag=" + ntry.tag + " path= " + lower.routerID + " " + tabRouteEntry.dumpAddrList(ntry.clustList));
    }

}

class rtrPvrpNeighRcvr implements Runnable {

    private final rtrPvrpNeigh lower;

    public rtrPvrpNeighRcvr(rtrPvrpNeigh parent) {
        lower = parent;
    }

    public tabRouteEntry<addrIP> parsePrefix(cmds cmd) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            int i = a.indexOf("=");
            String s = "";
            if (i >= 0) {
                s = a.substring(i + 1, a.length());
                a = a.substring(0, i);
            }
            a = a.toLowerCase().trim();
            if (a.equals("prefix")) {
                ntry.prefix = addrPrefix.str2ip(s);
                continue;
            }
            if (a.equals("metric")) {
                ntry.metric = bits.str2num(s);
                continue;
            }
            if (a.equals("label")) {
                if (!lower.lower.labels) {
                    continue;
                }
                ntry.labelRem = new ArrayList<Integer>();
                ntry.labelRem.add(bits.str2num(s));
                continue;
            }
            if (a.equals("tag")) {
                ntry.tag = bits.str2num(s);
                continue;
            }
            if (a.equals("path")) {
                break;
            }
        }
        if (ntry.prefix == null) {
            return null;
        }
        ntry.clustList = new ArrayList<addrIP>();
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            addrIP adr = new addrIP();
            adr.fromString(a);
            ntry.clustList.add(adr);
        }
        return ntry;
    }

    public void startWork() {
        new Thread(this).start();
    }

    public void run() {
        try {
            doRun();
        } catch (Exception e) {
            logger.traceback(e);
        }
        lower.stopWork();
    }

    private void doRun() {
        for (;;) {
            cmds cmd = lower.recvLn();
            if (cmd == null) {
                return;
            }
            String a = cmd.word();
            if (a.length() < 1) {
                continue;
            }
            lower.lastHeard = bits.getTime();
            if (a.equals("error")) {
                lower.stopWork();
                continue;
            }
            if (a.equals("warning")) {
                continue;
            }
            if (a.equals("discard")) {
                continue;
            }
            if (a.equals("echoed")) {
                continue;
            }
            if (a.equals("echo")) {
                lower.sendLn("echoed " + cmd.getRemaining());
                continue;
            }
            if (a.equals("close")) {
                lower.stopWork();
                continue;
            }
            if (a.equals("resend")) {
                lower.adverted.clear();
                continue;
            }
            if (a.equals("keepalive")) {
                lower.lastHeard += bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("reachable")) {
                tabRouteEntry<addrIP> ntry = parsePrefix(cmd);
                if (ntry == null) {
                    continue;
                }
                int cnt = tabRoute.delUpdatedEntry(lower.learned, rtrBgpUtil.safiUnicast, ntry, lower.iface.roumapIn, lower.iface.roupolIn, lower.iface.prflstIn);
                addrIP adr = new addrIP();
                adr.fromIPv4addr(lower.lower.routerID);
                if (rtrBgpUtil.findAddrList(ntry.clustList, adr) >= 0) {
                    if (cnt > 0) {
                        lower.lower.notif.wakeup();
                    }
                    continue;
                }
                ntry.metric += lower.iface.metricIn;
                ntry.nextHop = lower.peer.copyBytes();
                ntry.distance = lower.iface.distance;
                ntry.iface = lower.iface.iface;
                ntry.srcRtr = lower.peer.copyBytes();
                cnt += tabRoute.addUpdatedEntry(tabRoute.addType.always, lower.learned, rtrBgpUtil.safiUnicast, ntry, lower.iface.roumapIn, lower.iface.roupolIn, lower.iface.prflstIn);
                if (cnt > 0) {
                    lower.lower.notif.wakeup();
                }
                continue;
            }
            if (a.equals("withdraw")) {
                tabRouteEntry<addrIP> ntry = parsePrefix(cmd);
                if (ntry == null) {
                    continue;
                }
                if (tabRoute.delUpdatedEntry(lower.learned, rtrBgpUtil.safiUnicast, ntry, lower.iface.roumapIn, lower.iface.roupolIn, lower.iface.prflstIn) > 0) {
                    lower.lower.notif.wakeup();
                }
                continue;
            }
            lower.sendLn("warning badCommand " + a);
        }
    }

}
