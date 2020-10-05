package rtr;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrPrefix;
import auth.authConstant;
import cry.cryBase64;
import java.util.ArrayList;
import java.util.Comparator;
import cfg.cfgAll;
import ip.ipMpls;
import java.util.List;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtAccept;
import sec.secClient;
import sec.secServer;
import serv.servGeneric;
import tab.tabListing;
import tab.tabPrfxlstN;
import tab.tabRoute;
import tab.tabRouteAttr;
import tab.tabRouteEntry;
import user.userUpgrade;
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
    public final addrIP peer;

    /**
     * hostname of peer
     */
    public String name;

    /**
     * interface of peer
     */
    public String inam;

    /**
     * router id of peer
     */
    public final addrIPv4 rtrId;

    /**
     * learned routes
     */
    public tabRoute<addrIP> learned = new tabRoute<addrIP>("lrn");

    /**
     * advertised routes
     */
    public tabRoute<addrIP> adverted = new tabRoute<addrIP>("adv");

    /**
     * metric of peer
     */
    public int gotMet;

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

    /**
     * advertised metric
     */
    protected int sentMet;

    private pipeSide conn;

    private boolean need2run;

    /**
     * start one peer
     *
     * @param parent protocol handler
     * @param ifc interface handler
     * @param peerId router id
     * @param peerAd transport address
     */
    public rtrPvrpNeigh(rtrPvrp parent, rtrPvrpIface ifc, addrIPv4 peerId, addrIP peerAd) {
        lower = parent;
        iface = ifc;
        rtrId = peerId.copyBytes();
        peer = peerAd.copyBytes();
        lastHeard = bits.getTime();
        sentMet = -1;
        gotMet = 10;
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
        iface.dumpLine(false, a);
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
        iface.dumpLine(true, s);
    }

    /**
     * send error line
     *
     * @param s line to send
     */
    protected void sendErr(String s) {
        logger.info("sent error (" + s + ") to " + peer);
        sendLn("error " + s);
    }

    /**
     * send warning line
     *
     * @param s line to send
     */
    protected void sendWrn(String s) {
        logger.info("sent warning (" + s + ") to " + peer);
        sendLn("warning " + s);
    }

    /**
     * check prefix
     *
     * @param lst list
     * @param prf prefix
     * @return true if denied, false if allowed
     */
    protected boolean checkPrefix(tabListing<tabPrfxlstN, addrIP> lst, addrPrefix<addrIP> prf) {
        if (lst == null) {
            return false;
        }
        return !lst.matches(rtrBgpUtil.safiUnicast, 0, prf);
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
            prtAccept ac = new prtAccept(lower.tcpCore, new pipeLine(65536, false), iface.iface, rtrPvrp.port, peer, 0, "pvrp", null, -1);
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
        if (conn.wait4ready(iface.deadTimer)) {
            return;
        }
        if (!need2run) {
            sendErr("notNeeded");
            return;
        }
        if (iface.encryptionMethod > 0) {
            sendLn("startEncrypt " + servGeneric.proto2string(iface.encryptionMethod));
            cmds cmd = recvLn();
            if (cmd == null) {
                cmd = new cmds("", "");
            }
            String a = cmd.word();
            if (!a.equals("startEncrypt")) {
                sendErr("startEncryptRequired");
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
            conn.wait4ready(iface.deadTimer);
        }
        if (!need2run) {
            sendErr("notNeeded");
            return;
        }
        if (iface.authentication != null) {
            byte[] buf = new byte[128];
            for (int i = 0; i < buf.length; i++) {
                buf[i] = (byte) bits.randomB();
            }
            String b = cryBase64.encodeBytes(buf);
            sendLn("password-request " + b);
            cmds cmd = recvLn();
            if (cmd == null) {
                cmd = new cmds("", "");
            }
            String a = cmd.word();
            if (!a.equals("password-request")) {
                sendErr("passReqRequired");
                return;
            }
            a = cmd.word();
            List<String> lst = new ArrayList<String>();
            lst.add(a);
            lst.add(iface.authentication);
            lst.add(a);
            sendLn("password-reply " + userUpgrade.calcTextHash(lst));
            cmd = recvLn();
            if (cmd == null) {
                cmd = new cmds("", "");
            }
            a = cmd.word();
            if (!a.equals("password-reply")) {
                sendErr("passRepRequired");
                return;
            }
            lst = new ArrayList<String>();
            lst.add(b);
            lst.add(iface.authentication);
            lst.add(b);
            a = userUpgrade.calcTextHash(lst);
            if (!a.equals(cmd.word())) {
                sendErr("badPassword");
                return;
            }
        }
        if (!need2run) {
            sendErr("notNeeded");
            return;
        }
        sendLn("open rtrid=" + lower.routerID + " mtu=" + iface.iface.lower.getMTUsize() + " bfd=" + iface.bfdTrigger + " iface=" + iface.iface + " name=" + cfgAll.hostName);
        cmds cmd = recvLn();
        if (cmd == null) {
            cmd = new cmds("", "");
        }
        if (!cmd.word().equals("open")) {
            sendErr("openRequired");
            return;
        }
        name = "?";
        inam = "?";
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            if (a.startsWith("rtrid")) {
                continue;
            }
            if (a.startsWith("iface")) {
                inam = a.substring(6, a.length());
                continue;
            }
            if (a.startsWith("name")) {
                name = a.substring(5, a.length());
                continue;
            }
        }
        if (!need2run) {
            sendErr("notNeeded");
            return;
        }
        learned.clear();
        adverted.clear();
        logger.warn("neighbor " + name + " (" + peer + ") up");
        new rtrPvrpNeighRcvr(this).startWork();
        lower.notif.wakeup();
        if (iface.bfdTrigger) {
            iface.iface.bfdAdd(peer, this, "pvrp");
        }
        long lastKeep = 0;
        for (;;) {
            if (!need2run) {
                break;
            }
            notif.misleep(iface.helloTimer);
            long tim = bits.getTime();
            if ((lastKeep + iface.helloTimer) < tim) {
                sendLn("keepalive " + iface.deadTimer);
                lastKeep = tim - 1;
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
        if (sentMet != iface.metricIn) {
            sentMet = iface.metricIn;
            sendLn("metric " + sentMet);
        }
        int sent = 0;
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
            sent++;
        }
        for (int i = 0; i < iface.need2adv.size(); i++) {
            tabRouteEntry<addrIP> ntry = iface.need2adv.get(i);
            if (ntry == null) {
                continue;
            }
            if (!ntry.differs(tabRoute.addType.notyet, adverted.find(ntry))) {
                continue;
            }
            if (conn.ready2tx() < 1024) {
                return;
            }
            sendUpdate(ntry, true);
            sent++;
        }
        if (sent > 0) {
            sendLn("nomore");
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
        if (lower.labels && (ntry.best.labelLoc != null)) {
            int val = ntry.best.labelLoc.getValue();
            if (iface.labelPop && (lower.fwdCore.commonLabel.getValue() == val)) {
                val = ipMpls.labelImp;
            }
            a = " label=" + val;
            if (checkPrefix(iface.labelOut, ntry.prefix)) {
                a = "";
            }
        }
        sendLn(s + " prefix=" + addrPrefix.ip2str(ntry.prefix) + a + " metric=" + (ntry.best.metric + iface.metricOut) + " tag=" + ntry.best.tag + " external=" + ((ntry.best.rouSrc & 1) != 0) + " path= " + lower.routerID + " " + tabRouteAttr.dumpAddrList(ntry.best.clustList));
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
            if (a.equals("external")) {
                ntry.best.rouSrc = s.toLowerCase().equals("true") ? 1 : 0;
                continue;
            }
            if (a.equals("metric")) {
                ntry.best.metric = bits.str2num(s);
                continue;
            }
            if (a.equals("label")) {
                if (!lower.lower.labels) {
                    continue;
                }
                ntry.best.labelRem = new ArrayList<Integer>();
                ntry.best.labelRem.add(bits.str2num(s));
                continue;
            }
            if (a.equals("tag")) {
                ntry.best.tag = bits.str2num(s);
                continue;
            }
            if (a.equals("path")) {
                break;
            }
        }
        if (ntry.prefix == null) {
            return null;
        }
        if (lower.checkPrefix(lower.iface.labelIn, ntry.prefix)) {
            ntry.best.labelRem = null;
        }
        ntry.best.clustList = new ArrayList<addrIP>();
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            addrIP adr = new addrIP();
            adr.fromString(a);
            ntry.best.clustList.add(adr);
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
                logger.info("got error (" + cmd.getRemaining() + ") from " + lower.peer);
                lower.stopWork();
                continue;
            }
            if (a.equals("warning")) {
                logger.info("got warning (" + cmd.getRemaining() + ") from " + lower.peer);
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
            if (a.equals("request")) {
                tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
                ntry.prefix = addrPrefix.str2ip(cmd.word());
                lower.adverted.del(ntry);
                continue;
            }
            if (a.equals("resend")) {
                lower.adverted.clear();
                continue;
            }
            if (a.equals("nomore")) {
                continue;
            }
            if (a.equals("keepalive")) {
                lower.lastHeard += bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("metric")) {
                lower.gotMet = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("reachable")) {
                tabRouteEntry<addrIP> ntry = parsePrefix(cmd);
                if (ntry == null) {
                    continue;
                }
                int cnt = tabRoute.delUpdatedEntry(lower.learned, rtrBgpUtil.safiUnicast, 0, ntry, lower.iface.roumapIn, lower.iface.roupolIn, lower.iface.prflstIn);
                addrIP adr = new addrIP();
                adr.fromIPv4addr(lower.lower.routerID);
                if (rtrBgpUtil.findAddrList(ntry.best.clustList, adr) >= 0) {
                    if (cnt > 0) {
                        lower.lower.notif.wakeup();
                    }
                    continue;
                }
                if (lower.iface.acceptMetric) {
                    ntry.best.metric += lower.gotMet;
                } else {
                    ntry.best.metric += lower.iface.metricIn;
                }
                ntry.best.nextHop = lower.peer.copyBytes();
                ntry.best.distance = lower.iface.distance;
                ntry.best.iface = lower.iface.iface;
                ntry.best.srcRtr = lower.peer.copyBytes();
                cnt += tabRoute.addUpdatedEntry(tabRoute.addType.always, lower.learned, rtrBgpUtil.safiUnicast, 0, ntry, true, lower.iface.roumapIn, lower.iface.roupolIn, lower.iface.prflstIn);
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
                if (tabRoute.delUpdatedEntry(lower.learned, rtrBgpUtil.safiUnicast, 0, ntry, lower.iface.roumapIn, lower.iface.roupolIn, lower.iface.prflstIn) > 0) {
                    lower.lower.notif.wakeup();
                }
                continue;
            }
            lower.sendWrn("badCommand " + a);
        }
    }

}
