package org.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrPrefix;
import org.freertr.auth.authConstant;
import org.freertr.cfg.cfgAll;
import org.freertr.clnt.clntEcho;
import org.freertr.clnt.clntPing;
import org.freertr.clnt.clntTwamp;
import org.freertr.enc.encBase64;
import org.freertr.ip.ipMpls;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtAccept;
import org.freertr.prt.prtTcp;
import org.freertr.sec.secClient;
import org.freertr.sec.secInfoCls;
import org.freertr.sec.secInfoWrk;
import org.freertr.sec.secServer;
import org.freertr.serv.servGeneric;
import org.freertr.tab.tabAverage;
import org.freertr.tab.tabLabelBier;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRouteUtil;
import org.freertr.user.userUpgrade;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.keyword;
import org.freertr.util.logger;
import org.freertr.util.notifier;

/**
 * pvrp neighbor
 *
 * @author matecsaba
 */
public class rtrPvrpNeigh implements Runnable, rtrBfdClnt, Comparable<rtrPvrpNeigh> {

    /**
     * ipinfo result
     */
    public secInfoWrk ipInfoRes;

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
    public int gotMetric;

    /**
     * permission of peer
     */
    public boolean gotMeasure;

    /**
     * advertised segrou
     */
    protected int gotSegrouBeg;

    /**
     * advertised segrou
     */
    protected int gotSegrouMax;

    /**
     * advertised bier
     */
    protected int gotBierBeg;

    /**
     * advertised bier
     */
    protected int gotBierLen;

    /**
     * advertised bier
     */
    protected int gotBierMax;

    /**
     * time echo sent
     */
    public long echoTime;

    /**
     * data sent in echo
     */
    public int echoData;

    /**
     * calculated echo
     */
    public tabAverage echoCalc = new tabAverage(1, 1);

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

    /**
     * advertised metric
     */
    protected boolean sentMed;

    /**
     * advertised segrou
     */
    protected int sentSegrou;

    /**
     * advertised bier
     */
    protected int sentBier;

    private String signRx;

    private String signTx;

    private int seqRx;

    private int seqTx;

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
        sentMed = false;
        sentSegrou = -1;
        sentBier = -1;
        gotMetric = -1;
        gotMeasure = true;
    }

    public int compareTo(rtrPvrpNeigh o) {
        return peer.compareTo(o.peer);
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
        logger.startThread(this);
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
     * get peer metric
     *
     * @return metric
     */
    public int getMetric() {
        if (iface.ldpSync) {
            if (lower.fwdCore.ldpNeighFind(peer, false) == null) {
                return 0xffffff;
            }
        }
        int met = iface.metricIn;
        if (iface.acceptMetric && (gotMetric > 0) && (met > gotMetric)) {
            met = gotMetric;
        }
        if (!gotMeasure) {
            return met;
        }
        if (iface.dynamicMetric < 1) {
            return met;
        }
        return echoCalc.getResult(met);
    }

    /**
     * receive line
     *
     * @return commands
     */
    protected cmds recvLn() {
        seqRx++;
        if (conn.isClosed() != 0) {
            return null;
        }
        String a = conn.lineGet(0x11);
        a = a.trim();
        if (debugger.rtrPvrpTraf) {
            logger.debug(peer + " rx " + a);
        }
        if (iface.dumpFile != null) {
            iface.dumpFile.add(logger.getTimestamp() + " " + peer + " rx " + a);
        }
        cmds cmd = new cmds("rx", a);
        if (signRx == null) {
            keyword.update(iface.msgStatRx, a);
            return cmd;
        }
        a = cmd.word();
        if (!a.equals("signed")) {
            sendErr("missingSign");
            return null;
        }
        a = cmd.word();
        List<String> lst = new ArrayList<String>();
        lst.add(signRx);
        lst.add("" + seqRx);
        lst.add(cmd.getRemaining());
        lst.add(signRx);
        if (!a.equals(userUpgrade.calcTextHash(lst))) {
            sendErr("badSign");
            return null;
        }
        keyword.update(iface.msgStatRx, cmd.getRemaining());
        return cmd;
    }

    /**
     * send one line
     *
     * @param s line to send
     */
    protected synchronized void sendLn(String s) {
        seqTx++;
        s = s.trim();
        keyword.update(iface.msgStatTx, s);
        if (signTx != null) {
            List<String> lst = new ArrayList<String>();
            lst.add(signTx);
            lst.add("" + seqTx);
            lst.add(s);
            lst.add(signTx);
            s = "signed " + userUpgrade.calcTextHash(lst) + " " + s;
        }
        if (debugger.rtrPvrpTraf) {
            logger.debug(peer + " tx " + s);
        }
        if (iface.dumpFile != null) {
            iface.dumpFile.add(logger.getTimestamp() + " " + peer + " tx " + s);
        }
        conn.linePut(s);
    }

    /**
     * send error line
     *
     * @param s line to send
     */
    protected void sendErr(String s) {
        logger.info("sent error (" + s + ") to " + peer);
        sendLn("error " + s);
        stopWork();
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
        return !lst.matches(rtrBgpUtil.sfiUnicast, 0, prf);
    }

    private void doRun() {
        if (conn != null) {
            conn.setClose();
        }
        adverted.clear();
        learned.clear();
        bits.sleep(bits.random(1000, 5000));
        if (peer.compareTo(iface.iface.addr) > 0) {
            if (debugger.rtrPvrpEvnt) {
                logger.debug("accepting " + peer);
            }
            prtAccept ac = new prtAccept(lower.tcpCore, new pipeLine(65536, false), iface.iface, rtrPvrp.port, peer, 0, "pvrp", -1, null, iface.sendingTtl, iface.sendingTos);
            ac.wait4conn(30000);
            conn = ac.getConn(true);
        } else {
            if (debugger.rtrPvrpEvnt) {
                logger.debug("connecting " + peer);
            }
            conn = lower.tcpCore.streamConnect(new pipeLine(65536, false), iface.iface, 0, peer, rtrPvrp.port, "pvrp", -1, null, iface.sendingTtl, iface.sendingTos);
        }
        if (conn == null) {
            return;
        }
        conn.setTime(iface.deadTimer * 3);
        conn.lineRx = pipeSide.modTyp.modeCRtryLF;
        conn.lineTx = pipeSide.modTyp.modeCRLF;
        if (conn.wait4ready(iface.deadTimer)) {
            return;
        }
        if (iface.ipInfoCfg != null) {
            secInfoCls cls = new secInfoCls(null, null, null, lower.fwdCore, peer, prtTcp.protoNum, iface.iface.addr);
            ipInfoRes = new secInfoWrk(iface.ipInfoCfg, cls);
            ipInfoRes.doWork(false);
            if (ipInfoRes.need2drop()) {
                sendErr("notPingable");
                return;
            }
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
            if (peer.compareTo(iface.iface.addr) > 0) {
                if (debugger.rtrPvrpEvnt) {
                    logger.debug("secure client " + peer);
                }
                conn = secClient.openSec(conn, iface.encryptionMethod, null, "", "");
            } else {
                if (debugger.rtrPvrpEvnt) {
                    logger.debug("secure server " + peer);
                }
                conn = secServer.openSec(conn, iface.encryptionMethod, new pipeLine(65536, false), new authConstant(true), iface.keyRsa.key, iface.keyDsa.key, iface.keyEcDsa.key, iface.keyMlDsa.key, iface.certRsa.cert, iface.certDsa.cert, iface.certEcDsa.cert, iface.certMlDsa.cert);
            }
            if (conn == null) {
                return;
            }
            conn.setTime(iface.deadTimer * 3);
            conn.lineRx = pipeSide.modTyp.modeCRtryLF;
            conn.lineTx = pipeSide.modTyp.modeCRLF;
            conn.wait4ready(iface.deadTimer);
        }
        if (!need2run) {
            sendErr("notNeeded");
            return;
        }
        if ((!iface.authenDisable) && (iface.authentication != null)) {
            byte[] buf = new byte[128];
            for (int i = 0; i < buf.length; i++) {
                buf[i] = (byte) bits.randomB();
            }
            String b = encBase64.encodeBytes(buf);
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
            String c = cmd.word();
            if (c.length() < 16) {
                sendErr("passTooSmall");
                return;
            }
            List<String> lst = new ArrayList<String>();
            lst.add(c);
            lst.add(b);
            lst.add(iface.authentication);
            lst.add(c);
            lst.add(b);
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
            lst.add(c);
            lst.add(iface.authentication);
            lst.add(b);
            lst.add(c);
            a = userUpgrade.calcTextHash(lst);
            if (!a.equals(cmd.word())) {
                sendErr("badPassword");
                return;
            }
            signRx = b + iface.authentication + c;
            signTx = c + iface.authentication + b;
        }
        if (!need2run) {
            sendErr("notNeeded");
            return;
        }
        sendLn("open rtrid=" + lower.routerID + " mtu=" + iface.iface.mtu + " bfd=" + iface.bfdTrigger + " iface=" + iface.iface + " name=" + cfgAll.hostName);
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
        int mtu = 0;
        int bfd = 0;
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
            if (a.startsWith("mtu")) {
                mtu = bits.str2num(a.substring(4, a.length()));
                continue;
            }
            if (a.startsWith("bfd")) {
                bfd = bits.str2num(a.substring(4, a.length()));
                continue;
            }
        }
        if (mtu != iface.iface.mtu) {
            logger.info("mtu mismatch with " + peer);
        }
        if (bfd != iface.bfdTrigger) {
            logger.info("bfd mismatch with " + peer);
        }
        if (!need2run) {
            sendErr("notNeeded");
            return;
        }
        if ((bfd == 2) && (iface.bfdTrigger == 2)) {
            iface.iface.bfdAdd(peer, this, "pvrp");
            if (iface.iface.bfdWait(peer, iface.deadTimer)) {
                sendErr("bfdFail");
                return;
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
        if (iface.bfdTrigger > 0) {
            iface.iface.bfdAdd(peer, this, "pvrp");
        }
        long lastKeep = 0;
        for (;;) {
            if (!need2run) {
                break;
            }
            notif.misleep(iface.helloTimer);
            long tim = bits.getTime();
            if ((echoTime + iface.echoTimer) < tim) {
                echoCalc.updateFrom(iface.echoParam);
                switch (iface.dynamicMetric) {
                    case 1:
                        echoData = bits.randomD();
                        sendLn("echo " + echoData);
                        break;
                    case 2:
                        clntPing png = new clntPing();
                        png.meas = echoCalc;
                        png.fwd = lower.fwdCore;
                        png.src = iface.iface;
                        png.trg = peer;
                        png.doWork();
                        break;
                    case 3:
                        clntEcho ech = new clntEcho();
                        ech.meas = echoCalc;
                        ech.udp = lower.udpCore;
                        ech.src = iface.iface;
                        ech.trg = peer;
                        ech.doWork();
                        break;
                    case 4:
                        clntTwamp twm = new clntTwamp();
                        twm.meas = echoCalc;
                        twm.udp = lower.udpCore;
                        twm.src = iface.iface;
                        twm.trg = peer;
                        twm.doWork();
                        break;
                }
                echoTime = tim - 1;
            }
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
        int i = getMetric();
        if (sentMet != i) {
            sentMet = i;
            sendLn("metric " + sentMet);
        }
        if (sentMed != iface.dynamicForbid) {
            sentMed = iface.dynamicForbid;
            sendLn("measme " + (!sentMed));
        }
        i = 0;
        if (lower.segrouLab != null) {
            i = lower.segrouLab[0].label;
        }
        if (sentSegrou != i) {
            sentSegrou = i;
            sendLn("segrou " + i + " " + lower.segrouMax);
        }
        i = 0;
        if (lower.bierLab != null) {
            i = lower.bierLab[0].label;
        }
        if (sentBier != i) {
            sentBier = i;
            sendLn("bier " + i + " " + lower.bierLen + " " + lower.bierMax);
        }
        int sent = 0;
        for (i = 0; i < adverted.size(); i++) {
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
        for (i = 0; i < iface.need2adv.size(); i++) {
            tabRouteEntry<addrIP> ntry = iface.need2adv.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.differs(tabRoute.addType.notyet, adverted.find(ntry)) == 0) {
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
            int val = ntry.best.labelLoc.label;
            if (iface.labelPop && (lower.fwdCore.commonLabel.label == val)) {
                val = ipMpls.labelImp;
            }
            a = " label=" + val;
            if (checkPrefix(iface.labelOut, ntry.prefix)) {
                a = "";
            }
        }
        if ((lower.segrouLab != null) && (ntry.best.segrouIdx > 0)) {
            a += " segrou=" + ntry.best.segrouIdx;
        }
        if ((lower.bierLab != null) && (ntry.best.bierIdx > 0)) {
            a += " bieri=" + ntry.best.bierIdx;
            a += " biers=" + ntry.best.bierSub;
        }
        sendLn(s + " prefix=" + addrPrefix.ip2str(ntry.prefix) + a + " metric=" + (ntry.best.metric + iface.metricOut) + " tag=" + ntry.best.tag + " external=" + ((ntry.best.rouSrc & 1) != 0) + " path= " + lower.routerID + " " + tabRouteUtil.dumpAddrList(ntry.best.clustList));
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
            if (a.equals("segrou")) {
                if (lower.lower.segrouLab == null) {
                    continue;
                }
                ntry.best.segrouIdx = bits.str2num(s);
                ntry.best.segrouBeg = lower.gotSegrouBeg;
                ntry.best.segrouSiz = lower.gotSegrouMax;
                ntry.best.labelRem = new ArrayList<Integer>();
                ntry.best.labelRem.add(ntry.best.segrouBeg + ntry.best.segrouIdx);
                continue;
            }
            if (a.equals("bieri")) {
                if (lower.lower.bierLab == null) {
                    continue;
                }
                ntry.best.bierIdx = bits.str2num(s);
                ntry.best.bierBeg = lower.gotBierBeg;
                ntry.best.bierHdr = lower.gotBierLen;
                ntry.best.bierSiz = lower.gotBierMax;
                continue;
            }
            if (a.equals("biers")) {
                if (ntry.best.bierIdx < 1) {
                    continue;
                }
                ntry.best.bierSub = bits.str2num(s);
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
        logger.startThread(this);
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
                if (lower.echoData != bits.str2num(cmd.word())) {
                    continue;
                }
                lower.echoCalc.addValue((int) (bits.getTime() - lower.echoTime));
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
            if (a.equals("resending")) {
                lower.learned.clear();
                lower.lower.notif.wakeup();
                continue;
            }
            if (a.equals("resend")) {
                lower.sendLn("resending");
                lower.sentMet = -1;
                lower.sentMed = false;
                lower.sentSegrou = -1;
                lower.sentBier = -1;
                lower.adverted.clear();
                lower.notif.wakeup();
                continue;
            }
            if (a.equals("nomore")) {
                continue;
            }
            if (a.equals("keepalive")) {
                lower.lastHeard += bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("measme")) {
                lower.gotMeasure = cmd.word().equals("true");
                continue;
            }
            if (a.equals("metric")) {
                lower.gotMetric = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("segrou")) {
                lower.gotSegrouBeg = bits.str2num(cmd.word());
                lower.gotSegrouMax = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("bier")) {
                lower.gotBierBeg = bits.str2num(cmd.word());
                lower.gotBierLen = tabLabelBier.num2bsl(bits.str2num(cmd.word()));
                lower.gotBierMax = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("reachable")) {
                tabRouteEntry<addrIP> ntry = parsePrefix(cmd);
                if (ntry == null) {
                    continue;
                }
                int cnt = tabRoute.delUpdatedEntry(lower.learned, rtrBgpUtil.sfiUnicast, 0, ntry, lower.iface.roumapIn, lower.iface.roupolIn, lower.iface.prflstIn);
                addrIP adr = new addrIP();
                adr.fromIPv4addr(lower.lower.routerID);
                if (tabRouteUtil.findAddrList(ntry.best.clustList, adr) >= 0) {
                    if (cnt > 0) {
                        lower.lower.notif.wakeup();
                    }
                    continue;
                }
                ntry.best.metric += lower.getMetric();
                ntry.best.nextHop = lower.peer.copyBytes();
                ntry.best.distance = lower.iface.distance;
                ntry.best.iface = lower.iface.iface;
                ntry.best.srcRtr = lower.peer.copyBytes();
                cnt += tabRoute.addUpdatedEntry(tabRoute.addType.always, lower.learned, rtrBgpUtil.sfiUnicast, 0, ntry, true, lower.iface.roumapIn, lower.iface.roupolIn, lower.iface.prflstIn);
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
                if (tabRoute.delUpdatedEntry(lower.learned, rtrBgpUtil.sfiUnicast, 0, ntry, lower.iface.roumapIn, lower.iface.roupolIn, lower.iface.prflstIn) > 0) {
                    lower.lower.notif.wakeup();
                }
                continue;
            }
            lower.sendWrn("badCommand " + a);
        }
    }

}
