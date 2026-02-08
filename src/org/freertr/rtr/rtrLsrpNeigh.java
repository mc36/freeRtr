package org.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
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
import org.freertr.tab.tabGen;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabLabelEntry;
import org.freertr.user.userUpgrade;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.keyword;
import org.freertr.util.logger;
import org.freertr.util.notifier;

/**
 * lsrp neighbor
 *
 * @author matecsaba
 */
public class rtrLsrpNeigh implements Runnable, rtrBfdClnt, Comparable<rtrLsrpNeigh> {

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
     * metric of peer
     */
    public int gotMetric;

    /**
     * permission of peer
     */
    public boolean gotMeasure;

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
    protected final notifier notif = new notifier();

    /**
     * protocol handler
     */
    protected final rtrLsrp lower;

    /**
     * interface handler
     */
    protected final rtrLsrpIface iface;

    /**
     * uptime
     */
    protected long upTime;

    /**
     * nomore seen
     */
    protected boolean noMore;

    /**
     * nomore sent
     */
    protected boolean allSent;

    /**
     * segment routing label
     */
    protected tabLabelEntry segrouLab;

    /**
     * advertised data
     */
    protected tabGen<rtrLsrpData> advert;

    /**
     * advertised metric
     */
    protected int sentMet;

    /**
     * advertised metric
     */
    protected boolean sentMed;

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
    public rtrLsrpNeigh(rtrLsrp parent, rtrLsrpIface ifc, addrIPv4 peerId, addrIP peerAd) {
        lower = parent;
        iface = ifc;
        rtrId = peerId.copyBytes();
        peer = peerAd.copyBytes();
        lastHeard = bits.getTime();
        advert = new tabGen<rtrLsrpData>();
        sentMet = -1;
        sentMed = false;
        gotMetric = -1;
        gotMeasure = true;
    }

    public int compareTo(rtrLsrpNeigh o) {
        return peer.compareTo(o.peer);
    }

    /**
     * ready state
     *
     * @return false if no, true if yes
     */
    protected boolean isReady() {
        return noMore && allSent;
    }

    public String toString() {
        return "lsrp with " + peer;
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
        if (debugger.rtrLsrpEvnt) {
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
        if (debugger.rtrLsrpEvnt) {
            logger.debug("stopping peer " + rtrId + " (" + peer + ")");
        }
        boolean oldrun = need2run;
        need2run = false;
        noMore = false;
        allSent = false;
        if (conn != null) {
            conn.setClose();
        }
        if (oldrun) {
            iface.neighs.del(this);
        }
        tabLabel.release(segrouLab, tabLabelEntry.owner.lsrpAdj);
        lower.todo.set(0);
        lower.notif.wakeup();
        iface.iface.bfdDel(peer, this);
        notif.wakeup();
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
        int met = iface.metric;
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
        seqRx++;
        if (conn.isClosed() != 0) {
            return null;
        }
        String a = conn.lineGet(0x11);
        a = a.trim();
        if (debugger.rtrLsrpTraf) {
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
        if (debugger.rtrLsrpTraf) {
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

    private void doRun() {
        if (conn != null) {
            conn.setClose();
        }
        advert.clear();
        bits.sleep(bits.random(1000, 5000));
        if (peer.compareTo(iface.iface.addr) > 0) {
            if (debugger.rtrLsrpEvnt) {
                logger.debug("accepting " + peer);
            }
            prtAccept ac = new prtAccept(lower.tcpCore, new pipeLine(65536, false), iface.iface, rtrLsrp.port, peer, 0, "lsrp", -1, null, iface.sendingTtl, iface.sendingTos);
            ac.wait4conn(30000);
            conn = ac.getConn(true);
        } else {
            if (debugger.rtrLsrpEvnt) {
                logger.debug("connecting " + peer);
            }
            conn = lower.tcpCore.streamConnect(new pipeLine(65536, false), iface.iface, 0, peer, rtrLsrp.port, "lsrp", -1, null, iface.sendingTtl, iface.sendingTos);
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
                if (debugger.rtrLsrpEvnt) {
                    logger.debug("secure client " + peer);
                }
                conn = secClient.openSec(conn, iface.encryptionMethod, null, "", "");
            } else {
                if (debugger.rtrLsrpEvnt) {
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
            iface.iface.bfdAdd(peer, this, "lsrp");
            if (iface.iface.bfdWait(peer, iface.deadTimer)) {
                sendErr("bfdFail");
                return;
            }
        }
        if (!need2run) {
            sendErr("notNeeded");
            return;
        }
        advert.clear();
        logger.warn("neighbor " + name + " (" + peer + ") up");
        new rtrLsrpNeighRcvr(this).startWork();
        if (lower.segrouLab != null) {
            segrouLab = tabLabel.allocate(tabLabelEntry.owner.lsrpAdj);
            segrouLab.setFwdMpls(tabLabelEntry.owner.lsrpAdj, lower.fwdCore, iface.iface, peer, tabLabel.int2labels(ipMpls.labelImp));
        }
        lower.todo.set(0);
        lower.notif.wakeup();
        if (iface.bfdTrigger > 0) {
            iface.iface.bfdAdd(peer, this, "lsrp");
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
        for (i = 0; i < advert.size(); i++) {
            rtrLsrpData ntry = advert.get(i);
            if (ntry == null) {
                continue;
            }
            if (lower.database.find(ntry) == null) {
                advert.del(ntry);
                continue;
            }
        }
        int sent = 0;
        for (i = 0; i < lower.database.size(); i++) {
            rtrLsrpData ntry = lower.database.get(i);
            if (ntry == null) {
                continue;
            }
            if (iface.databaseFilter) {
                if (ntry.rtrId.compareTo(lower.routerID) != 0) {
                    continue;
                }
            }
            if (!ntry.differs(advert.find(ntry))) {
                continue;
            }
            if (conn.ready2tx() < 1024) {
                return;
            }
            sendLn("update " + ntry.dump(rtrLsrpData.dmpFull));
            advert.put(ntry.copyHead());
            sent++;
        }
        if (sent > 0) {
            sendLn("nomore");
        }
        boolean old = allSent;
        allSent = true;
        if (!old) {
            lower.todo.set(0);
            lower.notif.wakeup();
        }
    }

}

class rtrLsrpNeighRcvr implements Runnable {

    private final rtrLsrpNeigh lower;

    public rtrLsrpNeighRcvr(rtrLsrpNeigh parent) {
        lower = parent;
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
                rtrLsrpData ntry = new rtrLsrpData();
                ntry.rtrId = new addrIPv4();
                ntry.rtrId.fromString(cmd.word());
                lower.advert.del(ntry);
                continue;
            }
            if (a.equals("resending")) {
                continue;
            }
            if (a.equals("resend")) {
                lower.sendLn("resending");
                lower.sentMet = -1;
                lower.sentMed = false;
                lower.advert.clear();
                lower.notif.wakeup();
                continue;
            }
            if (a.equals("keepalive")) {
                lower.lastHeard += bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("measme")) {
                lower.gotMeasure = cmd.word().equals("true");
                lower.lower.todo.set(0);
                lower.lower.notif.wakeup();
                continue;
            }
            if (a.equals("metric")) {
                lower.gotMetric = bits.str2num(cmd.word());
                lower.lower.todo.set(0);
                lower.lower.notif.wakeup();
                continue;
            }
            if (a.equals("nomore")) {
                lower.noMore = true;
                lower.lower.todo.set(0);
                lower.lower.notif.wakeup();
                continue;
            }
            if (!a.equals("update")) {
                lower.sendWrn("badCommand " + a);
                continue;
            }
            rtrLsrpData ntry = new rtrLsrpData();
            if (ntry.fromString(cmd)) {
                lower.sendWrn("badUpdate");
                continue;
            }
            if (lower.lower.authentication != null) {
                if (ntry.password == null) {
                    lower.sendWrn("missingAuth");
                    continue;
                }
                if (!ntry.password.equals(ntry.calcPass(lower.lower.authentication))) {
                    lower.sendWrn("invalidAuth");
                    continue;
                }
            }
            if (ntry.differs(lower.advert.find(ntry))) {
                lower.advert.put(ntry.copyHead());
            }
            if (ntry.better(lower.lower.database.find(ntry))) {
                lower.iface.gotAdvert(ntry);
                lower.lower.database.put(ntry);
                lower.lower.todo.set(0);
                lower.lower.notif.wakeup();
                if ((ntry.rtrId.compareTo(lower.lower.routerID) == 0) && (!ntry.hostname.equals(cfgAll.hostName))) {
                    logger.error("duplicate routerid with " + ntry.hostname);
                }
            }
        }
    }

}
