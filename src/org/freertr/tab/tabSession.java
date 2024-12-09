package org.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgAceslst;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgInit;
import org.freertr.cfg.cfgPlymp;
import org.freertr.cfg.cfgSessn;
import org.freertr.cfg.cfgTrnsltn;
import org.freertr.pack.packHolder;
import org.freertr.pack.packNetflow;
import org.freertr.pack.packTls;
import org.freertr.pack.packTlsHndshk;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtTcp;
import org.freertr.serv.servHttp;
import org.freertr.user.userFormat;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.logger;
import org.freertr.util.notifier;

/**
 * one session record
 *
 * @author matecsaba
 */
public class tabSession implements Runnable {

    /**
     * name of this group
     */
    public String name;

    /**
     * list of prefixes
     */
    public final tabGen<tabSessionEntry> connects = new tabGen<tabSessionEntry>();

    private boolean need2run;

    private int count;

    /**
     * protocol version
     */
    public boolean ipv4;

    /**
     * flows / export
     */
    public int limit;

    /**
     * source id
     */
    public int source;

    /**
     * bidirection collecting
     */
    public final boolean bidir;

    /**
     * member of inspection
     */
    public tabSession master;

    /**
     * log before session
     */
    public boolean logBefore = false;

    /**
     * log after session
     */
    public boolean logAfter = false;

    /**
     * log mac addresses
     */
    public boolean logMacs = false;

    /**
     * log dropped sessions
     */
    public boolean logDrop = false;

    /**
     * drop new rx sessions
     */
    public boolean dropRx = false;

    /**
     * drop new rx sessions
     */
    public boolean dropTx = false;

    /**
     * drop fragmented datagrams
     */
    public boolean dropFrg = false;

    /**
     * maximum number of sessions
     */
    public int maxSess = 0;

    /**
     * maximum rate of sessions
     */
    public tabQos maxRate = null;

    /**
     * allow routing multicast
     */
    public boolean allowRoutng = false;

    /**
     * allow local multicast
     */
    public boolean allowSending = false;

    /**
     * allow link local
     */
    public boolean allowLnklc = false;

    /**
     * allow broadcast
     */
    public boolean allowBcast = false;

    /**
     * allow routed multicast
     */
    public boolean allowMcast = false;

    /**
     * to be notified
     */
    protected notifier notif;

    /**
     * allow specific packets
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> allowList;

    /**
     * allow specific urls
     */
    public cfgTrnsltn allowUrl;

    /**
     * session timeout
     */
    public int timeout = 60000;

    /**
     * default timeout
     */
    public final int defTim;

    /**
     * create new instance
     *
     * @param bid bidirectional
     * @param tim default timeout
     */
    public tabSession(boolean bid, int tim) {
        bidir = bid;
        defTim = tim;
        timeout = tim;
    }

    public String toString() {
        List<String> cfg = new ArrayList<String>();
        getConfig(cfg, "");
        String s = "";
        for (int i = 0; i < cfg.size(); i++) {
            String a = cfg.get(i);
            if (a.startsWith(cmds.negated)) {
                continue;
            }
            s += a + " ";
        }
        return s.trim();
    }

    /**
     * get configuration
     *
     * @param res where to write
     * @param beg beginning to use
     */
    public void getConfig(List<String> res, String beg) {
        if (master != null) {
            res.add(beg + "member " + master.name + " " + name);
        } else {
            res.add(beg + "no member");
        }
        cmds.cfgLine(res, !logMacs, beg, "mac", "");
        cmds.cfgLine(res, !logBefore, beg, "before", "");
        cmds.cfgLine(res, !logAfter, beg, "after", "");
        cmds.cfgLine(res, !logDrop, beg, "dropped", "");
        cmds.cfgLine(res, !dropRx, beg, "drop-rx", "");
        cmds.cfgLine(res, !dropTx, beg, "drop-tx", "");
        cmds.cfgLine(res, !dropFrg, beg, "drop-frg", "");
        cmds.cfgLine(res, !allowRoutng, beg, "allow-routing", "");
        cmds.cfgLine(res, !allowSending, beg, "allow-sending", "");
        cmds.cfgLine(res, !allowLnklc, beg, "allow-linklocal", "");
        cmds.cfgLine(res, !allowMcast, beg, "allow-multicast", "");
        cmds.cfgLine(res, !allowBcast, beg, "allow-broadcast", "");
        cmds.cfgLine(res, allowList == null, beg, "allow-list", "" + allowList);
        cmds.cfgLine(res, allowUrl == null, beg, "allow-url", "" + allowUrl);
        cmds.cfgLine(res, timeout == defTim, beg, "timeout", "" + timeout);
        cmds.cfgLine(res, maxSess < 1, beg, "sessions", "" + maxSess);
        cmds.cfgLine(res, maxRate == null, beg, "rate", "" + maxRate);
    }

    /**
     * convert from string
     *
     * @param cmd string
     */
    public void fromString(cmds cmd) {
        for (;;) {
            String a = cmd.word();
            if (a.length() < 1) {
                break;
            }
            boolean negated = a.equals(cmds.negated);
            if (negated) {
                a = cmd.word();
            }
            if (a.equals("mac")) {
                logMacs = !negated;
                continue;
            }
            if (a.equals("before")) {
                logBefore = !negated;
                continue;
            }
            if (a.equals("after")) {
                logAfter = !negated;
                continue;
            }
            if (a.equals("dropped")) {
                logDrop = !negated;
                continue;
            }
            if (a.equals("drop-rx")) {
                dropRx = !negated;
                continue;
            }
            if (a.equals("drop-tx")) {
                dropTx = !negated;
                continue;
            }
            if (a.equals("drop-frg")) {
                dropFrg = !negated;
                continue;
            }
            if (a.equals("allow-routing")) {
                allowRoutng = !negated;
                continue;
            }
            if (a.equals("allow-sending")) {
                allowSending = !negated;
                continue;
            }
            if (a.equals("allow-linklocal")) {
                allowLnklc = !negated;
                continue;
            }
            if (a.equals("allow-broadcast")) {
                allowBcast = !negated;
                continue;
            }
            if (a.equals("allow-multicast")) {
                allowMcast = !negated;
                continue;
            }
            if (a.equals("member")) {
                if (negated) {
                    master = null;
                    continue;
                }
                cfgSessn ntry = cfgAll.sessnFind(cmd.word(), false);
                if (ntry == null) {
                    continue;
                }
                master = ntry.connects;
                name = cmd.word();
                continue;
            }
            if (a.equals("allow-url")) {
                if (negated) {
                    allowUrl = null;
                    continue;
                }
                allowUrl = cfgAll.trnsltnFind(cmd.word(), false);
                continue;
            }
            if (a.equals("allow-list")) {
                if (negated) {
                    allowList = null;
                    continue;
                }
                cfgAceslst ntry = cfgAll.aclsFind(cmd.word(), false);
                if (ntry == null) {
                    continue;
                }
                allowList = ntry.aceslst;
                continue;
            }
            if (a.equals("timeout")) {
                if (negated) {
                    timeout = defTim;
                    continue;
                }
                timeout = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("sessions")) {
                if (negated) {
                    maxSess = 0;
                    continue;
                }
                maxSess = bits.str2num(cmd.word());
                continue;
            }
            if (a.equals("rate")) {
                if (negated) {
                    maxRate = null;
                    continue;
                }
                cfgPlymp plc = cfgAll.plmpFind(cmd.word(), false);
                if (plc == null) {
                    cmd.error("no such policy map");
                    continue;
                }
                maxRate = tabQos.convertPolicy(plc.plcmap);
                continue;
            }
        }
    }

    /**
     * pass one session
     *
     * @param ses session to pass
     * @return session entry
     */
    protected tabSessionEntry sessPass(tabSessionEntry ses) {
        ses.startTime = bits.getTime();
        ses.lastTime = ses.startTime;
        connects.add(ses);
        if (notif != null) {
            notif.wakeup();
        }
        if (logBefore) {
            logger.info("started " + ses);
        }
        return ses;
    }

    /**
     * drop one session
     *
     * @param ses session to drop
     * @return session entry
     */
    protected tabSessionEntry sessDrop(tabSessionEntry ses) {
        if (logDrop) {
            logger.info("dropped " + ses);
        }
        return null;
    }

    /**
     * inspect one session
     *
     * @param ses session to inspect
     * @param pck packet to inspect
     * @param dir direction of packet, true=tx, false=rx
     * @return entry to update, null if denied
     */
    public tabSessionEntry doSess(tabSessionEntry ses, packHolder pck, boolean dir) {
        if (dropFrg) {
            if (pck == null) {
                return sessDrop(ses);
            }
            if (pck.IPmf) {
                return sessDrop(ses);
            }
            if (pck.IPfrg > 0) {
                return sessDrop(ses);
            }
        }
        tabSessionEntry res;
        res = connects.find(ses);
        if ((res == null) && bidir) {
            ses = ses.reverseDirection();
            res = connects.find(ses);
        }
        if (res != null) {
            res.lastTime = bits.getTime();
            ses.dir = res.dir;
            return res;
        }
        if (maxSess > 0) {
            if (connects.size() >= maxSess) {
                return sessDrop(ses);
            }
        }
        if (!dropRx && !dropTx) {
            return sessPass(ses);
        }
        if (dir) {
            if (!dropTx) {
                return sessPass(ses);
            }
        } else {
            if (!dropRx) {
                return sessPass(ses);
            }
        }
        if (pck == null) {
            return sessDrop(ses);
        }
        if (maxRate != null) {
            if (maxRate.checkPacket(pck)) {
                return sessDrop(ses);
            }
        }
        if (allowSending && (pck.INTupper != 0)) {
            return sessPass(ses);
        }
        if (allowMcast && pck.IPmlt && pck.IPmlr) {
            return sessPass(ses);
        }
        if (allowRoutng && pck.IPmlt && !pck.IPmlr) {
            return sessPass(ses);
        }
        if (allowLnklc && pck.IPlnk) {
            return sessPass(ses);
        }
        if (allowBcast && pck.IPbrd) {
            return sessPass(ses);
        }
        if (allowList != null) {
            if (allowList.matches(false, false, pck)) {
                return sessPass(ses);
            }
        }
        if (allowUrl != null) {
            if (pck.IPprt != prtTcp.protoNum) {
                return sessDrop(ses);
            }
            int mod = 0;
            if (pck.UDPtrg == servHttp.clearPort) {
                mod = 1;
            }
            if (pck.UDPtrg == servHttp.securePort) {
                mod = 2;
            }
            if (mod < 1) {
                return sessDrop(ses);
            }
            ses.evaluating = new packHolder(true, true);
            ses.evaluating.TCPflg = mod;
            ses.evaluating.IPlnk = dir;
            return sessPass(ses);
        }
        return sessDrop(ses);
    }

    /**
     * inspect one packet
     *
     * @param pck packet to inspect
     * @param dir direction of packet, true=tx, false=rx
     * @return true to drop, false to forward
     */
    public boolean doPack(packHolder pck, boolean dir) {
        if (master != null) {
            return master.doPack(pck, dir);
        }
        int i = pck.dataSize();
        pck.getSkip(pck.IPsiz);
        tabQos.classifyLayer4(pck);
        pck.getSkip(pck.dataSize() - i);
        tabSessionEntry ses = tabSessionEntry.fromPack(pck, logMacs);
        ses.dir = dir;
        ses = doSess(ses, pck, dir);
        if (ses == null) {
            return true;
        }
        if (dir) {
            ses.cntr.tx(pck);
        } else {
            ses.cntr.rx(pck);
        }
        if (ses.evaluating == null) {
            return false;
        }
        if (ses.evaluating.IPlnk != dir) {
            return false;
        }
        if (ses.evaluating.IPbrd) {
            return true;
        }
        int o = pck.IPsiz + pck.UDPsiz;
        if (o > i) {
            return true;
        }
        byte[] buf = new byte[i - o];
        pck.getCopy(buf, 0, o, buf.length);
        pck = ses.evaluating;
        if ((pck.dataSize() + buf.length) >= packHolder.maxData) {
            return true;
        }
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        pck.merge2end();
        String s = "";
        switch (pck.TCPflg) {
            case 1:
                o = -100;
                boolean don = false;
                for (i = 0; i < pck.dataSize(); i++) {
                    if (pck.getByte(i) != 13) {
                        continue;
                    }
                    if ((i - o) > 2) {
                        o = i;
                        continue;
                    }
                    don = true;
                    break;
                }
                if (!don) {
                    return false;
                }
                ses.evaluating.IPbrd = true;
                don = false;
                for (i = 0; i <= o; i++) {
                    int p = pck.getByte(i);
                    switch (p) {
                        case 10:
                        case 13:
                            p = s.indexOf(":");
                            if (p < 0) {
                                s = "";
                                continue;
                            }
                            if (s.substring(0, p).trim().toLowerCase().equals("host")) {
                                s = s.substring(p + 1, s.length()).trim();
                                don = true;
                            } else {
                                s = "";
                            }
                            break;
                        default:
                            s += (char) p;
                            break;
                    }
                    if (don) {
                        break;
                    }
                }
                if (!don) {
                    return true;
                }
                break;
            case 2:
                packTls tlsp = new packTls(false);
                o = pck.dataSize() - tlsp.getHeadSize();
                if (o < 0) {
                    return false;
                }
                i = pck.msbGetW(3);
                if (o < i) {
                    return false;
                }
                ses.evaluating.IPbrd = true;
                tlsp.pckTyp = pck.getByte(0);
                pck.getSkip(tlsp.getHeadSize());
                pck.setBytesLeft(i);
                tlsp.pckDat = pck;
                packTlsHndshk tlsh = new packTlsHndshk(tlsp, false);
                if (tlsh.headerParse()) {
                    return true;
                }
                if (tlsh.clntHelloParse()) {
                    return true;
                }
                s = tlsh.servNam;
                if (s == null) {
                    return true;
                }
                break;
            default:
                return true;
        }
        ses.sawUrl = s;
        s = allowUrl.doTranslate(s);
        if (s == null) {
            if (logDrop) {
                logger.info("forbidden " + ses);
            }
            return true;
        }
        ses.evaluating = null;
        if (notif != null) {
            notif.wakeup();
        }
        return false;
    }

    /**
     * create show output
     *
     * @return show output
     */
    public userFormat doShowInsp() {
        if (master != null) {
            return master.doShowInsp();
        }
        userFormat l;
        if (logMacs) {
            l = new userFormat("|", "dir|prt|tos|addr|port|addr|port|url|rx|tx|rx|tx|time|src|trg", "3|2source|2target|1|2packet|2byte|1|2mac");
        } else {
            l = new userFormat("|", "dir|prt|tos|addr|port|addr|port|url|rx|tx|rx|tx|time", "3|2source|2target|1|2packet|2byte|1");
        }
        for (int i = 0; i < connects.size(); i++) {
            l.add(connects.get(i).dump());
        }
        return l;
    }

    /**
     * create show output
     *
     * @return show output
     */
    public userFormat doShowTalk() {
        if (master != null) {
            return master.doShowTalk();
        }
        tabGen<tabSessionEndpoint> ept = getTopTalker();
        userFormat l = new userFormat("|", "addr|rx|tx|rx|tx|time", "1|2packet|2byte|1");
        for (int i = 0; i < ept.size(); i++) {
            l.add(ept.get(i).dump());
        }
        return l;
    }

    private tabGen<tabSessionEndpoint> getTopTalker() {
        tabGen<tabSessionEndpoint> ept = new tabGen<tabSessionEndpoint>();
        for (int i = 0; i < connects.size(); i++) {
            tabSessionEntry cur = connects.get(i);
            if (cur == null) {
                continue;
            }
            updateTalker(ept, cur.srcAdr, cur.cntr, cur.hwCntr, cur.startTime);
            updateTalker(ept, cur.trgAdr, cur.cntr, cur.hwCntr, cur.startTime);
        }
        return ept;
    }

    private void updateTalker(tabGen<tabSessionEndpoint> ept, addrIP adr, counter cntr, counter hwCntr, long tim) {
        tabSessionEndpoint ntry = new tabSessionEndpoint();
        ntry.adr = adr.copyBytes();
        tabSessionEndpoint res = ept.find(ntry);
        if (res == null) {
            res = ntry;
            ept.add(res);
            ntry.cntr = new counter();
        }
        res.cntr = res.cntr.plus(cntr);
        if (hwCntr != null) {
            if (res.hwCntr == null) {
                res.hwCntr = new counter();
            }
            res.hwCntr = res.hwCntr.plus(hwCntr);
        }
        if ((res.tim == 0) || (res.tim > tim)) {
            res.tim = tim;
        }
    }

    /**
     * start timer
     */
    public void startTimer() {
        need2run = true;
        new Thread(this).start();
    }

    /**
     * set notifier
     *
     * @param n notifier
     */
    public void setNotifier(notifier n) {
        notif = n;
        if (master != null) {
            master.setNotifier(n);
        }
    }

    /**
     * stop timer
     */
    public void stopTimer() {
        need2run = false;
        notif = null;
    }

    /**
     * inspect processing
     */
    public void doInspect() {
        long tim = bits.getTime();
        int cnt = 0;
        for (int i = connects.size() - 1; i >= 0; i--) {
            tabSessionEntry cur = connects.get(i);
            if (cur == null) {
                continue;
            }
            if ((tim - cur.lastTime) < timeout) {
                continue;
            }
            connects.del(cur);
            cnt++;
            if (logAfter) {
                logger.info("finished " + cur);
            }
        }
        if ((cnt > 0) && (notif != null)) {
            notif.wakeup();
        }
    }

    private void doNetflow(packHolder pckB, pipeSide pipe, List<tabSessionEntry> lst, boolean tmp) {
        if (pipe == null) {
            return;
        }
        count++;
        pckB.clear();
        packNetflow pckF = new packNetflow();
        pckF.seq = count;
        pckF.tim = (int) (bits.getTime() / 1000);
        pckF.upt = pckF.tim - (int) (cfgInit.started / 1000);
        pckF.sou = source;
        pckF.ipv4 = ipv4;
        if (tmp) {
            pckF.putTemp(pckB);
        }
        pckF.putFlow(pckB, lst);
        pckF.putHead(pckB);
        pckB.pipeSend(pipe, 0, pckB.dataSize(), 2);
    }

    /**
     * netflow processing
     *
     * @param pipe where to send
     */
    public void doNetflow(pipeSide pipe) {
        long tim = bits.getTime();
        List<tabSessionEntry> lst = new ArrayList<tabSessionEntry>();
        boolean tmp = true;
        packHolder pck = new packHolder(true, true);
        int cnt = 0;
        for (int i = connects.size() - 1; i >= 0; i--) {
            tabSessionEntry cur = connects.get(i);
            if (cur == null) {
                continue;
            }
            lst.add(cur.copyBytes());
            cur.clearCounts();
            if (lst.size() > limit) {
                doNetflow(pck, pipe, lst, tmp);
                lst.clear();
                tmp = false;
            }
            if ((tim - cur.lastTime) < timeout) {
                continue;
            }
            connects.del(cur);
            cnt++;
            if (logAfter) {
                logger.info("finished " + cur);
            }
        }
        if (lst.size() > 0) {
            doNetflow(pck, pipe, lst, tmp);
        }
        if ((cnt > 0) && (notif != null)) {
            notif.wakeup();
        }
    }

    public void run() {
        for (;;) {
            if (!need2run) {
                return;
            }
            bits.sleep(timeout / 4);
            try {
                doInspect();
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
    }

}

class tabSessionEndpoint implements Comparable<tabSessionEndpoint> {

    public addrIP adr;

    public counter cntr;

    public counter hwCntr;

    public long tim;

    public String dump() {
        String hpr = "";
        String hpt = "";
        String hbr = "";
        String hbt = "";
        if (hwCntr != null) {
            hpr = "+" + hwCntr.packRx;
            hpt = "+" + hwCntr.packTx;
            hbr = "+" + hwCntr.byteRx;
            hbt = "+" + hwCntr.byteTx;
        }
        return adr + "|" + cntr.packRx + hpr + "|" + cntr.packTx + hpt + "|" + cntr.byteRx + hbr + "|" + cntr.byteTx + hbt + "|" + bits.timePast(tim);
    }

    public int compareTo(tabSessionEndpoint o) {
        return adr.compareTo(o.adr);
    }

}
