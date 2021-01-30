package tab;

import addr.addrIP;
import cfg.cfgAceslst;
import cfg.cfgAll;
import cfg.cfgInit;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import pack.packNetflow;
import pipe.pipeSide;
import user.userFormat;
import util.bits;
import util.cmds;
import util.logger;

/**
 * one session record
 *
 * @author matecsaba
 */
public class tabSession implements Runnable {

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
     * allow routing multicast
     */
    public boolean allowRoutng = false;

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
     * allow specific packets
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> allowList;

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
            if (a.startsWith("no")) {
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
        cmds.cfgLine(res, !logMacs, beg, "mac", "");
        cmds.cfgLine(res, !logBefore, beg, "before", "");
        cmds.cfgLine(res, !logAfter, beg, "after", "");
        cmds.cfgLine(res, !logDrop, beg, "dropped", "");
        cmds.cfgLine(res, !dropRx, beg, "drop-rx", "");
        cmds.cfgLine(res, !dropTx, beg, "drop-tx", "");
        cmds.cfgLine(res, !allowRoutng, beg, "allow-routing", "");
        cmds.cfgLine(res, !allowLnklc, beg, "allow-linklocal", "");
        cmds.cfgLine(res, !allowMcast, beg, "allow-multicast", "");
        cmds.cfgLine(res, !allowBcast, beg, "allow-broadcast", "");
        cmds.cfgLine(res, allowList == null, beg, "allow-list", "" + allowList);
        cmds.cfgLine(res, timeout == defTim, beg, "timeout", "" + timeout);
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
            boolean negated = a.equals("no");
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
            if (a.equals("allow-routing")) {
                allowRoutng = !negated;
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
                } else {
                    timeout = bits.str2num(cmd.word());
                }
                continue;
            }
        }
    }

    private tabSessionEntry sessPass(tabSessionEntry ses) {
        ses.startTime = bits.getTime();
        ses.lastTime = ses.startTime;
        connects.add(ses);
        if (!logBefore) {
            return ses;
        }
        logger.info("started " + ses);
        return ses;
    }

    private tabSessionEntry sessDrop(tabSessionEntry ses) {
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
        tabSessionEntry res = connects.find(ses);
        if ((res == null) && bidir) {
            ses = ses.reverseDirection();
            res = connects.find(ses);
        }
        if (res != null) {
            res.lastTime = bits.getTime();
            ses.dir = res.dir;
            return res;
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
        if (allowMcast && pck.IPmlt) {
            return sessPass(ses);
        }
        if (allowRoutng && pck.IPmlr) {
            return sessPass(ses);
        }
        if (allowLnklc && pck.IPlnk) {
            return sessPass(ses);
        }
        if (allowBcast && pck.IPbrd) {
            return sessPass(ses);
        }
        if (allowList == null) {
            return sessDrop(ses);
        }
        if (allowList.matches(false, false, pck)) {
            return sessPass(ses);
        } else {
            return sessDrop(ses);
        }
    }

    /**
     * inspect one packet
     *
     * @param pck packet to inspect
     * @param dir direction of packet, true=tx, false=rx
     * @return true to drop, false to forward
     */
    public boolean doPack(packHolder pck, boolean dir) {
        int i = pck.dataSize();
        pck.getSkip(pck.IPsiz);
        tabQos.classifyLayer4(pck);
        int o = pck.dataSize();
        pck.getSkip(o - i);
        tabSessionEntry ses = tabSessionEntry.fromPack(pck, logMacs);
        ses.dir = dir;
        ses = doSess(ses, pck, dir);
        if (ses == null) {
            return true;
        }
        if (dir) {
            ses.txByte += o;
            ses.txPack++;
        } else {
            ses.rxByte += o;
            ses.rxPack++;
        }
        return false;
    }

    /**
     * create show output
     *
     * @return show output
     */
    public userFormat doShowInsp() {
        userFormat l;
        if (logMacs) {
            l = new userFormat("|", "dir|prt|tos|addr|port|addr|port|rx|tx|rx|tx|time|src|trg", "3|2source|2target|2packet|2byte|1|2mac");
        } else {
            l = new userFormat("|", "dir|prt|tos|addr|port|addr|port|rx|tx|rx|tx|time", "3|2source|2target|2packet|2byte|1");
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
            updateTalker(ept, cur.srcAdr, cur.rxByte, cur.txByte, cur.rxPack, cur.txPack, cur.startTime);
            updateTalker(ept, cur.trgAdr, cur.rxByte, cur.txByte, cur.rxPack, cur.txPack, cur.startTime);
        }
        return ept;
    }

    private void updateTalker(tabGen<tabSessionEndpoint> ept, addrIP adr, long rxb, long txb, long rxp, long txp, long tim) {
        tabSessionEndpoint ntry = new tabSessionEndpoint();
        ntry.adr = adr.copyBytes();
        tabSessionEndpoint res = ept.find(ntry);
        if (res == null) {
            res = ntry;
            ept.add(res);
        }
        res.rxb += rxb;
        res.txb += txb;
        res.rxp += rxp;
        res.txp += txp;
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
     * stop timer
     */
    public void stopTimer() {
        need2run = false;
    }

    /**
     * inspect processing
     */
    public void doInspect() {
        long tim = bits.getTime();
        for (int i = connects.size() - 1; i >= 0; i--) {
            tabSessionEntry cur = connects.get(i);
            if (cur == null) {
                continue;
            }
            if ((tim - cur.lastTime) < timeout) {
                continue;
            }
            connects.del(cur);
            if (logAfter) {
                logger.info("finished " + cur);
            }
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
            if (logAfter) {
                logger.info("finished " + cur);
            }
        }
        if (lst.size() > 0) {
            doNetflow(pck, pipe, lst, tmp);
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

class tabSessionEndpoint implements Comparator<tabSessionEndpoint> {

    public addrIP adr;

    public long rxb;

    public long txb;

    public long rxp;

    public long txp;

    public long tim;

    public String dump() {
        return adr + "|" + rxp + "|" + txp + "|" + rxb + "|" + txb + "|" + bits.timePast(tim);
    }

    public int compare(tabSessionEndpoint o1, tabSessionEndpoint o2) {
        return o1.adr.compare(o1.adr, o2.adr);
    }

}
