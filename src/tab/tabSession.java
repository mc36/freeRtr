package tab;

import addr.addrIP;
import cfg.cfgInit;
import java.util.ArrayList;
import pack.packHolder;
import prt.prtDccp;
import prt.prtSctp;
import prt.prtTcp;
import prt.prtUdp;
import prt.prtLudp;
import user.userFormat;
import util.bits;
import util.logger;
import java.util.Comparator;
import java.util.List;
import pack.packNetflow;
import pipe.pipeSide;
import util.cmds;

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
     * unidirection collecting
     */
    public boolean unidir = true;

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
     * session timeout
     */
    public int timeout = 60000;

    public String toString() {
        String a = "";
        if (logMacs) {
            a = " mac";
        }
        if (logBefore) {
            a = " before";
        }
        if (logAfter) {
            a = " after";
        }
        return a.trim();
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
            if (a.equals("mac")) {
                logMacs = true;
            }
            if (a.equals("before")) {
                logBefore = true;
            }
            if (a.equals("after")) {
                logAfter = true;
            }
        }
    }

    /**
     * inspect one packet
     *
     * @param pck packet to inspect
     * @param dir direction of packet
     * @return true to drop, false to forward
     */
    public boolean doPack(packHolder pck, boolean dir) {
        int i = pck.dataSize();
        pck.getSkip(pck.IPsiz);
        boolean b = false;
        pck.UDPsrc = 0;
        pck.UDPtrg = 0;
        pck.UDPsiz = 0;
        switch (pck.IPprt) {
            case prtTcp.protoNum:
                b = prtTcp.parseTCPheader(pck);
                break;
            case prtUdp.protoNum:
                b = prtUdp.parseUDPheader(pck);
                break;
            case prtLudp.protoNum:
                b = prtLudp.parseLUDPheader(pck);
                break;
            case prtDccp.protoNum:
                b = prtDccp.parseDCCPheader(pck);
                break;
            case prtSctp.protoNum:
                b = prtSctp.parseSCTPheader(pck);
                break;
        }
        int o = pck.dataSize();
        pck.getSkip(o - i);
        if (b) {
            return true;
        }
        tabSessionEntry ses = new tabSessionEntry(logMacs);
        ses.ipPrt = pck.IPprt;
        ses.dir = dir;
        ses.srcPrt = pck.UDPsrc;
        ses.trgPrt = pck.UDPtrg;
        ses.srcAdr = pck.IPsrc.copyBytes();
        ses.trgAdr = pck.IPtrg.copyBytes();
        tabSessionEntry res = connects.find(ses);
        if ((res == null) && unidir) {
            ses.srcPrt = pck.UDPtrg;
            ses.trgPrt = pck.UDPsrc;
            ses.srcAdr = pck.IPtrg.copyBytes();
            ses.trgAdr = pck.IPsrc.copyBytes();
            res = connects.find(ses);
        }
        if (res == null) {
            ses.startTime = bits.getTime();
            if (logMacs) {
                ses.srcMac = pck.ETHsrc.copyBytes();
                ses.trgMac = pck.ETHtrg.copyBytes();
            }
            connects.add(ses);
            res = ses;
            if (logBefore) {
                logger.info("started " + res);
            }
        }
        res.lastTime = bits.getTime();
        if (dir) {
            res.txByte += o;
            res.txPack++;
        } else {
            res.rxByte += o;
            res.rxPack++;
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
            l = new userFormat("|", "dir|prt|tos|src|src|trg|trg|rx|tx|time|src|trg");
        } else {
            l = new userFormat("|", "dir|prt|tos|src|src|trg|trg|rx|tx|time");
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
        userFormat l = new userFormat("|", "addr|rx|tx|time");
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
            updateTalker(ept, cur.srcAdr, cur.rxByte, cur.txByte, cur.startTime);
            updateTalker(ept, cur.trgAdr, cur.rxByte, cur.txByte, cur.startTime);
        }
        return ept;
    }

    private void updateTalker(tabGen<tabSessionEndpoint> ept, addrIP adr, long rx, long tx, long tim) {
        tabSessionEndpoint ntry = new tabSessionEndpoint();
        ntry.adr = adr.copyBytes();
        tabSessionEndpoint res = ept.find(ntry);
        if (res == null) {
            res = ntry;
            ept.add(res);
        }
        res.rx += rx;
        res.tx += tx;
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
        pckF.upt = pckF.tim - (int) (cfgInit.jvmStarted / 1000);
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

    public long rx;

    public long tx;

    public long tim;

    public String dump() {
        return adr + "|" + rx + "|" + tx + "|" + bits.timePast(tim);
    }

    public int compare(tabSessionEndpoint o1, tabSessionEndpoint o2) {
        return o1.adr.compare(o1.adr, o2.adr);
    }

}
