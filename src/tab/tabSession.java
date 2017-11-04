package tab;

import addr.addrType;
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

/**
 * one session record
 *
 * @param <T> address type
 * @author matecsaba
 */
public class tabSession<T extends addrType> implements Runnable {

    /**
     * list of prefixes
     */
    public final tabGen<tabSessionEntry<T>> connects = new tabGen<tabSessionEntry<T>>();

    private boolean need2run;

    /**
     * log before session
     */
    public boolean logBefore = true;

    /**
     * log after session
     */
    public boolean logAfter = true;

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
            a = "mac";
        }
        return a;
    }

    /**
     * inspect one packet
     *
     * @param pck packet to inspect
     * @param dir direction of packet
     * @return true to drop, false to forward
     */
    @SuppressWarnings("unchecked")
    public boolean doPack(packHolder pck, boolean dir) {
        int i = pck.dataSize();
        pck.getSkip(pck.IPsiz);
        boolean b;
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
            default:
                pck.getSkip(-pck.IPsiz);
                return false;
        }
        int o = pck.dataSize();
        pck.getSkip(o - i);
        if (b) {
            return true;
        }
        tabSessionEntry<T> ses = new tabSessionEntry<T>(logMacs);
        ses.ipPrt = pck.IPprt;
        ses.dir = dir;
        ses.srcPrt = pck.UDPsrc;
        ses.trgPrt = pck.UDPtrg;
        ses.srcAdr = (T) pck.IPsrc.copyBytes();
        ses.trgAdr = (T) pck.IPtrg.copyBytes();
        tabSessionEntry<T> res = connects.find(ses);
        if (res == null) {
            ses.srcPrt = pck.UDPtrg;
            ses.trgPrt = pck.UDPsrc;
            ses.srcAdr = (T) pck.IPtrg.copyBytes();
            ses.trgAdr = (T) pck.IPsrc.copyBytes();
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
            l = new userFormat("|", "dir|prt|src|src|trg|trg|rx|tx|time|src|trg");
        } else {
            l = new userFormat("|", "dir|prt|src|src|trg|trg|rx|tx|time");
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
        tabGen<tabSessionEndpoint<T>> ept = getTopTalker();
        userFormat l = new userFormat("|", "addr|rx|tx|time");
        for (int i = 0; i < ept.size(); i++) {
            l.add(ept.get(i).dump());
        }
        return l;
    }

    private tabGen<tabSessionEndpoint<T>> getTopTalker() {
        tabGen<tabSessionEndpoint<T>> ept = new tabGen<tabSessionEndpoint<T>>();
        for (int i = 0; i < connects.size(); i++) {
            tabSessionEntry<T> ntry = connects.get(i);
            updateTalker(ept, ntry.srcAdr, ntry.rxByte, ntry.txByte, ntry.startTime);
            updateTalker(ept, ntry.trgAdr, ntry.rxByte, ntry.txByte, ntry.startTime);
        }
        return ept;
    }

    @SuppressWarnings("unchecked")
    private void updateTalker(tabGen<tabSessionEndpoint<T>> ept, T adr, long rx, long tx, long tim) {
        tabSessionEndpoint<T> ntry = new tabSessionEndpoint<T>();
        ntry.adr = (T) adr.copyBytes();
        tabSessionEndpoint<T> res = ept.find(ntry);
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

    public void run() {
        for (;;) {
            if (!need2run) {
                return;
            }
            bits.sleep(30000);
            try {
                long tim = bits.getTime();
                for (int i = connects.size() - 1; i >= 0; i--) {
                    tabSessionEntry<T> cur = connects.get(i);
                    if ((tim - cur.lastTime) < timeout) {
                        continue;
                    }
                    connects.del(cur);
                    if (!logAfter) {
                        continue;
                    }
                    logger.info("finished " + cur);
                }
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
    }

}

class tabSessionEndpoint<T extends addrType> implements
        Comparator<tabSessionEndpoint<T>> {

    public T adr;

    public long rx;

    public long tx;

    public long tim;

    public String dump() {
        return adr + "|" + rx + "|" + tx + "|" + bits.timePast(tim);
    }

    public int compare(tabSessionEndpoint<T> o1, tabSessionEndpoint<T> o2) {
        return o1.adr.compare(o1.adr, o2.adr);
    }

}
