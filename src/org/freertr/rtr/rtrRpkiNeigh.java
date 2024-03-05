package org.freertr.rtr;

import java.util.Comparator;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgIfc;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipFwdIface;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabRoautNtry;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * resource public key infrastructure
 *
 * @author matecsaba
 */
public class rtrRpkiNeigh implements Comparator<rtrRpkiNeigh>, Runnable {

    /**
     * parent to bind to
     */
    public final rtrRpki lower;

    /**
     * accepted ipv4 prefixes
     */
    public tabGen<tabRoautNtry> table4 = new tabGen<tabRoautNtry>();

    /**
     * accepted ipv6 prefixes
     */
    public tabGen<tabRoautNtry> table6 = new tabGen<tabRoautNtry>();

    /**
     * counter to use
     */
    public counter cntr = new counter();

    /**
     * server to use
     */
    public final addrIP peer;

    /**
     * port to use
     */
    public int port;

    /**
     * time started
     */
    public long upTime;

    /**
     * peer description
     */
    public String description;

    /**
     * source interface
     */
    public cfgIfc srcIface;

    /**
     * query time
     */
    public int queryTimer = 30000;

    /**
     * query time
     */
    public int flushTimer = 120000;

    /**
     * preference
     */
    public int preference = 100;

    private pipeSide pipe;

    private boolean need2run;

    private int serial;

    private int session;

    /**
     * create new instance
     *
     * @param parent process
     * @param addr address of peer
     */
    public rtrRpkiNeigh(rtrRpki parent, addrIP addr) {
        lower = parent;
        peer = addr;
    }

    public String toString() {
        return "" + peer;
    }

    public int compare(rtrRpkiNeigh o1, rtrRpkiNeigh o2) {
        return o1.peer.compare(o1.peer, o2.peer);
    }

    /**
     * stop this peer
     */
    protected void stopNow() {
        if (debugger.rtrRpkiEvnt) {
            logger.debug("stopping " + peer);
        }
        need2run = false;
        if (pipe != null) {
            pipe.setClose();
        }
    }

    /**
     * flap this peer
     */
    public void flapNow() {
        if (debugger.rtrRpkiEvnt) {
            logger.debug("flapping " + peer);
        }
        if (pipe != null) {
            pipe.setClose();
        }
    }

    /**
     * start this peer
     */
    protected void startNow() {
        if (need2run) {
            return;
        }
        if (debugger.rtrRpkiEvnt) {
            logger.debug("starting " + peer);
        }
        need2run = true;
        new Thread(this).start();
    }

    /**
     * get configuration
     *
     * @param l list to append
     * @param beg beginning
     */
    public void getConfig(List<String> l, String beg) {
        String s = beg + "neighbor " + peer;
        l.add(s + " port " + port);
        String a = null;
        if (srcIface != null) {
            a = srcIface.name;
        }
        cmds.cfgLine(l, a == null, beg, "neighbor " + peer + " update-source", a);
        cmds.cfgLine(l, need2run, beg, "neighbor " + peer + " shutdown", "");
        cmds.cfgLine(l, description == null, beg, "neighbor " + peer + " description", "" + description);
        l.add(s + " preference " + preference);
        l.add(s + " timers " + queryTimer + " " + flushTimer);
    }

    public void run() {
        try {
            for (;;) {
                doWork();
                if (!need2run) {
                    break;
                }
                bits.sleep(1000);
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

    private void doWork() {
        if (!need2run) {
            return;
        }
        ipFwdIface ifc = null;
        if (srcIface != null) {
            ifc = srcIface.getFwdIfc(peer);
        }
        pipe = lower.tcpCore.streamConnect(new pipeLine(65536, false), ifc, 0, peer, port, "rpki", -1, null, -1, -1);
        if (pipe == null) {
            return;
        }
        pipe.setTime(flushTimer);
        rtrRpkiSpeak pck = new rtrRpkiSpeak(new packHolder(true, true), pipe, cntr);
        pck.typ = rtrRpkiSpeak.msgResetQuery;
        pck.sendPack();
        table4.clear();
        table6.clear();
        logger.warn("neighbor " + peer + " up");
        upTime = bits.getTime();
        for (;;) {
            int i = doOneClntRnd(pck);
            if (i == 3) {
                break;
            }
            if (i >= 0) {
                continue;
            }
            table4.clear();
            table6.clear();
            logger.error("neighbor " + peer + " down");
            pipe.setClose();
            pipe = null;
            return;
        }
        lower.compute.wakeup();
        long last = bits.getTime();
        for (;;) {
            if (pipe.isClosed() != 0) {
                break;
            }
            if (!need2run) {
                break;
            }
            bits.sleep(1000);
            long tim = bits.getTime();
            if ((tim - last) > queryTimer) {
                pck.serial = serial;
                pck.sess = session;
                pck.typ = rtrRpkiSpeak.msgSerialQuery;
                pck.sendPack();
                last = tim;
            }
            int cntr = 0;
            for (;;) {
                if (pipe.ready2rx() < 1) {
                    break;
                }
                int i = doOneClntRnd(pck);
                if (i == 0) {
                    continue;
                }
                cntr++;
                if (i != 1) {
                    break;
                }
            }
            if (cntr < 1) {
                continue;
            }
            lower.compute.wakeup();
        }
        table4.clear();
        table6.clear();
        lower.compute.wakeup();
        logger.error("neighbor " + peer + " down");
        pipe.setClose();
        pipe = null;
    }

    private int processOneRoa(rtrRpkiSpeak pck, tabGen<tabRoautNtry> table) {
        tabRoautNtry ntry = pck.roa;
        ntry.distan = preference;
        ntry.srcRtr = lower.rouTyp;
        ntry.srcNum = lower.rtrNum;
        ntry.srcIP = peer.copyBytes();
        if (!pck.withdraw) {
            ntry.time = bits.getTime();
            table.put(ntry);
            return 1;
        }
        tabRoautNtry old = table.del(ntry);
        if (old == null) {
            return 0;
        }
        return 1;
    }

    private int doOneClntRnd(rtrRpkiSpeak pck) {
        if (pck.recvPack()) {
            pipe.setClose();
            return -1;
        }
        switch (pck.typ) {
            case rtrRpkiSpeak.msgIpv4addr:
                return processOneRoa(pck, table4);
            case rtrRpkiSpeak.msgIpv6addr:
                return processOneRoa(pck, table6);
            case rtrRpkiSpeak.msgCacheReply:
                return 0;
            case rtrRpkiSpeak.msgCacheReset:
                table4.clear();
                table6.clear();
                pck.typ = rtrRpkiSpeak.msgResetQuery;
                pck.sendPack();
                return 2;
            case rtrRpkiSpeak.msgEndData:
                session = pck.sess;
                serial = pck.serial;
                if (debugger.rtrRpkiEvnt) {
                    logger.info("neighbor " + peer + " done " + table4.size() + " " + table6.size());
                }
                return 3;
            default:
                return 0;
        }
    }

    /**
     * get final table
     *
     * @param ipVer ip version
     * @return current table
     */
    public tabGen<tabRoautNtry> getFinalTab(int ipVer) {
        if (ipVer == ipCor4.protocolVersion) {
            return table4;
        } else {
            return table6;
        }
    }

}
