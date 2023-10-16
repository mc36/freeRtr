package net.freertr.rtr;

import java.util.Comparator;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgIfc;
import net.freertr.ip.ipFwdIface;
import net.freertr.pack.packHolder;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteEntry;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logger;

/**
 * resource public key infrastructure
 *
 * @author matecsaba
 */
public class rtrRpkiNeigh implements Comparator<rtrRpkiNeigh>, Runnable {

    /**
     * server to use
     */
    public String server;

    /**
     * server to use
     */
    public addrIP peer = new addrIP();

    /**
     * port to use
     */
    public int port;

    /**
     * accepted ipv4 prefixes
     */
    public tabRoute<addrIP> table4 = new tabRoute<addrIP>("rx");

    /**
     * accepted ipv6 prefixes
     */
    public tabRoute<addrIP> table6 = new tabRoute<addrIP>("rx");

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

    private final rtrRpki lower;

    private pipeSide pipe;

    private boolean need2run;

    private int serial;

    private int session;

    /**
     * create new instance
     *
     * @param parent process
     */
    public rtrRpkiNeigh(rtrRpki parent) {
        lower = parent;
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
        cmds.cfgLine(l, srcIface == null, beg, "neighbor " + peer + " update-source", "" + srcIface);
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
        rtrRpkiSpeak pck = new rtrRpkiSpeak(new packHolder(true, true), pipe);
        pck.typ = rtrRpkiSpeak.msgResetQuery;
        pck.sendPack();
        if (debugger.rtrRpkiTraf) {
            logger.debug("tx " + pck.dump());
        }
        table4.clear();
        table6.clear();
        logger.warn("neighbor " + peer + " up");
        for (;;) {
            int i = doOneRound(pck);
            if (i == 3) {
                break;
            }
            if (i >= 0) {
                continue;
            }
            table4.clear();
            table6.clear();
            pipe.setClose();
            return;
        }
        lower.compute.wakeup();
        upTime = bits.getTime();
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
                if (debugger.rtrRpkiTraf) {
                    logger.debug("tx " + pck.dump());
                }
                last = tim;
            }
            if (debugger.rtrRpkiTraf) {
                logger.debug("tx " + pck.dump());
            }
            int chngCntr = 0;
            for (;;) {
                if (pipe.ready2rx() < 1) {
                    break;
                }
                if (doOneRound(pck) != 1) {
                    break;
                }
            }
            if (chngCntr > 0) {
                lower.compute.wakeup();
            }
            chngCntr = 0;
        }
        upTime = bits.getTime();
        table4.clear();
        table6.clear();
        lower.compute.wakeup();
        logger.error("neighbor " + peer + " down");
        pipe.setClose();
        pipe = null;
    }

    private int doOneRound(rtrRpkiSpeak pck) {
        if (pck.recvPack()) {
            pipe.setClose();
            return -1;
        }
        if (debugger.rtrRpkiTraf) {
            logger.debug("rx " + pck.dump());
        }
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        switch (pck.typ) {
            case rtrRpkiSpeak.msgIpv4addr:
                ntry.prefix = pck.pref;
                ntry.best.metric = pck.max;
                ntry.best.rouSrc = pck.as;
                ntry.best.locPref = preference;
                ntry.best.rouTyp = lower.rouTyp;
                ntry.best.protoNum = lower.rtrNum;
                ntry.best.aggrRtr = peer;
                if (pck.withdraw) {
                    table4.del(ntry);
                } else {
                    table4.add(tabRoute.addType.always, ntry, true, true);
                }
                return 1;
            case rtrRpkiSpeak.msgIpv6addr:
                ntry.prefix = pck.pref;
                ntry.best.metric = pck.max;
                ntry.best.rouSrc = pck.as;
                ntry.best.locPref = preference;
                ntry.best.rouTyp = lower.rouTyp;
                ntry.best.protoNum = lower.rtrNum;
                ntry.best.aggrRtr = peer;
                if (pck.withdraw) {
                    table6.del(ntry);
                } else {
                    table6.add(tabRoute.addType.always, ntry, true, true);
                }
                return 1;
            case rtrRpkiSpeak.msgCacheReply:
                return 0;
            case rtrRpkiSpeak.msgCacheReset:
                table4.clear();
                table6.clear();
                pck.typ = rtrRpkiSpeak.msgResetQuery;
                pck.sendPack();
                if (debugger.rtrRpkiTraf) {
                    logger.debug("tx " + pck.dump());
                }
                return 2;
            case rtrRpkiSpeak.msgEndData:
                session = pck.sess;
                serial = pck.serial;
                logger.info("neighbor " + peer + " done " + table4.size() + " " + table6.size());
                return 3;
            default:
                return 0;
        }
    }

}
