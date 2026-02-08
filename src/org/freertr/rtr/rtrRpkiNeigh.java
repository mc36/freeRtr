package org.freertr.rtr;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.cfg.cfgIfc;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipFwdIface;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabRpkiAspa;
import org.freertr.tab.tabRpkiKey;
import org.freertr.tab.tabRpkiRoa;
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
public class rtrRpkiNeigh implements Comparable<rtrRpkiNeigh>, Runnable {

    /**
     * parent to bind to
     */
    public final rtrRpki lower;

    /**
     * accepted ipv4 prefixes
     */
    public tabGen<tabRpkiRoa> table4 = new tabGen<tabRpkiRoa>();

    /**
     * accepted ipv6 prefixes
     */
    public tabGen<tabRpkiRoa> table6 = new tabGen<tabRpkiRoa>();

    /**
     * accepted key entries
     */
    public tabGen<tabRpkiKey> tableK = new tabGen<tabRpkiKey>();

    /**
     * accepted aspa entries
     */
    public tabGen<tabRpkiAspa> tableA = new tabGen<tabRpkiAspa>();

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

    public int compareTo(rtrRpkiNeigh o) {
        return peer.compareTo(o.peer);
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
        logger.startThread(this);
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
        if (pipe.wait4ready(flushTimer)) {
            return;
        }
        rtrRpkiSpeak pck = new rtrRpkiSpeak(new packHolder(true, true), pipe, cntr);
        pck.typ = rtrRpkiSpeak.msgResetQuery;
        pck.sendPack();
        table4.clear();
        table6.clear();
        tableA.clear();
        tableK.clear();
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
            tableA.clear();
            tableK.clear();
            pipe.setClose();
            break;
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
            if (pipe.ready2rx() < 1) {
                continue;
            }
            int cntr = 0;
            for (;;) {
                int i = doOneClntRnd(pck);
                if (i == 0) {
                    continue;
                }
                if (i != 1) {
                    break;
                }
                cntr++;
            }
            if (cntr < 1) {
                continue;
            }
            lower.compute.wakeup();
        }
        table4.clear();
        table6.clear();
        tableA.clear();
        tableK.clear();
        lower.compute.wakeup();
        logger.error("neighbor " + peer + " down");
        pipe.setClose();
        pipe = null;
    }

    private int processOneKey(rtrRpkiSpeak pck, tabGen<tabRpkiKey> table) {
        tabRpkiKey ntry = table.find(pck.key);
        if (pck.withdraw) {
            if (ntry == null) {
                return 0;
            }
            table.del(pck.key);
            return 1;
        }
        ntry = pck.key;
        ntry.time = bits.getTime();
        ntry.distan = preference;
        ntry.srcRtr = lower.rouTyp;
        ntry.srcNum = lower.rtrNum;
        ntry.srcIP = peer.copyBytes();
        table.put(ntry);
        return 1;
    }

    private int processOneAspa(rtrRpkiSpeak pck, tabGen<tabRpkiAspa> table) {
        tabRpkiAspa ntry = table.find(pck.aspa);
        if (pck.aspa.provs.size() < 1) {
            return 0;
        }
        if (pck.withdraw) {
            if (ntry == null) {
                return 0;
            }
            for (int i = 0; i < pck.aspa.provs.size(); i++) {
                int o = pck.aspa.provs.get(i);
                int p = ntry.provs.indexOf(o);
                if (p < 0) {
                    continue;
                }
                ntry.provs.remove(p);
            }
            Collections.sort(ntry.provs);
            if (ntry.provs.size() > 0) {
                return 1;
            }
            table.del(ntry);
            return 1;
        }
        if (ntry != null) {
            for (int i = 0; i < pck.aspa.provs.size(); i++) {
                int o = pck.aspa.provs.get(i);
                int p = ntry.provs.indexOf(o);
                if (p >= 0) {
                    continue;
                }
                ntry.provs.add(o);
            }
            Collections.sort(ntry.provs);
            return 1;
        }
        ntry = pck.aspa;
        ntry.time = bits.getTime();
        ntry.distan = preference;
        ntry.srcRtr = lower.rouTyp;
        ntry.srcNum = lower.rtrNum;
        ntry.srcIP = peer.copyBytes();
        Collections.sort(ntry.provs);
        table.put(ntry);
        return 1;
    }

    private int processOneRoa(rtrRpkiSpeak pck, tabGen<tabRpkiRoa> table) {
        tabRpkiRoa ntry = table.find(pck.roa);
        if (pck.withdraw) {
            if (ntry == null) {
                return 0;
            }
            int i = ntry.asns.indexOf(pck.roa.distan);
            if (i < 0) {
                return 1;
            }
            ntry.asns.remove(i);
            Collections.sort(ntry.asns);
            if (ntry.asns.size() > 0) {
                return 1;
            }
            table.del(ntry);
            return 1;
        }
        if (ntry != null) {
            if (pck.roa.max > ntry.max) {
                ntry.max = pck.roa.max;
            }
            if (ntry.asns.indexOf(pck.roa.distan) >= 0) {
                return 1;
            }
            ntry.asns.add(pck.roa.distan);
            Collections.sort(ntry.asns);
            return 1;
        }
        ntry = pck.roa;
        ntry.asns = new ArrayList<Integer>();
        ntry.asns.add(pck.roa.distan);
        Collections.sort(ntry.asns);
        ntry.time = bits.getTime();
        ntry.distan = preference;
        ntry.srcRtr = lower.rouTyp;
        ntry.srcNum = lower.rtrNum;
        ntry.srcIP = peer.copyBytes();
        table.put(ntry);
        return 1;
    }

    private int doOneClntRnd(rtrRpkiSpeak pck) {
        if (pck.recvPack()) {
            pipe.setClose();
            return -1;
        }
        if (debugger.rtrRpkiTraf) {
            logger.debug("rx " + pck.dump());
        }
        switch (pck.typ) {
            case rtrRpkiSpeak.msgIpv4addr:
                return processOneRoa(pck, table4);
            case rtrRpkiSpeak.msgIpv6addr:
                return processOneRoa(pck, table6);
            case rtrRpkiSpeak.msgRouterKey:
                return processOneKey(pck, tableK);
            case rtrRpkiSpeak.msgAspaPdu:
                return processOneAspa(pck, tableA);
            case rtrRpkiSpeak.msgCacheReply:
                return 0;
            case rtrRpkiSpeak.msgCacheReset:
                table4.clear();
                table6.clear();
                tableA.clear();
                tableK.clear();
                pck.typ = rtrRpkiSpeak.msgResetQuery;
                pck.sendPack();
                return 2;
            case rtrRpkiSpeak.msgEndData:
                session = pck.sess;
                serial = pck.serial;
                if (debugger.rtrRpkiEvnt) {
                    logger.debug("neighbor " + peer + " done " + table4.size() + " " + table6.size() + " " + tableK.size() + " " + tableA.size());
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
    public tabGen<tabRpkiRoa> getFinalTabRoa(int ipVer) {
        if (ipVer == ipCor4.protocolVersion) {
            return table4;
        } else {
            return table6;
        }
    }

    /**
     * get final table
     *
     * @return current table
     */
    public tabGen<tabRpkiAspa> getFinalTabAspa() {
        return tableA;
    }

    /**
     * get final table
     *
     * @return current table
     */
    public tabGen<tabRpkiKey> getFinalTabKey() {
        return tableK;
    }

}
