package org.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgRtr;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipFwdTab;
import org.freertr.ip.ipRtr;
import org.freertr.prt.prtTcp;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.tab.tabRpkiRoa;
import org.freertr.tab.tabRpkiUtil;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRpkiAspa;
import org.freertr.tab.tabRpkiKey;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.notifier;

/**
 * resource public key infrastructure (rfc6810) protocol
 *
 * @author matecsaba
 */
public class rtrRpki extends ipRtr implements Runnable {

    /**
     * the forwarder protocol
     */
    public final ipFwd fwdCore;

    /**
     * the tcp protocol
     */
    protected final prtTcp tcpCore;

    /**
     * route type
     */
    protected final tabRouteAttr.routeType rouTyp;

    /**
     * router number
     */
    protected final int rtrNum;

    /**
     * scan time interval
     */
    protected int scanTime = 1000;

    /**
     * list of neighbors
     */
    protected tabGen<rtrRpkiNeigh> neighs = new tabGen<rtrRpkiNeigh>();

    /**
     * list of notifiers
     */
    protected tabGen<rtrRpkiWake> wakes = new tabGen<rtrRpkiWake>();

    /**
     * accepted ipv4 roas
     */
    private tabGen<tabRpkiRoa> computed4 = new tabGen<tabRpkiRoa>();

    /**
     * accepted ipv6 roas
     */
    private tabGen<tabRpkiRoa> computed6 = new tabGen<tabRpkiRoa>();

    /**
     * accepted aspas
     */
    private tabGen<tabRpkiAspa> computedA = new tabGen<tabRpkiAspa>();

    /**
     * accepted keys
     */
    private tabGen<tabRpkiKey> computedK = new tabGen<tabRpkiKey>();

    /**
     * sequence number
     */
    private int seqNum;

    /**
     * sequence time
     */
    private long seqTim;

    /**
     * sequence notified
     */
    private int seqNot;

    /**
     * sequence changed
     */
    private long seqChg;

    /**
     * notified to wake up
     */
    protected final notifier compute = new notifier();

    private boolean need2run;

    /**
     * create instance
     *
     * @param forwarder forwarder to use
     * @param protocol protocol to use
     * @param id process id
     */
    public rtrRpki(ipFwd forwarder, prtTcp protocol, int id) {
        if (debugger.rtrRpkiEvnt) {
            logger.debug("startup");
        }
        fwdCore = forwarder;
        tcpCore = protocol;
        rtrNum = id;
        switch (fwdCore.ipVersion) {
            case ipCor4.protocolVersion:
                rouTyp = tabRouteAttr.routeType.rpki4;
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteAttr.routeType.rpki6;
                break;
            default:
                rouTyp = null;
                break;
        }
        seqNum = 0;
        routerCreateComputed();
        need2run = true;
        logger.startThread(this);
        fwdCore.routerAdd(this, rouTyp, id);
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "rpki on " + fwdCore;
    }

    /**
     * get neighbor count
     *
     * @return count
     */
    public int routerNeighCount() {
        return neighs.size();
    }

    /**
     * list neighbors
     *
     * @param tab list
     */
    public void routerNeighList(tabRoute<addrIP> tab) {
        for (int i = 0; i < neighs.size(); i++) {
            rtrRpkiNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = new addrPrefix<addrIP>(nei.peer, addrIP.size * 8);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, tab, rtrBgpUtil.sfiUnicast, 0, ntry, true, null, null, routerAutoMesh);
        }
    }

    /**
     * get interface count
     *
     * @return count
     */
    public int routerIfaceCount() {
        return 0;
    }

    /**
     * maximum recursion depth
     *
     * @return allowed number
     */
    public int routerRecursions() {
        return 1;
    }

    /**
     * get list of link states
     *
     * @param tab table to update
     * @param par parameter
     * @param asn asn
     * @param adv advertiser
     */
    public void routerLinkStates(tabRoute<addrIP> tab, int par, int asn, addrIPv4 adv) {
    }

    /**
     * get state information
     *
     * @param lst list to append
     */
    public void routerStateGet(List<String> lst) {
    }

    /**
     * set state information
     *
     * @param cmd string to append
     * @return true on error, false on success
     */
    public boolean routerStateSet(cmds cmd) {
        return true;
    }

    private int getIface(addrIP adr) {
        ipFwdIface ifc = ipFwdTab.findSendingIface(fwdCore, adr);
        if (ifc == null) {
            return 0;
        }
        return ifc.ifwNum;
    }

    /**
     * create computed
     */
    public synchronized void routerCreateComputed() {
        seqNum++;
        seqTim = bits.getTime();
        tabGen<tabRpkiRoa> tab4 = new tabGen<tabRpkiRoa>();
        tabGen<tabRpkiRoa> tab6 = new tabGen<tabRpkiRoa>();
        tabGen<tabRpkiAspa> tabA = new tabGen<tabRpkiAspa>();
        tabGen<tabRpkiKey> tabK = new tabGen<tabRpkiKey>();
        for (int i = 0; i < neighs.size(); i++) {
            rtrRpkiNeigh ntry = neighs.get(i);
            tabRpkiUtil.mergeTwoRoa(tab4, ntry.table4);
            tabRpkiUtil.mergeTwoRoa(tab6, ntry.table6);
            tabRpkiUtil.mergeTwoAspa(tabA, ntry.tableA);
            tabRpkiUtil.mergeTwoKey(tabK, ntry.tableK);
        }
        boolean chg = tabRpkiUtil.compareTwoRoa(tab4, computed4);
        chg &= tabRpkiUtil.compareTwoRoa(tab6, computed6);
        chg &= tabRpkiUtil.compareTwoAspa(tabA, computedA);
        chg &= tabRpkiUtil.compareTwoKey(tabK, computedK);
        if (chg) {
            return;
        }
        seqNot++;
        seqChg = seqTim;
        computed4 = tab4;
        computed6 = tab6;
        computedA = tabA;
        computedK = tabK;
        if (debugger.rtrRpkiEvnt) {
            logger.debug("rpki changed");
        }
        for (int i = 0; i < wakes.size(); i++) {
            rtrRpkiWake w = wakes.get(i);
            if (w == null) {
                continue;
            }
            cfgRtr rtrC = cfgAll.rtrFind(w.remT, w.remN, false);
            if (rtrC == null) {
                continue;
            }
            ipRtr rtrI = rtrC.getRouter();
            if (rtrI == null) {
                continue;
            }
            rtrI.routerRedistChanged();
        }
        routerComputedU = new tabRoute<addrIP>("rx");
        routerComputedM = new tabRoute<addrIP>("rx");
        routerComputedF = new tabRoute<addrIP>("rx");
        routerComputedI = new tabGen<tabIndex<addrIP>>();
    }

    /**
     * redistribution changed
     */
    public void routerRedistChanged() {
    }

    /**
     * others changed
     */
    public void routerOthersChanged() {
    }

    public void routerGetHelp(userHelp l) {
        List<String> neis = new ArrayList<String>();
        for (int i = 0; i < neighs.size(); i++) {
            rtrRpkiNeigh ntry = neighs.get(i);
            neis.add("" + ntry.peer);
        }
        l.add(null, false, 1, new int[]{2}, "neighbor", "specify neighbor parameters");
        l.add(neis, false, 2, new int[]{3}, "<addr:loc>", "address of peer");
        l.add(null, false, 3, new int[]{4}, "port", "set target port");
        l.add(null, false, 4, new int[]{-1}, "<num>", "value");
        l.add(null, false, 3, new int[]{4}, "description", "describe this neighbor");
        l.add(null, false, 4, new int[]{4, -1}, "<text>", "description of neighbor");
        l.add(null, false, 3, new int[]{4}, "update-source", "connection source for this peer");
        l.add(null, false, 4, new int[]{-1}, "<name:ifc>", "name of interface");
        l.add(null, false, 3, new int[]{4}, "preference", "set preference");
        l.add(null, false, 4, new int[]{-1}, "<num>", "value");
        l.add(null, false, 3, new int[]{4}, "timer", "neighbor keepalive times");
        l.add(null, false, 4, new int[]{5}, "<num>", "query time in ms");
        l.add(null, false, 5, new int[]{-1}, "<num>", "flush time in ms");
        l.add(null, false, 1, new int[]{2}, "scantime", "scan time interval");
        l.add(null, false, 2, new int[]{-1}, "<num>", "ms between scans");
        l.add(null, false, 1, new int[]{2}, "wakeup", "notify other process on changes");
        cfgRtr.getRouterList(l, 0, "");
        l.add(null, false, 3, new int[]{-1}, "<num>", "process number");
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void routerGetConfig(List<String> l, String beg, int filter) {
        for (int i = 0; i < neighs.size(); i++) {
            rtrRpkiNeigh ntry = neighs.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.getConfig(l, beg);
        }
        for (int i = 0; i < wakes.size(); i++) {
            rtrRpkiWake w = wakes.get(i);
            if (w == null) {
                continue;
            }
            l.add(beg + "wakeup " + cfgRtr.num2name(w.remT) + " " + w.remN);
        }
        l.add(beg + "scantime " + scanTime);
    }

    /**
     * configure
     *
     * @param cmd command
     * @return false if success, true if error
     */
    public boolean routerConfigure(cmds cmd) {
        String s = cmd.word();
        boolean negated = false;
        if (s.equals(cmds.negated)) {
            s = cmd.word();
            negated = true;
        }
        if (s.equals("wakeup")) {
            tabRouteAttr.routeType t = cfgRtr.name2num(cmd.word());
            int n = bits.str2num(cmd.word());
            rtrRpkiWake w = new rtrRpkiWake(t, n);
            if (negated) {
                wakes.del(w);
            } else {
                wakes.add(w);
            }
            return false;
        }
        if (s.equals("scantime")) {
            scanTime = bits.str2num(cmd.word());
            if (!negated) {
                return false;
            }
            scanTime = 1000;
            return false;
        }
        if (!s.equals("neighbor")) {
            return true;
        }
        s = cmd.word().trim();
        addrIP addr = cfgRtr.string2addr(routerProtoTyp, s, null);
        if (addr == null) {
            cmd.error("bad address");
            return false;
        }
        rtrRpkiNeigh ntry = new rtrRpkiNeigh(this, addr);
        s = cmd.word();
        if (s.equals("port")) {
            if (negated) {
                ntry = neighs.del(ntry);
                if (ntry == null) {
                    return false;
                }
                ntry.stopNow();
                return false;
            }
            if (neighs.add(ntry) != null) {
                return false;
            }
            ntry.port = bits.str2num(cmd.word());
            ntry.startNow();
            return false;
        }
        ntry = neighs.find(ntry);
        if (ntry == null) {
            cmd.error("no such neighbor");
            return false;
        }
        if (s.equals("update-source")) {
            if (negated) {
                ntry.srcIface = null;
                return false;
            }
            cfgIfc res = cfgAll.ifcFind(cmd.word(), 0);
            if (res == null) {
                cmd.error("no such interface");
                return false;
            }
            if (res.vrfFor == null) {
                cmd.error("not in vrf");
                return false;
            }
            if (res.vrfFor.getFwd(ntry.peer) != fwdCore) {
                cmd.error("in other vrf");
                return false;
            }
            ntry.srcIface = res;
            return false;
        }
        if (s.equals("description")) {
            if (negated) {
                ntry.description = null;
                return false;
            }
            ntry.description = cmd.getRemaining();
            return false;
        }
        if (s.equals("timer")) {
            ntry.queryTimer = bits.str2num(cmd.word());
            ntry.flushTimer = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("shutdown")) {
            if (!negated) {
                ntry.stopNow();
            } else {
                ntry.startNow();
            }
            return false;
        }
        return true;
    }

    /**
     * stop work
     */
    public void routerCloseNow() {
        need2run = false;
        for (int i = 0; i < neighs.size(); i++) {
            rtrRpkiNeigh ntry = neighs.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.stopNow();
        }
        fwdCore.routerDel(this);
    }

    /**
     * get neighbor show
     *
     * @return list of neighbors
     */
    public userFormat getNeighShow() {
        userFormat l = new userFormat("|", "address|ipv4|ipv6|key|aspa|uptime");
        for (int i = 0; i < neighs.size(); i++) {
            rtrRpkiNeigh ntry = neighs.get(i);
            if (ntry == null) {
                continue;
            }
            l.add(ntry.peer + "|" + ntry.table4.size() + "|" + ntry.table6.size() + "|" + ntry.tableK.size() + "|" + ntry.tableA.size() + "|" + bits.timePast(ntry.upTime));
        }
        return l;
    }

    /**
     * get neighbor show
     *
     * @return list of neighbors
     */
    public userFormat getGenShow() {
        userFormat l = new userFormat("|", "category|value|additional");
        l.add("peers|" + neighs.size());
        l.add("ipv4 roas|" + computed4.size());
        l.add("ipv6 roas|" + computed6.size());
        l.add("keys|" + computedK.size());
        l.add("aspas|" + computedA.size());
        l.add("sequence event|" + seqNum + "|times");
        l.add("sequence time|" + bits.timePast(seqTim) + "|" + bits.time2str(cfgAll.timeZoneName, seqTim + cfgAll.timeServerOffset, 3));
        l.add("wakeup event|" + seqNot + "|times");
        l.add("sequence time|" + bits.timePast(seqChg) + "|" + bits.time2str(cfgAll.timeZoneName, seqChg + cfgAll.timeServerOffset, 3));
        return l;
    }

    /**
     * get neighbor show
     *
     * @return list of neighbors
     */
    public userFormat showConnSumm() {
        userFormat l = new userFormat("|", "neighbor|rx|tx|rx|tx", "1|2pack|2byte");
        for (int i = 0; i < neighs.size(); i++) {
            rtrRpkiNeigh ntry = neighs.get(i);
            if (ntry == null) {
                continue;
            }
            l.add(ntry.peer + "|" + ntry.cntr.packRx + "|" + ntry.cntr.packTx + "|" + ntry.cntr.byteRx + "|" + ntry.cntr.byteTx);
        }
        return l;
    }

    /**
     * find neighbor
     *
     * @param adr address
     * @return neighbor, null if not found
     */
    public rtrRpkiNeigh findPeer(addrIP adr) {
        rtrRpkiNeigh nei = new rtrRpkiNeigh(this, adr);
        return neighs.find(nei);
    }

    public void run() {
        for (;;) {
            if (compute.misleep(0) > 0) {
                bits.sleep(scanTime);
            }
            if (!need2run) {
                break;
            }
            try {
                routerCreateComputed();
            } catch (Exception e) {
                logger.traceback(e);
            }
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
            return computed4;
        } else {
            return computed6;
        }
    }

    /**
     * get final table
     *
     * @return current table
     */
    public tabGen<tabRpkiKey> getFinalTabKey() {
        return computedK;
    }

    /**
     * get final table
     *
     * @return current table
     */
    public tabGen<tabRpkiAspa> getFinalTabAspa() {
        return computedA;
    }

    /**
     * get sequence number
     *
     * @return sequence number
     */
    public int getSeqNum() {
        return seqNum;
    }

}
