package rtr;

import addr.addrIP;
import cfg.cfgAll;
import clnt.clntDns;
import ip.ipFwdIface;
import ip.ipFwdTab;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import pack.packDnsRec;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtAccept;
import tab.tabPlcmapN;
import tab.tabPrfxlstN;
import tab.tabRoute;
import tab.tabRouteEntry;
import tab.tabRtrmapN;
import tab.tabRtrplc;
import util.bits;
import util.debugger;
import util.logger;
import util.notifier;

/**
 * bgp4 neighbor
 *
 * @author matecsaba
 */
public class rtrBgpNeigh extends rtrBgpParam implements Comparator<rtrBgpNeigh>, Runnable {

    /**
     * address of peer
     */
    public addrIP peerAddr;

    /**
     * local interface
     */
    public ipFwdIface localIfc = null;

    /**
     * sending interface
     */
    public ipFwdIface sendingIfc = null;

    /**
     * local address
     */
    public addrIP localAddr = new addrIP();

    /**
     * accepted unicast prefixes
     */
    public tabRoute<addrIP> accUni = new tabRoute<addrIP>("rx");

    /**
     * accepted multicast prefixes
     */
    public tabRoute<addrIP> accMlt = new tabRoute<addrIP>("rx");

    /**
     * accepted other prefixes
     */
    public tabRoute<addrIP> accOtr = new tabRoute<addrIP>("rx");

    /**
     * accepted flowspec prefixes
     */
    public tabRoute<addrIP> accFlw = new tabRoute<addrIP>("rx");

    /**
     * accepted vpnuni prefixes
     */
    public tabRoute<addrIP> accVpnU = new tabRoute<addrIP>("rx");

    /**
     * accepted vpnmulti prefixes
     */
    public tabRoute<addrIP> accVpnM = new tabRoute<addrIP>("rx");

    /**
     * accepted vpnflow prefixes
     */
    public tabRoute<addrIP> accVpnF = new tabRoute<addrIP>("rx");

    /**
     * accepted other vpnuni prefixes
     */
    public tabRoute<addrIP> accVpoU = new tabRoute<addrIP>("rx");

    /**
     * accepted other vpnmulti prefixes
     */
    public tabRoute<addrIP> accVpoM = new tabRoute<addrIP>("rx");

    /**
     * accepted other vpnflow prefixes
     */
    public tabRoute<addrIP> accVpoF = new tabRoute<addrIP>("rx");

    /**
     * accepted vpls prefixes
     */
    public tabRoute<addrIP> accVpls = new tabRoute<addrIP>("rx");

    /**
     * accepted mspw prefixes
     */
    public tabRoute<addrIP> accMspw = new tabRoute<addrIP>("rx");

    /**
     * accepted evpn prefixes
     */
    public tabRoute<addrIP> accEvpn = new tabRoute<addrIP>("rx");

    /**
     * accepted mdt prefixes
     */
    public tabRoute<addrIP> accMdt = new tabRoute<addrIP>("rx");

    /**
     * accepted srte prefixes
     */
    public tabRoute<addrIP> accSrte = new tabRoute<addrIP>("rx");

    /**
     * accepted mvpn prefixes
     */
    public tabRoute<addrIP> accMvpn = new tabRoute<addrIP>("rx");

    /**
     * accepted other mvpn prefixes
     */
    public tabRoute<addrIP> accMvpo = new tabRoute<addrIP>("rx");

    /**
     * willing unicast prefixes
     */
    public tabRoute<addrIP> wilUni = new tabRoute<addrIP>("tx");

    /**
     * willing multicast prefixes
     */
    public tabRoute<addrIP> wilMlt = new tabRoute<addrIP>("tx");

    /**
     * willing other prefixes
     */
    public tabRoute<addrIP> wilOtr = new tabRoute<addrIP>("tx");

    /**
     * willing flowspec prefixes
     */
    public tabRoute<addrIP> wilFlw = new tabRoute<addrIP>("tx");

    /**
     * willing vpnuni prefixes
     */
    public tabRoute<addrIP> wilVpnU = new tabRoute<addrIP>("tx");

    /**
     * willing vpnmulti prefixes
     */
    public tabRoute<addrIP> wilVpnM = new tabRoute<addrIP>("tx");

    /**
     * willing vpnflow prefixes
     */
    public tabRoute<addrIP> wilVpnF = new tabRoute<addrIP>("tx");

    /**
     * willing other vpnuni prefixes
     */
    public tabRoute<addrIP> wilVpoU = new tabRoute<addrIP>("tx");

    /**
     * willing other vpnmulti prefixes
     */
    public tabRoute<addrIP> wilVpoM = new tabRoute<addrIP>("tx");

    /**
     * willing other vpnflow prefixes
     */
    public tabRoute<addrIP> wilVpoF = new tabRoute<addrIP>("tx");

    /**
     * willing vpls prefixes
     */
    public tabRoute<addrIP> wilVpls = new tabRoute<addrIP>("tx");

    /**
     * willing mspw prefixes
     */
    public tabRoute<addrIP> wilMspw = new tabRoute<addrIP>("tx");

    /**
     * willing evpn prefixes
     */
    public tabRoute<addrIP> wilEvpn = new tabRoute<addrIP>("tx");

    /**
     * willing mdt prefixes
     */
    public tabRoute<addrIP> wilMdt = new tabRoute<addrIP>("tx");

    /**
     * willing srte prefixes
     */
    public tabRoute<addrIP> wilSrte = new tabRoute<addrIP>("tx");

    /**
     * willing mvpn prefixes
     */
    public tabRoute<addrIP> wilMvpn = new tabRoute<addrIP>("tx");

    /**
     * willing other mvpn prefixes
     */
    public tabRoute<addrIP> wilMvpo = new tabRoute<addrIP>("tx");

    /**
     * changed unicast prefixes
     */
    public tabRoute<addrIP> chgUni = new tabRoute<addrIP>("chg");

    /**
     * changed multicast prefixes
     */
    public tabRoute<addrIP> chgMlt = new tabRoute<addrIP>("chg");

    /**
     * changed other prefixes
     */
    public tabRoute<addrIP> chgOtr = new tabRoute<addrIP>("chg");

    /**
     * changed flowspec prefixes
     */
    public tabRoute<addrIP> chgFlw = new tabRoute<addrIP>("chg");

    /**
     * changed vpnuni prefixes
     */
    public tabRoute<addrIP> chgVpnU = new tabRoute<addrIP>("chg");

    /**
     * changed vpnmulti prefixes
     */
    public tabRoute<addrIP> chgVpnM = new tabRoute<addrIP>("chg");

    /**
     * changed vpnflow prefixes
     */
    public tabRoute<addrIP> chgVpnF = new tabRoute<addrIP>("chg");

    /**
     * changed other vpnuni prefixes
     */
    public tabRoute<addrIP> chgVpoU = new tabRoute<addrIP>("chg");

    /**
     * changed other vpnmulti prefixes
     */
    public tabRoute<addrIP> chgVpoM = new tabRoute<addrIP>("chg");

    /**
     * changed other vpnflow prefixes
     */
    public tabRoute<addrIP> chgVpoF = new tabRoute<addrIP>("chg");

    /**
     * changed vpls prefixes
     */
    public tabRoute<addrIP> chgVpls = new tabRoute<addrIP>("chg");

    /**
     * changed mspw prefixes
     */
    public tabRoute<addrIP> chgMspw = new tabRoute<addrIP>("chg");

    /**
     * changed evpn prefixes
     */
    public tabRoute<addrIP> chgEvpn = new tabRoute<addrIP>("chg");

    /**
     * changed mdt prefixes
     */
    public tabRoute<addrIP> chgMdt = new tabRoute<addrIP>("chg");

    /**
     * changed srte prefixes
     */
    public tabRoute<addrIP> chgSrte = new tabRoute<addrIP>("chg");

    /**
     * changed mvpn prefixes
     */
    public tabRoute<addrIP> chgMvpn = new tabRoute<addrIP>("chg");

    /**
     * changed other mvpn prefixes
     */
    public tabRoute<addrIP> chgMvpo = new tabRoute<addrIP>("chg");

    /**
     * update group number
     */
    public int groupMember = -1;

    /**
     * neighbor reachable
     */
    public boolean reachable = false;

    /**
     * old reachable state
     */
    public boolean reachOld;

    /**
     * neighbor reachable change time
     */
    public long reachTim;

    /**
     * neighbor reachable change number
     */
    public long reachNum;

    /**
     * speaker
     */
    public rtrBgpSpeak conn;

    /**
     * type of peer
     */
    public int peerType;

    /**
     * full compute last
     */
    public long fullLast;

    /**
     * incremental compute last
     */
    public long incrLast;

    /**
     * full compute count
     */
    public int fullCount;

    /**
     * incremental compute count
     */
    public int incrCount;

    /**
     * full compute time
     */
    public int fullTime;

    /**
     * incremental compute time
     */
    public int incrTime;

    /**
     * transmit sleeper
     */
    protected notifier transmit = new notifier();

    private boolean need2run;

    /**
     * create neighbor
     *
     * @param parent bgp process
     */
    public rtrBgpNeigh(rtrBgp parent) {
        super(parent, false);
        peerAddr = new addrIP();
        conn = new rtrBgpSpeak(lower, this, null);
    }

    public int compare(rtrBgpNeigh o1, rtrBgpNeigh o2) {
        return o1.peerAddr.compare(o1.peerAddr, o2.peerAddr);
    }

    public String toString() {
        return "" + peerAddr;
    }

    public void flapBgpConn() {
        conn.closeNow();
    }

    public void doTempCfg(String cmd, boolean negated) {
    }

    public void getConfig(List<String> l, String beg, boolean filter) {
        l.addAll(getParamCfg(beg, "neighbor " + peerAddr + " ", filter));
    }

    /**
     * get status of peer
     *
     * @param l list to append
     */
    public void getStatus(List<String> l) {
        l.add("peer = " + peerAddr);
        l.add("reachable state = " + reachable);
        l.add("reachable changed = " + bits.timePast(reachTim));
        l.add("reachable changes = " + reachNum);
        l.add("update group = " + groupMember);
        l.add("type = " + rtrBgpUtil.peerType2string(peerType));
        l.add("safi = " + rtrBgpParam.mask2string(conn.peerAfis));
        l.add("local = " + localAddr);
        l.add("router id = " + conn.peerRouterID);
        l.add("uptime = " + bits.timePast(conn.upTime));
        l.add("hold time = " + bits.timeDump(conn.peerHold / 1000));
        l.add("keepalive time = " + bits.timeDump(conn.peerKeep / 1000));
        l.add("32bit as = " + conn.peer32bitAS);
        l.add("refresh = " + conn.peerRefresh + ", rx=" + conn.refreshRx + ", tx=" + conn.refreshTx);
        l.add("description = " + description);
        l.add("hostname = " + conn.peerHostname);
        l.add("compression = rx=" + (conn.compressRx != null) + ", tx=" + (conn.compressTx != null));
        l.add("graceful = " + rtrBgpParam.mask2string(conn.peerGrace));
        l.add("addpath rx = " + rtrBgpParam.mask2string(conn.addpathRx));
        l.add("addpath tx = " + rtrBgpParam.mask2string(conn.addpathTx));
        l.add("unicast advertised = " + conn.advUni.size() + " of " + wilUni.size() + ", list = " + chgUni.size() + ", accepted = " + accUni.size() + " of " + conn.lrnUni.size());
        l.add("multicast advertised = " + conn.advMlt.size() + " of " + wilMlt.size() + ", list = " + chgMlt.size() + ", accepted = " + accMlt.size() + " of " + conn.lrnMlt.size());
        l.add("other advertised = " + conn.advOtr.size() + " of " + wilOtr.size() + ", list = " + chgOtr.size() + ", accepted = " + accOtr.size() + " of " + conn.lrnOtr.size());
        l.add("flowspec advertised = " + conn.advFlw.size() + " of " + wilFlw.size() + ", list = " + chgFlw.size() + ", accepted = " + accFlw.size() + " of " + conn.lrnFlw.size());
        l.add("vpnuni advertised = " + conn.advVpnU.size() + " of " + wilVpnU.size() + ", list = " + chgVpnU.size() + ", accepted = " + accVpnU.size() + " of " + conn.lrnVpnU.size());
        l.add("vpnmlt advertised = " + conn.advVpnM.size() + " of " + wilVpnM.size() + ", list = " + chgVpnM.size() + ", accepted = " + accVpnM.size() + " of " + conn.lrnVpnM.size());
        l.add("vpnflw advertised = " + conn.advVpnF.size() + " of " + wilVpnF.size() + ", list = " + chgVpnF.size() + ", accepted = " + accVpnF.size() + " of " + conn.lrnVpnF.size());
        l.add("ovpnuni advertised = " + conn.advVpoU.size() + " of " + wilVpoU.size() + ", list = " + chgVpoU.size() + ", accepted = " + accVpoU.size() + " of " + conn.lrnVpoU.size());
        l.add("ovpnmlt advertised = " + conn.advVpoM.size() + " of " + wilVpoM.size() + ", list = " + chgVpoM.size() + ", accepted = " + accVpoM.size() + " of " + conn.lrnVpoM.size());
        l.add("ovpnflw advertised = " + conn.advVpoF.size() + " of " + wilVpoF.size() + ", list = " + chgVpoF.size() + ", accepted = " + accVpoF.size() + " of " + conn.lrnVpoF.size());
        l.add("vpls advertised = " + conn.advVpls.size() + " of " + wilVpls.size() + ", list = " + chgVpls.size() + ", accepted = " + accVpls.size() + " of " + conn.lrnVpls.size());
        l.add("mspw advertised = " + conn.advMspw.size() + " of " + wilMspw.size() + ", list = " + chgMspw.size() + ", accepted = " + accMspw.size() + " of " + conn.lrnMspw.size());
        l.add("evpn advertised = " + conn.advEvpn.size() + " of " + wilEvpn.size() + ", list = " + chgEvpn.size() + ", accepted = " + accEvpn.size() + " of " + conn.lrnEvpn.size());
        l.add("mdt advertised = " + conn.advMdt.size() + " of " + wilMdt.size() + ", list = " + chgMdt.size() + ", accepted = " + accMdt.size() + " of " + conn.lrnMdt.size());
        l.add("srte advertised = " + conn.advSrte.size() + " of " + wilSrte.size() + ", list = " + chgSrte.size() + ", accepted = " + accSrte.size() + " of " + conn.lrnSrte.size());
        l.add("mvpn advertised = " + conn.advMvpn.size() + " of " + wilMvpn.size() + ", list = " + chgMvpn.size() + ", accepted = " + accMvpn.size() + " of " + conn.lrnMvpn.size());
        l.add("omvpn advertised = " + conn.advMvpo.size() + " of " + wilMvpo.size() + ", list = " + chgMvpo.size() + ", accepted = " + accMvpo.size() + " of " + conn.lrnMvpo.size());
        l.add("version = " + conn.adversion + " of " + lower.compRound + ", needfull=" + conn.needFull + ", buffull=" + conn.buffFull);
        l.add("full = " + fullCount + ", " + bits.time2str(cfgAll.timeZoneName, fullLast + cfgAll.timeServerOffset, 3) + ", " + bits.timePast(fullLast) + " ago, " + fullTime + " ms");
        l.add("incr = " + incrCount + ", " + bits.time2str(cfgAll.timeZoneName, incrLast + cfgAll.timeServerOffset, 3) + ", " + bits.timePast(incrLast) + " ago, " + incrTime + " ms");
        l.add("connection = " + conn.cntr.getShStat());
        l.add("uncompressed = " + conn.compressCntr.getShStat());
        l.add("buffer = " + pipeSide.getStatus(conn.pipe));
    }

    /**
     * update peer structures
     */
    public void updatePeer() {
        if (localAs != remoteAs) {
            peerType = rtrBgpUtil.peerExtrn;
            if (remoteConfed) {
                peerType = rtrBgpUtil.peerCnfed;
            }
            if (serverClnt) {
                peerType = rtrBgpUtil.peerServr;
            }
        } else {
            peerType = rtrBgpUtil.peerIntrn;
            if (reflectClnt) {
                peerType = rtrBgpUtil.peerRflct;
            }
        }
    }

    /**
     * start this neighbor process
     */
    public void startNow() {
        if (need2run) {
            return;
        }
        need2run = true;
        new Thread(this).start();
    }

    /**
     * stop this neighbor process
     */
    public void stopNow() {
        need2run = false;
        shutdown = true;
        conn.closeNow();
    }

    public void run() {
        try {
            doWork();
        } catch (Exception e) {
            logger.traceback(e);
        }
        need2run = false;
        conn.closeNow();
    }

    private void doWork() {
        long lastKeep = 0;
        for (;;) {
            transmit.misleep(1000);
            long tim = bits.getTime();
            if ((lastKeep + conn.peerKeep) < tim) {
                if (conn.ready2adv) {
                    conn.sendKeepAlive();
                }
                lastKeep = tim;
            }
            if (!need2run) {
                return;
            }
            if (shutdown) {
                continue;
            }
            if (conn.txFree() >= 0) {
                doAdvert();
                continue;
            }
            lastKeep = 0;
            switch (socketMode) {
                case 1: // active
                    openConn(0);
                    break;
                case 2: // passive
                    openConn(60);
                    break;
                case 3: // both
                    if (!openConn(bits.random(10, 30))) {
                        break;
                    }
                    openConn(0);
                    break;
                case 4: // dynamic
                    lower.lstnNei.del(this);
                    stopNow();
                    return;
                case 5: // deleted dynamic
                    return;
            }
        }
    }

    private boolean openConn(int tim) {
        ipFwdIface ifc;
        if (srcIface == null) {
            ifc = ipFwdTab.findSendingIface(lower.fwdCore, peerAddr);
        } else {
            ifc = srcIface.getFwdIfc(peerAddr);
        }
        if (ifc == null) {
            return true;
        }
        if (ifc.addr == null) {
            return true;
        }
        pipeSide pipe;
        if (tim < 1) {
            pipe = lower.tcpCore.streamConnect(new pipeLine(bufferSize, false), ifc, 0, peerAddr, rtrBgp.port, "bgp", passwd, ttlSecurity);
        } else {
            prtAccept ac = new prtAccept(lower.tcpCore, new pipeLine(bufferSize, false), ifc, rtrBgp.port, peerAddr, 0, 0, "bgp", passwd, ttlSecurity);
            ac.wait4conn(tim * 1000);
            pipe = ac.getConn(true);
        }
        if (pipe == null) {
            return true;
        }
        localIfc = ifc;
        localAddr = ifc.addr.copyBytes();
        if (fallOver) {
            sendingIfc = ipFwdTab.findSendingIface(lower.fwdCore, peerAddr);
        }
        conn.closeNow();
        conn = new rtrBgpSpeak(lower, this, pipe);
        return false;
    }

    private boolean advertFullTable(int safi, int mask, tabRoute<addrIP> will, tabRoute<addrIP> done) {
        if ((conn.peerAfis & mask) == 0) {
            return false;
        }
        if (conn.needFull.get() < 2) {
            will = new tabRoute<addrIP>(will);
        }
        List<tabRouteEntry<addrIP>> lst = new ArrayList<tabRouteEntry<addrIP>>();
        tabRouteEntry<addrIP> sen = null;
        for (int i = 0; i < will.size(); i++) {
            tabRouteEntry<addrIP> ntry = will.get(i);
            if (ntry == null) {
                continue;
            }
            if (!ntry.differs(done.find(ntry))) {
                continue;
            }
            if (sen != null) {
                sen.prefix = ntry.prefix;
            }
            if (ntry.differs(sen)) {
                if (lst.size() > 0) {
                    conn.sendUpdate(safi, lst, true);
                }
                if (conn.txFree() < 2048) {
                    return true;
                }
                lst.clear();
                sen = ntry.copyBytes();
            }
            done.add(tabRoute.addType.always, ntry, false, false);
            lst.add(ntry);
            if (lst.size() > 64) {
                sen = null;
            }
        }
        if (lst.size() > 0) {
            conn.sendUpdate(safi, lst, true);
            if (conn.txFree() < 2048) {
                return true;
            }
        }
        lst.clear();
        for (int i = done.size() - 1; i >= 0; i--) {
            tabRouteEntry<addrIP> ntry = done.get(i);
            if (ntry == null) {
                continue;
            }
            if (will.find(ntry) != null) {
                continue;
            }
            done.del(ntry);
            lst.add(ntry);
            if (lst.size() < 64) {
                continue;
            }
            conn.sendUpdate(safi, lst, false);
            if (conn.txFree() < 2048) {
                return true;
            }
            lst.clear();
        }
        if (lst.size() > 0) {
            conn.sendUpdate(safi, lst, false);
            if (conn.txFree() < 2048) {
                return true;
            }
        }
        return false;
    }

    private boolean advertFull() {
        long tim = bits.getTime();
        if (advertFullTable(lower.afiUni, rtrBgpParam.mskUni, wilUni, conn.advUni)) {
            return true;
        }
        if (advertFullTable(lower.afiLab, rtrBgpParam.mskLab, wilUni, conn.advUni)) {
            return true;
        }
        if (advertFullTable(lower.afiMlt, rtrBgpParam.mskMlt, wilMlt, conn.advMlt)) {
            return true;
        }
        if (advertFullTable(lower.afiOtr, rtrBgpParam.mskOtr, wilOtr, conn.advOtr)) {
            return true;
        }
        if (advertFullTable(lower.afiFlw, rtrBgpParam.mskFlw, wilFlw, conn.advFlw)) {
            return true;
        }
        if (advertFullTable(lower.afiVpnU, rtrBgpParam.mskVpnU, wilVpnU, conn.advVpnU)) {
            return true;
        }
        if (advertFullTable(lower.afiVpnM, rtrBgpParam.mskVpnM, wilVpnM, conn.advVpnM)) {
            return true;
        }
        if (advertFullTable(lower.afiVpnF, rtrBgpParam.mskVpnF, wilVpnF, conn.advVpnF)) {
            return true;
        }
        if (advertFullTable(lower.afiVpoU, rtrBgpParam.mskVpoU, wilVpoU, conn.advVpoU)) {
            return true;
        }
        if (advertFullTable(lower.afiVpoM, rtrBgpParam.mskVpoM, wilVpoM, conn.advVpoM)) {
            return true;
        }
        if (advertFullTable(lower.afiVpoF, rtrBgpParam.mskVpoF, wilVpoF, conn.advVpoF)) {
            return true;
        }
        if (advertFullTable(lower.afiVpls, rtrBgpParam.mskVpls, wilVpls, conn.advVpls)) {
            return true;
        }
        if (advertFullTable(lower.afiMspw, rtrBgpParam.mskMspw, wilMspw, conn.advMspw)) {
            return true;
        }
        if (advertFullTable(lower.afiEvpn, rtrBgpParam.mskEvpn, wilEvpn, conn.advEvpn)) {
            return true;
        }
        if (advertFullTable(lower.afiMdt, rtrBgpParam.mskMdt, wilMdt, conn.advMdt)) {
            return true;
        }
        if (advertFullTable(lower.afiSrte, rtrBgpParam.mskSrte, wilSrte, conn.advSrte)) {
            return true;
        }
        if (advertFullTable(lower.afiMvpn, rtrBgpParam.mskMvpn, wilMvpn, conn.advMvpn)) {
            return true;
        }
        if (advertFullTable(lower.afiMvpo, rtrBgpParam.mskMvpo, wilMvpo, conn.advMvpo)) {
            return true;
        }
        int ver = conn.needFull.ver();
        if (conn.needFull.get() == 1) {
            conn.needFull.setIf(0, ver);
        } else {
            conn.needFull.setIf(1, ver);
        }
        fullLast = bits.getTime();
        fullTime = (int) (fullLast - tim);
        fullCount++;
        return false;
    }

    private boolean advertIncrTable(int safi, int mask, tabRoute<addrIP> will, tabRoute<addrIP> chg, tabRoute<addrIP> done) {
        if ((conn.peerAfis & mask) == 0) {
            return false;
        }
        chg = new tabRoute<addrIP>(chg);
        List<tabRouteEntry<addrIP>> lst = new ArrayList<tabRouteEntry<addrIP>>();
        for (int i = 0; i < chg.size(); i++) {
            tabRouteEntry<addrIP> cur = chg.get(i);
            if (cur == null) {
                continue;
            }
            tabRouteEntry<addrIP> wil = will.find(cur);
            tabRouteEntry<addrIP> don = done.find(cur);
            if (wil == null) {
                if (don == null) {
                    continue;
                }
                done.del(don);
                lst.clear();
                lst.add(don);
                conn.sendUpdate(safi, lst, false);
            } else {
                if (!wil.differs(don)) {
                    continue;
                }
                done.add(tabRoute.addType.always, wil, false, false);
                lst.clear();
                lst.add(wil);
                conn.sendUpdate(safi, lst, true);
            }
            if (conn.txFree() < 1024) {
                return true;
            }
        }
        return false;
    }

    private boolean advertIncr() {
        long tim = bits.getTime();
        if (advertIncrTable(lower.afiUni, rtrBgpParam.mskUni, wilUni, chgUni, conn.advUni)) {
            return true;
        }
        if (advertIncrTable(lower.afiLab, rtrBgpParam.mskLab, wilUni, chgUni, conn.advUni)) {
            return true;
        }
        if (advertIncrTable(lower.afiMlt, rtrBgpParam.mskMlt, wilMlt, chgMlt, conn.advMlt)) {
            return true;
        }
        if (advertIncrTable(lower.afiOtr, rtrBgpParam.mskOtr, wilOtr, chgOtr, conn.advOtr)) {
            return true;
        }
        if (advertIncrTable(lower.afiFlw, rtrBgpParam.mskFlw, wilFlw, chgFlw, conn.advFlw)) {
            return true;
        }
        if (advertIncrTable(lower.afiVpnU, rtrBgpParam.mskVpnU, wilVpnU, chgVpnU, conn.advVpnU)) {
            return true;
        }
        if (advertIncrTable(lower.afiVpnM, rtrBgpParam.mskVpnM, wilVpnM, chgVpnM, conn.advVpnM)) {
            return true;
        }
        if (advertIncrTable(lower.afiVpnF, rtrBgpParam.mskVpnF, wilVpnF, chgVpnF, conn.advVpnF)) {
            return true;
        }
        if (advertIncrTable(lower.afiVpoU, rtrBgpParam.mskVpoU, wilVpoU, chgVpoU, conn.advVpoU)) {
            return true;
        }
        if (advertIncrTable(lower.afiVpoM, rtrBgpParam.mskVpoM, wilVpoM, chgVpoM, conn.advVpoM)) {
            return true;
        }
        if (advertIncrTable(lower.afiVpoF, rtrBgpParam.mskVpoF, wilVpoF, chgVpoF, conn.advVpoF)) {
            return true;
        }
        if (advertIncrTable(lower.afiVpls, rtrBgpParam.mskVpls, wilVpls, chgVpls, conn.advVpls)) {
            return true;
        }
        if (advertIncrTable(lower.afiMspw, rtrBgpParam.mskMspw, wilMspw, chgMspw, conn.advMspw)) {
            return true;
        }
        if (advertIncrTable(lower.afiEvpn, rtrBgpParam.mskEvpn, wilEvpn, chgEvpn, conn.advEvpn)) {
            return true;
        }
        if (advertIncrTable(lower.afiMdt, rtrBgpParam.mskMdt, wilMdt, chgMdt, conn.advMdt)) {
            return true;
        }
        if (advertIncrTable(lower.afiSrte, rtrBgpParam.mskSrte, wilSrte, chgSrte, conn.advSrte)) {
            return true;
        }
        if (advertIncrTable(lower.afiMvpn, rtrBgpParam.mskMvpn, wilMvpn, chgMvpn, conn.advMvpn)) {
            return true;
        }
        if (advertIncrTable(lower.afiMvpo, rtrBgpParam.mskMvpo, wilMvpo, chgMvpo, conn.advMvpo)) {
            return true;
        }
        incrLast = bits.getTime();
        incrTime = (int) (incrLast - tim);
        incrCount++;
        return false;
    }

    private void doAdvert() {
        if ((bits.getTime() - conn.lastRx) > holdTimer) {
            conn.sendNotify(4, 0);
            conn.closeNow();
            return;
        }
        int doing = lower.compRound.get();
        if (doing == conn.adversion.get()) {
            return;
        }
        if (conn.txFree() < (bufferSize / 2)) {
            conn.needFull.add(1);
            conn.buffFull++;
            return;
        }
        if (unidirection && (conn.rxReady() > (bufferSize / 8))) {
            conn.needFull.add(1);
            return;
        }
        boolean b;
        if (conn.needFull.get() != 0) {
            b = advertFull();
        } else {
            b = advertIncr();
        }
        if (b) {
            conn.needFull.add(1);
            return;
        }
        if (conn.needFull.get() != 0) {
            doing--;
        }
        conn.adversion.set(doing);
    }

    /**
     * set accepted list
     */
    public void setAccepted() {
        accUni = new tabRoute<addrIP>("bgp");
        accMlt = new tabRoute<addrIP>("bgp");
        accOtr = new tabRoute<addrIP>("bgp");
        accFlw = new tabRoute<addrIP>("bgp");
        accVpnU = new tabRoute<addrIP>("bgp");
        accVpnM = new tabRoute<addrIP>("bgp");
        accVpnF = new tabRoute<addrIP>("bgp");
        accVpoU = new tabRoute<addrIP>("bgp");
        accVpoM = new tabRoute<addrIP>("bgp");
        accVpoF = new tabRoute<addrIP>("bgp");
        accVpls = new tabRoute<addrIP>("bgp");
        accMspw = new tabRoute<addrIP>("bgp");
        accEvpn = new tabRoute<addrIP>("bgp");
        accMdt = new tabRoute<addrIP>("bgp");
        accSrte = new tabRoute<addrIP>("bgp");
        accMvpn = new tabRoute<addrIP>("bgp");
        accMvpo = new tabRoute<addrIP>("bgp");
        reachable = false;
        if (sendingIfc != null) {
            ipFwdIface ifc = ipFwdTab.findSendingIface(lower.fwdCore, peerAddr);
            if (ifc == null) {
                return;
            }
            if (ifc != sendingIfc) {
                return;
            }
        }
        if (lower.nhtRoumap != null) {
            tabRouteEntry<addrIP> rou = lower.fwdCore.actualU.route(peerAddr);
            if (rou == null) {
                return;
            }
            tabRtrmapN rme = lower.nhtRoumap.find(lower.afiUni, rou);
            if (rme == null) {
                return;
            }
            if (rme.action != tabPlcmapN.actionType.actPermit) {
                return;
            }
        }
        if (lower.nhtRouplc != null) {
            tabRouteEntry<addrIP> rou = lower.fwdCore.actualU.route(peerAddr);
            if (rou == null) {
                return;
            }
            rou = tabRtrplc.doRpl(lower.afiUni, rou, lower.nhtRouplc, true);
            if (rou == null) {
                return;
            }
        }
        if (lower.nhtPfxlst != null) {
            tabRouteEntry<addrIP> rou = lower.fwdCore.actualU.route(peerAddr);
            if (rou == null) {
                return;
            }
            tabPrfxlstN ple = lower.nhtPfxlst.find(lower.afiUni, rou);
            if (ple == null) {
                return;
            }
            if (ple.action != tabPlcmapN.actionType.actPermit) {
                return;
            }
        }
        reachable = true;
        if (!softReconfig) {
            accUni = conn.lrnUni;
            accMlt = conn.lrnMlt;
            accOtr = conn.lrnOtr;
            accFlw = conn.lrnFlw;
            accVpnU = conn.lrnVpnU;
            accVpnM = conn.lrnVpnM;
            accVpnF = conn.lrnVpnF;
            accVpoU = conn.lrnVpoU;
            accVpoM = conn.lrnVpoM;
            accVpoF = conn.lrnVpoF;
            accVpls = conn.lrnVpls;
            accMspw = conn.lrnMspw;
            accEvpn = conn.lrnEvpn;
            accMdt = conn.lrnMdt;
            accSrte = conn.lrnSrte;
            accMvpn = conn.lrnMvpn;
            accMvpo = conn.lrnMvpo;
            return;
        }
        tabRoute.addUpdatedTable(tabRoute.addType.better, lower.afiUni, accUni, conn.lrnUni, roumapIn, roupolIn, prflstIn);
        tabRoute.addUpdatedTable(tabRoute.addType.better, lower.afiMlt, accMlt, conn.lrnMlt, roumapIn, roupolIn, prflstIn);
        tabRoute.addUpdatedTable(tabRoute.addType.better, lower.afiOtr, accOtr, conn.lrnOtr, roumapIn, roupolIn, prflstIn);
        tabRoute.addUpdatedTable(tabRoute.addType.better, lower.afiFlw, accFlw, conn.lrnFlw, voumapIn, voupolIn, null);
        tabRoute.addUpdatedTable(tabRoute.addType.better, lower.afiVpnU, accVpnU, conn.lrnVpnU, voumapIn, voupolIn, null);
        tabRoute.addUpdatedTable(tabRoute.addType.better, lower.afiVpnM, accVpnM, conn.lrnVpnM, voumapIn, voupolIn, null);
        tabRoute.addUpdatedTable(tabRoute.addType.better, lower.afiVpnF, accVpnF, conn.lrnVpnF, voumapIn, voupolIn, null);
        tabRoute.addUpdatedTable(tabRoute.addType.better, lower.afiVpoU, accVpoU, conn.lrnVpoU, voumapIn, voupolIn, null);
        tabRoute.addUpdatedTable(tabRoute.addType.better, lower.afiVpoM, accVpoM, conn.lrnVpoM, voumapIn, voupolIn, null);
        tabRoute.addUpdatedTable(tabRoute.addType.better, lower.afiVpoF, accVpoF, conn.lrnVpoF, voumapIn, voupolIn, null);
        tabRoute.addUpdatedTable(tabRoute.addType.better, lower.afiVpls, accVpls, conn.lrnVpls, voumapIn, voupolIn, null);
        tabRoute.addUpdatedTable(tabRoute.addType.better, lower.afiMspw, accMspw, conn.lrnMspw, voumapIn, voupolIn, null);
        tabRoute.addUpdatedTable(tabRoute.addType.better, lower.afiEvpn, accEvpn, conn.lrnEvpn, voumapIn, voupolIn, null);
        tabRoute.addUpdatedTable(tabRoute.addType.better, lower.afiMdt, accMdt, conn.lrnMdt, voumapIn, voupolIn, null);
        tabRoute.addUpdatedTable(tabRoute.addType.better, lower.afiSrte, accSrte, conn.lrnSrte, voumapIn, voupolIn, null);
        tabRoute.addUpdatedTable(tabRoute.addType.better, lower.afiMvpn, accMvpn, conn.lrnMvpn, voumapIn, voupolIn, null);
        tabRoute.addUpdatedTable(tabRoute.addType.better, lower.afiMvpo, accMvpo, conn.lrnMvpo, voumapIn, voupolIn, null);
    }

    /**
     * set merged list
     *
     * @param uni unicast
     * @param mlt multicast
     * @param otr other
     * @param flw flowspec
     * @param vpnU vpn uni
     * @param vpnM vpn multi
     * @param vpnF vpn flow
     * @param vpoU ovpn uni
     * @param vpoM ovpn multi
     * @param vpoF ovpn flow
     * @param vpls vpls
     * @param mspw mspw
     * @param evpn evpn
     * @param mdt mdt
     * @param srte srte
     * @param mvpn mvpn
     * @param mvpo omvpn
     */
    public void setMerge(tabRoute<addrIP> uni, tabRoute<addrIP> mlt, tabRoute<addrIP> otr, tabRoute<addrIP> flw,
            tabRoute<addrIP> vpnU, tabRoute<addrIP> vpnM, tabRoute<addrIP> vpnF,
            tabRoute<addrIP> vpoU, tabRoute<addrIP> vpoM, tabRoute<addrIP> vpoF,
            tabRoute<addrIP> vpls, tabRoute<addrIP> mspw, tabRoute<addrIP> evpn,
            tabRoute<addrIP> mdt, tabRoute<addrIP> srte, tabRoute<addrIP> mvpn, tabRoute<addrIP> mvpo) {
        uni.mergeFrom(tabRoute.addType.better, new tabRoute<addrIP>(accUni), null, false, tabRouteEntry.distanLim);
        mlt.mergeFrom(tabRoute.addType.better, new tabRoute<addrIP>(accMlt), null, false, tabRouteEntry.distanLim);
        otr.mergeFrom(tabRoute.addType.better, new tabRoute<addrIP>(accOtr), null, false, tabRouteEntry.distanLim);
        flw.mergeFrom(tabRoute.addType.better, new tabRoute<addrIP>(accFlw), null, false, tabRouteEntry.distanLim);
        vpnU.mergeFrom(tabRoute.addType.better, new tabRoute<addrIP>(accVpnU), null, false, tabRouteEntry.distanLim);
        vpnM.mergeFrom(tabRoute.addType.better, new tabRoute<addrIP>(accVpnM), null, false, tabRouteEntry.distanLim);
        vpnF.mergeFrom(tabRoute.addType.better, new tabRoute<addrIP>(accVpnF), null, false, tabRouteEntry.distanLim);
        vpoU.mergeFrom(tabRoute.addType.better, new tabRoute<addrIP>(accVpoU), null, false, tabRouteEntry.distanLim);
        vpoM.mergeFrom(tabRoute.addType.better, new tabRoute<addrIP>(accVpoM), null, false, tabRouteEntry.distanLim);
        vpoF.mergeFrom(tabRoute.addType.better, new tabRoute<addrIP>(accVpoF), null, false, tabRouteEntry.distanLim);
        vpls.mergeFrom(tabRoute.addType.better, new tabRoute<addrIP>(accVpls), null, false, tabRouteEntry.distanLim);
        mspw.mergeFrom(tabRoute.addType.better, new tabRoute<addrIP>(accMspw), null, false, tabRouteEntry.distanLim);
        evpn.mergeFrom(tabRoute.addType.better, new tabRoute<addrIP>(accEvpn), null, false, tabRouteEntry.distanLim);
        mdt.mergeFrom(tabRoute.addType.better, new tabRoute<addrIP>(accMdt), null, false, tabRouteEntry.distanLim);
        srte.mergeFrom(tabRoute.addType.better, new tabRoute<addrIP>(accSrte), null, false, tabRouteEntry.distanLim);
        mvpn.mergeFrom(tabRoute.addType.better, new tabRoute<addrIP>(accMvpn), null, false, tabRouteEntry.distanLim);
        mvpo.mergeFrom(tabRoute.addType.better, new tabRoute<addrIP>(accMvpo), null, false, tabRouteEntry.distanLim);
    }

    /**
     * set group membership
     */
    public void setGroup() {
        if (reachable != reachOld) {
            reachOld = reachable;
            reachTim = bits.getTime();
            reachNum++;
            if (debugger.rtrBgpEvnt) {
                logger.debug("reachable neighbor " + peerAddr + " " + reachable);
            }
        }
        groupMember = -1;
        if (shutdown) {
            return;
        }
        for (int i = 0; i < lower.groups.size(); i++) {
            rtrBgpGroup ntry = lower.groups.get(i);
            if (ntry.peerType != peerType) {
                continue;
            }
            if (nxtHopSelf || (!nxtHopUnchgd)) {
                if (localAddr.compare(localAddr, ntry.localAddr) != 0) {
                    continue;
                }
            }
            if (ntry.sameOutput(this)) {
                continue;
            }
            groupMember = i;
            return;
        }
        groupMember = lower.groups.size();
        rtrBgpGroup ntry = new rtrBgpGroup(lower, groupMember);
        ntry.copyFrom(this);
        ntry.peerType = peerType;
        ntry.localAddr = localAddr.copyBytes();
        lower.groups.add(ntry);
    }

    /**
     * set needed prefixes
     */
    public void setNeeded() {
        if (groupMember < 0) {
            wilUni = new tabRoute<addrIP>("tx");
            wilMlt = new tabRoute<addrIP>("tx");
            wilOtr = new tabRoute<addrIP>("tx");
            wilFlw = new tabRoute<addrIP>("tx");
            wilVpnU = new tabRoute<addrIP>("tx");
            wilVpnM = new tabRoute<addrIP>("tx");
            wilVpnF = new tabRoute<addrIP>("tx");
            wilVpoU = new tabRoute<addrIP>("tx");
            wilVpoM = new tabRoute<addrIP>("tx");
            wilVpoF = new tabRoute<addrIP>("tx");
            wilVpls = new tabRoute<addrIP>("tx");
            wilMspw = new tabRoute<addrIP>("tx");
            wilEvpn = new tabRoute<addrIP>("tx");
            wilMdt = new tabRoute<addrIP>("tx");
            wilSrte = new tabRoute<addrIP>("tx");
            wilMvpn = new tabRoute<addrIP>("tx");
            wilMvpo = new tabRoute<addrIP>("tx");
            chgUni = new tabRoute<addrIP>("chg");
            chgMlt = new tabRoute<addrIP>("chg");
            chgOtr = new tabRoute<addrIP>("chg");
            chgFlw = new tabRoute<addrIP>("chg");
            chgVpnU = new tabRoute<addrIP>("chg");
            chgVpnM = new tabRoute<addrIP>("chg");
            chgVpnF = new tabRoute<addrIP>("chg");
            chgVpoU = new tabRoute<addrIP>("chg");
            chgVpoM = new tabRoute<addrIP>("chg");
            chgVpoF = new tabRoute<addrIP>("chg");
            chgVpls = new tabRoute<addrIP>("chg");
            chgMspw = new tabRoute<addrIP>("chg");
            chgEvpn = new tabRoute<addrIP>("chg");
            chgMdt = new tabRoute<addrIP>("chg");
            chgSrte = new tabRoute<addrIP>("chg");
            chgMvpn = new tabRoute<addrIP>("chg");
            chgMvpo = new tabRoute<addrIP>("chg");
        } else {
            rtrBgpGroup grp = lower.groups.get(groupMember);
            wilUni = grp.wilUni;
            wilMlt = grp.wilMlt;
            wilOtr = grp.wilOtr;
            wilFlw = grp.wilFlw;
            wilVpnU = grp.wilVpnU;
            wilVpnM = grp.wilVpnM;
            wilVpnF = grp.wilVpnF;
            wilVpoU = grp.wilVpoU;
            wilVpoM = grp.wilVpoM;
            wilVpoF = grp.wilVpoF;
            wilVpls = grp.wilVpls;
            wilMspw = grp.wilMspw;
            wilEvpn = grp.wilEvpn;
            wilMdt = grp.wilMdt;
            wilSrte = grp.wilSrte;
            wilMvpn = grp.wilMvpn;
            wilMvpo = grp.wilMvpo;
            chgUni = grp.chgUni;
            chgMlt = grp.chgMlt;
            chgOtr = grp.chgOtr;
            chgFlw = grp.chgFlw;
            chgVpnU = grp.chgVpnU;
            chgVpnM = grp.chgVpnM;
            chgVpnF = grp.chgVpnF;
            chgVpoU = grp.chgVpoU;
            chgVpoM = grp.chgVpoM;
            chgVpoF = grp.chgVpoF;
            chgVpls = grp.chgVpls;
            chgMspw = grp.chgMspw;
            chgEvpn = grp.chgEvpn;
            chgMdt = grp.chgMdt;
            chgSrte = grp.chgSrte;
            chgMvpn = grp.chgMvpn;
            chgMvpo = grp.chgMvpo;
        }
        conn.needFull.add(1);
    }

    private void setValidityTable(tabRoute<addrIP> tab) {
        for (int i = 0; i < tab.size(); i++) {
            lower.setValidity(tab.get(i));
        }
    }

    /**
     * validate prefixes
     */
    public void setValidity() {
        if (lower.rpkis.size() < 1) {
            return;
        }
        setValidityTable(accUni);
        setValidityTable(accMlt);
    }

    /**
     * group version
     */
    public void setGrpVer() {
        if (groupMember < 0) {
            return;
        }
        if (conn.needFull.get() > 1) {
            return;
        }
        rtrBgpGroup grp = lower.groups.get(groupMember);
        int i = conn.adversion.get();
        if (i < grp.minversion) {
            grp.minversion = i;
        }
    }

    /**
     * get accepted
     *
     * @param safi safi to query
     * @return table
     */
    public tabRoute<addrIP> getAccepted(int safi) {
        if (safi == lower.afiUni) {
            return accUni;
        }
        if (safi == lower.afiLab) {
            return accUni;
        }
        if (safi == lower.afiMlt) {
            return accMlt;
        }
        if (safi == lower.afiOtr) {
            return accOtr;
        }
        if (safi == lower.afiFlw) {
            return accFlw;
        }
        if (safi == lower.afiVpnU) {
            return accVpnU;
        }
        if (safi == lower.afiVpnM) {
            return accVpnM;
        }
        if (safi == lower.afiVpnF) {
            return accVpnF;
        }
        if (safi == lower.afiVpoU) {
            return accVpoU;
        }
        if (safi == lower.afiVpoM) {
            return accVpoM;
        }
        if (safi == lower.afiVpoF) {
            return accVpoF;
        }
        if (safi == lower.afiVpls) {
            return accVpls;
        }
        if (safi == lower.afiMspw) {
            return accMspw;
        }
        if (safi == lower.afiEvpn) {
            return accEvpn;
        }
        if (safi == lower.afiMdt) {
            return accMdt;
        }
        if (safi == lower.afiSrte) {
            return accSrte;
        }
        if (safi == lower.afiMvpn) {
            return accMvpn;
        }
        if (safi == lower.afiMvpo) {
            return accMvpo;
        }
        return null;
    }

    /**
     * get willing
     *
     * @param safi safi to query
     * @return table
     */
    public tabRoute<addrIP> getWilling(int safi) {
        if (safi == lower.afiUni) {
            return wilUni;
        }
        if (safi == lower.afiLab) {
            return wilUni;
        }
        if (safi == lower.afiMlt) {
            return wilMlt;
        }
        if (safi == lower.afiOtr) {
            return wilOtr;
        }
        if (safi == lower.afiFlw) {
            return wilFlw;
        }
        if (safi == lower.afiVpnU) {
            return wilVpnU;
        }
        if (safi == lower.afiVpnM) {
            return wilVpnM;
        }
        if (safi == lower.afiVpnF) {
            return wilVpnF;
        }
        if (safi == lower.afiVpoU) {
            return wilVpoU;
        }
        if (safi == lower.afiVpoM) {
            return wilVpoM;
        }
        if (safi == lower.afiVpoF) {
            return wilVpoF;
        }
        if (safi == lower.afiVpls) {
            return wilVpls;
        }
        if (safi == lower.afiMspw) {
            return wilMspw;
        }
        if (safi == lower.afiEvpn) {
            return wilEvpn;
        }
        if (safi == lower.afiMdt) {
            return wilMdt;
        }
        if (safi == lower.afiSrte) {
            return wilSrte;
        }
        if (safi == lower.afiMvpn) {
            return wilMvpn;
        }
        if (safi == lower.afiMvpo) {
            return wilMvpo;
        }
        return null;
    }

    private String tabSiz(tabRoute<addrIP> tab) {
        if (tab == null) {
            return "-";
        }
        return "" + tab.size();
    }

    /**
     * neighbor list entry
     *
     * @param safi safi to query
     * @return line of string
     */
    public String showNeighs(int safi) {
        return bits.num2str(remoteAs) + "|" + tabSiz(conn.getLearned(safi)) + "|" + tabSiz(getAccepted(safi)) + "|" + tabSiz(getWilling(safi)) + "|" + tabSiz(conn.getAdverted(safi)) + "|" + peerAddr + "|" + bits.timePast(conn.upTime);
    }

    /**
     * neighbor list entry
     *
     * @param mod mode
     * @return line of string
     */
    public String showSummary(int mod) {
        switch (mod) {
            case 1:
                return bits.num2str(remoteAs) + "|" + rtrBgpParam.mask2string(conn.peerAfis) + "|" + conn.ready2adv + "|" + peerAddr + "|" + bits.timePast(conn.upTime);
            case 2:
                return bits.num2str(remoteAs) + "|" + groupMember + "|" + peerAddr + "|" + bits.timePast(conn.upTime);
            case 3:
                return bits.num2str(remoteAs) + "|" + reachable + "|" + bits.timePast(reachTim) + "|" + reachNum + "|" + peerAddr + "|" + bits.timePast(conn.upTime);
            case 4:
                return bits.num2str(remoteAs) + "|" + rtrBgpParam.mask2string(conn.peerGrace) + "|" + rtrBgpParam.mask2string(graceRestart & addrFams) + "|" + peerAddr;
            case 5:
                return bits.num2str(remoteAs) + "|" + rtrBgpParam.mask2string(conn.addpathRx) + "|" + rtrBgpParam.mask2string(conn.addpathTx) + "|" + peerAddr;
            case 6:
                return bits.num2str(remoteAs) + "|" + conn.peerRouterID + "|" + conn.peer32bitAS + "|" + conn.peerRefresh + "|" + peerAddr;
            case 7:
                return bits.num2str(remoteAs) + "|" + pipeSide.getStatus(conn.pipe) + "|" + conn.buffFull + "|" + conn.adversion + "|" + incrCount + "|" + fullCount + "|" + conn.needFull + "|" + peerAddr;
            case 8:
                return bits.num2str(remoteAs) + "|" + peerAddr + "|" + description;
            case 9:
                return bits.num2str(remoteAs) + "|" + peerAddr + "|" + conn.peerHostname;
            case 10:
                return bits.num2str(remoteAs) + "|" + (conn.compressRx != null) + "|" + (conn.compressTx != null) + "|" + bits.percent(conn.cntr.byteRx, conn.compressCntr.byteRx) + "|" + bits.percent(conn.cntr.byteTx, conn.compressCntr.byteTx) + "|" + peerAddr;
            case 11:
                return bits.num2str(remoteAs) + "|" + conn.cntr.packRx + "|" + conn.cntr.packTx + "|" + conn.cntr.byteRx + "|" + conn.cntr.byteTx + "|" + conn.refreshRx + "|" + conn.refreshTx + "|" + peerAddr;
            case 12:
                clntDns clnt = new clntDns();
                clnt.doResolvList(cfgAll.nameServerAddr, packDnsRec.generateReverse(peerAddr), packDnsRec.typePTR);
                return bits.num2str(remoteAs) + "|" + peerAddr + "|" + clnt.getPTR() + "|" + bits.timePast(conn.upTime);
            default:
                return null;
        }
    }

}
