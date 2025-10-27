package org.freertr.ip;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.clnt.clntMplsTeP2p;
import org.freertr.clnt.clntNetflow;
import org.freertr.ifc.ifcEthTyp;
import org.freertr.ifc.ifcNshFwd;
import org.freertr.pack.packHolder;
import org.freertr.prt.prtTcp;
import org.freertr.rtr.rtrLdpIface;
import org.freertr.rtr.rtrLdpNeigh;
import org.freertr.rtr.rtrLdpTrgtd;
import org.freertr.tab.tabAceslstN;
import org.freertr.tab.tabConnect;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabLabelBier;
import org.freertr.tab.tabLabelBierN;
import org.freertr.tab.tabLabelDup;
import org.freertr.tab.tabLabelEntry;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabNatCfgN;
import org.freertr.tab.tabNatTraN;
import org.freertr.tab.tabPbrN;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.tab.tabQos;
import org.freertr.tab.tabRateLimit;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRtrmapN;
import org.freertr.tab.tabRtrplcN;
import org.freertr.util.bits;
import org.freertr.util.counter;
import org.freertr.util.debugger;
import org.freertr.util.history;
import org.freertr.util.logger;
import org.freertr.util.notifier;
import org.freertr.util.state;
import org.freertr.util.syncInt;

/**
 * does ip forwarding, services protocols
 *
 * @author matecsaba
 */
public class ipFwd implements Runnable, Comparable<ipFwd> {

    /**
     * the ip version
     */
    public final int ipVersion;

    /**
     * configured name of routing table
     */
    public final String cfgName;

    /**
     * name of routing table
     */
    public final String vrfName;

    /**
     * number of routing table
     */
    public final int vrfNum;

    /**
     * other afi
     */
    public ipFwd other;

    /**
     * delay between updates
     */
    public int updateInterval;

    /**
     * number of updates
     */
    public int updateCount;

    /**
     * update time took
     */
    public int updateTime;

    /**
     * time of update
     */
    public long updateLast;

    /**
     * number of full updates
     */
    public int updateFullCnt;

    /**
     * number of incremental updates
     */
    public int updateIncrCnt;

    /**
     * last full update
     */
    public long updateFullLst;

    /**
     * last incremental update
     */
    public long updateIncrLst;

    /**
     * number of changes
     */
    public int changeCount;

    /**
     * time of change
     */
    public long changeLast;

    /**
     * optimize for lookup
     */
    public boolean optimize;

    /**
     * route distinguisher
     */
    public long rd;

    /**
     * route target import
     */
    public List<Long> rtImp = new ArrayList<Long>();

    /**
     * route target export
     */
    public List<Long> rtExp = new ArrayList<Long>();

    /**
     * color import
     */
    public List<Integer> clrImp = new ArrayList<Integer>();

    /**
     * color export
     */
    public List<Integer> clrExp = new ArrayList<Integer>();

    /**
     * unicast route limit
     */
    public int routeLimitU;

    /**
     * labeled route limit
     */
    public int routeLimitL;

    /**
     * multicast route limit
     */
    public int routeLimitM;

    /**
     * flowspec route limit
     */
    public int routeLimitF;

    /**
     * current list of routers
     */
    public final tabGen<ipRtr> routers;

    /**
     * current list of protocols
     */
    public final tabConnect<addrIP, ipFwdProto> protos;

    /**
     * list of current interfaces
     */
    public final tabGen<ipFwdIface> ifaces;

    /**
     * the configured static unicast route table
     */
    public final tabGen<ipFwdRoute> staticU;

    /**
     * the configured static multicast route table
     */
    public final tabGen<ipFwdRoute> staticM;

    /**
     * the computed connected table
     */
    public tabRoute<addrIP> connedR;

    /**
     * the computed direct table
     */
    public tabRoute<addrIP> directR;

    /**
     * the labeled table
     */
    public tabRoute<addrIP> labeldR;

    /**
     * the computed unicast routing table
     */
    public tabRoute<addrIP> actualU;

    /**
     * the computed multicast routing table
     */
    public tabRoute<addrIP> actualM;

    /**
     * the computed flowspec routing table
     */
    public tabRoute<addrIP> actualF;

    /**
     * the computed index to prefix table
     */
    public tabGen<tabIndex<addrIP>> actualIU;

    /**
     * the computed index to connected table
     */
    public tabGen<tabIndex<addrIP>> actualIC;

    /**
     * list of multicast groups
     */
    public final tabGen<ipFwdMcast> groups;

    /**
     * the configured pbr entries
     */
    public final tabListing<tabPbrN, addrIP> pbrCfg;

    /**
     * the configured nat entries
     */
    public final tabListing<tabNatCfgN, addrIP> natCfg;

    /**
     * current nat entries
     */
    public final tabGen<tabNatTraN> natTrns;

    /**
     * current echo sessions
     */
    public final tabGen<ipFwdEcho> echoes;

    /**
     * echo packets sent
     */
    public int echoSent;

    /**
     * echo responses got
     */
    public int echoRply;

    /**
     * echo packets got
     */
    public int echoRcvd;

    /**
     * echo packets sent
     */
    public int errorSent;

    /**
     * echo responses got
     */
    public int errorRcvd;

    /**
     * traffic engineering tunnels
     */
    public final tabGen<ipFwdTrfng> trafEngs;

    /**
     * multipoint label paths
     */
    public final tabGen<ipFwdMpmp> mp2mpLsp;

    /**
     * ldp neighbors
     */
    public final tabGen<rtrLdpNeigh> ldpNeighs;

    /**
     * targeted ldp neighbors
     */
    public final tabGen<rtrLdpTrgtd> ldpTarget;

    /**
     * auto mesh te neighbors
     */
    public final tabGen<clntMplsTeP2p> autoMesh;

    /**
     * total counter for this vrf
     */
    public counter cntrT;

    /**
     * total historic for this vrf
     */
    public history hstryT;

    /**
     * hardware counter for this vrf
     */
    public counter cntrH;

    /**
     * hardware historic for this vrf
     */
    public history hstryH;

    /**
     * local counter for this vrf
     */
    public counter cntrL;

    /**
     * local historic for this vrf
     */
    public history hstryL;

    /**
     * route counter for this vrf
     */
    public counter cntrR;

    /**
     * route historic for this vrf
     */
    public history hstryR;

    /**
     * traffic threshold for this vrf
     */
    public int thresholdT;

    /**
     * route threshold for this vrf
     */
    public int thresholdR;

    /**
     * netflow exporter
     */
    public clntNetflow netflow;

    /**
     * allocate label for which prefix
     */
    public labelMode prefixMode = labelMode.common;

    /**
     * multicast distribution tree
     */
    public mdtMode mdtMod = mdtMode.none;

    /**
     * mpls propagate ip ttl
     */
    public boolean mplsPropTtl = true;

    /**
     * mpls extended report
     */
    public boolean mplsExtRep = true;

    /**
     * unreachable last
     */
    public tabRateLimit unreach;

    /**
     * label allocation filter
     */
    public tabListing<tabPrfxlstN, addrIP> labelFilter;

    /**
     * packet forwarding filter
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> packetFilter;

    /**
     * data plane qos
     */
    public tabQos dapp;

    /**
     * flowspec qos
     */
    public tabQos flowspec;

    /**
     * receive control plane qos
     */
    public tabQos coppIn;

    /**
     * transmit control plane qos
     */
    public tabQos coppOut;

    /**
     * import list
     */
    public tabListing<tabPrfxlstN, addrIP> importList;

    /**
     * export list
     */
    public tabListing<tabPrfxlstN, addrIP> exportList;

    /**
     * import map
     */
    public tabListing<tabRtrmapN, addrIP> importMap;

    /**
     * export map
     */
    public tabListing<tabRtrmapN, addrIP> exportMap;

    /**
     * import policy
     */
    public tabListing<tabRtrplcN, addrIP> importPol;

    /**
     * export policy
     */
    public tabListing<tabRtrplcN, addrIP> exportPol;

    /**
     * time when recompute automatically
     */
    public int untriggeredRecomputation = 120 * 1000;

    /**
     * incremental limit
     */
    public int incrLimit = 1000;

    /**
     * notify when table changed
     */
    public notifier tableChanged;

    /**
     * common label
     */
    public tabLabelEntry commonLabel;

    /**
     * ip core to use
     */
    protected final ipCor ipCore;

    /**
     * icmp core to use
     */
    public final ipIcmp icmpCore;

    /**
     * igmp/mmld core to use
     */
    public final ipMhost mhostCore;

    /**
     * next interface number
     */
    public int nextIfaceNumber = bits.randomD();

    /**
     * candidate for incrementals
     */
    protected boolean incrCandid;

    /**
     * should compute tables
     */
    protected final notifier triggerUpdate;

    /**
     * need full round
     */
    protected final syncInt needFull;

    /**
     * the changed unicast routes
     */
    protected final tabRoute<addrIP> changedUni;

    /**
     * the changed multicast routes
     */
    protected final tabRoute<addrIP> changedMlt;

    /**
     * the changed flowspec routes
     */
    protected final tabRoute<addrIP> changedFlw;

    private static int nextVrfNumber = bits.randomD();

    private static int nextRouterNumber = bits.randomD();

    private int nextEchoNumber = bits.randomD();

    /**
     * label modes
     */
    public enum labelMode {

        /**
         * common label for vrf
         */
        common,
        /**
         * label for connected routes
         */
        conn,
        /**
         * label for host routes
         */
        host,
        /**
         * label for igp prefixes
         */
        igp,
        /**
         * label for all prefix
         */
        all

    }

    /**
     * mdt modes
     */
    public enum mdtMode {

        /**
         * mldp mode
         */
        mldp,
        /**
         * bier mode
         */
        bier,
        /**
         * no mdt mode
         */
        none

    }

    public String toString() {
        return vrfName;
    }

    public int compareTo(ipFwd o) {
        if (vrfNum < o.vrfNum) {
            return -1;
        }
        if (vrfNum > o.vrfNum) {
            return +1;
        }
        if (ipVersion < o.ipVersion) {
            return -1;
        }
        if (ipVersion > o.ipVersion) {
            return +1;
        }
        return 0;
    }

    /**
     * the constructor of vrf
     *
     * @param ipc handler of ip core
     * @param icc handler of icmp core
     * @param mhst handler of igmp/mdl core
     * @param cfg configured name of this vrf
     * @param nam name of this vrf
     */
    public ipFwd(ipCor ipc, ipIcmp icc, ipMhost mhst, String cfg, String nam) {
        nextVrfNumber = (nextVrfNumber & 0x3fffffff) + 1;
        cfgName = cfg;
        vrfName = nam;
        vrfNum = nextVrfNumber + 10000;
        ipCore = ipc;
        icmpCore = icc;
        mhostCore = mhst;
        ipVersion = ipCore.getVersion();
        commonLabel = tabLabel.allocate(tabLabelEntry.owner.vrfComm);
        echoes = new tabGen<ipFwdEcho>();
        trafEngs = new tabGen<ipFwdTrfng>();
        mp2mpLsp = new tabGen<ipFwdMpmp>();
        ifaces = new tabGen<ipFwdIface>();
        groups = new tabGen<ipFwdMcast>();
        protos = new tabConnect<addrIP, ipFwdProto>(new addrIP(), "protocols");
        routers = new tabGen<ipRtr>();
        ldpNeighs = new tabGen<rtrLdpNeigh>();
        ldpTarget = new tabGen<rtrLdpTrgtd>();
        autoMesh = new tabGen<clntMplsTeP2p>();
        connedR = new tabRoute<addrIP>("conn");
        directR = new tabRoute<addrIP>("direct");
        labeldR = new tabRoute<addrIP>("labeled");
        actualU = new tabRoute<addrIP>("computed");
        actualM = new tabRoute<addrIP>("computed");
        actualF = new tabRoute<addrIP>("computed");
        actualIU = new tabGen<tabIndex<addrIP>>();
        actualIC = new tabGen<tabIndex<addrIP>>();
        staticU = new tabGen<ipFwdRoute>();
        staticM = new tabGen<ipFwdRoute>();
        natTrns = new tabGen<tabNatTraN>();
        pbrCfg = new tabListing<tabPbrN, addrIP>();
        pbrCfg.myCor = ipCore;
        pbrCfg.myIcmp = icc;
        natCfg = new tabListing<tabNatCfgN, addrIP>();
        natCfg.myCor = ipCore;
        natCfg.myIcmp = icc;
        changedUni = new tabRoute<addrIP>("chg");
        changedMlt = new tabRoute<addrIP>("chg");
        changedFlw = new tabRoute<addrIP>("chg");
        cntrH = new counter();
        hstryH = new history();
        cntrT = new counter();
        hstryT = new history();
        cntrL = new counter();
        hstryL = new history();
        cntrR = new counter();
        hstryR = new history();
        needFull = new syncInt(3);
        triggerUpdate = new notifier();
        ipFwdTab.updateEverything(this);
        icc.setForwarder(this);
        mhst.setForwarder(this, icc);
    }

    /**
     * update vrf history
     */
    public void alertHistory() {
        String a = hstryT.threshold(thresholdT);
        if (a != null) {
            logger.info("vrf " + vrfName + " traffic " + a);
        }
        a = hstryH.threshold(thresholdT);
        if (a != null) {
            logger.info("vrf " + vrfName + " hwtraffic " + a);
        }
        a = hstryR.threshold(thresholdR);
        if (a != null) {
            logger.info("vrf " + vrfName + " routes " + a);
        }
    }

    /**
     * update vrf history
     */
    public void updateHistory() {
        hstryH.update(cntrH, true);
        hstryT.update(cntrT, true);
        hstryL.update(cntrL, true);
        cntrR.byteRx = actualU.size() / 8;
        cntrR.byteTx = actualM.size() / 8;
        cntrR.byteDr = actualF.size() / 8;
        cntrR.packRx = labeldR.size() / 8;
        cntrR.packTx = natTrns.size() / 8;
        cntrR.packDr = groups.size() / 8;
        hstryR.update(cntrR, false);
    }

    /**
     * start this vrf now
     */
    public void startThisVrf() {
        new Thread(this).start();
    }

    /**
     * stop this routing table completely
     */
    public void stopThisVrf() {
        if (debugger.ipFwdEvnt) {
            logger.debug("stop vrf");
        }
        untriggeredRecomputation = -1;
        needFull.or(3);
        triggerUpdate.wakeup();
        prefixMode = labelMode.common;
        for (int i = 0; i < labeldR.size(); i++) {
            tabRouteEntry<addrIP> ntry = labeldR.get(i);
            if (ntry == null) {
                continue;
            }
            tabLabel.release(ntry.best.labelLoc, tabLabelEntry.owner.vrfUni);
        }
        for (int i = 0; i < trafEngs.size(); i++) {
            ipFwdTrfng ntry = trafEngs.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.labStop();
        }
        for (int i = 0; i < mp2mpLsp.size(); i++) {
            ipFwdMpmp ntry = mp2mpLsp.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.stopLabels();
        }
        for (int i = ldpNeighs.size() - 1; i >= 0; i--) {
            rtrLdpNeigh ntry = ldpNeighs.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.stopPeer();
        }
        for (int i = ldpTarget.size() - 1; i >= 0; i--) {
            rtrLdpTrgtd ntry = ldpTarget.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.workStop();
        }
        for (int i = routers.size() - 1; i >= 0; i--) {
            ipRtr rtr = routers.get(i);
            if (rtr == null) {
                continue;
            }
            routerDel(rtr);
        }
        for (int i = 0; i < autoMesh.size(); i++) {
            clntMplsTeP2p ntry = autoMesh.get(i);
            if (ntry == null) {
                continue;
            }
            ntry.workStop();
        }
        tabLabel.release(commonLabel, tabLabelEntry.owner.vrfComm);
        for (int i = ifaces.size() - 1; i >= 0; i--) {
            ipFwdIface ifc = ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            ifaceDel(ifc);
        }
    }

    /**
     * wake up when tables changed
     */
    public void tableChanger() {
        if (tableChanged == null) {
            return;
        }
        tableChanged.wakeup();
    }

    /**
     * find one ldp neighbor
     *
     * @param addr peer address
     * @param create create if not yet
     * @return found neighbor, null if nothing
     */
    public rtrLdpNeigh ldpNeighFind(addrIP addr, boolean create) {
        rtrLdpNeigh ntry = new rtrLdpNeigh(addr);
        if (!create) {
            return ldpNeighs.find(ntry);
        }
        ntry.ip = this;
        rtrLdpNeigh old = ldpNeighs.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one ldp neighbor
     *
     * @param ntry entry to delete
     */
    public void ldpNeighDel(rtrLdpNeigh ntry) {
        if (debugger.ipFwdEvnt) {
            logger.debug("delete ldp " + ntry);
        }
        rtrLdpNeigh old = ldpNeighs.del(ntry);
        if (old != null) {
            old.stopPeer();
        }
        ntry.stopPeer();
        for (int i = 0; i < ntry.pmpLearn.size(); i++) {
            ipFwdMpmp mp = ntry.pmpLearn.get(i);
            if (mp == null) {
                continue;
            }
            mp.delPeer(ntry.peer);
            mp.updateState(this);
        }
        for (int i = 0; i < mp2mpLsp.size(); i++) {
            ipFwdMpmp mp = mp2mpLsp.get(i);
            if (mp == null) {
                continue;
            }
            if (mp.delPeer(ntry.peer)) {
                continue;
            }
            mp.updateState(this);
        }
        needFull.or(1);
        triggerUpdate.wakeup();
    }

    /**
     * find one ldp targeted
     *
     * @param iface receiving interface
     * @param ldpi ldp interface
     * @param addr peer address
     * @param create create if not yet
     * @return found neighbor, null if nothing
     */
    public rtrLdpTrgtd ldpTargetFind(ipFwdIface iface, rtrLdpIface ldpi, addrIP addr, boolean create) {
        iface = ifaces.find(iface);
        if (iface == null) {
            return null;
        }
        rtrLdpTrgtd ntry = new rtrLdpTrgtd(addr);
        if (!create) {
            return ldpTarget.find(ntry);
        }
        ntry.ifc = iface;
        ntry.ip = this;
        if (ldpi == null) {
            ldpi = new rtrLdpIface(null, null, null, null, null, null);
        }
        ntry.ldp = ldpi;
        rtrLdpTrgtd old = ldpTarget.add(ntry);
        if (old != null) {
            return old;
        }
        return ntry;
    }

    /**
     * delete one ldp targeted
     *
     * @param ntry entry to delete
     */
    public void ldpTargetDel(rtrLdpTrgtd ntry) {
        if (debugger.ipFwdEvnt) {
            logger.debug("delete ldp " + ntry);
        }
        ntry = ldpTarget.del(ntry);
        if (ntry != null) {
            ntry.workStop();
        }
        needFull.or(1);
        triggerUpdate.wakeup();
    }

    /**
     * add label to flood list
     *
     * @param grp group to flood
     * @param src source of group
     * @param trg label to flood
     */
    public void mcastAddFloodMpls(addrIP grp, addrIP src, ipFwdMpmp trg) {
        ipFwdMcast g = new ipFwdMcast(grp, src);
        if (trg == null) {
            ipFwdMcast og = groups.find(g);
            if (og == null) {
                return;
            }
            og.label = null;
            return;
        }
        ipFwdMcast og = groups.add(g);
        if (og != null) {
            g = og;
        } else {
            ipFwdTab.updateOneGroup(this, g);
            ipFwdTab.joinOneGroup(this, g, 1);
        }
        g.label = trg;
    }

    /**
     * add bier peer to flood list
     *
     * @param grp group to flood
     * @param src source of group
     * @param lab vpn label
     * @param fwd forwarder
     * @param trg peer address
     * @param id local node id
     * @param exp expiration time, negative if not expires
     */
    public void mcastAddFloodBier(addrIP grp, addrIP src, int lab, ipFwd fwd, addrIP trg, int id, long exp) {
        ipFwdMcast g = new ipFwdMcast(grp, src);
        ipFwdMcast og = groups.add(g);
        if (og != null) {
            g = og;
        } else {
            ipFwdTab.updateOneGroup(this, g);
            ipFwdTab.joinOneGroup(this, g, 1);
        }
        ipFwdBier ntry = g.bier;
        if (ntry == null) {
            ntry = new ipFwdBier(id);
            g.bier = ntry;
        }
        ntry.addPeer(fwd, trg, lab, exp);
        ntry.updatePeers();
        tableChanger();
    }

    /**
     * del bier peer from flood list
     *
     * @param grp group to flood
     * @param src source of group
     * @param lab vpn label
     * @param fwd forwarder
     * @param trg peer address
     */
    public void mcastDelFloodBier(addrIP grp, addrIP src, int lab, ipFwd fwd, addrIP trg) {
        ipFwdMcast g = new ipFwdMcast(grp, src);
        g = groups.find(g);
        if (g == null) {
            return;
        }
        if (g.bier == null) {
            return;
        }
        g.bier.delPeer(fwd, trg, lab);
        g.bier.updatePeers();
        tableChanger();
    }

    /**
     * add interface to flood list
     *
     * @param grp group to flood
     * @param src source of group
     * @param ifc interface to add, null=local
     * @param exp expiration time, negative if not expires, -1=static,
     * -2=globalCfg, -3=ifaceCfg
     */
    public void mcastAddFloodIfc(addrIP grp, addrIP src, ipFwdIface ifc, long exp) {
        if (exp > 0) {
            exp += bits.getTime();
        }
        ipFwdMcast g = new ipFwdMcast(grp, src);
        g.local = ifc == null;
        ipFwdMcast og = groups.add(g);
        if (og != null) {
            g = og;
        } else {
            ipFwdTab.updateOneGroup(this, g);
            ipFwdTab.joinOneGroup(this, g, 1);
        }
        g.configG = exp == -2;
        g.configI = exp == -3;
        if (ifc == null) {
            g.local = true;
            return;
        }
        ipFwdIface oi = g.flood.add(ifc);
        tableChanger();
        if (oi != null) {
            ifc = oi;
        }
        if (ifc.expires < 0) {
            return;
        }
        ifc.expires = exp;
    }

    /**
     * del interface from flood list
     *
     * @param grp group to flood
     * @param src source of group
     * @param ifc interface to add, null=local
     */
    public void mcastDelFloodIfc(addrIP grp, addrIP src, ipFwdIface ifc) {
        ipFwdMcast g = new ipFwdMcast(grp, src);
        g = groups.find(g);
        if (g == null) {
            return;
        }
        if (ifc == null) {
            g.local = false;
            g.configG = false;
            return;
        }
        g.configI = false;
        g.flood.del(ifc);
        tableChanger();
    }

    /**
     * add local tunnel
     *
     * @param ntry tunnel entry
     * @param p2mp set true for point to multipoint
     */
    public void tetunAdd(ipFwdTrfng ntry, boolean p2mp) {
        ntry.srcLoc = 1;
        ntry.trgLab = -1;
        ntry.timeout = 3 * untriggeredRecomputation;
        if (p2mp) {
            trafEngs.put(ntry);
            ipFwdTab.refreshTrfngAdd(this, ntry);
            return;
        }
        for (;;) {
            ntry.srcId = bits.randomW();
            if (trafEngs.add(ntry) == null) {
                break;
            }
        }
        ipFwdTab.refreshTrfngAdd(this, ntry);
    }

    /**
     * del local tunnel
     *
     * @param ntry tunnel entry
     */
    public void tetunDel(ipFwdTrfng ntry) {
        ntry = trafEngs.del(ntry);
        if (ntry == null) {
            return;
        }
        if (ntry.srcLoc == 0) {
            return;
        }
        ntry.srcLoc = 2;
        ntry.labStop();
        ipFwdTab.refreshTrfngDel(this, ntry);
    }

    /**
     * refresh local tunnel
     *
     * @param ntry tunnel entry
     */
    public void tetunSignal(ipFwdTrfng ntry) {
        ntry = trafEngs.find(ntry);
        if (ntry == null) {
            return;
        }
        if (ntry.srcLoc == 0) {
            return;
        }
        ipFwdTab.refreshTrfngAdd(this, ntry);
    }

    /**
     * add mldp tunnel
     *
     * @param ntry tunnel entry
     */
    public void mldpAdd(ipFwdMpmp ntry) {
        ntry.local = true;
        ipFwdMpmp old = mp2mpLsp.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.local = true;
        ntry.updateState(this);
    }

    /**
     * del mldp tunnel
     *
     * @param ntry tunnel entry
     */
    public void mldpDel(ipFwdMpmp ntry) {
        ntry = mp2mpLsp.find(ntry);
        if (ntry == null) {
            return;
        }
        ntry.local = false;
        ntry.updateState(this);
    }

    /**
     * del static route
     *
     * @param uni true=unicast, false=multicast
     * @param rou route
     */
    public void staticDel(boolean uni, ipFwdRoute rou) {
        if (debugger.ipFwdEvnt) {
            logger.debug("delete static " + rou);
        }
        if (uni) {
            rou = staticU.del(rou);
        } else {
            rou = staticM.del(rou);
        }
        if (rou == null) {
            return;
        }
        if (rou.track != null) {
            rou.track.clients.del(this);
        }
        needFull.or(1);
        triggerUpdate.wakeup();
    }

    /**
     * add static route
     *
     * @param uni true=unicast, false=multicast
     * @param rou route
     */
    public void staticAdd(boolean uni, ipFwdRoute rou) {
        if (debugger.ipFwdEvnt) {
            logger.debug("install static " + rou);
        }
        rou.fwdCor = this;
        if (uni) {
            staticU.add(rou);
        } else {
            staticM.add(rou);
        }
        if (rou.track != null) {
            rou.track.clients.add(this);
        }
        needFull.or(1);
        triggerUpdate.wakeup();
    }

    /**
     * add one interface
     *
     * @param lower interface to add
     * @return interface handler
     */
    public ipFwdIface ifaceAdd(ipIfc lower) {
        if (debugger.ipFwdEvnt) {
            logger.debug("add ifc " + lower);
        }
        ipFwdIface ntry;
        for (;;) {
            nextIfaceNumber = (nextIfaceNumber & 0x3fffffff) + 1;
            ntry = new ipFwdIface(nextIfaceNumber + 10000, lower);
            ntry.addr = new addrIP();
            ntry.network = new addrPrefix<addrIP>(ntry.addr, ntry.addr.maxBits());
            ntry.ready = lower.getState() == state.states.up;
            ntry.mtu = lower.getMTUsize() - ipCore.getHeaderSize();
            ntry.bandwidth = lower.getBandwidth();
            if (ifaces.add(ntry) == null) {
                break;
            }
        }
        lower.setUpper(this, ntry);
        needFull.or(1);
        triggerUpdate.wakeup();
        return ntry;
    }

    /**
     * delete one interface
     *
     * @param ifc interface handler
     */
    public void ifaceDel(ipFwdIface ifc) {
        ifc = ifaces.del(ifc);
        if (ifc == null) {
            return;
        }
        if (debugger.ipFwdEvnt) {
            logger.debug("del ifc " + ifc.lower);
        }
        ifc.ready = false;
        for (;;) {
            ipFwdProto prt = protos.delNext(ifc, null, 0, 0);
            if (prt == null) {
                break;
            }
            prt.upper.closeUp(ifc);
        }
        for (int i = 0; i < groups.size(); i++) {
            ipFwdMcast grp = groups.get(i);
            if (grp == null) {
                continue;
            }
            grp.flood.del(ifc);
        }
        needFull.or(1);
        triggerUpdate.wakeup();
    }

    /**
     * change interface state
     *
     * @param ifc interface handler
     * @param stat new status of interface
     */
    public void ifaceState(ipFwdIface ifc, state.states stat) {
        ifc.cntr.stateChange(stat);
        boolean old = ifc.ready;
        ifc.ready = (stat == state.states.up);
        ifc.mtu = ifc.lower.getMTUsize() - ipCore.getHeaderSize();
        ifc.bandwidth = ifc.lower.getBandwidth();
        if (old == ifc.ready) {
            return;
        }
        if (debugger.ipFwdEvnt) {
            logger.debug("iface state " + ifc.ready + " " + ifc.lower);
        }
        for (int i = protos.size() - 1; i >= 0; i--) {
            ipFwdProto prt = protos.get(i);
            if (prt == null) {
                continue;
            }
            if ((prt.iface == null) || (prt.iface == ifc)) {
                prt.upper.setState(ifc, stat);
            }
        }
        needFull.or(1);
        triggerUpdate.wakeup();
    }

    /**
     * change interface address
     *
     * @param ifc interface handler
     * @param addr new address
     * @param mask net netmask
     */
    public void ifaceAddr(ipFwdIface ifc, addrIP addr, int mask) {
        if (debugger.ipFwdEvnt) {
            logger.debug("iface addr " + addr + " " + mask);
        }
        ifc.addr = addr.copyBytes();
        ifc.mask = mask;
        ifc.network = new addrPrefix<addrIP>(addr, mask);
        needFull.or(1);
        triggerUpdate.wakeup();
    }

    private void ifaceProto(ipFwdIface lower, packHolder pck, addrIP hop) {
        cntrT.tx(pck);
        if (!lower.ready) {
            lower.cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        pck.putStart();
        if (hop == null) {
            hop = pck.IPtrg;
            if (hop == null) {
                lower.cntr.drop(pck, counter.reasons.badAddr);
                return;
            }
        }
        if (lower.blockHost2host && (pck.INTupper == 0)) {
            if (pck.INTiface == lower.ifwNum) {
                lower.cntr.drop(pck, counter.reasons.denied);
                return;
            }
        }
        if (lower.pmtudOut > 0) {
            if (pck.dataSize() > lower.pmtudOut) {
                doDrop(pck, lower, counter.reasons.fragment, lower.pmtudOut);
                return;
            }
        }
        if (lower.cfilterOut != null) {
            if (!lower.cfilterOut.matches(false, true, pck)) {
                doDrop(pck, lower, counter.reasons.denied, 0);
                return;
            }
        }
        if (lower.filterOut != null) {
            if (!lower.filterOut.matches(false, true, pck)) {
                doDrop(pck, lower, counter.reasons.denied, 0);
                return;
            }
        }
        if (lower.inspect != null) {
            if (lower.inspect.doPack(pck, true)) {
                doDrop(pck, lower, counter.reasons.denied, 0);
                return;
            }
        }
        lower.cntr.tx(pck);
        if ((netflow != null) && lower.netflowTx) {
            netflow.session.doPack(pck, true);
        }
        if (lower.tcpMssOut > 0) {
            ifaceAdjustMss(pck, lower.tcpMssOut);
        }
        if (debugger.ipFwdTraf) {
            logger.debug("tx " + pck.IPsrc + " -> " + pck.IPtrg + " hop=" + hop + " pr=" + pck.IPprt + " tos=" + pck.IPtos);
        }
        lower.lower.sendProto(pck, hop);
    }

    private void ifaceMpls(ipFwdIface lower, packHolder pck, addrIP hop) {
        cntrT.tx(pck);
        if (!lower.ready) {
            lower.cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        pck.putStart();
        if (hop == null) {
            lower.cntr.drop(pck, counter.reasons.badAddr);
            return;
        }
        if (debugger.ipFwdTraf) {
            logger.debug("tx label=" + hop);
        }
        lower.lower.sendMpls(pck, hop);
    }

    private void ifaceAdjustMss(packHolder pck, int mss) {
        if (pck.IPprt != prtTcp.protoNum) {
            return;
        }
        pck.getSkip(pck.IPsiz);
        prtTcp.parseTCPports(pck);
        if ((pck.TCPflg & prtTcp.flagSYN) == 0) {
            pck.getSkip(-pck.IPsiz);
            return;
        }
        prtTcp.updateTCPheader(pck, pck.UDPsrc, pck.UDPtrg, -1, -1, mss);
        pck.getSkip(-pck.IPsiz);
        ipCore.updateIPheader(pck, pck.IPsrc, pck.IPtrg, -1, -1, -1, -1, pck.UDPsiz);
    }

    /**
     * interface signals that it got a packet
     *
     * @param lower interface handler
     * @param pck packet to process
     */
    public void ifacePack(ipFwdIface lower, packHolder pck) {
        if (lower == null) {
            cntrT.drop(pck, counter.reasons.noIface);
            return;
        }
        if (!lower.ready) {
            lower.cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        if (ipCore.parseIPheader(pck, true)) {
            lower.cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        if (lower.protocolSecurity) {
            if (ipFwdTab.safeProtocol(pck.IPprt)) {
                doDrop(pck, lower, counter.reasons.denied, 0);
                return;
            }
        }
        if (lower.pmtudIn > 0) {
            if (pck.dataSize() > lower.pmtudIn) {
                doDrop(pck, lower, counter.reasons.fragment, lower.pmtudIn);
                return;
            }
        }
        if (lower.cfilterIn != null) {
            if (!lower.cfilterIn.matches(false, true, pck)) {
                doDrop(pck, lower, counter.reasons.denied, 0);
                return;
            }
        }
        if (lower.filterIn != null) {
            if (!lower.filterIn.matches(false, true, pck)) {
                doDrop(pck, lower, counter.reasons.denied, 0);
                return;
            }
        }
        pck.putStart();
        pck.INTiface = lower.ifwNum;
        if (lower.verifySource && !pck.IPlnk && !pck.IPbrd) {
            tabRouteEntry<addrIP> prf = actualU.route(pck.IPsrc);
            if (prf == null) {
                lower.cntr.drop(pck, counter.reasons.denied);
                return;
            }
            if ((lower.verifyStricht) && (prf.best.iface != lower)) {
                lower.cntr.drop(pck, counter.reasons.denied);
                return;
            }
            prf.cntr.rx(pck);
        }
        if (lower.inspect != null) {
            if (lower.inspect.doPack(pck, false)) {
                lower.cntr.drop(pck, counter.reasons.denied);
                return;
            }
        }
        if ((netflow != null) && lower.netflowRx) {
            netflow.session.doPack(pck, false);
        }
        if (debugger.ipFwdTraf) {
            logger.debug("rx " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt + " tos=" + pck.IPtos);
        }
        if (lower.tcpMssIn > 0) {
            ifaceAdjustMss(pck, lower.tcpMssIn);
        }
        pck.INTupper = 0;
        ipMpls.beginMPLSfields(pck, (mplsPropTtl | lower.mplsPropTtlAlways) & lower.mplsPropTtlAllow);
        if (doPbrFwd(lower.pbrCfg, 1, lower, pck)) {
            return;
        }
        forwardPacket(1, lower, null, pck);
    }

    /**
     * add protocol to interface
     *
     * @param upper protocol handler
     * @param ifc interface handle, null for all
     * @param trg target address, null means all
     * @return true if error happened, false if success
     */
    public boolean protoAdd(ipPrt upper, ipFwdIface ifc, addrIP trg) {
        if (debugger.ipFwdEvnt) {
            logger.debug("add proto=" + upper + " iface=" + ifc + " trg=" + trg);
        }
        ipFwdProto ntry = new ipFwdProto();
        ntry.proto = upper.getProtoNum();
        ntry.iface = ifc;
        ntry.upper = upper;
        return protos.add(ifc, trg, ntry.proto, ntry.proto, ntry, "" + upper);
    }

    /**
     * delete protocol from interface
     *
     * @param upper protocol handler
     * @param ifc interface handle, null for all
     * @param trg target address, null means all
     */
    public void protoDel(ipPrt upper, ipFwdIface ifc, addrIP trg) {
        if (debugger.ipFwdEvnt) {
            logger.debug("del proto=" + upper + " iface=" + ifc + " trg=" + trg);
        }
        int i = upper.getProtoNum();
        for (;;) {
            ipFwdProto prt = protos.delNext(ifc, trg, i, i);
            if (prt == null) {
                break;
            }
        }
    }

    private void protoSend(ipFwdIface lower, packHolder pck) {
        cntrL.rx(pck);
        if (pck.IPmf || (pck.IPfrg != 0)) {
            if (lower.reasmBuf == null) {
                doDrop(pck, lower, counter.reasons.reassembly, 0);
                return;
            }
            if (debugger.ipFwdReasm) {
                logger.debug("reasm " + pck.IPsrc + " -> " + pck.IPtrg + " id=" + pck.IPid + " ofs=" + pck.IPfrg + " mf=" + pck.IPmf);
            }
            lower.frgCnt.rx(pck);
            int o = -1;
            for (int i = 0; i < lower.reasmBuf.size(); i++) {
                packHolder asm = lower.reasmBuf.get(i);
                if (asm.IPid != pck.IPid) {
                    continue;
                }
                if (asm.IPsrc.compareTo(pck.IPsrc) != 0) {
                    continue;
                }
                if (asm.IPtrg.compareTo(pck.IPtrg) != 0) {
                    continue;
                }
                o = i;
                break;
            }
            if (o < 0) {
                if (pck.IPfrg != 0) {
                    return;
                }
                lower.reasmNxt = (lower.reasmNxt + 1) % lower.reasmBuf.size();
                packHolder asm = lower.reasmBuf.get(lower.reasmNxt);
                asm.copyFrom(pck, true, true);
                return;
            }
            packHolder asm = lower.reasmBuf.get(o);
            if (pck.IPfrg != (asm.dataSize() - asm.IPsiz)) {
                asm.clear();
                return;
            }
            if ((pck.IPfrg + pck.dataSize()) >= packHolder.maxHead) {
                asm.clear();
                return;
            }
            pck.getSkip(pck.IPsiz);
            byte[] buf = pck.getCopy();
            asm.putCopy(buf, 0, 0, buf.length);
            asm.putSkip(buf.length);
            asm.merge2end();
            if (pck.IPmf) {
                return;
            }
            pck.copyFrom(asm, true, true);
            pck.IPmf = false;
            pck.IPfrg = 0;
            asm.clear();
        }
        if (coppIn != null) {
            if (coppIn.checkPacket(pck)) {
                cntrL.drop(pck, counter.reasons.noBuffer);
                return;
            }
        }
        if (debugger.ipFwdTraf) {
            logger.debug("rcv " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt + " tos=" + pck.IPtos);
        }
        ipFwdProto prt = null;
        if (prt == null) {
            prt = protos.get(lower, pck.IPsrc, pck.IPprt, pck.IPprt);
        }
        if (prt == null) {
            prt = protos.get(null, pck.IPsrc, pck.IPprt, pck.IPprt);
        }
        if (prt == null) {
            prt = protos.get(lower, null, pck.IPprt, pck.IPprt);
        }
        if (prt == null) {
            prt = protos.get(null, null, pck.IPprt, pck.IPprt);
        }
        if (prt == null) {
            doDrop(pck, lower, counter.reasons.badProto, 0);
            return;
        }
        pck.getSkip(pck.IPsiz);
        prt.upper.recvPack(lower, pck);
    }

    private boolean protoAlert(ipFwdIface lower, packHolder pck) {
        cntrL.rx(pck);
        if (pck.IPmf || (pck.IPfrg != 0)) {
            return true;
        }
        if (coppIn != null) {
            if (coppIn.checkPacket(pck)) {
                cntrL.drop(pck, counter.reasons.noBuffer);
                return false;
            }
        }
        if (debugger.ipFwdTraf) {
            logger.debug("alrt " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt + " tos=" + pck.IPtos);
        }
        ipFwdProto prt = null;
        if (prt == null) {
            prt = protos.get(lower, pck.IPsrc, pck.IPprt, pck.IPprt);
        }
        if (prt == null) {
            prt = protos.get(null, pck.IPsrc, pck.IPprt, pck.IPprt);
        }
        if (prt == null) {
            prt = protos.get(lower, null, pck.IPprt, pck.IPprt);
        }
        if (prt == null) {
            prt = protos.get(null, null, pck.IPprt, pck.IPprt);
        }
        if (prt == null) {
            return true;
        }
        pck.getSkip(pck.IPsiz);
        boolean b = prt.upper.alertPack(lower, pck);
        if (b) {
            pck.getSkip(-pck.IPsiz);
            return true;
        }
        return false;
    }

    /**
     * protocol wants to create one packet
     *
     * @param pck packet to update
     */
    public void createIPheader(packHolder pck) {
        pck.INTiface = -1;
        pck.INTupper = pck.IPprt;
        pck.merge2beg();
        ipCore.createIPheader(pck);
        if (debugger.ipFwdTraf) {
            logger.debug("snd " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt + " tos=" + pck.IPtos);
        }
        ipMpls.beginMPLSfields(pck, mplsPropTtl);
    }

    /**
     * protocol wants to update one packet
     *
     * @param pck packet to update
     * @param src new source address, null=don't set
     * @param trg new target address, null=don't set
     * @param prt new protocol value, -1=dont set
     * @param ttl new ttl value, -1=dont set, -2=decrement
     * @param tos new tos value, -1=dont set
     * @param id new flow value, -1=dont set
     * @param len new payload length, -1=dont set
     */
    public void updateIPheader(packHolder pck, addrIP src, addrIP trg, int prt, int ttl, int tos, int id, int len) {
        pck.INTiface = -1;
        pck.INTupper = pck.IPprt;
        ipCore.updateIPheader(pck, src, trg, prt, ttl, tos, id, len);
        ipMpls.beginMPLSfields(pck, mplsPropTtl);
    }

    /**
     * protocol wants to send one packet
     *
     * @param iface interface to use for source address
     * @param hop forced nexthop
     * @param pck packet to send
     */
    public void protoPack(ipFwdIface iface, addrIP hop, packHolder pck) {
        cntrL.tx(pck);
        if (iface == null) {
            cntrL.drop(pck, counter.reasons.noIface);
            return;
        }
        if (!iface.ready) {
            iface.cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        pck.INTiface = iface.ifwNum;
        pck.INTupper = pck.IPprt;
        pck.IPfrg = 0;
        pck.IPmf = false;
        pck.merge2beg();
        if (debugger.ipFwdTraf) {
            logger.debug("snd " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt + " tos=" + pck.IPtos);
        }
        if ((iface.fragments < 1) || (pck.dataSize() <= iface.fragments)) {
            ipCore.createIPheader(pck);
            if (coppOut != null) {
                if (coppOut.checkPacket(pck)) {
                    cntrL.drop(pck, counter.reasons.noBuffer);
                    return;
                }
            }
            ipMpls.beginMPLSfields(pck, (mplsPropTtl | iface.mplsPropTtlAlways) & iface.mplsPropTtlAllow);
            forwardPacket(4, iface, hop, pck);
            return;
        }
        if (pck.IPdf) {
            ipCore.createIPheader(pck);
            doDrop(pck, iface, counter.reasons.fragment, iface.fragments);
            return;
        }
        packHolder snd = new packHolder(true, true);
        byte[] buf = new byte[iface.fragments];
        int idn = bits.randomW();
        int ofs = 0;
        if (debugger.ipFwdFrag) {
            logger.debug("frag " + pck.IPsrc + " -> " + pck.IPtrg + " id=" + idn);
        }
        for (;;) {
            int len = pck.dataSize() - ofs;
            if (len < 1) {
                break;
            }
            if (len > buf.length) {
                len = buf.length;
            }
            pck.getCopy(buf, 0, ofs, len);
            snd.copyFrom(pck, false, false);
            snd.setDataSize(0);
            snd.putCopy(buf, 0, 0, len);
            snd.putSkip(len);
            snd.merge2beg();
            snd.IPfrg = ofs;
            ofs += len;
            snd.IPmf = ofs < pck.dataSize();
            snd.IPid = idn;
            ipCore.createIPheader(snd);
            if (coppOut != null) {
                if (coppOut.checkPacket(snd)) {
                    cntrL.drop(snd, counter.reasons.noBuffer);
                    return;
                }
            }
            ipMpls.beginMPLSfields(snd, (mplsPropTtl | iface.mplsPropTtlAlways) & iface.mplsPropTtlAllow);
            iface.frgCnt.tx(snd);
            forwardPacket(4, iface, hop, snd);
        }
    }

    /**
     * find one routing protocol
     *
     * @param typ route type that it provides
     * @param id process id
     * @return router process, null if not found
     */
    public ipRtr routerFind(tabRouteAttr.routeType typ, int id) {
        for (int i = 0; i < routers.size(); i++) {
            ipRtr ntry = routers.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.routerProtoTyp != typ) {
                continue;
            }
            if (ntry.routerProcNum != id) {
                continue;
            }
            return ntry;
        }
        return null;
    }

    /**
     * add one routing protocol
     *
     * @param rtr routing protocol handle
     * @param typ route type that it provides
     * @param id process id
     */
    public void routerAdd(ipRtr rtr, tabRouteAttr.routeType typ, int id) {
        if (debugger.ipFwdEvnt) {
            logger.debug("add rtr " + rtr);
        }
        nextRouterNumber = (nextRouterNumber & 0x3fffffff) + 1;
        rtr.routerProtoNum = nextRouterNumber + 10000;
        rtr.routerProtoTyp = typ;
        rtr.routerProcNum = id;
        routers.add(rtr);
        needFull.or(3);
        triggerUpdate.wakeup();
    }

    /**
     * delete one routing protocol
     *
     * @param rtr routing protocol handle
     */
    public void routerDel(ipRtr rtr) {
        if (debugger.ipFwdEvnt) {
            logger.debug("del rtr " + rtr);
        }
        if (routers.del(rtr) == null) {
            return;
        }
        needFull.or(3);
        triggerUpdate.wakeup();
        rtr.routerCloseNow();
    }

    /**
     * routing protocol notified that change happened
     *
     * @param rtr routing protocol handle
     * @param lab provided labels changed
     */
    public void routerChg(ipRtr rtr, boolean lab) {
        if (debugger.ipFwdEvnt) {
            logger.debug("chgd rtr " + rtr + " " + lab);
        }
        if (routers.find(rtr) == null) {
            return;
        }
        rtr.routerComputeChg++;
        rtr.routerComputeTim = bits.getTime();
        tabRoute<addrIP> chgU = rtr.routerChangedU;
        tabRoute<addrIP> chgM = rtr.routerChangedM;
        tabRoute<addrIP> chgF = rtr.routerChangedF;
        rtr.routerChangedU = null;
        rtr.routerChangedM = null;
        rtr.routerChangedF = null;
        if (lab) {
            needFull.or(1);
            triggerUpdate.wakeup();
            return;
        }
        if ((chgU != null) && (chgM != null) && (chgF != null)) {
            changedUni.mergeFrom(tabRoute.addType.always, chgU, tabRouteAttr.distanLim);
            changedMlt.mergeFrom(tabRoute.addType.always, chgM, tabRouteAttr.distanLim);
            changedFlw.mergeFrom(tabRoute.addType.always, chgF, tabRouteAttr.distanLim);
        } else {
            needFull.or(1);
        }
        triggerUpdate.wakeup();
    }

    /**
     * static route change happened
     */
    public void routerStaticChg() {
        if (debugger.ipFwdEvnt) {
            logger.debug("static changed");
        }
        needFull.or(1);
        triggerUpdate.wakeup();
    }

    /**
     * router config change happened
     */
    public void routerConfigChg() {
        if (debugger.ipFwdEvnt) {
            logger.debug("config changed");
        }
        needFull.or(3);
        triggerUpdate.wakeup();
    }

    /**
     * send unreachable
     *
     * @param pck packet to report
     * @param lower interface
     * @param reason reason
     * @param data data
     */
    public void doDrop(packHolder pck, ipFwdIface lower, counter.reasons reason, int data) {
        cntrT.drop(pck, reason);
        if (unreach != null) {
            if (unreach.check(1)) {
                return;
            }
        }
        if (debugger.ipFwdTraf) {
            logger.debug("drop " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt + " reason=" + counter.reason2string(reason));
        }
        if (pck.IPmlt || pck.IPbrd) {
            return;
        }
        if (lower == null) {
            return;
        }
        addrIP src = lower.getUnreachAddr();
        if (src == null) {
            return;
        }
        if (icmpCore.createError(pck, reason, data, src, mplsExtRep)) {
            return;
        }
        errorSent++;
        ipCore.createIPheader(pck);
        if (coppOut != null) {
            if (coppOut.checkPacket(pck)) {
                cntrL.drop(pck, counter.reasons.noBuffer);
                return;
            }
        }
        pck.INTupper = -1;
        ipMpls.beginMPLSfields(pck, (mplsPropTtl | lower.mplsPropTtlAlways) & lower.mplsPropTtlAllow);
        forwardPacket(4, lower, null, pck);
    }

    private void doMpls(ipFwdIface ifc, addrIP hop, List<Integer> labs, packHolder pck) {
        if (ifc == null) {
            cntrT.drop(pck, counter.reasons.noIface);
            return;
        }
        if (labs != null) {
            ipMpls.createMPLSlabels(pck, labs);
        }
        if (pck.MPLSbottom) {
            ifaceProto(ifc, pck, hop);
            return;
        }
        ifaceMpls(ifc, pck, hop);
    }

    /**
     * mpls signals that it got a packet
     *
     * @param fwd4 ipv4 forwarder
     * @param fwd6 ipv6 forwarder
     * @param fwdE ethernet forwarder
     * @param lab local label
     * @param pck packet to process
     */
    protected void mplsRxPack(ipFwd fwd4, ipFwd fwd6, ifcEthTyp fwdE, tabLabelEntry lab, packHolder pck) {
        if (debugger.ipFwdTraf) {
            logger.debug("rx label=" + lab.label);
        }
        if (lab.nextHop != null) {
            pck.MPLSttl--;
            if (pck.MPLSttl < 1) {
                if (ipMpls.createError(pck, lab, counter.reasons.ttlExceed, 0)) {
                    return;
                }
            }
            if ((lab.remoteLab == null) && (!pck.MPLSbottom)) {
                logger.info("no label for " + lab.label);
                cntrT.drop(pck, counter.reasons.notInTab);
                return;
            }
            doMpls(lab.iface, lab.nextHop, lab.remoteLab, pck);
            return;
        }
        if (lab.duplicate != null) {
            pck.MPLSttl--;
            if (pck.MPLSttl < 1) {
                if (ipMpls.createError(pck, lab, counter.reasons.ttlExceed, 0)) {
                    return;
                }
            }
            for (int i = 0; i < lab.duplicate.size(); i++) {
                tabLabelDup ntry = lab.duplicate.get(i);
                if (ntry == null) {
                    continue;
                }
                doMpls(ntry.iface, ntry.hop, ntry.label, pck.copyBytes(true, true));
            }
            if (!lab.needLocal) {
                return;
            }
        }
        if (lab.bier != null) {
            pck.MPLSttl--;
            if (pck.MPLSttl < 1) {
                if (ipMpls.createError(pck, lab, counter.reasons.ttlExceed, 0)) {
                    return;
                }
            }
            pck.BIERsi = lab.label - lab.bier.base;
            pck.BIERbsl = lab.bier.bsl;
            if (ipMpls.parseBIERheader(pck)) {
                logger.info("received invalid bier header on label " + lab.label);
                cntrT.drop(pck, counter.reasons.badHdr);
                return;
            }
            int bsl = tabLabelBier.bsl2num(lab.bier.bsl);
            int sis = bsl * pck.BIERsi;
            boolean nedLoc = tabLabelBier.untestMine(pck.BIERbs, bsl, lab.bier.idx - 1 - sis);
            nedLoc |= tabLabelBier.untestMine(pck.BIERbs, bsl, lab.bier.idx2 - 1 - sis);
            for (int i = 0; i < lab.bier.peers.size(); i++) {
                tabLabelBierN ntry = lab.bier.peers.get(i);
                if (ntry == null) {
                    continue;
                }
                packHolder p = pck.copyBytes(true, true);
                p.BIERbs = ntry.getAndShr(pck.BIERbs, sis);
                if (p.BIERbs == null) {
                    continue;
                }
                if (p.BIERbs.length < 1) {
                    continue;
                }
                ipMpls.createBIERheader(p);
                p.MPLSlabel = ntry.label + pck.BIERsi;
                ipMpls.createMPLSheader(p);
                doMpls(ntry.iface, ntry.hop, null, p);
            }
            if (nedLoc) {
                if (ipMpls.gotBierPck(fwd4, fwd6, fwdE, pck)) {
                    logger.info("received invalid bier protocol on label " + lab.label);
                }
            }
            return;
        }
        if (!pck.MPLSbottom) {
            cntrT.drop(pck, counter.reasons.badProto);
            return;
        }
        if (lab.pweIfc != null) {
            pck.getSkip(lab.pweDel);
            if (lab.pweAdd != null) {
                pck.putCopy(lab.pweAdd, 0, 0, lab.pweAdd.length);
                pck.putSkip(lab.pweAdd.length);
                pck.merge2beg();
            }
            lab.pweIfc.recvPack(pck);
            return;
        }
        if (ipCore.parseIPheader(pck, true)) {
            cntrT.drop(pck, counter.reasons.badHdr);
            return;
        }
        pck.INTiface = 0;
        pck.INTupper = -3;
        ipFwdIface ifc;
        if (pck.IPmlt || pck.IPbrd) {
            ifc = ipFwdTab.findStableIface(this);
        } else {
            ipFwd fwd = lab.forwarder;
            if (fwd == null) {
                cntrT.drop(pck, counter.reasons.notInTab);
                return;
            }
            tabRouteEntry<addrIP> prf = fwd.actualU.route(pck.IPtrg);
            if (prf == null) {
                cntrT.drop(pck, counter.reasons.noRoute);
                return;
            }
            if (prf.best.rouTab != null) {
                prf = prf.best.rouTab.actualU.route(prf.best.nextHop);
                if (prf == null) {
                    cntrT.drop(pck, counter.reasons.noRoute);
                    return;
                }
            }
            ifc = (ipFwdIface) prf.best.iface;
        }
        if (ifc == null) {
            cntrT.drop(pck, counter.reasons.noIface);
            return;
        }
        forwardPacket(3, ifc, null, pck);
    }

    /**
     * protocol signals that it sends a packet over mpls
     *
     * @param trg target address
     * @param pck packet to process
     * @param req require labeled path
     */
    public void mplsTxPack(addrIP trg, packHolder pck, boolean req) {
        pck.IPtrg.setAddr(trg);
        tabRouteEntry<addrIP> prf = actualU.route(trg);
        if (prf == null) {
            cntrT.drop(pck, counter.reasons.noRoute);
            return;
        }
        if ((prf.best.labelRem == null) && (req)) {
            if (prf.best.rouTyp == tabRouteAttr.routeType.conn) {
                doMpls((ipFwdIface) prf.best.iface, trg, null, pck);
                return;
            }
            logger.info("no label for " + trg);
            cntrT.drop(pck, counter.reasons.notInTab);
            return;
        }
        if (prf.best.rouTab != null) {
            ipMpls.createMPLSlabels(pck, prf.best.labelRem);
            prf.best.rouTab.mplsTxPack(prf.best.nextHop, pck, true);
            return;
        }
        if (prf.best.nextHop != null) {
            trg = prf.best.nextHop;
        }
        doMpls((ipFwdIface) prf.best.iface, trg, prf.best.labelRem, pck);
    }

    /**
     * forwards one parsed packet by policy routing
     *
     * @param cfg config
     * @param from source
     * @param rxIfc receiving interface
     * @param pck packet
     * @return true if sent, false if not
     */
    private boolean doPbrFwd(tabListing<tabPbrN, addrIP> cfg, int from, ipFwdIface rxIfc, packHolder pck) {
        if (cfg.size() < 1) {
            return false;
        }
        cfg.packParse(false, true, true, pck);
        tabPbrN pbr = cfg.find(pck);
        if (pbr == null) {
            return false;
        }
        if ((pbr.setSp > 0) && (pbr.setSi > 0)) {
            if (ipVersion == ipCor4.protocolVersion) {
                pck.IPprt = ifcNshFwd.protIp4;
            } else {
                pck.IPprt = ifcNshFwd.protIp6;
            }
            pck.NSHttl = 63;
            pck.NSHmdt = 2;
            pck.NSHmdv = new byte[0];
            pck.NSHsp = pbr.setSp;
            pck.NSHsi = pbr.setSi;
            ipMpls.gotNshPack(pck);
            return true;
        }
        if (pbr.setIfc != null) {
            pck.INTiface = -2;
            ifaceProto(pbr.setIfc, pck, pbr.setHop);
            return true;
        }
        if (pbr.setHop == null) {
            pck.INTiface = -2;
            pbr.setVrf.forwardPacket(from, rxIfc, null, pck);
            return true;
        }
        tabRouteEntry<addrIP> ntry = pbr.setVrf.actualU.route(pbr.setHop);
        if (ntry == null) {
            return false;
        }
        if (ntry.best.iface == null) {
            return false;
        }
        pck.INTiface = -2;
        ifaceProto((ipFwdIface) ntry.best.iface, pck, pbr.setHop);
        return true;
    }

    /**
     * forwards one parsed packet
     *
     * @param from source, 1=ifc, 2=mpls, 4=proto
     * @param rxIfc receiving interface
     * @param hop target hop
     * @param pck packet
     */
    private void forwardPacket(int from, ipFwdIface rxIfc, addrIP hop, packHolder pck) {
        cntrT.rx(pck);
        pck.INTsent++;
        if (pck.INTsent > ifcEthTyp.loopMax) {
            ifcEthTyp.loopDrops++;
            cntrT.drop(pck, counter.reasons.tooLong);
            return;
        }
        if (rxIfc == null) {
            cntrT.drop(pck, counter.reasons.noIface);
            return;
        }
        if (packetFilter != null) {
            if (!packetFilter.matches(false, true, pck)) {
                doDrop(pck, rxIfc, counter.reasons.denied, 0);
                return;
            }
        }
        if (!rxIfc.disableDapp && (dapp != null)) {
            if (dapp.checkPacket(pck)) {
                cntrT.drop(pck, counter.reasons.noBuffer);
                return;
            }
        }
        if (!rxIfc.disableFlowspec && (flowspec != null)) {
            if (flowspec.checkPacket(pck)) {
                cntrT.drop(pck, counter.reasons.noBuffer);
                return;
            }
        }
        if (pck.IPdivert != null) {
            ipFwd dvrt = pck.IPdivert;
            pck.IPdivert = null;
            pck.INTiface = -2;
            dvrt.forwardPacket(from, rxIfc, hop, pck);
            return;
        }
        if (debugger.ipFwdTraf) {
            logger.debug("fwd " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt + " tos=" + pck.IPtos);
        }
        pck.ETHcos = (pck.IPtos >>> 5) & 7;
        pck.MPLSexp = pck.ETHcos;
        if ((natTrns.size() > 0) || (natCfg.size() > 0)) {
            if (pck.IPmf || (pck.IPfrg != 0)) {
                doDrop(pck, rxIfc, counter.reasons.reassembly, 0);
                return;
            }
            natCfg.packParse(false, true, true, pck);
            tabNatTraN natT = tabNatTraN.fromPack(pck);
            natT = natTrns.find(natT);
            if (natT != null) {
                long tim = bits.getTime();
                natT.lastUsed = tim;
                natT.reverse.lastUsed = tim;
                natT.updatePack(pck);
                natCfg.packUpdate(pck);
                if (debugger.ipFwdTraf) {
                    logger.debug("nat " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt + " tos=" + pck.IPtos);
                }
            } else {
                tabNatCfgN natC = natCfg.find(pck);
                if (natC != null) {
                    if (natC.maxSess > 0) {
                        if (natTrns.size() > natC.maxSess) {
                            cntrT.drop(pck, counter.reasons.noBuffer);
                            return;
                        }
                    }
                    if (natC.maxRate != null) {
                        if (natC.maxRate.checkPacket(pck)) {
                            cntrT.drop(pck, counter.reasons.noBuffer);
                            return;
                        }
                    }
                    natT = natC.createEntry(pck, icmpCore);
                    tabNatTraN natR = natT.reverseEntry();
                    if (natT.needDuplicateCheck()) {
                        boolean ok = true;
                        for (int i = 0; i < 16; i++) {
                            ok = (natTrns.find(natR) == null);
                            if (ok) {
                                break;
                            }
                            natT.pickRandomSrcPort(natR);
                        }
                        if (!ok) {
                            cntrT.drop(pck, counter.reasons.notInTab);
                            return;
                        }
                    }
                    natTrns.add(natT);
                    natTrns.add(natR);
                    natT.updatePack(pck);
                    natCfg.packUpdate(pck);
                    if (debugger.ipFwdTraf) {
                        logger.debug("nat " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt + " tos=" + pck.IPtos);
                    }
                    tableChanger();
                }
            }
        }
        boolean alerted = (pck.IPalrt != -1);
        pck.IPalrt = -1;
        if (doPbrFwd(pbrCfg, from, rxIfc, pck)) {
            return;
        }
        if (hop != null) {
            ifaceProto(rxIfc, pck, hop);
            return;
        }
        if (rxIfc.gateLoc) {
            if (rxIfc.lower.checkMyAddress(pck.IPtrg)) {
                protoSend(rxIfc, pck);
                return;
            }
            if (rxIfc.lower.checkMyAlias(pck.IPtrg) != null) {
                protoSend(rxIfc, pck);
                return;
            }
        }
        if (pck.IPlnk) {
            if ((from & 1) == 0) {
                ifaceProto(rxIfc, pck, null);
                return;
            }
            if (pck.IPmlt || pck.IPbrd) {
                protoSend(rxIfc, pck);
                return;
            }
            if ((from & 2) != 0) {
                ipFwdIface ifc = ipFwdTab.findMyaddrIface(this, pck.IPtrg);
                if (ifc != null) {
                    protoSend(ifc, pck);
                    return;
                }
            }
            doDrop(pck, rxIfc, counter.reasons.noRoute, 0);
            return;
        }
        if (pck.IPmlt || pck.IPbrd) {
            if (pck.IPbrd || !pck.IPmlr) {
                if ((from & 1) != 0) {
                    protoSend(rxIfc, pck);
                } else {
                    ifaceProto(rxIfc, pck, null);
                }
                return;
            }
            if (pck.IPttl < 2) {
                cntrT.drop(pck, counter.reasons.ttlExceed);
                return;
            }
            if (from != 4) {
                ipCore.updateIPheader(pck, null, null, -1, -2, -1, -1, -1);
            }
            ipFwdMcast grp = new ipFwdMcast(pck.IPtrg, pck.IPsrc);
            grp = groups.find(grp);
            if (grp == null) {
                cntrT.drop(pck, counter.reasons.badNet);
                return;
            }
            if ((from & 3) == 1) {
                if (grp.iface == null) {
                    cntrT.drop(pck, counter.reasons.noRoute);
                    return;
                }
                if (grp.iface.ifwNum != rxIfc.ifwNum) {
                    cntrT.drop(pck, counter.reasons.noRoute);
                    return;
                }
            }
            grp.cntr.tx(pck);
            for (int i = 0; i < grp.flood.size(); i++) {
                ipFwdIface ifc = grp.flood.get(i);
                if (ifc == null) {
                    continue;
                }
                if ((from & 3) == 1) {
                    if (grp.iface.ifwNum == ifc.ifwNum) {
                        continue;
                    }
                }
                if (pck.IPttl < ifc.mcastTtl) {
                    continue;
                }
                ifaceProto(ifc, pck.copyBytes(true, true), null);
            }
            if (grp.label != null) {
                grp.label.sendPack(this, pck);
            }
            if (grp.bier != null) {
                pck.ETHtype = rxIfc.lower.getEthtyp();
                grp.bier.sendPack(pck);
            }
            if (grp.local && ((from & 1) != 0)) {
                protoSend(rxIfc, pck);
            }
            return;
        }
        tabRouteEntry<addrIP> prf = actualU.route(pck.IPtrg);
        if (prf == null) {
            doDrop(pck, rxIfc, counter.reasons.noRoute, 0);
            return;
        }
        prf.cntr.tx(pck);
        if (prf.best.rouTab != null) {
            cntrT.tx(pck);
            if (prf.best.segrouPrf != null) {
                pck.putDefaults();
                pck.IPtrg.setAddr(prf.best.segrouPrf);
                pck.IPsrc.setAddr(prf.best.segrouPrf);
                pck.IPprt = ipCore.getProtocol();
                prf.best.rouTab.createIPheader(pck);
                ipMpls.beginMPLSfields(pck, false);
                prf.best.rouTab.forwardPacket(from, rxIfc, null, pck);
                return;
            }
            if (prf.best.labelRem == null) {
                doDrop(pck, rxIfc, counter.reasons.notInTab, 0);
                return;
            }
            ipMpls.createMPLSlabels(pck, prf.best.labelRem);
            prf.best.rouTab.mplsTxPack(prf.best.nextHop, pck, true);
            return;
        }
        if (prf.best.iface == null) {
            doDrop(pck, rxIfc, counter.reasons.noIface, 0);
            return;
        }
        ipFwdIface txIfc = (ipFwdIface) prf.best.iface;
        if (txIfc.gateLoc) {
            if (txIfc.lower.checkMyAddress(pck.IPtrg)) {
                protoSend(txIfc, pck);
                return;
            }
            if (txIfc.lower.checkMyAlias(pck.IPtrg) != null) {
                protoSend(txIfc, pck);
                return;
            }
        }
        if (txIfc.gatePrc) {
            protoSend(txIfc, pck);
            return;
        }
        if (((from & 1) != 0) && alerted) {
            if (!protoAlert(rxIfc, pck)) {
                return;
            }
        }
        if (from != 4) {
            if (pck.MPLSttl < 2) {
                doDrop(pck, rxIfc, counter.reasons.ttlExceed, 0);
                return;
            }
            if ((mplsPropTtl | txIfc.mplsPropTtlAlways) & txIfc.mplsPropTtlAllow) {
                ipCore.updateIPheader(pck, null, null, -1, pck.MPLSttl - 1, -1, -1, -1);
            } else {
                ipCore.updateIPheader(pck, null, null, -1, -2, -1, -1, -1);
            }
            pck.MPLSttl--;
        }
        if (prf.best.rouTyp == tabRouteAttr.routeType.conn) {
            ifaceProto(txIfc, pck, null);
            return;
        }
        if (alerted) {
            ifaceProto(txIfc, pck, prf.best.nextHop);
            return;
        }
        doMpls(txIfc, prf.best.nextHop, prf.best.labelRem, pck);
    }

    /**
     * got error report
     *
     * @param err error code
     * @param iface receiving interface
     * @param pck protocol packet
     */
    public void errorReport(counter.reasons err, ipFwdIface iface, packHolder pck) {
        errorRcvd++;
        int oldSiz = pck.UDPsiz;
        int oldPrt = pck.IPprt;
        addrIP rtr = pck.IPsrc.copyBytes();
        if (ipCore.parseIPheader(pck, false)) {
            iface.cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        natCfg.packParse(false, true, false, pck);
        tabNatTraN natT = tabNatTraN.fromError(pck);
        natT = natTrns.find(natT);
        if (natT != null) {
            long tim = bits.getTime();
            natT.lastUsed = tim;
            natT.reverse.lastUsed = tim;
            natT.updateError(pck);
            natCfg.packUpdate(pck);
            pck.IPprt = oldPrt;
            pck.IPsrc.setAddr(rtr);
            pck.IPtrg.setAddr(natT.newTrgAddr);
            pck.putDefaults();
            pck.putStart();
            pck.getSkip(-oldSiz);
            icmpCore.updateICMPheader(pck);
            ipCore.createIPheader(pck);
            pck.INTupper = -1;
            ipMpls.beginMPLSfields(pck, (mplsPropTtl | iface.mplsPropTtlAlways) & iface.mplsPropTtlAllow);
            forwardPacket(4, iface, null, pck);
            return;
        }
        if (debugger.ipFwdTraf) {
            logger.debug("err " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt + " rtr=" + rtr + " reason=" + counter.reason2string(err));
        }
        ipFwdProto prt = null;
        if (prt == null) {
            prt = protos.get(iface, pck.IPtrg, pck.IPprt, pck.IPprt);
        }
        if (prt == null) {
            prt = protos.get(null, pck.IPtrg, pck.IPprt, pck.IPprt);
        }
        if (prt == null) {
            prt = protos.get(iface, null, pck.IPprt, pck.IPprt);
        }
        if (prt == null) {
            prt = protos.get(null, null, pck.IPprt, pck.IPprt);
        }
        if (prt == null) {
            return;
        }
        pck.getSkip(pck.IPsiz);
        prt.upper.errorPack(err, rtr, iface, pck);
    }

    /**
     * send echo request
     *
     * @param src source address, null if nearest
     * @param trg target address
     * @param mpls mpls mode, bit0=reverse, 0=icmp, 2=mpls, 4=bier
     * @param hop forced nexthop address
     * @param size size of payload
     * @param df dont fragment
     * @param alrt alert to use
     * @param ttl ttl to use
     * @param sgt sgt to use
     * @param tos tos to use
     * @param id flow to use
     * @param dat filler byte
     * @param mul multiple responses
     * @return notifier notified on reply
     */
    public ipFwdEcho echoSendReq(addrIP src, addrIP trg, int mpls, addrIP hop, int size, boolean df, int alrt, int ttl, int sgt, int tos, int id, int dat, boolean mul) {
        final int maxSize = 8192;
        final int minSize = 16;
        if (size < minSize) {
            size = minSize;
        }
        if (size > maxSize) {
            size = maxSize;
        }
        packHolder pck = new packHolder(true, true);
        pck.putFill(0, size, dat);
        pck.putSkip(size);
        pck.merge2beg();
        ipFwdEcho ntry = new ipFwdEcho();
        ntry.notif = new notifier();
        ipFwdIface ifc;
        if (src == null) {
            ifc = ipFwdTab.findSendingIface(this, trg);
            if (ifc == null) {
                return null;
            }
            src = ifc.addr.copyBytes();
        } else {
            ifc = ipFwdTab.findSendingIface(this, src);
            if (ifc == null) {
                return null;
            }
        }
        ntry.src = src.copyBytes();
        ntry.trg = trg.copyBytes();
        ntry.multi = mul;
        ntry.created = bits.getTime();
        for (;;) {
            nextEchoNumber = (nextEchoNumber & 0x3fffffff) + 1;
            ntry.echoNum = nextEchoNumber + 10000;
            if (echoes.add(ntry) == null) {
                break;
            }
        }
        echoSent++;
        if ((mpls & 1) == 0) {
            icmpCore.createEcho(pck, src, trg, ntry.echoNum, false);
        } else {
            icmpCore.createEcho(pck, trg, src, ntry.echoNum, true);
        }
        pck.IPttl = ttl;
        pck.IPtos = tos;
        pck.IPid = id;
        pck.IPdf = df;
        pck.SGTid = sgt;
        pck.IPalrt = alrt;
        pck.INTupper = -1;
        if (hop != null) {
            ifc = ipFwdTab.findSendingIface(this, hop);
            if (ifc == null) {
                return null;
            }
        }
        if (mpls < 2) {
            protoPack(ifc, hop, pck);
            return ntry;
        }
        ipCore.createIPheader(pck);
        ipMpls.beginMPLSfields(pck, false);
        if (mpls < 4) {
            mplsTxPack(trg, pck, true);
            return ntry;
        }
        pck.ETHtype = ifc.lower.getEthtyp();
        ipFwdBier clnt = new ipFwdBier(0);
        clnt.addPeer(this, trg, 0, -1);
        clnt.updatePeers();
        clnt.sendPack(pck);
        return ntry;
    }

    /**
     * got echo reply packet
     *
     * @param pck packet received
     * @param id id received
     */
    public void echoRecvRep(packHolder pck, int id) {
        ipFwdEcho ntry = new ipFwdEcho();
        echoRply++;
        ntry.echoNum = id;
        ntry = echoes.find(ntry);
        if (ntry == null) {
            return;
        }
        if (ntry.src.compareTo(pck.IPtrg) != 0) {
            return;
        }
        if (!ntry.multi) {
            echoes.del(ntry);
        }
        ipFwdEchod res = new ipFwdEchod();
        res.tim = (int) (bits.getTime() - ntry.created);
        res.err = null;
        res.rtr = pck.IPsrc.copyBytes();
        res.lab = -1;
        res.ttl = pck.IPttl;
        res.tos = pck.IPtos;
        synchronized (ntry) {
            ntry.res.add(res);
        }
        ntry.notif.wakeup();
    }

    /**
     * got error report to ping
     *
     * @param pck packet received
     * @param id id received
     * @param err error reported
     * @param rtr reporting router
     */
    public void echoRecvErr(packHolder pck, int id, counter.reasons err, addrIP rtr) {
        ipFwdEcho ntry = new ipFwdEcho();
        ntry.echoNum = id;
        ntry = echoes.find(ntry);
        if (ntry == null) {
            return;
        }
        if (ntry.trg.compareTo(pck.IPtrg) != 0) {
            return;
        }
        if (ntry.src.compareTo(pck.IPsrc) != 0) {
            return;
        }
        if (!ntry.multi) {
            echoes.del(ntry);
        }
        ipFwdEchod res = new ipFwdEchod();
        res.tim = (int) (bits.getTime() - ntry.created);
        res.err = err;
        res.rtr = rtr.copyBytes();
        res.lab = ipFwdEcho.getMplsExt(pck);
        res.ttl = pck.IPttl;
        res.tos = pck.IPtos;
        synchronized (ntry) {
            ntry.res.add(res);
        }
        ntry.notif.wakeup();
    }

    public void run() {
        try {
            if (debugger.ipFwdEvnt) {
                logger.debug("startup");
            }
            for (;;) {
                if (triggerUpdate.misleep(untriggeredRecomputation) > 0) {
                    if (debugger.ipFwdEvnt) {
                        logger.debug("too fast table updates");
                    }
                }
                if (untriggeredRecomputation <= 0) {
                    break;
                }
                ipFwdTab.updateEverything(this);
                if (updateInterval < 1) {
                    continue;
                }
                bits.sleep(updateInterval);
            }
            untriggeredRecomputation -= 1;
            if (debugger.ipFwdEvnt) {
                logger.debug("shutdown");
            }
        } catch (Exception e) {
            logger.exception(e);
        }
    }

}
