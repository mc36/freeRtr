package ip;

import addr.addrIP;
import addr.addrPrefix;
import cfg.cfgAll;
import clnt.clntMplsTeP2p;
import clnt.clntNetflow;
import ifc.ifcEthTyp;
import ifc.ifcNshFwd;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import prt.prtTcp;
import rtr.rtrLdpNeigh;
import rtr.rtrLdpTrgtd;
import tab.tabAceslstN;
import tab.tabConnect;
import tab.tabGen;
import tab.tabLabel;
import tab.tabLabelBier;
import tab.tabLabelBierN;
import tab.tabLabelDup;
import tab.tabLabelNtry;
import tab.tabListing;
import tab.tabNatCfgN;
import tab.tabNatTraN;
import tab.tabPbrN;
import tab.tabPrfxlstN;
import tab.tabRoute;
import tab.tabRouteEntry;
import tab.tabRouteEntry.routeType;
import tab.tabRtrmapN;
import tab.tabRtrplcN;
import util.bits;
import util.counter;
import util.debugger;
import util.history;
import util.logger;
import util.notifier;
import util.state;

/**
 * does ip forwarding, services protocols
 *
 * @author matecsaba
 */
public class ipFwd implements Runnable, Comparator<ipFwd> {

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
     * route distinguisher
     */
    public long rd;

    /**
     * multicast distribution tree
     */
    public boolean mdt;

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
     * current icmp sessions
     */
    public final tabGen<ipFwdEcho> echoes;

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
     * counter for this vrf
     */
    public counter cntr;

    /**
     * historic for this vrf
     */
    public history hstry;

    /**
     * netflow exporter
     */
    public clntNetflow netflow;

    /**
     * allocate label for which prefix
     */
    public labelMode prefixMode = labelMode.common;

    /**
     * mpls propagate ip ttl
     */
    public boolean mplsPropTtl = true;

    /**
     * label allocation filter
     */
    public tabListing<tabPrfxlstN, addrIP> labelFilter;

    /**
     * packet forwarding filter
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> packetFilter;

    /**
     * source routing filter
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> sourceRoute;

    /**
     * traffic counter filter
     */
    public tabListing<tabRtrmapN, addrIP> counterMap;

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
     * time when purge nat entries
     */
    public int natTimeout = 300 * 1000;

    /**
     * notify when table changed
     */
    public notifier tableChanged;

    /**
     * common label
     */
    public tabLabelNtry commonLabel;

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

    private notifier triggerUpdate;

    private static int nextVrfNumber = 10;

    private int nextIfaceNumber = 10;

    private int nextRouterNumber = 10;

    private int nextEchoNumber = bits.randomW();

    /**
     * label modes
     */
    public enum labelMode {

        /**
         * common label for vrf
         */
        common,
        /**
         * label for igp prefixes
         */
        igp,
        /**
         * label for all prefix
         */
        all

    }

    public String toString() {
        return vrfName;
    }

    public int compare(ipFwd o1, ipFwd o2) {
        if (o1.vrfNum < o2.vrfNum) {
            return -1;
        }
        if (o1.vrfNum > o2.vrfNum) {
            return +1;
        }
        if (o1.ipVersion < o2.ipVersion) {
            return -1;
        }
        if (o1.ipVersion > o2.ipVersion) {
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
        nextVrfNumber++;
        cfgName = cfg;
        vrfName = nam;
        vrfNum = nextVrfNumber;
        ipCore = ipc;
        icmpCore = icc;
        mhostCore = mhst;
        ipVersion = ipCore.getVersion();
        commonLabel = tabLabel.allocate(1);
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
        labeldR = new tabRoute<addrIP>("labeled");
        actualU = new tabRoute<addrIP>("computed");
        actualM = new tabRoute<addrIP>("computed");
        actualF = new tabRoute<addrIP>("computed");
        staticU = new tabGen<ipFwdRoute>();
        staticM = new tabGen<ipFwdRoute>();
        natTrns = new tabGen<tabNatTraN>();
        pbrCfg = new tabListing<tabPbrN, addrIP>();
        pbrCfg.myCor = ipCore;
        pbrCfg.myIcmp = icc;
        natCfg = new tabListing<tabNatCfgN, addrIP>();
        natCfg.myCor = ipCore;
        natCfg.myIcmp = icc;
        cntr = new counter();
        hstry = new history();
        triggerUpdate = new notifier();
        ipFwdTab.updateEverything(this);
        icc.setForwarder(this);
        mhst.setForwarder(this, icc);
        new Thread(this).start();
    }

    /**
     * stop this routing table completely
     */
    public void stopThisVrf() {
        untriggeredRecomputation = -1;
        triggerUpdate.wakeup();
        prefixMode = labelMode.common;
        for (int i = 0; i < labeldR.size(); i++) {
            tabRouteEntry<addrIP> ntry = labeldR.get(i);
            if (ntry == null) {
                continue;
            }
            tabLabel.release(ntry.labelLoc, 2);
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
        tabLabel.release(commonLabel, 1);
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
     * @param iface receiving interface
     * @param addr peer address
     * @param create create if not yet
     * @return found neighbor, null if nothing
     */
    public rtrLdpNeigh ldpNeighFind(ipFwdIface iface, addrIP addr, boolean create) {
        if (iface != null) {
            iface = ifaces.find(iface);
            if (iface == null) {
                return null;
            }
            tabRouteEntry<addrIP> route = connedR.route(addr);
            if (route == null) {
                return null;
            }
        }
        rtrLdpNeigh ntry = new rtrLdpNeigh(addr);
        if (!create) {
            return ldpNeighs.find(ntry);
        }
        ntry.ifc = iface;
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
        rtrLdpNeigh old = ldpNeighs.del(ntry);
        if (old != null) {
            old.stopPeer();
        }
        ntry.stopPeer();
        triggerUpdate.wakeup();
    }

    /**
     * find one ldp targeted
     *
     * @param iface receiving interface
     * @param addr peer address
     * @param create create if not yet
     * @return found neighbor, null if nothing
     */
    public rtrLdpTrgtd ldpTargetFind(ipFwdIface iface, addrIP addr, boolean create) {
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
        ntry = ldpTarget.del(ntry);
        if (ntry != null) {
            ntry.workStop();
        }
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
     * @param trg peer address
     * @param id local node id
     * @param typ ethertype
     * @param exp expiration time, negative if not expires
     */
    public void mcastAddFloodBier(addrIP grp, addrIP src, addrIP trg, int id, int typ, long exp) {
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
            ntry = new ipFwdBier();
            ntry.fwd = this;
            ntry.id = id;
            ntry.typ = typ;
            g.bier = ntry;
            ntry.workStart();
        }
        ntry.addPeer(trg, exp);
        tableChanger();
    }

    /**
     * del bier peer from flood list
     *
     * @param grp group to flood
     * @param src source of group
     * @param trg peer address
     */
    public void mcastDelFloodBier(addrIP grp, addrIP src, addrIP trg) {
        ipFwdMcast g = new ipFwdMcast(grp, src);
        g = groups.find(g);
        if (g == null) {
            return;
        }
        if (g.bier == null) {
            return;
        }
        g.bier.delPeer(trg);
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
        if (uni) {
            rou = staticU.del(rou);
        } else {
            rou = staticM.del(rou);
        }
        if (rou != null) {
            if (rou.track != null) {
                rou.track.clients.del(this);
            }
        }
        triggerUpdate.wakeup();
    }

    /**
     * add static route
     *
     * @param uni true=unicast, false=multicast
     * @param rou route
     */
    public void staticAdd(boolean uni, ipFwdRoute rou) {
        rou.fwdCor = this;
        if (uni) {
            staticU.add(rou);
        } else {
            staticM.add(rou);
        }
        if (rou.track != null) {
            rou.track.clients.add(this);
        }
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
            nextIfaceNumber = ((nextIfaceNumber + 1) & 0x3fffffff);
            ntry = new ipFwdIface(nextIfaceNumber + 10000, lower);
            ntry.addr = new addrIP();
            ntry.network = new addrPrefix<addrIP>(ntry.addr, ntry.addr.maxBits());
            ntry.ready = true;
            ntry.mtu = lower.getMTUsize() - ipCore.getHeaderSize();
            ntry.bandwidth = lower.getBandwidth();
            if (ifaces.add(ntry) == null) {
                break;
            }
        }
        lower.setUpper(this, ntry);
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
            ipFwdProto prt = protos.delChild(ifc.ifwNum, null, 0, 0, 0);
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
        boolean st = (stat == state.states.up);
        if (ifc.ready == st) {
            return;
        }
        ifc.ready = st;
        if (debugger.ipFwdEvnt) {
            logger.debug("iface state " + st + " " + ifc.lower);
        }
        for (int i = protos.size() - 1; i >= 0; i--) {
            ipFwdProto prt = protos.get(i);
            if (prt == null) {
                continue;
            }
            if ((prt.iface == 0) || (prt.iface == ifc.ifwNum)) {
                prt.upper.setState(ifc, stat);
            }
        }
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
        ifc.point2point = mask >= (addrIP.size * 8 - 1);
        triggerUpdate.wakeup();
    }

    private void ifaceProto(ipFwdIface lower, packHolder pck, addrIP hop) {
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
        if (!lower.point2point) {
            if (lower.blockBroadcast && (pck.INTupper == 0)) {
                if (pck.IPtrg.compare(pck.IPtrg, lower.network.network) == 0) {
                    lower.cntr.drop(pck, counter.reasons.denied);
                    return;
                }
                if (pck.IPtrg.compare(pck.IPtrg, lower.network.broadcast) == 0) {
                    lower.cntr.drop(pck, counter.reasons.denied);
                    return;
                }
            }
        }
        if (lower.blockHost2host && (pck.INTupper == 0)) {
            if (pck.INTiface == lower.ifwNum) {
                lower.cntr.drop(pck, counter.reasons.denied);
                return;
            }
        }
        lower.cntr.tx(pck);
        if (lower.filterOut != null) {
            if (!lower.filterOut.matches(false, true, pck)) {
                doDrop(pck, lower, counter.reasons.denied);
                return;
            }
        }
        if (lower.tcpMssOut > 0) {
            ifaceAdjustMss(pck, lower.tcpMssOut);
        }
        if (debugger.ipFwdTraf) {
            logger.debug("tx " + pck.IPsrc + " -> " + pck.IPtrg + " hop=" + hop + " pr=" + pck.IPprt + " tos=" + pck.IPtos);
        }
        if (lower.inspect != null) {
            if (lower.inspect.doPack(pck, true)) {
                return;
            }
        }
        lower.lower.sendProto(pck, hop);
    }

    private void ifaceMpls(ipFwdIface lower, packHolder pck, addrIP hop) {
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
        ipCore.updateIPheader(pck, pck.IPsrc, pck.IPtrg, -1, -1, -1, pck.UDPsiz);
    }

    /**
     * interface signals that it got a packet
     *
     * @param lower interface handler
     * @param pck packet to process
     */
    public void ifacePack(ipFwdIface lower, packHolder pck) {
        if (lower == null) {
            cntr.drop(pck, counter.reasons.noIface);
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
        if (lower.filterIn != null) {
            if (!lower.filterIn.matches(false, true, pck)) {
                doDrop(pck, lower, counter.reasons.denied);
                return;
            }
        }
        pck.putStart();
        pck.INTiface = lower.ifwNum;
        if (lower.verifySource && !pck.IPlnk) {
            tabRouteEntry<addrIP> prf = actualU.route(pck.IPsrc);
            if (prf == null) {
                lower.cntr.drop(pck, counter.reasons.denied);
                return;
            }
            if ((lower.verifyStricht) && (prf.iface != lower)) {
                lower.cntr.drop(pck, counter.reasons.denied);
                return;
            }
        }
        if (debugger.ipFwdTraf) {
            logger.debug("rx " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt + " tos=" + pck.IPtos);
        }
        if (lower.tcpMssIn > 0) {
            ifaceAdjustMss(pck, lower.tcpMssIn);
        }
        if (lower.inspect != null) {
            if (lower.inspect.doPack(pck, false)) {
                return;
            }
        }
        pck.INTupper = 0;
        ipMpls.beginMPLSfields(pck, mplsPropTtl);
        if (doPbrFwd(lower.pbrCfg, true, false, lower, pck)) {
            return;
        }
        forwardPacket(true, false, lower, pck);
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
        int iface = 0;
        if (ifc != null) {
            ifc = ifaces.find(ifc);
            if (ifc == null) {
                return true;
            }
            iface = ifc.ifwNum;
        }
        if (debugger.ipFwdEvnt) {
            logger.debug("add proto=" + upper + " iface=" + iface + " trg=" + trg);
        }
        ipFwdProto ntry = new ipFwdProto();
        ntry.proto = upper.getProtoNum();
        ntry.iface = iface;
        ntry.upper = upper;
        return protos.add(iface, trg, 0, ntry.proto, ntry.proto, ntry, "" + upper, false);
    }

    /**
     * delete protocol from interface
     *
     * @param upper protocol handler
     * @param ifc interface handle, null for all
     * @param trg target address, null means all
     */
    public void protoDel(ipPrt upper, ipFwdIface ifc, addrIP trg) {
        int iface = ipFwdIface.getNum(ifc);
        if (debugger.ipFwdEvnt) {
            logger.debug("del proto=" + upper + " iface=" + iface + " trg=" + trg);
        }
        int i = upper.getProtoNum();
        for (;;) {
            ipFwdProto prt = protos.delChild(iface, trg, 0, i, i);
            if (prt == null) {
                break;
            }
        }
    }

    private void protoSend(ipFwdIface lower, packHolder pck) {
        if ((pck.IPmf) || (pck.IPfrg != 0)) {
            if (cfgAll.ruinPmtuD) {
                doDrop(pck, lower, counter.reasons.fragment);
            } else {
                lower.cntr.drop(pck, counter.reasons.fragment);
            }
            return;
        }
        if (debugger.ipFwdTraf) {
            logger.debug("rcv " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt + " tos=" + pck.IPtos);
        }
        ipFwdProto prt = protos.get(lower.ifwNum, pck.IPsrc, 0, pck.IPprt, pck.IPprt, true);
        if (prt == null) {
            doDrop(pck, lower, counter.reasons.badProto);
            return;
        }
        pck.getSkip(pck.IPsiz);
        prt.upper.recvPack(lower, pck);
    }

    private boolean protoAlert(ipFwdIface lower, packHolder pck) {
        if ((pck.IPmf) || (pck.IPfrg != 0)) {
            return true;
        }
        if (debugger.ipFwdTraf) {
            logger.debug("alrt " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt + " tos=" + pck.IPtos);
        }
        ipFwdProto prt = protos.get(lower.ifwNum, pck.IPsrc, 0, pck.IPprt, pck.IPprt, true);
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
        ipCore.testIPaddress(pck, pck.IPtrg);
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
     * @param len new payload length, -1=dont set
     */
    public void updateIPheader(packHolder pck, addrIP src, addrIP trg, int prt, int ttl, int tos, int len) {
        pck.INTiface = -1;
        pck.INTupper = pck.IPprt;
        ipCore.updateIPheader(pck, src, trg, prt, ttl, tos, len);
        ipMpls.beginMPLSfields(pck, mplsPropTtl);
    }

    /**
     * protocol wants to send one packet
     *
     * @param iface interface to use for source address
     * @param pck packet to send
     */
    public void protoPack(ipFwdIface iface, packHolder pck) {
        if (iface == null) {
            cntr.drop(pck, counter.reasons.noIface);
            return;
        }
        if (!iface.ready) {
            iface.cntr.drop(pck, counter.reasons.notUp);
            return;
        }
        pck.INTiface = iface.ifwNum;
        pck.INTupper = pck.IPprt;
        pck.merge2beg();
        ipCore.createIPheader(pck);
        if (debugger.ipFwdTraf) {
            logger.debug("snd " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt + " tos=" + pck.IPtos);
        }
        ipCore.testIPaddress(pck, pck.IPtrg);
        ipMpls.beginMPLSfields(pck, mplsPropTtl);
        forwardPacket(false, false, iface, pck);
    }

    /**
     * add one routing protocol
     *
     * @param rtr routing protocol handle
     * @param typ route type that it provides
     * @param id process id
     */
    public void routerAdd(ipRtr rtr, tabRouteEntry.routeType typ, int id) {
        if (debugger.ipFwdEvnt) {
            logger.debug("add rtr " + rtr);
        }
        rtr.routerProtoNum = nextRouterNumber++;
        rtr.routerProtoTyp = typ;
        rtr.routerProcNum = id;
        routers.add(rtr);
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
        triggerUpdate.wakeup();
        rtr.routerCloseNow();
    }

    /**
     * routing protocol notified that change happened
     *
     * @param rtr routing protocol handle
     */
    public void routerChg(ipRtr rtr) {
        if (debugger.ipFwdEvnt) {
            logger.debug("chgd rtr " + rtr);
        }
        if (routers.find(rtr) == null) {
            return;
        }
        rtr.routerComputeChg++;
        rtr.routerComputeTim = bits.getTime();
        triggerUpdate.wakeup();
    }

    /**
     * static route change happened
     */
    public void routerStaticChg() {
        triggerUpdate.wakeup();
    }

    /**
     * send unreachable
     *
     * @param pck packet to report
     * @param lower interface
     * @param reason reason
     */
    public void doDrop(packHolder pck, ipFwdIface lower, counter.reasons reason) {
        cntr.drop(pck, reason);
        if (cfgAll.unreachInt > 0) {
            long tim = bits.getTime();
            if ((tim - cfgAll.unreachLst) < cfgAll.unreachInt) {
                return;
            }
            cfgAll.unreachLst = tim;
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
        if (pck.IPprt == ipCorSrh.protoNum) {
            ipCorSrh.skipHeader(pck);
        }
        if (icmpCore.createError(pck, reason, src.copyBytes())) {
            return;
        }
        //ipFwdEcho.addMplsFields(pck);
        ipCore.createIPheader(pck);
        pck.INTupper = -1;
        ipMpls.beginMPLSfields(pck, mplsPropTtl);
        forwardPacket(false, false, lower, pck);
    }

    private void doMpls(ipFwdIface ifc, addrIP hop, List<Integer> labs, packHolder pck) {
        if (ifc == null) {
            cntr.drop(pck, counter.reasons.noIface);
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
    protected void mplsRxPack(ipFwd fwd4, ipFwd fwd6, ifcEthTyp fwdE, tabLabelNtry lab, packHolder pck) {
        if (debugger.ipFwdTraf) {
            logger.debug("rx label=" + lab.getValue());
        }
        pck.MPLSttl--;
        if (pck.MPLSttl < 2) {
            if (ipMpls.createError(pck, lab, counter.reasons.ttlExceed)) {
                return;
            }
        }
        if (lab.nextHop != null) {
            if ((lab.remoteLab == null) && (!pck.MPLSbottom)) {
                logger.info("no label for " + lab.getValue());
                cntr.drop(pck, counter.reasons.notInTab);
                return;
            }
            doMpls(lab.iface, lab.nextHop, lab.remoteLab, pck);
            return;
        }
        if (lab.duplicate != null) {
            for (int i = 0; i < lab.duplicate.size(); i++) {
                tabLabelDup ntry = lab.duplicate.get(i);
                if (ntry == null) {
                    continue;
                }
                doMpls(ntry.ifc, ntry.hop, ntry.lab, pck.copyBytes(true, true));
            }
            if (!lab.needLocal) {
                return;
            }
        }
        if (lab.bier != null) {
            pck.BIERsi = lab.getValue() - lab.bier.base;
            pck.BIERbsl = lab.bier.bsl;
            if (ipMpls.parseBIERheader(pck)) {
                logger.info("received invalid bier header on label " + lab.getValue());
                cntr.drop(pck, counter.reasons.badHdr);
                return;
            }
            int bsl = tabLabelBier.bsl2num(lab.bier.bsl);
            int sis = bsl * pck.BIERsi;
            int i = lab.bier.idx - 1 - sis;
            boolean nedLoc;
            if ((i >= 0) && (i < bsl)) {
                nedLoc = pck.BIERbs.testBit(i);
                if (nedLoc) {
                    pck.BIERbs = pck.BIERbs.clearBit(i);
                }
            } else {
                nedLoc = false;
            }
            for (i = 0; i < lab.bier.peers.size(); i++) {
                tabLabelBierN ntry = lab.bier.peers.get(i);
                if (ntry == null) {
                    continue;
                }
                packHolder p = pck.copyBytes(true, true);
                p.BIERbs = pck.BIERbs.and(ntry.ned.shiftRight(sis));
                if (p.BIERbs.bitCount() < 1) {
                    continue;
                }
                ipMpls.createBIERheader(p);
                p.MPLSlabel = ntry.lab + pck.BIERsi;
                ipMpls.createMPLSheader(p);
                doMpls(ntry.ifc, ntry.hop, null, p);
            }
            if (nedLoc) {
                if (ipMpls.gotBierPck(fwd4, fwd6, fwdE, pck)) {
                    logger.info("received invalid bier protocol on label " + lab.getValue());
                }
            }
            return;
        }
        if (!pck.MPLSbottom) {
            cntr.drop(pck, counter.reasons.badProto);
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
            cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        pck.INTiface = 0;
        pck.INTupper = -3;
        ipFwdIface ifc;
        if (pck.IPmlt || pck.IPbrd) {
            ifc = ipFwdTab.findStableIface(this);
        } else {
            if (lab.forwarder == null) {
                cntr.drop(pck, counter.reasons.notInTab);
                return;
            }
            tabRouteEntry<addrIP> prf = lab.forwarder.actualU.route(pck.IPtrg);
            if (prf == null) {
                cntr.drop(pck, counter.reasons.noRoute);
                return;
            }
            if (prf.rouTab != null) {
                prf = prf.rouTab.actualU.route(prf.nextHop);
                if (prf == null) {
                    cntr.drop(pck, counter.reasons.noRoute);
                    return;
                }
            }
            ifc = (ipFwdIface) prf.iface;
        }
        if (ifc == null) {
            cntr.drop(pck, counter.reasons.noIface);
            return;
        }
        forwardPacket(true, true, ifc, pck);
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
            cntr.drop(pck, counter.reasons.noRoute);
            return;
        }
        if ((prf.labelRem == null) && (req)) {
            if (prf.rouTyp == routeType.conn) {
                doMpls((ipFwdIface) prf.iface, trg, null, pck);
                return;
            }
            logger.info("no label for " + trg);
            cntr.drop(pck, counter.reasons.notInTab);
            return;
        }
        if (prf.rouTab != null) {
            ipMpls.createMPLSlabels(pck, prf.labelRem);
            prf.rouTab.mplsTxPack(prf.nextHop, pck, true);
            return;
        }
        if (prf.nextHop != null) {
            trg = prf.nextHop;
        }
        doMpls((ipFwdIface) prf.iface, trg, prf.labelRem, pck);
    }

    /**
     * forwards one parsed packet by policy routing
     */
    private boolean doPbrFwd(tabListing<tabPbrN, addrIP> cfg, boolean fromIfc, boolean fromMpls, ipFwdIface rxIfc, packHolder pck) {
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
            ipMpls.gotNshPack(null, pck);
            return true;
        }
        if (pbr.setIfc != null) {
            pck.INTiface = -2;
            ifaceProto(pbr.setIfc, pck, pbr.setHop);
            return true;
        }
        if (pbr.setHop == null) {
            pck.INTiface = -2;
            pbr.setVrf.forwardPacket(fromIfc, fromMpls, rxIfc, pck);
            return true;
        }
        tabRouteEntry<addrIP> ntry = pbr.setVrf.actualU.route(pbr.setHop);
        if (ntry == null) {
            return false;
        }
        if (ntry.iface == null) {
            return false;
        }
        pck.INTiface = -2;
        ifaceProto((ipFwdIface) ntry.iface, pck, pbr.setHop);
        return true;
    }

    /**
     * forwards one parsed packet
     */
    private void forwardPacket(boolean fromIfc, boolean fromMpls, ipFwdIface rxIfc, packHolder pck) {
        cntr.rx(pck);
        if (rxIfc == null) {
            cntr.drop(pck, counter.reasons.noIface);
            return;
        }
        if (packetFilter != null) {
            if (!packetFilter.matches(false, true, pck)) {
                doDrop(pck, rxIfc, counter.reasons.denied);
                return;
            }
        }
        if (debugger.ipFwdTraf) {
            logger.debug("fwd " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt + " tos=" + pck.IPtos);
        }
        if (netflow != null) {
            netflow.session.doPack(pck, true);
        }
        pck.ETHcos = (pck.IPtos >>> 5) & 7;
        pck.MPLSexp = pck.ETHcos;
        if (natCfg.size() > 0) {
            natCfg.packParse(false, true, true, pck);
            tabNatTraN natT = tabNatTraN.fromPack(pck);
            natT = natTrns.find(natT);
            if (natT != null) {
                long tim = bits.getTime();
                natT.lastUsed = tim;
                natT.reverse.lastUsed = tim;
                natT.updatePack(pck);
                natCfg.packUpdate(pck);
            } else {
                tabNatCfgN natC = natCfg.find(pck);
                if (natC != null) {
                    natT = natC.createEntry(pck);
                    natTrns.add(natT);
                    natTrns.add(natT.reverseEntry());
                    natT.updatePack(pck);
                    natCfg.packUpdate(pck);
                }
            }
        }
        if (doPbrFwd(pbrCfg, fromIfc, fromMpls, rxIfc, pck)) {
            return;
        }
        boolean alerted = (pck.IPalrt != -1);
        pck.IPalrt = -1;
        if (pck.IPlnk) {
            if (fromIfc) {
                if (rxIfc.lower.checkMyAddress(pck.IPtrg)) {
                    protoSend(rxIfc, pck);
                    return;
                }
                if (rxIfc.lower.checkMyAlias(pck.IPtrg) != null) {
                    protoSend(rxIfc, pck);
                    return;
                }
                doDrop(pck, rxIfc, counter.reasons.noRoute);
            } else {
                ifaceProto(rxIfc, pck, null);
            }
            return;
        }
        if (pck.IPmlt || pck.IPbrd) {
            if (pck.IPbrd || !pck.IPtrg.isRoutedMcast()) {
                if (fromIfc) {
                    protoSend(rxIfc, pck);
                } else {
                    ifaceProto(rxIfc, pck, null);
                }
                return;
            }
            if (pck.IPttl < 2) {
                cntr.drop(pck, counter.reasons.ttlExceed);
                return;
            }
            ipCore.updateIPheader(pck, null, null, -1, -2, -1, -1);
            ipFwdMcast grp = new ipFwdMcast(pck.IPtrg, pck.IPsrc);
            grp = groups.find(grp);
            if (grp == null) {
                cntr.drop(pck, counter.reasons.badNet);
                return;
            }
            if (fromIfc && (!fromMpls)) {
                if (grp.iface == null) {
                    cntr.drop(pck, counter.reasons.noRoute);
                    return;
                }
                if (grp.iface.ifwNum != rxIfc.ifwNum) {
                    cntr.drop(pck, counter.reasons.noRoute);
                    return;
                }
            }
            for (int i = 0; i < grp.flood.size(); i++) {
                ipFwdIface ifc = grp.flood.get(i);
                if (ifc == null) {
                    continue;
                }
                if (fromIfc && (!fromMpls)) {
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
                grp.bier.sendPack(pck);
            }
            if (grp.local && fromIfc) {
                protoSend(rxIfc, pck);
            }
            return;
        }
        tabRouteEntry<addrIP> prf = actualU.route(pck.IPtrg);
        if (prf == null) {
            doDrop(pck, rxIfc, counter.reasons.noRoute);
            return;
        }
        if (prf.cntr != null) {
            prf.cntr.tx(pck);
        }
        if (prf.rouTab != null) {
            if (prf.labelRem == null) {
                doDrop(pck, rxIfc, counter.reasons.notInTab);
                return;
            }
            ipMpls.createMPLSlabels(pck, prf.labelRem);
            prf.rouTab.mplsTxPack(prf.nextHop, pck, true);
            return;
        }
        if (prf.iface == null) {
            doDrop(pck, rxIfc, counter.reasons.noIface);
            return;
        }
        ipFwdIface txIfc = (ipFwdIface) prf.iface;
        if (txIfc.lower.checkMyAddress(pck.IPtrg)) {
            if (pck.IPprt != ipCorSrh.protoNum) {
                protoSend(txIfc, pck);
                return;
            }
            int res = ipCorSrh.parseHeader(pck);
            switch (res) {
                case 2:
                    protoSend(txIfc, pck);
                    return;
                case 1:
                    cntr.drop(pck, counter.reasons.badHdr);
                    return;
                default:
                    break;
            }
            if (mplsPropTtl) {
                pck.IPttl = pck.MPLSttl;
            }
            ipCore.updateIPheader(pck, pck.IPsrc, pck.IPtrg, pck.IPprt, pck.IPttl, pck.IPtos, pck.dataSize() - pck.IPsiz);
            if (sourceRoute == null) {
                doDrop(pck, rxIfc, counter.reasons.denied);
                return;
            }
            if (!sourceRoute.matches(false, true, pck)) {
                doDrop(pck, rxIfc, counter.reasons.denied);
                return;
            }
            pck.INTiface = -3;
            pck.INTupper = -3;
            ipCore.testIPaddress(pck, pck.IPtrg);
            ipMpls.beginMPLSfields(pck, mplsPropTtl);
            forwardPacket(fromIfc, fromMpls, rxIfc, pck);
            return;
        }
        if (txIfc.lower.checkMyAlias(pck.IPtrg) != null) {
            protoSend(txIfc, pck);
            return;
        }
        if (pck.MPLSttl < 2) {
            doDrop(pck, rxIfc, counter.reasons.ttlExceed);
            return;
        }
        if (fromIfc && alerted) {
            if (!protoAlert(rxIfc, pck)) {
                return;
            }
        }
        if (mplsPropTtl) {
            ipCore.updateIPheader(pck, null, null, -1, pck.MPLSttl - 1, -1, -1);
        } else {
            ipCore.updateIPheader(pck, null, null, -1, -2, -1, -1);
        }
        cntr.tx(pck);
        if (prf.rouTyp == tabRouteEntry.routeType.conn) {
            ifaceProto(txIfc, pck, null);
            return;
        }
        if (alerted) {
            ifaceProto(txIfc, pck, prf.nextHop);
            return;
        }
        doMpls(txIfc, prf.nextHop, prf.labelRem, pck);
    }

    /**
     * got error report
     *
     * @param err error code
     * @param iface receiving interface
     * @param pck protocol packet
     */
    public void errorReport(counter.reasons err, ipFwdIface iface, packHolder pck) {
        addrIP rtr = pck.IPsrc.copyBytes();
        if (ipCore.parseIPheader(pck, false)) {
            iface.cntr.drop(pck, counter.reasons.badHdr);
            return;
        }
        if (pck.IPprt == ipCorSrh.protoNum) {
            ipCorSrh.skipHeader(pck);
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
            if (icmpCore.createError(pck, err, rtr)) {
                return;
            }
            ipCore.createIPheader(pck);
            pck.INTupper = -1;
            ipMpls.beginMPLSfields(pck, mplsPropTtl);
            forwardPacket(false, false, iface, pck);
            return;
        }
        if (debugger.ipFwdTraf) {
            logger.debug("err " + pck.IPsrc + " -> " + pck.IPtrg + " pr=" + pck.IPprt + " rtr=" + rtr + " reason=" + counter.reason2string(err));
        }
        ipFwdProto prt = protos.get(iface.ifwNum, pck.IPtrg, 0, pck.IPprt, pck.IPprt, true);
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
     * @param size size of payload
     * @param ttl ttl to use
     * @param tos tos to use
     * @return notifier notified on reply
     */
    public notifier echoSendReq(addrIP src, addrIP trg, int size, int ttl, int tos) {
        final int maxSize = 8192;
        final int minSize = 16;
        if (size < minSize) {
            size = minSize;
        }
        if (size > maxSize) {
            size = maxSize;
        }
        packHolder pck = new packHolder(true, true);
        pck.putFill(0, size, 0);
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
        for (;;) {
            ntry.echoNum = nextEchoNumber++;
            if (echoes.add(ntry) == null) {
                break;
            }
        }
        ntry.created = bits.getTime();
        if (icmpCore.createEcho(pck, src, trg, ntry.echoNum)) {
            return null;
        }
        pck.IPttl = ttl;
        pck.IPtos = tos;
        pck.INTupper = -1;
        ipCore.createIPheader(pck);
        ipMpls.beginMPLSfields(pck, mplsPropTtl);
        forwardPacket(false, false, ifc, pck);
        return ntry.notif;
    }

    /**
     * got echo reply packet
     *
     * @param pck packet received
     * @param id id received
     */
    public void echoRecvRep(packHolder pck, int id) {
        ipFwdEcho ntry = new ipFwdEcho();
        ntry.echoNum = id;
        ntry = echoes.find(ntry);
        if (ntry == null) {
            return;
        }
        if (!ntry.trg.isMulticast()) {
            if (ntry.trg.compare(ntry.trg, pck.IPsrc) != 0) {
                return;
            }
        }
        if (ntry.trg.compare(ntry.src, pck.IPtrg) != 0) {
            return;
        }
        echoes.del(ntry);
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
                if (debugger.ipFwdEvnt) {
                    logger.debug("update tables");
                }
                ipFwdTab.updateEverything(this);
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
