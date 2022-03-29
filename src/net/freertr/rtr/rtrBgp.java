package net.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrMac;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAceslst;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgInit;
import net.freertr.cfg.cfgPlymp;
import net.freertr.cfg.cfgPrfxlst;
import net.freertr.cfg.cfgProxy;
import net.freertr.cfg.cfgRoump;
import net.freertr.cfg.cfgRouplc;
import net.freertr.cfg.cfgRtr;
import net.freertr.cfg.cfgVrf;
import net.freertr.ifc.ifcDot1ah;
import net.freertr.ip.ipCor4;
import net.freertr.ip.ipCor6;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipFwdIface;
import net.freertr.ip.ipFwdTab;
import net.freertr.ip.ipRtr;
import net.freertr.pipe.pipeLine;
import net.freertr.pipe.pipeSide;
import net.freertr.prt.prtGenConn;
import net.freertr.prt.prtServS;
import net.freertr.prt.prtTcp;
import net.freertr.tab.tabAceslstN;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabIndex;
import net.freertr.tab.tabIntMatcher;
import net.freertr.tab.tabLabel;
import net.freertr.tab.tabLabelBier;
import net.freertr.tab.tabLabelBierN;
import net.freertr.tab.tabLabelEntry;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabPlcmapN;
import net.freertr.tab.tabPrfxlstN;
import net.freertr.tab.tabQos;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRtrmapN;
import net.freertr.tab.tabRtrplcN;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.debugger;
import net.freertr.util.logFil;
import net.freertr.util.logger;
import net.freertr.util.notifier;
import net.freertr.util.shrtPthFrst;
import net.freertr.util.syncInt;

/**
 * border gateway protocol (rfc4271) version 4
 *
 * @author matecsaba
 */
public class rtrBgp extends ipRtr implements prtServS, Runnable {

    /**
     * port to use
     */
    public final static int port = 179;

    /**
     * local as number
     */
    public int localAs;

    /**
     * address families
     */
    public int addrFams;

    /**
     * router id
     */
    public addrIPv4 routerID;

    /**
     * safe ebgp
     */
    public boolean safeEbgp;

    /**
     * segment routing index
     */
    public int segrouIdx = 0;

    /**
     * segment routing maximum
     */
    public int segrouMax = 0;

    /**
     * segment routing base
     */
    public int segrouBase = 0;

    /**
     * segment routing labels
     */
    protected tabLabelEntry[] segrouLab;

    /**
     * bier index
     */
    public int bierIdx = 0;

    /**
     * bier length
     */
    public int bierLen = 0;

    /**
     * bier maximum
     */
    public int bierMax = 0;

    /**
     * bier labels
     */
    protected tabLabelEntry[] bierLab;

    /**
     * scan time interval
     */
    public int scanTime;

    /**
     * initial delay
     */
    public int scanDelay;

    /**
     * recursion depth
     */
    public int recursion;

    /**
     * restart time
     */
    public int restartTime;

    /**
     * external distance
     */
    public int distantExt;

    /**
     * internal distance
     */
    public int distantInt;

    /**
     * local distance
     */
    public int distantLoc;

    /**
     * update groups
     */
    public List<rtrBgpGroup> groups;

    /**
     * group minimum
     */
    public int groupMin;

    /**
     * group maximum
     */
    public int groupMax;

    /**
     * listen template
     */
    public rtrBgpTemp lstnTmp;

    /**
     * listen interface
     */
    public ipFwdIface lstnIfc;

    /**
     * listen access list
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> lstnAcl;

    /**
     * list of dynamic neighbors
     */
    protected tabGen<rtrBgpNeigh> lstnNei;

    /**
     * route type
     */
    protected final tabRouteAttr.routeType rouTyp;

    /**
     * unicast afi
     */
    protected final int afiUni;

    /**
     * labeled unicast afi
     */
    protected final int afiLab;

    /**
     * multicast afi
     */
    protected final int afiMlt;

    /**
     * other labeled unicast afi
     */
    protected final int afiOtrL;

    /**
     * other unicast afi
     */
    protected final int afiOtrU;

    /**
     * other multicast afi
     */
    protected final int afiOtrM;

    /**
     * other flowspec afi
     */
    protected final int afiOtrF;

    /**
     * other srte afi
     */
    protected final int afiOtrS;

    /**
     * flow specification afi
     */
    protected final int afiFlw;

    /**
     * unicast vpn afi
     */
    protected final int afiVpnU;

    /**
     * multicast vpn afi
     */
    protected final int afiVpnM;

    /**
     * flowspec vpn afi
     */
    protected final int afiVpnF;

    /**
     * other unicast vpn afi
     */
    protected final int afiVpoU;

    /**
     * other multicast vpn afi
     */
    protected final int afiVpoM;

    /**
     * other flowspec vpn afi
     */
    protected final int afiVpoF;

    /**
     * vpls afi
     */
    protected final int afiVpls;

    /**
     * mspw afi
     */
    protected final int afiMspw;

    /**
     * evpn afi
     */
    protected final int afiEvpn;

    /**
     * mdt afi
     */
    protected final int afiMdt;

    /**
     * nsh afi
     */
    protected final int afiNsh;

    /**
     * srte afi
     */
    protected final int afiSrte;

    /**
     * linkstate afi
     */
    protected final int afiLnks;

    /**
     * mvpn afi
     */
    protected final int afiMvpn;

    /**
     * other mvpn afi
     */
    protected final int afiMvpo;

    /**
     * router number
     */
    protected final int rtrNum;

    /**
     * other changes trigger full computation
     */
    protected boolean otherTrigger;

    /**
     * next hop tracking route map
     */
    protected tabListing<tabRtrmapN, addrIP> nhtRoumap;

    /**
     * next hop tracking route policy
     */
    protected tabListing<tabRtrplcN, addrIP> nhtRouplc;

    /**
     * next hop tracking policy map
     */
    protected tabListing<tabPrfxlstN, addrIP> nhtPfxlst;

    /**
     * flow specification
     */
    protected tabListing<tabPlcmapN, addrIP> flowSpec;

    /**
     * link states
     */
    protected tabGen<rtrBgpLnkst> linkStates;

    /**
     * install flow specification
     */
    protected boolean flowInst;

    /**
     * list of rpkis
     */
    protected tabGen<rtrBgpRpki> rpkis;

    /**
     * the computed rpki routes
     */
    public tabRoute<addrIP> computedRpki = new tabRoute<addrIP>("rx");

    /**
     * the computed other unicast routes
     */
    public tabRoute<addrIP> computedOtrU = new tabRoute<addrIP>("rx");

    /**
     * the computed other multicast routes
     */
    public tabRoute<addrIP> computedOtrM = new tabRoute<addrIP>("rx");

    /**
     * the computed other flowspec routes
     */
    public tabRoute<addrIP> computedOtrF = new tabRoute<addrIP>("rx");

    /**
     * the computed other srte routes
     */
    public tabRoute<addrIP> computedOtrS = new tabRoute<addrIP>("rx");

    /**
     * the computed vpnuni routes
     */
    public tabRoute<addrIP> computedVpnU = new tabRoute<addrIP>("rx");

    /**
     * the computed vpnmlt routes
     */
    public tabRoute<addrIP> computedVpnM = new tabRoute<addrIP>("rx");

    /**
     * the computed vpnflw routes
     */
    public tabRoute<addrIP> computedVpnF = new tabRoute<addrIP>("rx");

    /**
     * the computed other vpnuni routes
     */
    public tabRoute<addrIP> computedVpoU = new tabRoute<addrIP>("rx");

    /**
     * the computed other vpnmlt routes
     */
    public tabRoute<addrIP> computedVpoM = new tabRoute<addrIP>("rx");

    /**
     * the computed other vpnflw routes
     */
    public tabRoute<addrIP> computedVpoF = new tabRoute<addrIP>("rx");

    /**
     * the computed vpls routes
     */
    public tabRoute<addrIP> computedVpls = new tabRoute<addrIP>("rx");

    /**
     * the computed mspw routes
     */
    public tabRoute<addrIP> computedMspw = new tabRoute<addrIP>("rx");

    /**
     * the computed evpn routes
     */
    public tabRoute<addrIP> computedEvpn = new tabRoute<addrIP>("rx");

    /**
     * the computed mdt routes
     */
    public tabRoute<addrIP> computedMdt = new tabRoute<addrIP>("rx");

    /**
     * the computed nsh routes
     */
    public tabRoute<addrIP> computedNsh = new tabRoute<addrIP>("rx");

    /**
     * the computed srte routes
     */
    public tabRoute<addrIP> computedSrte = new tabRoute<addrIP>("rx");

    /**
     * the computed linkstate routes
     */
    public tabRoute<addrIP> computedLnks = new tabRoute<addrIP>("rx");

    /**
     * the computed mvpn routes
     */
    public tabRoute<addrIP> computedMvpn = new tabRoute<addrIP>("rx");

    /**
     * the computed other mvpn routes
     */
    public tabRoute<addrIP> computedMvpo = new tabRoute<addrIP>("rx");

    /**
     * the changed unicast routes
     */
    public final tabRoute<addrIP> changedUni = new tabRoute<addrIP>("rx");

    /**
     * the changed multicast routes
     */
    public final tabRoute<addrIP> changedMlt = new tabRoute<addrIP>("rx");

    /**
     * the changed other unicast routes
     */
    public final tabRoute<addrIP> changedOtrU = new tabRoute<addrIP>("rx");

    /**
     * the changed other multicast routes
     */
    public final tabRoute<addrIP> changedOtrM = new tabRoute<addrIP>("rx");

    /**
     * the changed other flowspec routes
     */
    public final tabRoute<addrIP> changedOtrF = new tabRoute<addrIP>("rx");

    /**
     * the changed other srte routes
     */
    public final tabRoute<addrIP> changedOtrS = new tabRoute<addrIP>("rx");

    /**
     * the changed flowspec routes
     */
    public final tabRoute<addrIP> changedFlw = new tabRoute<addrIP>("rx");

    /**
     * the changed vpnuni routes
     */
    public final tabRoute<addrIP> changedVpnU = new tabRoute<addrIP>("rx");

    /**
     * the changed vpnmlt routes
     */
    public final tabRoute<addrIP> changedVpnM = new tabRoute<addrIP>("rx");

    /**
     * the changed vpnflw routes
     */
    public final tabRoute<addrIP> changedVpnF = new tabRoute<addrIP>("rx");

    /**
     * the changed other vpnuni routes
     */
    public final tabRoute<addrIP> changedVpoU = new tabRoute<addrIP>("rx");

    /**
     * the changed other vpnmlt routes
     */
    public final tabRoute<addrIP> changedVpoM = new tabRoute<addrIP>("rx");

    /**
     * the changed other vpnflw routes
     */
    public final tabRoute<addrIP> changedVpoF = new tabRoute<addrIP>("rx");

    /**
     * the changed vpls routes
     */
    public final tabRoute<addrIP> changedVpls = new tabRoute<addrIP>("rx");

    /**
     * the changed mspw routes
     */
    public final tabRoute<addrIP> changedMspw = new tabRoute<addrIP>("rx");

    /**
     * the changed evpn routes
     */
    public final tabRoute<addrIP> changedEvpn = new tabRoute<addrIP>("rx");

    /**
     * the changed mdt routes
     */
    public final tabRoute<addrIP> changedMdt = new tabRoute<addrIP>("rx");

    /**
     * the changed nsh routes
     */
    public final tabRoute<addrIP> changedNsh = new tabRoute<addrIP>("rx");

    /**
     * the changed srte routes
     */
    public final tabRoute<addrIP> changedSrte = new tabRoute<addrIP>("rx");

    /**
     * the changed linkstate routes
     */
    public final tabRoute<addrIP> changedLnks = new tabRoute<addrIP>("rx");

    /**
     * the changed mvpn routes
     */
    public final tabRoute<addrIP> changedMvpn = new tabRoute<addrIP>("rx");

    /**
     * the changed other mvpn routes
     */
    public final tabRoute<addrIP> changedMvpo = new tabRoute<addrIP>("rx");

    /**
     * the originated other unicast routes
     */
    public tabRoute<addrIP> origntedOtrU = new tabRoute<addrIP>("tx");

    /**
     * the originated other multicast routes
     */
    public tabRoute<addrIP> origntedOtrM = new tabRoute<addrIP>("tx");

    /**
     * the originated other flowspec routes
     */
    public tabRoute<addrIP> origntedOtrF = new tabRoute<addrIP>("tx");

    /**
     * the originated other srte routes
     */
    public tabRoute<addrIP> origntedOtrS = new tabRoute<addrIP>("tx");

    /**
     * the originated flowspec routes
     */
    public tabRoute<addrIP> origntedFlw = new tabRoute<addrIP>("tx");

    /**
     * the originated vpnuni routes
     */
    public tabRoute<addrIP> origntedVpnU = new tabRoute<addrIP>("tx");

    /**
     * the originated vpnmlt routes
     */
    public tabRoute<addrIP> origntedVpnM = new tabRoute<addrIP>("tx");

    /**
     * the originated vpnflw routes
     */
    public tabRoute<addrIP> origntedVpnF = new tabRoute<addrIP>("tx");

    /**
     * the originated other vpnuni routes
     */
    public tabRoute<addrIP> origntedVpoU = new tabRoute<addrIP>("tx");

    /**
     * the originated other vpnmlt routes
     */
    public tabRoute<addrIP> origntedVpoM = new tabRoute<addrIP>("tx");

    /**
     * the originated other vpnflw routes
     */
    public tabRoute<addrIP> origntedVpoF = new tabRoute<addrIP>("tx");

    /**
     * the originated vpls routes
     */
    public tabRoute<addrIP> origntedVpls = new tabRoute<addrIP>("tx");

    /**
     * the originated mspw routes
     */
    public tabRoute<addrIP> origntedMspw = new tabRoute<addrIP>("tx");

    /**
     * the originated evpn routes
     */
    public tabRoute<addrIP> origntedEvpn = new tabRoute<addrIP>("tx");

    /**
     * the originated mdt routes
     */
    public tabRoute<addrIP> origntedMdt = new tabRoute<addrIP>("tx");

    /**
     * the originated nsh routes
     */
    public tabRoute<addrIP> origntedNsh = new tabRoute<addrIP>("tx");

    /**
     * the originated srte routes
     */
    public tabRoute<addrIP> origntedSrte = new tabRoute<addrIP>("tx");

    /**
     * the originated linkstate routes
     */
    public tabRoute<addrIP> origntedLnks = new tabRoute<addrIP>("tx");

    /**
     * the originated mvpn routes
     */
    public tabRoute<addrIP> origntedMvpn = new tabRoute<addrIP>("tx");

    /**
     * the originated other mvpn routes
     */
    public tabRoute<addrIP> origntedMvpo = new tabRoute<addrIP>("tx");

    /**
     * incremental limit
     */
    public int incrLimit;

    /**
     * conquer bestpath
     */
    public boolean conquer;

    /**
     * flap statistics
     */
    public tabGen<rtrBgpFlap> flaps;

    /**
     * list of monitors
     */
    protected tabGen<rtrBgpMon> mons;

    /**
     * list of dumps
     */
    protected tabGen<rtrBgpMrt> dmps;

    /**
     * list of neighbors
     */
    protected tabGen<rtrBgpNeigh> neighs;

    /**
     * list of templates
     */
    protected tabGen<rtrBgpTemp> temps;

    /**
     * other afi router
     */
    protected rtrBgpOther other;

    /**
     * list of vrfs
     */
    protected tabGen<rtrBgpVrf> vrfs;

    /**
     * list of other vrfs
     */
    protected tabGen<rtrBgpVrf> ovrfs;

    /**
     * list of vpls
     */
    protected tabGen<rtrBgpVpls> vpls;

    /**
     * list of evpns
     */
    protected tabGen<rtrBgpEvpn> evpn;

    /**
     * evpn receiver
     */
    protected rtrBgpEvpnPbb evpnRcv;

    /**
     * evpn unicast label
     */
    protected tabLabelEntry evpnUni;

    /**
     * evpn multicast label
     */
    protected tabLabelEntry evpnMul;

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
     * changed prefixes current
     */
    public int changedCur;

    /**
     * changed prefixes total
     */
    public long changedTot;

    /**
     * the tcp protocol
     */
    public final prtTcp tcpCore;

    /**
     * the forwarder protocol
     */
    public final ipFwd fwdCore;

    /**
     * the forwarder vrf
     */
    protected cfgVrf vrfCore;

    /**
     * notifier for table computation
     */
    protected final notifier compute = new notifier();

    /**
     * computation round
     */
    protected final syncInt compRound = new syncInt(0);

    /**
     * need full round
     */
    protected final syncInt needFull = new syncInt(0);

    private boolean oldAggr;

    private boolean need2run;

    /**
     * create bgp process
     *
     * @param forwarder forwarder to update
     * @param vrfcfg vrf config to use
     * @param protoT tcp protocol to use
     * @param id process id
     */
    public rtrBgp(ipFwd forwarder, cfgVrf vrfcfg, prtTcp protoT, int id) {
        if (debugger.rtrBgpEvnt) {
            logger.debug("startup");
        }
        vrfCore = vrfcfg;
        fwdCore = forwarder;
        tcpCore = protoT;
        vrfs = new tabGen<rtrBgpVrf>();
        ovrfs = new tabGen<rtrBgpVrf>();
        vpls = new tabGen<rtrBgpVpls>();
        evpn = new tabGen<rtrBgpEvpn>();
        evpnUni = tabLabel.allocate(10);
        evpnMul = tabLabel.allocate(10);
        evpnRcv = new rtrBgpEvpnPbb(this);
        evpnUni.setFwdPwe(10, fwdCore, evpnRcv, 0, null);
        evpnMul.setFwdPwe(10, fwdCore, evpnRcv, 0, null);
        routerID = new addrIPv4();
        safeEbgp = false;
        addrFams = rtrBgpParam.mskUni;
        rtrNum = id;
        switch (fwdCore.ipVersion) {
            case ipCor4.protocolVersion:
                rouTyp = tabRouteAttr.routeType.bgp4;
                afiUni = rtrBgpUtil.safiIp4uni;
                afiLab = rtrBgpUtil.safiIp4lab;
                afiMlt = rtrBgpUtil.safiIp4multi;
                afiOtrL = rtrBgpUtil.safiIp6lab;
                afiOtrU = rtrBgpUtil.safiIp6uni;
                afiOtrM = rtrBgpUtil.safiIp6multi;
                afiOtrF = rtrBgpUtil.safiIp6flow;
                afiOtrS = rtrBgpUtil.safiIp6srte;
                afiFlw = rtrBgpUtil.safiIp4flow;
                afiVpnU = rtrBgpUtil.safiIp4vpnU;
                afiVpnM = rtrBgpUtil.safiIp4vpnM;
                afiVpnF = rtrBgpUtil.safiIp4vpnF;
                afiVpoU = rtrBgpUtil.safiIp6vpnU;
                afiVpoM = rtrBgpUtil.safiIp6vpnM;
                afiVpoF = rtrBgpUtil.safiIp6vpnF;
                afiVpls = rtrBgpUtil.safiVpls46;
                afiMspw = rtrBgpUtil.safiMspw46;
                afiEvpn = rtrBgpUtil.safiEvpn46;
                afiMdt = rtrBgpUtil.safiIp4mdt;
                afiNsh = rtrBgpUtil.safiNsh46;
                afiLnks = rtrBgpUtil.safiIp46lnks;
                afiSrte = rtrBgpUtil.safiIp4srte;
                afiMvpn = rtrBgpUtil.safiIp4mvpn;
                afiMvpo = rtrBgpUtil.safiIp6mvpn;
                other = new rtrBgpOther(this, vrfCore.fwd6);
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteAttr.routeType.bgp6;
                afiUni = rtrBgpUtil.safiIp6uni;
                afiLab = rtrBgpUtil.safiIp6lab;
                afiMlt = rtrBgpUtil.safiIp6multi;
                afiOtrL = rtrBgpUtil.safiIp4lab;
                afiOtrU = rtrBgpUtil.safiIp4uni;
                afiOtrM = rtrBgpUtil.safiIp4multi;
                afiOtrF = rtrBgpUtil.safiIp4flow;
                afiOtrS = rtrBgpUtil.safiIp4srte;
                afiFlw = rtrBgpUtil.safiIp6flow;
                afiVpnU = rtrBgpUtil.safiIp6vpnU;
                afiVpnM = rtrBgpUtil.safiIp6vpnM;
                afiVpnF = rtrBgpUtil.safiIp6vpnF;
                afiVpoU = rtrBgpUtil.safiIp4vpnU;
                afiVpoM = rtrBgpUtil.safiIp4vpnM;
                afiVpoF = rtrBgpUtil.safiIp4vpnF;
                afiVpls = rtrBgpUtil.safiVpls46;
                afiMspw = rtrBgpUtil.safiMspw46;
                afiEvpn = rtrBgpUtil.safiEvpn46;
                afiMdt = rtrBgpUtil.safiIp6mdt;
                afiNsh = rtrBgpUtil.safiNsh46;
                afiLnks = rtrBgpUtil.safiIp46lnks;
                afiSrte = rtrBgpUtil.safiIp6srte;
                afiMvpn = rtrBgpUtil.safiIp6mvpn;
                afiMvpo = rtrBgpUtil.safiIp4mvpn;
                other = new rtrBgpOther(this, vrfCore.fwd4);
                break;
            default:
                rouTyp = null;
                afiUni = 0;
                afiLab = 0;
                afiMlt = 0;
                afiOtrL = 0;
                afiOtrU = 0;
                afiOtrM = 0;
                afiOtrF = 0;
                afiOtrS = 0;
                afiFlw = 0;
                afiVpnU = 0;
                afiVpnM = 0;
                afiVpnF = 0;
                afiVpoU = 0;
                afiVpoM = 0;
                afiVpoF = 0;
                afiVpls = 0;
                afiMspw = 0;
                afiEvpn = 0;
                afiMdt = 0;
                afiNsh = 0;
                afiLnks = 0;
                afiSrte = 0;
                afiMvpn = 0;
                afiMvpo = 0;
                other = new rtrBgpOther(this, null);
                break;
        }
        incrLimit = 1000;
        conquer = false;
        flaps = null;
        scanTime = 1000;
        scanDelay = 1000;
        recursion = 1;
        restartTime = 60 * 1000;
        distantExt = 20;
        distantInt = 200;
        distantLoc = 200;
        linkStates = new tabGen<rtrBgpLnkst>();
        lstnNei = new tabGen<rtrBgpNeigh>();
        neighs = new tabGen<rtrBgpNeigh>();
        mons = new tabGen<rtrBgpMon>();
        dmps = new tabGen<rtrBgpMrt>();
        rpkis = new tabGen<rtrBgpRpki>();
        temps = new tabGen<rtrBgpTemp>();
        routerComputedU = new tabRoute<addrIP>("rx");
        routerComputedM = new tabRoute<addrIP>("rx");
        routerComputedF = new tabRoute<addrIP>("rx");
        routerComputedI = new tabGen<tabIndex<addrIP>>();
        needFull.add(1);
        compRound.add(1);
        routerCreateComputed();
        need2run = true;
        new Thread(this).start();
        fwdCore.routerAdd(this, rouTyp, id);
    }

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "bgp on " + fwdCore;
    }

    /**
     * convert safi to mask
     *
     * @param safi safi
     * @return mask
     */
    public int safi2mask(int safi) {
        if (safi == afiUni) {
            return rtrBgpParam.mskUni;
        }
        if (safi == afiLab) {
            return rtrBgpParam.mskLab;
        }
        if (safi == afiMlt) {
            return rtrBgpParam.mskMlt;
        }
        if (safi == afiOtrL) {
            return rtrBgpParam.mskOtrL;
        }
        if (safi == afiOtrU) {
            return rtrBgpParam.mskOtrU;
        }
        if (safi == afiOtrM) {
            return rtrBgpParam.mskOtrM;
        }
        if (safi == afiOtrF) {
            return rtrBgpParam.mskOtrF;
        }
        if (safi == afiOtrS) {
            return rtrBgpParam.mskOtrS;
        }
        if (safi == afiFlw) {
            return rtrBgpParam.mskFlw;
        }
        if (safi == afiVpnU) {
            return rtrBgpParam.mskVpnU;
        }
        if (safi == afiVpnM) {
            return rtrBgpParam.mskVpnM;
        }
        if (safi == afiVpnF) {
            return rtrBgpParam.mskVpnF;
        }
        if (safi == afiVpoU) {
            return rtrBgpParam.mskVpoU;
        }
        if (safi == afiVpoM) {
            return rtrBgpParam.mskVpoM;
        }
        if (safi == afiVpoF) {
            return rtrBgpParam.mskVpoF;
        }
        if (safi == afiVpls) {
            return rtrBgpParam.mskVpls;
        }
        if (safi == afiMspw) {
            return rtrBgpParam.mskMspw;
        }
        if (safi == afiEvpn) {
            return rtrBgpParam.mskEvpn;
        }
        if (safi == afiMdt) {
            return rtrBgpParam.mskMdt;
        }
        if (safi == afiNsh) {
            return rtrBgpParam.mskNsh;
        }
        if (safi == afiSrte) {
            return rtrBgpParam.mskSrte;
        }
        if (safi == afiLnks) {
            return rtrBgpParam.mskLnks;
        }
        if (safi == afiMvpn) {
            return rtrBgpParam.mskMvpn;
        }
        if (safi == afiMvpo) {
            return rtrBgpParam.mskMvpo;
        }
        logger.info("unknown safi (" + safi + ") requested");
        return -1;
    }

    /**
     * convert mask to safi
     *
     * @param mask mask
     * @return safi
     */
    public int mask2safi(int mask) {
        switch (mask) {
            case rtrBgpParam.mskUni:
                return afiUni;
            case rtrBgpParam.mskLab:
                return afiLab;
            case rtrBgpParam.mskMlt:
                return afiMlt;
            case rtrBgpParam.mskOtrL:
                return afiOtrL;
            case rtrBgpParam.mskOtrU:
                return afiOtrU;
            case rtrBgpParam.mskOtrM:
                return afiOtrM;
            case rtrBgpParam.mskOtrF:
                return afiOtrF;
            case rtrBgpParam.mskOtrS:
                return afiOtrS;
            case rtrBgpParam.mskFlw:
                return afiFlw;
            case rtrBgpParam.mskVpnU:
                return afiVpnU;
            case rtrBgpParam.mskVpnM:
                return afiVpnM;
            case rtrBgpParam.mskVpnF:
                return afiVpnF;
            case rtrBgpParam.mskVpoU:
                return afiVpoU;
            case rtrBgpParam.mskVpoM:
                return afiVpoM;
            case rtrBgpParam.mskVpoF:
                return afiVpoF;
            case rtrBgpParam.mskVpls:
                return afiVpls;
            case rtrBgpParam.mskMspw:
                return afiMspw;
            case rtrBgpParam.mskEvpn:
                return afiEvpn;
            case rtrBgpParam.mskMdt:
                return afiMdt;
            case rtrBgpParam.mskNsh:
                return afiNsh;
            case rtrBgpParam.mskSrte:
                return afiSrte;
            case rtrBgpParam.mskLnks:
                return afiLnks;
            case rtrBgpParam.mskMvpn:
                return afiMvpn;
            case rtrBgpParam.mskMvpo:
                return afiMvpo;
            default:
                logger.info("unknown safi (" + mask + ") requested");
                return -1;
        }
    }

    /**
     * mask to list
     *
     * @param mask mask
     * @return list
     */
    public List<Integer> mask2list(int mask) {
        List<Integer> safis = new ArrayList<Integer>();
        if ((mask & rtrBgpParam.mskUni) != 0) {
            safis.add(afiUni);
        }
        if ((mask & rtrBgpParam.mskLab) != 0) {
            safis.add(afiLab);
        }
        if ((mask & rtrBgpParam.mskMlt) != 0) {
            safis.add(afiMlt);
        }
        if ((mask & rtrBgpParam.mskOtrL) != 0) {
            safis.add(afiOtrL);
        }
        if ((mask & rtrBgpParam.mskOtrU) != 0) {
            safis.add(afiOtrU);
        }
        if ((mask & rtrBgpParam.mskOtrM) != 0) {
            safis.add(afiOtrM);
        }
        if ((mask & rtrBgpParam.mskOtrF) != 0) {
            safis.add(afiOtrF);
        }
        if ((mask & rtrBgpParam.mskOtrS) != 0) {
            safis.add(afiOtrS);
        }
        if ((mask & rtrBgpParam.mskFlw) != 0) {
            safis.add(afiFlw);
        }
        if ((mask & rtrBgpParam.mskVpnU) != 0) {
            safis.add(afiVpnU);
        }
        if ((mask & rtrBgpParam.mskVpnM) != 0) {
            safis.add(afiVpnM);
        }
        if ((mask & rtrBgpParam.mskVpnF) != 0) {
            safis.add(afiVpnF);
        }
        if ((mask & rtrBgpParam.mskVpoU) != 0) {
            safis.add(afiVpoU);
        }
        if ((mask & rtrBgpParam.mskVpoM) != 0) {
            safis.add(afiVpoM);
        }
        if ((mask & rtrBgpParam.mskVpoF) != 0) {
            safis.add(afiVpoF);
        }
        if ((mask & rtrBgpParam.mskVpls) != 0) {
            safis.add(afiVpls);
        }
        if ((mask & rtrBgpParam.mskMspw) != 0) {
            safis.add(afiMspw);
        }
        if ((mask & rtrBgpParam.mskEvpn) != 0) {
            safis.add(afiEvpn);
        }
        if ((mask & rtrBgpParam.mskMdt) != 0) {
            safis.add(afiMdt);
        }
        if ((mask & rtrBgpParam.mskNsh) != 0) {
            safis.add(afiNsh);
        }
        if ((mask & rtrBgpParam.mskSrte) != 0) {
            safis.add(afiSrte);
        }
        if ((mask & rtrBgpParam.mskLnks) != 0) {
            safis.add(afiLnks);
        }
        if ((mask & rtrBgpParam.mskMvpn) != 0) {
            safis.add(afiMvpn);
        }
        if ((mask & rtrBgpParam.mskMvpo) != 0) {
            safis.add(afiMvpo);
        }
        return safis;
    }

    /**
     * get database
     *
     * @param safi safi to query
     * @return table
     */
    public tabRoute<addrIP> getDatabase(int safi) {
        if (safi == afiUni) {
            return routerComputedU;
        }
        if (safi == afiLab) {
            return routerComputedU;
        }
        if (safi == afiMlt) {
            return routerComputedM;
        }
        if (safi == afiOtrL) {
            return computedOtrU;
        }
        if (safi == afiOtrU) {
            return computedOtrU;
        }
        if (safi == afiOtrM) {
            return computedOtrM;
        }
        if (safi == afiOtrF) {
            return computedOtrF;
        }
        if (safi == afiOtrS) {
            return computedOtrS;
        }
        if (safi == afiFlw) {
            return routerComputedF;
        }
        if (safi == afiVpnU) {
            return computedVpnU;
        }
        if (safi == afiVpnM) {
            return computedVpnM;
        }
        if (safi == afiVpnF) {
            return computedVpnF;
        }
        if (safi == afiVpoU) {
            return computedVpoU;
        }
        if (safi == afiVpoM) {
            return computedVpoM;
        }
        if (safi == afiVpoF) {
            return computedVpoF;
        }
        if (safi == afiVpls) {
            return computedVpls;
        }
        if (safi == afiMspw) {
            return computedMspw;
        }
        if (safi == afiEvpn) {
            return computedEvpn;
        }
        if (safi == afiMdt) {
            return computedMdt;
        }
        if (safi == afiNsh) {
            return computedNsh;
        }
        if (safi == afiSrte) {
            return computedSrte;
        }
        if (safi == afiLnks) {
            return computedLnks;
        }
        if (safi == afiMvpn) {
            return computedMvpn;
        }
        if (safi == afiMvpo) {
            return computedMvpo;
        }
        logger.info("unknown safi (" + safi + ") requested");
        return null;
    }

    /**
     * get changed
     *
     * @param safi safi to query
     * @return table
     */
    public tabRoute<addrIP> getChanged(int safi) {
        if (safi == afiUni) {
            return changedUni;
        }
        if (safi == afiLab) {
            return changedUni;
        }
        if (safi == afiMlt) {
            return changedMlt;
        }
        if (safi == afiOtrL) {
            return changedOtrU;
        }
        if (safi == afiOtrU) {
            return changedOtrU;
        }
        if (safi == afiOtrM) {
            return changedOtrM;
        }
        if (safi == afiOtrF) {
            return changedOtrF;
        }
        if (safi == afiOtrS) {
            return changedOtrS;
        }
        if (safi == afiFlw) {
            return changedFlw;
        }
        if (safi == afiVpnU) {
            return changedVpnU;
        }
        if (safi == afiVpnM) {
            return changedVpnM;
        }
        if (safi == afiVpnF) {
            return changedVpnF;
        }
        if (safi == afiVpoU) {
            return changedVpoU;
        }
        if (safi == afiVpoM) {
            return changedVpoM;
        }
        if (safi == afiVpoF) {
            return changedVpoF;
        }
        if (safi == afiVpls) {
            return changedVpls;
        }
        if (safi == afiMspw) {
            return changedMspw;
        }
        if (safi == afiEvpn) {
            return changedEvpn;
        }
        if (safi == afiMdt) {
            return changedMdt;
        }
        if (safi == afiNsh) {
            return changedNsh;
        }
        if (safi == afiSrte) {
            return changedSrte;
        }
        if (safi == afiLnks) {
            return changedLnks;
        }
        if (safi == afiMvpn) {
            return changedMvpn;
        }
        if (safi == afiMvpo) {
            return changedMvpo;
        }
        logger.info("unknown safi (" + safi + ") requested");
        return null;
    }

    public void run() {
        for (;;) {
            if (!cfgInit.booting) {
                break;
            }
            bits.sleep(1000);
        }
        bits.sleep(scanDelay);
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
     * close interface
     *
     * @param ifc interface
     */
    public void closedInterface(ipFwdIface ifc) {
    }

    /**
     * start connection
     *
     * @param pipe pipeline
     * @param id connection
     * @return false if success, true if error
     */
    public boolean streamAccept(pipeSide pipe, prtGenConn id) {
        if (lstnAcl == null) {
            return true;
        }
        if (lstnTmp == null) {
            return true;
        }
        if (!lstnAcl.matches(id)) {
            return true;
        }
        rtrBgpNeigh ntry = new rtrBgpNeigh(this);
        ntry.peerAddr = id.peerAddr.copyBytes();
        ntry.localIfc = id.iface;
        ntry.localAddr = id.iface.addr.copyBytes();
        ntry.updateOddr();
        if (neighs.find(ntry) != null) {
            return true;
        }
        ntry.copyFrom(lstnTmp);
        ntry.template = lstnTmp;
        if (ntry.fallOver) {
            ntry.sendingIfc = ipFwdTab.findSendingIface(fwdCore, ntry.peerAddr);
        }
        ntry.updatePeer();
        rtrBgpNeigh res = lstnNei.put(ntry);
        if (res != null) {
            res.socketMode = 5;
            res.stopNow();
        }
        ntry.conn = new rtrBgpSpeak(this, ntry, pipe);
        ntry.socketMode = 4;
        ntry.startNow();
        return false;
    }

    /**
     * add listen peer
     *
     * @param peer peer address
     * @param temp template to use
     * @return neighbor instance
     */
    public rtrBgpNeigh addListenPeer(addrIP peer, rtrBgpTemp temp) {
        rtrBgpNeigh ntry = new rtrBgpNeigh(this);
        ntry.peerAddr = peer.copyBytes();
        if (neighs.find(ntry) != null) {
            return null;
        }
        ntry.copyFrom(temp);
        ntry.template = temp;
        ntry.updatePeer();
        rtrBgpNeigh res = lstnNei.put(ntry);
        if (res != null) {
            res.socketMode = 5;
            res.stopNow();
        }
        ntry.socketMode = 5;
        return ntry;
    }

    /**
     * get blocking mode
     *
     * @return mode
     */
    public boolean streamForceBlock() {
        return true;
    }

    /**
     * redistribution changed
     */
    public void routerRedistChanged() {
        if (debugger.rtrBgpFull) {
            logger.debug("redist changed");
        }
        needFull.add(1);
        compute.wakeup();
    }

    /**
     * others changed
     */
    public void routerOthersChanged() {
        if (otherTrigger) {
            if (debugger.rtrBgpFull) {
                logger.debug("others changed");
            }
            needFull.add(1);
            compute.wakeup();
            return;
        }
        if ((nhtRoumap != null) || (nhtRouplc != null) || (nhtPfxlst != null)) {
            compute.wakeup();
            return;
        }
    }

    private void computeFull() {
        long tim = bits.getTime();
        if (debugger.rtrBgpIncr) {
            logger.debug("bestpath for everything");
        }
        needFull.set(0);
        tabGen<rtrBgpNeigh> lstn = new tabGen<rtrBgpNeigh>(lstnNei);
        changedUni.clear();
        changedMlt.clear();
        changedOtrU.clear();
        changedOtrM.clear();
        changedOtrF.clear();
        changedOtrS.clear();
        changedFlw.clear();
        changedVpnU.clear();
        changedVpnM.clear();
        changedVpnF.clear();
        changedVpoU.clear();
        changedVpoM.clear();
        changedVpoF.clear();
        changedVpls.clear();
        changedMspw.clear();
        changedEvpn.clear();
        changedMdt.clear();
        changedNsh.clear();
        changedSrte.clear();
        changedLnks.clear();
        changedMvpn.clear();
        changedMvpo.clear();
        tabRoute<addrIP> nUni = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nMlt = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nOtrU = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nOtrM = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nOtrF = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nOtrS = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nFlw = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nVpnU = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nVpnM = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nVpnF = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nVpoU = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nVpoM = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nVpoF = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nVpls = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nMspw = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nEvpn = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nMdt = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nNsh = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nSrte = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nLnks = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nMvpn = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nMvpo = new tabRoute<addrIP>("bst");
        if (flowSpec != null) {
            rtrBgpFlow.doAdvertise(nFlw, flowSpec, new tabRouteEntry<addrIP>(), afiUni == rtrBgpUtil.safiIp6uni, localAs);
        }
        for (int i = 0; i < linkStates.size(); i++) {
            rtrBgpLnkst ls = linkStates.get(i);
            ls.rtr.routerLinkStates(nLnks, ls.par, localAs, routerID);
        }
        for (int i = 0; i < routerRedistedF.size(); i++) {
            tabRouteEntry<addrIP> ntry = routerRedistedF.get(i);
            ntry = ntry.copyBytes(tabRoute.addType.notyet);
            ntry.best.rouTyp = rouTyp;
            ntry.best.protoNum = rtrNum;
            ntry.best.distance = distantLoc;
            nFlw.add(tabRoute.addType.better, ntry, false, false);
        }
        other.doAdvertise(nOtrU, nOtrM, nOtrF);
        for (int i = 0; i < vrfs.size(); i++) {
            vrfs.get(i).doer.doAdvertise(nVpnU, nVpnM, nVpnF, nMvpn);
        }
        for (int i = 0; i < ovrfs.size(); i++) {
            ovrfs.get(i).doer.doAdvertise(nVpoU, nVpoM, nVpoF, nMvpo);
        }
        for (int i = 0; i < vpls.size(); i++) {
            vpls.get(i).doAdvertise(nVpls);
        }
        for (int i = 0; i < evpn.size(); i++) {
            evpn.get(i).doAdvertise(nEvpn);
        }
        origntedOtrU = new tabRoute<addrIP>(nOtrU);
        origntedOtrM = new tabRoute<addrIP>(nOtrM);
        origntedOtrF = new tabRoute<addrIP>(nOtrF);
        origntedOtrS = new tabRoute<addrIP>(nOtrS);
        origntedFlw = new tabRoute<addrIP>(nFlw);
        origntedVpnU = new tabRoute<addrIP>(nVpnU);
        origntedVpnM = new tabRoute<addrIP>(nVpnM);
        origntedVpnF = new tabRoute<addrIP>(nVpnF);
        origntedVpoU = new tabRoute<addrIP>(nVpoU);
        origntedVpoM = new tabRoute<addrIP>(nVpoM);
        origntedVpoF = new tabRoute<addrIP>(nVpoF);
        origntedVpls = new tabRoute<addrIP>(nVpls);
        origntedMspw = new tabRoute<addrIP>(nMspw);
        origntedEvpn = new tabRoute<addrIP>(nEvpn);
        origntedMdt = new tabRoute<addrIP>(nMdt);
        origntedNsh = new tabRoute<addrIP>(nNsh);
        origntedSrte = new tabRoute<addrIP>(nSrte);
        origntedLnks = new tabRoute<addrIP>(nLnks);
        origntedMvpn = new tabRoute<addrIP>(nMvpn);
        origntedMvpo = new tabRoute<addrIP>(nMvpo);
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " rpki");
        }
        computedRpki = new tabRoute<addrIP>("bgp");
        for (int i = 0; i < rpkis.size(); i++) {
            computedRpki.mergeFrom(tabRoute.addType.better, rpkis.get(i).table, tabRouteAttr.distanLim);
        }
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " neighbors");
        }
        groups = new ArrayList<rtrBgpGroup>();
        for (int i = 0; i < lstn.size(); i++) {
            rtrBgpNeigh nei = lstn.get(i);
            if (nei == null) {
                continue;
            }
            nei.setAccepted();
            nei.setGroup();
            nei.setValidity();
            nei.setMerge(nUni, nMlt, nOtrU, nOtrM, nOtrF, nOtrS, nFlw, nVpnU, nVpnM, nVpnF, nVpoU, nVpoM, nVpoF, nVpls, nMspw, nEvpn, nMdt, nNsh, nSrte, nLnks, nMvpn, nMvpo);
        }
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            nei.setAccepted();
            nei.setGroup();
            nei.setValidity();
            nei.setMerge(nUni, nMlt, nOtrU, nOtrM, nOtrF, nOtrS, nFlw, nVpnU, nVpnM, nVpnF, nVpoU, nVpoM, nVpoF, nVpls, nMspw, nEvpn, nMdt, nNsh, nSrte, nLnks, nMvpn, nMvpo);
        }
        if (conquer) {
            if (debugger.rtrBgpComp) {
                logger.debug("round " + compRound + " counquer");
            }
            computeConquerTable(routerComputedU, nUni);
            computeConquerTable(routerComputedM, nMlt);
            computeConquerTable(computedOtrU, nOtrU);
            computeConquerTable(computedOtrM, nOtrM);
            computeConquerTable(computedOtrF, nOtrF);
            computeConquerTable(computedOtrS, nOtrS);
            computeConquerTable(routerComputedF, nFlw);
            computeConquerTable(computedVpnU, nVpnU);
            computeConquerTable(computedVpnM, nVpnM);
            computeConquerTable(computedVpnF, nVpnF);
            computeConquerTable(computedVpoU, nVpoU);
            computeConquerTable(computedVpoM, nVpoM);
            computeConquerTable(computedVpoF, nVpoF);
            computeConquerTable(computedVpls, nVpls);
            computeConquerTable(computedMspw, nMspw);
            computeConquerTable(computedEvpn, nEvpn);
            computeConquerTable(computedMdt, nMdt);
            computeConquerTable(computedNsh, nNsh);
            computeConquerTable(computedSrte, nSrte);
            computeConquerTable(computedLnks, nLnks);
            computeConquerTable(computedMvpn, nMvpn);
            computeConquerTable(computedMvpo, nMvpo);
        }
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " groups");
        }
        for (int i = 0; i < groups.size(); i++) {
            groups.get(i).createNeeded(nUni, nMlt, nOtrU, nOtrM, nOtrF, nOtrS, nFlw, nVpnU, nVpnM, nVpnF, nVpoU, nVpoM, nVpoF, nVpls, nMspw, nEvpn, nMdt, nNsh, nSrte, nLnks, nMvpn, nMvpo);
        }
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " neigroups");
        }
        routerComputedU = nUni;
        routerComputedM = nMlt;
        computedOtrU = nOtrU;
        computedOtrM = nOtrM;
        computedOtrF = nOtrF;
        computedOtrS = nOtrS;
        routerComputedF = nFlw;
        computedVpnU = nVpnU;
        computedVpnM = nVpnM;
        computedVpnF = nVpnF;
        computedVpoU = nVpoU;
        computedVpoM = nVpoM;
        computedVpoF = nVpoF;
        computedVpls = nVpls;
        computedMspw = nMspw;
        computedEvpn = nEvpn;
        computedMdt = nMdt;
        computedNsh = nNsh;
        computedSrte = nSrte;
        computedLnks = nLnks;
        computedMvpn = nMvpn;
        computedMvpo = nMvpo;
        for (int i = 0; i < lstn.size(); i++) {
            rtrBgpNeigh nei = lstn.get(i);
            if (nei == null) {
                continue;
            }
            nei.setNeeded();
        }
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            nei.setNeeded();
        }
        if (segrouLab != null) {
            if (debugger.rtrBgpComp) {
                logger.debug("round " + compRound + " segrou");
            }
            tabGen<tabIndex<addrIP>> segrouUsd = new tabGen<tabIndex<addrIP>>();
            for (int i = 0; i < nUni.size(); i++) {
                tabRouteEntry<addrIP> ntry = nUni.get(i);
                if (ntry == null) {
                    continue;
                }
                if (ntry.best.segrouBeg < 1) {
                    continue;
                }
                if ((ntry.best.segrouIdx <= 0) || (ntry.best.segrouIdx >= segrouMax)) {
                    continue;
                }
                rtrBgpNeigh nei = findPeer(ntry.best.nextHop);
                if (nei == null) {
                    continue;
                }
                List<Integer> lab = tabLabel.int2labels(ntry.best.segrouBeg + ntry.best.segrouIdx);
                segrouLab[ntry.best.segrouIdx].setFwdMpls(13, fwdCore, nei.localIfc, nei.peerAddr, lab);
                tabIndex.add2table(segrouUsd, new tabIndex<addrIP>(ntry.best.segrouIdx, ntry.prefix));
            }
            tabIndex.add2table(segrouUsd, new tabIndex<addrIP>(segrouIdx, new addrPrefix<addrIP>(new addrIP(), 0)));
            segrouLab[segrouIdx].setFwdCommon(13, fwdCore);
            for (int i = 0; i < segrouLab.length; i++) {
                if (segrouUsd.find(new tabIndex<addrIP>(i, null)) != null) {
                    continue;
                }
                segrouLab[i].setFwdDrop(13);
            }
            routerComputedI = segrouUsd;
        }
        if (bierLab != null) {
            if (debugger.rtrBgpComp) {
                logger.debug("round " + compRound + " bier");
            }
            tabLabelBier res = new tabLabelBier(bierLab[0].label, tabLabelBier.num2bsl(bierLen));
            res.idx = bierIdx;
            for (int i = 0; i < nUni.size(); i++) {
                tabRouteEntry<addrIP> ntry = nUni.get(i);
                if (ntry == null) {
                    continue;
                }
                if (ntry.best.bierBeg < 1) {
                    continue;
                }
                if ((ntry.best.bierIdx <= 0) || (ntry.best.bierIdx >= bierMax)) {
                    continue;
                }
                rtrBgpNeigh nei = findPeer(ntry.best.nextHop);
                if (nei == null) {
                    continue;
                }
                tabLabelBierN per = new tabLabelBierN(nei.localIfc, nei.peerAddr, ntry.best.bierBeg);
                tabLabelBierN old = res.peers.add(per);
                if (old != null) {
                    per = old;
                }
                per.setBit(ntry.best.bierIdx - 1);
            }
            for (int i = 0; i < bierLab.length; i++) {
                bierLab[i].setBierMpls(22, fwdCore, res);
            }
        }
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " export");
        }
        otherTrigger = (addrFams & rtrBgpParam.mskLab) != 0;
        otherTrigger |= linkStates.size() > 0;
        if (flowInst) {
            fwdCore.flowspec = tabQos.convertPolicy(rtrBgpFlow.doDecode(routerComputedF, afiUni == rtrBgpUtil.safiIp6uni));
        }
        other.doPeers(nOtrU, nOtrM, nOtrF);
        for (int i = 0; i < vrfs.size(); i++) {
            otherTrigger |= vrfs.get(i).doer.doPeers(nVpnU, nVpnM, nVpnF);
        }
        for (int i = 0; i < ovrfs.size(); i++) {
            otherTrigger |= ovrfs.get(i).doer.doPeers(nVpoU, nVpoM, nVpoF);
        }
        for (int i = 0; i < vpls.size(); i++) {
            vpls.get(i).doPeers(nVpls);
        }
        for (int i = 0; i < evpn.size(); i++) {
            evpn.get(i).doPeers(nEvpn);
        }
        fullLast = bits.getTime();
        fullTime = (int) (fullLast - tim);
        fullCount++;
    }

    private tabRouteEntry<addrIP> computeIncrBest(int afi, rtrBgpNeigh nei, tabRouteEntry<addrIP> best, tabRouteEntry<addrIP> curr) {
        if (nei == null) {
            return best;
        }
        if (!nei.reachable) {
            return best;
        }
        tabRoute<addrIP> acc = nei.getAccepted(afi);
        if (acc == null) {
            if (debugger.rtrBgpFull) {
                logger.debug("table not found");
            }
            needFull.add(1);
            return best;
        }
        tabRouteEntry<addrIP> ntry = acc.find(curr);
        if (ntry == null) {
            return best;
        }
        if ((rpkis.size() > 0) && ((afi == afiUni) || (afi == afiMlt))) {
            setValidity(ntry);
        }
        if (best == null) {
            return ntry.copyBytes(tabRoute.addType.lnkEcmp);
        }
        if (best.best.isOtherBetter(ntry.best, false)) {
            return ntry.copyBytes(tabRoute.addType.lnkEcmp);
        }
        if (ntry.best.isOtherBetter(best.best, false)) {
            return best;
        }
        ntry = ntry.copyBytes(tabRoute.addType.lnkEcmp);
        best.addAlt(ntry.alts);
        return best;
    }

    private void computeIncrVersion(tabRouteEntry<addrIP> curr) {
        int ver = compRound.get() + 1;
        for (int i = 0; i < curr.alts.size(); i++) {
            curr.alts.get(i).version = ver;
        }
    }

    private void computeIncrEntry(int afi, tabRouteEntry<addrIP> curr, tabRoute<addrIP> cmp, tabRoute<addrIP> org) {
        if (debugger.rtrBgpIncr) {
            logger.debug("bestpath for " + tabRtrmapN.rd2string(curr.rouDst) + " " + curr.prefix + " in " + rtrBgpUtil.safi2string(afi));
        }
        tabRouteEntry<addrIP> best = org.find(curr);
        if (best != null) {
            best = best.copyBytes(tabRoute.addType.altEcmp);
            best.best.rouSrc = rtrBgpUtil.peerOriginate;
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            best = computeIncrBest(afi, lstnNei.get(i), best, curr);
        }
        for (int i = 0; i < neighs.size(); i++) {
            best = computeIncrBest(afi, neighs.get(i), best, curr);
        }
        if (best == null) {
            cmp.del(curr);
            computeIncrVersion(curr);
            for (int i = 0; i < groups.size(); i++) {
                rtrBgpGroup grp = groups.get(i);
                tabRoute<addrIP> wil = grp.getWilling(afi);
                tabRoute<addrIP> chg = grp.getChanged(afi);
                if ((wil == null) || (chg == null)) {
                    if (debugger.rtrBgpFull) {
                        logger.debug("table not found");
                    }
                    needFull.add(1);
                    continue;
                }
                if (wil.del(curr)) {
                    continue;
                }
                chg.add(tabRoute.addType.always, curr, false, false);
            }
            return;
        }
        if (routerEcmp) {
            best.hashBest();
        } else {
            best.selectBest();
        }
        computeIncrVersion(best);
        if (conquer) {
            tabRouteEntry<addrIP> res = computeConquerEntry(cmp, best);
            if (res != null) {
                best = res;
            }
        }
        if ((best.best.rouSrc == rtrBgpUtil.peerOriginate) && ((afi == afiUni) || (afi == afiMlt))) {
            cmp.del(best);
        } else {
            cmp.add(tabRoute.addType.always, best, false, false);
        }
        for (int i = 0; i < groups.size(); i++) {
            rtrBgpGroup grp = groups.get(i);
            tabRoute<addrIP> wil = grp.getWilling(afi);
            tabRoute<addrIP> chg = grp.getChanged(afi);
            if ((wil == null) || (chg == null)) {
                if (debugger.rtrBgpFull) {
                    logger.debug("table not found");
                }
                needFull.add(1);
                continue;
            }
            tabRouteEntry<addrIP> ntry = null;
            tabRouteEntry<addrIP> old = wil.find(best);
            if (best.best.rouSrc == rtrBgpUtil.peerOriginate) {
                ntry = grp.originatePrefix(afi, best);
            } else {
                ntry = grp.readvertPrefix(afi, best);
            }
            if ((afi == afiUni) || (afi == afiMlt)) {
                ntry = tabRoute.doUpdateEntry(afi, grp.remoteAs, ntry, grp.roumapOut, grp.roupolOut, grp.prflstOut);
            } else if ((afi == afiOtrU) || (afi == afiOtrM)) {
                ntry = tabRoute.doUpdateEntry(afi, grp.remoteAs, ntry, grp.oroumapOut, grp.oroupolOut, grp.oprflstOut);
            } else if ((afi == afiOtrF) || (afi == afiOtrS) || (afi == afiMvpo) || (afi == afiVpoU) || (afi == afiVpoM) || (afi == afiVpoF)) {
                ntry = tabRoute.doUpdateEntry(afi, grp.remoteAs, ntry, grp.wroumapOut, grp.wroupolOut, null);
            } else {
                ntry = tabRoute.doUpdateEntry(afi, grp.remoteAs, ntry, grp.vroumapOut, grp.vroupolOut, null);
            }
            if ((ntry == null) && (old == null)) {
                continue;
            }
            if (ntry == null) {
                wil.del(best);
                chg.add(tabRoute.addType.always, best, false, false);
                continue;
            }
            if (!ntry.differs(tabRoute.addType.alters, old)) {
                continue;
            }
            wil.add(tabRoute.addType.always, ntry, false, false);
            chg.add(tabRoute.addType.always, ntry, false, false);
        }
    }

    private void computeIncrUpdate(int afi, tabRoute<addrIP> chg, tabRoute<addrIP> cmp, tabRoute<addrIP> org) {
        for (int i = chg.size() - 1; i >= 0; i--) {
            tabRouteEntry<addrIP> ntry = chg.get(i);
            chg.del(ntry);
            computeIncrEntry(afi, ntry, cmp, org);
        }
    }

    private void computeIncrPurge(int ver, tabRoute<addrIP> chg) {
        for (int i = chg.size() - 1; i >= 0; i--) {
            tabRouteEntry<addrIP> ntry = chg.get(i);
            if (ntry.best.version >= ver) {
                continue;
            }
            chg.del(ntry);
        }
    }

    private boolean computeIncr() {
        long tim = bits.getTime();
        if (changedCur > incrLimit) {
            if (debugger.rtrBgpFull) {
                logger.debug("limit exceeded");
            }
            return true;
        }
        if (routerAggregating.size() > 0) {
            if (debugger.rtrBgpFull) {
                logger.debug("aggregation");
            }
            oldAggr = true;
            return true;
        } else {
            if (oldAggr) {
                if (debugger.rtrBgpFull) {
                    logger.debug("old aggregation");
                }
                oldAggr = false;
                return true;
            }
        }
        if ((segrouLab != null) || (bierLab != null)) {
            return true;
        }
        for (int i = 0; i < groups.size(); i++) {
            rtrBgpGroup grp = groups.get(i);
            if (grp.sendDefRou || grp.sendOtrDefRou) {
                return true;
            }
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            rtrBgpNeigh nei = lstnNei.get(i);
            if (nei == null) {
                continue;
            }
            if (nei.softReconfig) {
                return true;
            }
            nei.setAccepted();
            if (nei.reachOld != nei.reachable) {
                return true;
            }
        }
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            if (nei.softReconfig) {
                return true;
            }
            nei.setAccepted();
            if (nei.reachOld != nei.reachable) {
                return true;
            }
        }
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " purge");
        }
        for (int i = 0; i < groups.size(); i++) {
            groups.get(i).minversion = compRound.get();
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            rtrBgpNeigh nei = lstnNei.get(i);
            if (nei == null) {
                continue;
            }
            nei.setGrpVer();
        }
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            nei.setGrpVer();
        }
        groupMin = compRound.get();
        groupMax = 0;
        for (int i = 0; i < groups.size(); i++) {
            rtrBgpGroup grp = groups.get(i);
            if (grp.minversion < groupMin) {
                groupMin = grp.minversion;
            }
            if (grp.minversion > groupMax) {
                groupMax = grp.minversion;
            }
            computeIncrPurge(grp.minversion, grp.chgUni);
            computeIncrPurge(grp.minversion, grp.chgMlt);
            computeIncrPurge(grp.minversion, grp.chgOtrU);
            computeIncrPurge(grp.minversion, grp.chgOtrM);
            computeIncrPurge(grp.minversion, grp.chgOtrF);
            computeIncrPurge(grp.minversion, grp.chgOtrS);
            computeIncrPurge(grp.minversion, grp.chgFlw);
            computeIncrPurge(grp.minversion, grp.chgVpnU);
            computeIncrPurge(grp.minversion, grp.chgVpnM);
            computeIncrPurge(grp.minversion, grp.chgVpnF);
            computeIncrPurge(grp.minversion, grp.chgVpoU);
            computeIncrPurge(grp.minversion, grp.chgVpoM);
            computeIncrPurge(grp.minversion, grp.chgVpoF);
            computeIncrPurge(grp.minversion, grp.chgVpls);
            computeIncrPurge(grp.minversion, grp.chgMspw);
            computeIncrPurge(grp.minversion, grp.chgEvpn);
            computeIncrPurge(grp.minversion, grp.chgMdt);
            computeIncrPurge(grp.minversion, grp.chgNsh);
            computeIncrPurge(grp.minversion, grp.chgSrte);
            computeIncrPurge(grp.minversion, grp.chgLnks);
            computeIncrPurge(grp.minversion, grp.chgMvpn);
            computeIncrPurge(grp.minversion, grp.chgMvpo);
        }
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " changes");
        }
        computeIncrUpdate(afiUni, changedUni, routerComputedU, routerRedistedU);
        computeIncrUpdate(afiMlt, changedMlt, routerComputedM, routerRedistedM);
        computeIncrUpdate(afiOtrU, changedOtrU, computedOtrU, origntedOtrU);
        computeIncrUpdate(afiOtrM, changedOtrM, computedOtrM, origntedOtrM);
        computeIncrUpdate(afiOtrF, changedOtrF, computedOtrF, origntedOtrF);
        computeIncrUpdate(afiOtrS, changedOtrS, computedOtrS, origntedOtrS);
        computeIncrUpdate(afiFlw, changedFlw, routerComputedF, origntedFlw);
        computeIncrUpdate(afiVpnU, changedVpnU, computedVpnU, origntedVpnU);
        computeIncrUpdate(afiVpnM, changedVpnM, computedVpnM, origntedVpnM);
        computeIncrUpdate(afiVpnF, changedVpnF, computedVpnF, origntedVpnF);
        computeIncrUpdate(afiVpoU, changedVpoU, computedVpoU, origntedVpoU);
        computeIncrUpdate(afiVpoM, changedVpoM, computedVpoM, origntedVpoM);
        computeIncrUpdate(afiVpoF, changedVpoF, computedVpoF, origntedVpoF);
        computeIncrUpdate(afiVpls, changedVpls, computedVpls, origntedVpls);
        computeIncrUpdate(afiMspw, changedMspw, computedMspw, origntedMspw);
        computeIncrUpdate(afiEvpn, changedEvpn, computedEvpn, origntedEvpn);
        computeIncrUpdate(afiMdt, changedMdt, computedMdt, origntedMdt);
        computeIncrUpdate(afiNsh, changedNsh, computedNsh, origntedNsh);
        computeIncrUpdate(afiSrte, changedSrte, computedSrte, origntedSrte);
        computeIncrUpdate(afiLnks, changedLnks, computedLnks, origntedLnks);
        computeIncrUpdate(afiMvpn, changedMvpn, computedMvpn, origntedMvpn);
        computeIncrUpdate(afiMvpo, changedMvpo, computedMvpo, origntedMvpo);
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " export");
        }
        if (flowInst) {
            fwdCore.flowspec = tabQos.convertPolicy(rtrBgpFlow.doDecode(routerComputedF, afiUni == rtrBgpUtil.safiIp6uni));
        }
        other.doPeers(computedOtrU, computedOtrM, computedOtrF);
        for (int i = 0; i < vrfs.size(); i++) {
            vrfs.get(i).doer.doPeers(computedVpnU, computedVpnM, computedVpnF);
        }
        for (int i = 0; i < ovrfs.size(); i++) {
            ovrfs.get(i).doer.doPeers(computedVpoU, computedVpoM, computedVpoF);
        }
        for (int i = 0; i < vpls.size(); i++) {
            vpls.get(i).doPeers(computedVpls);
        }
        for (int i = 0; i < evpn.size(); i++) {
            evpn.get(i).doPeers(computedEvpn);
        }
        incrLast = bits.getTime();
        incrTime = (int) (incrLast - tim);
        incrCount++;
        return false;
    }

    private tabRouteEntry<addrIP> computeConquerEntry(tabRoute<addrIP> cmp, tabRouteEntry<addrIP> best) {
        if (best.best.nextHop == null) {
            return null;
        }
        tabRouteEntry<addrIP> old = cmp.find(best);
        if (old == null) {
            return null;
        }
        if (old.best.nextHop == null) {
            return null;
        }
        best = best.copyBytes(tabRoute.addType.notyet);
        if (best.best.locPref < old.best.locPref) {
            best.best.locPref = old.best.locPref;
        }
        if (old.best.nextHop.compare(old.best.nextHop, best.best.nextHop) != 0) {
            best.best.locPref++;
        }
        return best;
    }

    private void computeConquerTable(tabRoute<addrIP> old, tabRoute<addrIP> cmp) {
        for (int i = 0; i < cmp.size(); i++) {
            tabRouteEntry<addrIP> ntry = cmp.get(i);
            ntry = computeConquerEntry(old, ntry);
            if (ntry == null) {
                continue;
            }
            cmp.add(tabRoute.addType.always, ntry, false, false);
        }
    }

    /**
     * set validity
     *
     * @param ntry entry to update
     */
    protected void setValidity(tabRouteEntry<addrIP> ntry) {
        tabRouteEntry<addrIP> res = computedRpki.route(ntry.prefix.broadcast);
        for (int o = 0; o < ntry.alts.size(); o++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(o);
            if (res == null) {
                attr.validity = 2;
                continue;
            }
            if (attr.pathSeq == null) {
                attr.validity = 3;
                continue;
            }
            int i = attr.pathSeq.size();
            if (i < 1) {
                attr.validity = 3;
                continue;
            }
            if (attr.pathSeq.get(i - 1) != res.best.rouSrc) {
                attr.validity = 3;
                continue;
            }
            attr.validity = 1;
        }
        ntry.selectBest();
    }

    /**
     * update flap statistics
     *
     * @param afi afi
     * @param rd rd
     * @param prf prefix
     * @param pth path
     */
    protected void prefixFlapped(int afi, long rd, addrPrefix<addrIP> prf, String pth) {
        rtrBgpFlap ntry = new rtrBgpFlap();
        ntry.afi = afi;
        ntry.rd = rd;
        ntry.prefix = prf.copyBytes();
        rtrBgpFlap old = flaps.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.count++;
        ntry.last = bits.getTime();
        rtrBgpFlapath pe = new rtrBgpFlapath();
        pe.path = pth;
        rtrBgpFlapath op = ntry.paths.add(pe);
        if (op != null) {
            pe = op;
        }
        pe.count++;
        pe.last = ntry.last;
    }

    /**
     * create computed table
     */
    public synchronized void routerCreateComputed() {
        if (debugger.rtrBgpEvnt) {
            logger.debug("create table");
        }
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " start");
        }
        changedCur = changedUni.size() + changedMlt.size() + changedOtrU.size()
                + changedOtrM.size() + changedOtrF.size() + changedOtrS.size() + changedFlw.size()
                + changedVpnU.size() + changedVpnM.size() + changedVpnF.size()
                + changedVpoU.size() + changedVpoM.size() + changedVpoF.size()
                + changedVpls.size() + changedMspw.size() + changedEvpn.size()
                + changedMdt.size() + changedNsh.size() + changedSrte.size()
                + changedLnks.size() + changedMvpn.size() + changedMvpo.size();
        changedTot += changedCur;
        if (needFull.get() > 0) {
            computeFull();
        } else if (computeIncr()) {
            computeFull();
        }
        compRound.add(1);
        for (int i = 0; i < lstnNei.size(); i++) {
            rtrBgpNeigh nei = lstnNei.get(i);
            if (nei == null) {
                continue;
            }
            nei.transmit.wakeup();
        }
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            nei.transmit.wakeup();
        }
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " done");
        }
        fwdCore.routerChg(this);
    }

    /**
     * stop work
     */
    public void routerCloseNow() {
        if (debugger.rtrBgpEvnt) {
            logger.debug("shutdown");
        }
        need2run = false;
        compute.wakeup();
        for (int i = 0; i < rpkis.size(); i++) {
            rtrBgpRpki ntry = rpkis.get(i);
            ntry.stopNow();
        }
        for (int i = 0; i < mons.size(); i++) {
            rtrBgpMon ntry = mons.get(i);
            ntry.stopNow();
        }
        for (int i = 0; i < dmps.size(); i++) {
            rtrBgpMrt ntry = dmps.get(i);
            ntry.fileHandle.close();
        }
        if (lstnTmp != null) {
            tcpCore.listenStop(lstnIfc, port, null, 0);
        }
        for (int i = lstnNei.size() - 1; i >= 0; i--) {
            rtrBgpNeigh nei = lstnNei.get(i);
            if (nei == null) {
                continue;
            }
            nei.stopNow();
            nei.conn.closeNow();
        }
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            nei.stopNow();
            nei.conn.closeNow();
        }
        other.unregister2ip();
        for (int i = 0; i < vrfs.size(); i++) {
            vrfs.get(i).doer.unregister2ip();
        }
        for (int i = 0; i < ovrfs.size(); i++) {
            ovrfs.get(i).doer.unregister2ip();
        }
        for (int i = 0; i < vpls.size(); i++) {
            vpls.get(i).doStop();
        }
        for (int i = 0; i < evpn.size(); i++) {
            evpn.get(i).doStop();
        }
        tabLabel.release(evpnUni, 10);
        tabLabel.release(evpnMul, 10);
        tabLabel.release(segrouLab, 13);
        tabLabel.release(bierLab, 22);
        fwdCore.routerDel(this);
    }

    /**
     * get help
     *
     * @param l list
     */
    public void routerGetHelp(userHelping l) {
        List<String> tmps = new ArrayList<String>();
        for (int i = 0; i < temps.size(); i++) {
            rtrBgpTemp ntry = temps.get(i);
            tmps.add(ntry.tempName);
        }
        l.add(null, "1 2   address-family              specify address families");
        rtrBgpParam.getAfiList(l, "2 2,.", "to use", true);
        l.add(null, "1 2   local-as                    specify local as number");
        l.add(null, "2 .     <num>                     autonomous system number");
        l.add(null, "1 .   conquer                     conquer bestpath advertisements");
        l.add(null, "1 .   flapstat                    count flap statistics");
        l.add(null, "1 .   safe-ebgp                   safe ebgp policy");
        l.add(null, "1 2   incremental                 limit on incremental bestpath calculation");
        l.add(null, "2 .     <num>                     maximum prefixes");
        l.add(null, "1 2   router-id                   specify router id");
        l.add(null, "2 .     <addr>                    router id");
        l.add(null, "1 2   scantime                    scan time interval");
        l.add(null, "2 .     <num>                     ms between scans");
        l.add(null, "1 2   scandelay                   initial scan time delay");
        l.add(null, "2 .     <num>                     ms before scan");
        l.add(null, "1 2   graceful-restart            graceful restart interval");
        l.add(null, "2 .     <num>                     ms to recover");
        l.add(null, "1 2   template                    specify template parameters");
        l.add(tmps, "2 3     <name:loc>                name of template");
        rtrBgpParam.getParamHelp(l);
        l.add(null, "1 2   nexthop                     specify next hop tracking parameter");
        l.add(null, "2 3     recursion                 specify recursion depth");
        l.add(null, "3 .       <num>                   maximum rounds");
        l.add(null, "2 3     route-map                 filter next hops");
        l.add(null, "3 .       <name:rm>               name of route map");
        l.add(null, "2 3     route-policy              filter next hops");
        l.add(null, "3 .       <name:rpl>              name of route policy");
        l.add(null, "2 3     prefix-list               filter next hops");
        l.add(null, "3 .       <name:pl>               name of prefix list");
        l.add(null, "1 2   segrout                     segment routing parameters");
        l.add(null, "2 3     <num>                     maximum index");
        l.add(null, "3 4,.     <num>                   this node index");
        l.add(null, "4 5         base                  specify base");
        l.add(null, "5 4,.         <num>               label base");
        l.add(null, "1 2   bier                        bier parameters");
        l.add(null, "2 3     <num>                     bitstring length");
        l.add(null, "3 4       <num>                   maximum index");
        l.add(null, "4 .         <num>                 this node index");
        l.add(null, "1 2   afi-links                   specify link state parameter");
        cfgRtr.getRouterList(l, 0, " to advertise");
        l.add(null, "3 4       <num>                   process id");
        l.add(null, "4 .         <num>                 area/level number");
        l.add(null, "1 .   flowspec-install            specify flowspec installation");
        l.add(null, "1 2   flowspec-advert             specify flowspec parameter");
        l.add(null, "2 .     <name:pm>                 name of policy map");
        l.add(null, "1 2   neighbor                    specify neighbor parameters");
        l.add(null, "2 3     <addr>                    address of peer");
        l.add(null, "3 4       template                get configuration from template");
        l.add(tmps, "4 5,.       <name:loc>            name of source template");
        l.add(null, "5 .           shutdown            connection disabled for this peer");
        rtrBgpParam.getParamHelp(l);
        l.add(null, "1 2   distance                    specify default distance");
        l.add(null, "2 3     <num>                     external peer distance");
        l.add(null, "3 4       <num>                   internal peer distance");
        l.add(null, "4 .         <num>                 locally generated distance");
        l.add(null, "1 2   listen                      passively listen for clients");
        l.add(null, "2 3     <name:acl>                access list name");
        l.add(tmps, "3 .       <name:loc>              template name");
        l.add(null, "1 2   dump                        setup bgp dump file");
        l.add(null, "2 3     <str>                     name of mrt");
        l.add(null, "3 4,.     <file>                  name of file");
        l.add(null, "4 5         <num>                 ms between backup");
        l.add(null, "5 6,.         <file>              name of backup");
        l.add(null, "6 .             <num>             maximum size of backup");
        l.add(null, "1 2   monitor                     setup bgp monitor protocol server");
        l.add(null, "2 3     <str>                     name of bmp");
        l.add(null, "3 4       <name:prx>              proxy profile");
        l.add(null, "4 5         <str>                 hostname");
        l.add(null, "5 .           <num>               port number");
        l.add(null, "1 2   rpki                        setup resource public key infrastructure server");
        l.add(null, "2 3     <str>                     name of rpki");
        l.add(null, "3 4       <name:prx>              proxy profile");
        l.add(null, "4 5         <str>                 hostname");
        l.add(null, "5 .           <num>               port number");
        l.add(null, "1 2   afi-other                   select other to advertise");
        l.add(null, "2 .     enable                    enable processing");
        l.add(null, "2 .     vpn-mode                  enable vpn mode");
        l.add(null, "2 3     srv6                      srv6 advertisement");
        l.add(null, "3 .       <name:ifc>              select source to advertise");
        l.add(null, "2 3     distance                  set import distance");
        l.add(null, "3 .       <num>                   distance");
        l.add(null, "2 .     flowspec-install          specify flowspec installation");
        l.add(null, "2 3     flowspec-advert           specify flowspec parameter");
        l.add(null, "3 .       <name:pm>               name of policy map");
        cfgRtr.getRedistHelp(l, 1);
        l.add(null, "1 2   afi-vrf                     select vrf to advertise");
        l.add(null, "2 3     <name:vrf>                name of routing table");
        l.add(null, "3 .       enable                  enable processing");
        l.add(null, "3 4       mvpn                    mvpn advertisement");
        l.add(null, "4 .         <name:ifc>            select source to advertise");
        l.add(null, "3 4       srv6                    srv6 advertisement");
        l.add(null, "4 .         <name:ifc>            select source to advertise");
        l.add(null, "3 4       distance                set import distance");
        l.add(null, "4 .         <num>                 distance");
        l.add(null, "3 .       default-originate       generate default route");
        l.add(null, "3 .       flowspec-install        specify flowspec installation");
        l.add(null, "3 4       flowspec-advert         specify flowspec parameter");
        l.add(null, "4 .         <name:pm>             name of policy map");
        cfgRtr.getRedistHelp(l, 2);
        l.add(null, "1 2   afi-ovrf                    select other vrf to advertise");
        l.add(null, "2 3     <name:vrf>                name of routing table");
        l.add(null, "3 .       enable                  enable processing");
        l.add(null, "3 4       mvpn                    mvpn advertisement");
        l.add(null, "4 .         <name:ifc>            select source to advertise");
        l.add(null, "3 4       srv6                    srv6 advertisement");
        l.add(null, "4 .         <name:ifc>            select source to advertise");
        l.add(null, "3 4       distance                set import distance");
        l.add(null, "4 .         <num>                 distance");
        l.add(null, "3 .       default-originate       generate default route");
        l.add(null, "3 .       flowspec-install        specify flowspec installation");
        l.add(null, "3 4       flowspec-advert         specify flowspec parameter");
        l.add(null, "4 .         <name:pm>             name of policy map");
        cfgRtr.getRedistHelp(l, 2);
        l.add(null, "1 2   afi-vpls                    select vpls to advertise");
        l.add(null, "2 3     <id>                      vpls id in ASnum:IDnum format");
        l.add(null, "3 4       bridge-group            enable processing");
        l.add(null, "4 .         <num>                 bridge group number");
        l.add(null, "3 4       update-source           select source to advertise");
        l.add(null, "4 .         <name:ifc>            name of interface");
        l.add(null, "3 4       ve-id                   specify ve id");
        l.add(null, "4 5         <num>                 ve id number");
        l.add(null, "5 .           <num>               ve maximum number");
        l.add(null, "1 2   afi-evpn                    select evpn to advertise");
        l.add(null, "2 3     <id>                      evpn id");
        l.add(null, "3 4       bridge-group            enable processing");
        l.add(null, "4 .         <num>                 bridge group number");
        l.add(null, "3 4       srv6                    srv6 advertisement");
        l.add(null, "4 .         <name:ifc>            select source to advertise");
        l.add(null, "3 4       bmac                    set backbone mac");
        l.add(null, "4 .         <addr>                mac address");
        l.add(null, "3 4       update-source           select source to advertise");
        l.add(null, "4 .         <name:ifc>            name of interface");
        l.add(null, "3 4       encapsulation           specify encapsulation to use");
        l.add(null, "4 .         pbb                   pbb");
        l.add(null, "4 .         vxlan                 vxlan");
        l.add(null, "4 .         vpws                  vpws");
        l.add(null, "4 .         cmac                  cmac");
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void routerGetConfig(List<String> l, String beg, int filter) {
        l.add(beg + "local-as " + bits.num2str(localAs));
        l.add(beg + "router-id " + routerID);
        cmds.cfgLine(l, !safeEbgp, beg, "safe-ebgp", "");
        l.add(beg + "address-family" + rtrBgpParam.mask2string(addrFams));
        l.add(beg + "distance " + distantExt + " " + distantInt + " " + distantLoc);
        l.add(beg + "scantime " + scanTime);
        l.add(beg + "scandelay " + scanDelay);
        l.add(beg + "incremental " + incrLimit);
        l.add(beg + "graceful-restart " + restartTime);
        cmds.cfgLine(l, !conquer, beg, "conquer", "");
        cmds.cfgLine(l, flaps == null, beg, "flapstat", "");
        cmds.cfgLine(l, nhtRoumap == null, beg, "nexthop route-map", "" + nhtRoumap);
        cmds.cfgLine(l, nhtRouplc == null, beg, "nexthop route-policy", "" + nhtRouplc);
        cmds.cfgLine(l, nhtPfxlst == null, beg, "nexthop prefix-list", "" + nhtPfxlst);
        l.add(beg + "nexthop recursion " + recursion);
        String a = "";
        if (segrouBase != 0) {
            a += " base " + segrouBase;
        }
        cmds.cfgLine(l, segrouMax < 1, beg, "segrout", "" + segrouMax + " " + segrouIdx + a);
        cmds.cfgLine(l, bierMax < 1, beg, "bier", bierLen + " " + bierMax + " " + bierIdx);
        for (int i = 0; i < mons.size(); i++) {
            mons.get(i).getConfig(l, beg);
        }
        for (int i = 0; i < dmps.size(); i++) {
            dmps.get(i).getConfig(l, beg);
        }
        for (int i = 0; i < rpkis.size(); i++) {
            rpkis.get(i).getConfig(l, beg);
        }
        for (int i = 0; i < temps.size(); i++) {
            temps.get(i).getConfig(l, beg, filter);
        }
        if (lstnTmp == null) {
            l.add(beg + "no listen");
        } else {
            l.add(beg + "listen " + lstnAcl.listName + " " + lstnTmp.tempName);
        }
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            nei.getConfig(l, beg, filter);
        }
        other.getConfig(l, beg + "afi-other ");
        for (int i = 0; i < vrfs.size(); i++) {
            vrfs.get(i).doer.getConfig(l, beg);
        }
        for (int i = 0; i < ovrfs.size(); i++) {
            ovrfs.get(i).doer.getConfig(l, beg);
        }
        for (int i = 0; i < vpls.size(); i++) {
            vpls.get(i).getConfig(l, beg);
        }
        for (int i = 0; i < evpn.size(); i++) {
            evpn.get(i).getConfig(l, beg);
        }
        for (int i = 0; i < linkStates.size(); i++) {
            rtrBgpLnkst ls = linkStates.get(i);
            l.add(beg + "afi-links " + ls.rtr.routerGetName() + " " + ls.par);
        }
        cmds.cfgLine(l, !flowInst, beg, "flowspec-install", "");
        cmds.cfgLine(l, flowSpec == null, beg, "flowspec-advert", "" + flowSpec);
    }

    /**
     * configure router
     *
     * @param cmd command
     * @return false if success, true if error
     */
    public boolean routerConfigure(cmds cmd) {
        String s = cmd.word();
        boolean negated = false;
        if (s.equals("no")) {
            s = cmd.word();
            negated = true;
        }
        if (s.equals("local-as")) {
            localAs = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("router-id")) {
            routerID.fromString(cmd.word());
            return false;
        }
        if (s.equals("safe-ebgp")) {
            safeEbgp = !negated;
            if (!safeEbgp) {
                return false;
            }
            for (int i = 0; i < neighs.size(); i++) {
                rtrBgpNeigh nei = neighs.get(i);
                if (nei == null) {
                    continue;
                }
                if (!nei.checkShutdown()) {
                    continue;
                }
                nei.flapBgpConn();
            }
            for (int i = 0; i < lstnNei.size(); i++) {
                rtrBgpNeigh nei = lstnNei.get(i);
                if (nei == null) {
                    continue;
                }
                if (!nei.checkShutdown()) {
                    continue;
                }
                nei.flapBgpConn();
            }
            return false;
        }
        if (s.equals("address-family")) {
            addrFams = rtrBgpParam.string2mask(cmd);
            return false;
        }
        if (s.equals("distance")) {
            distantExt = bits.str2num(cmd.word());
            distantInt = bits.str2num(cmd.word());
            distantLoc = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("scantime")) {
            scanTime = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("scandelay")) {
            scanDelay = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("incremental")) {
            incrLimit = bits.str2num(cmd.word());
            needFull.add(1);
            compute.wakeup();
            return false;
        }
        if (s.equals("conquer")) {
            conquer = !negated;
            needFull.add(1);
            compute.wakeup();
            return false;
        }
        if (s.equals("flapstat")) {
            if (negated) {
                flaps = null;
            } else {
                flaps = new tabGen<rtrBgpFlap>();
            }
            return false;
        }
        if (s.equals("segrout")) {
            tabLabel.release(segrouLab, 13);
            segrouLab = null;
            if (negated) {
                segrouIdx = 0;
                segrouMax = 0;
                segrouBase = 0;
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            segrouMax = bits.str2num(cmd.word());
            segrouIdx = bits.str2num(cmd.word());
            segrouBase = 0;
            for (;;) {
                s = cmd.word();
                if (s.length() < 1) {
                    break;
                }
                if (s.equals("base")) {
                    segrouBase = bits.str2num(cmd.word());
                    continue;
                }
            }
            segrouLab = tabLabel.allocate(13, segrouBase, segrouMax);
            needFull.add(1);
            compute.wakeup();
            return false;
        }
        if (s.equals("bier")) {
            tabLabel.release(bierLab, 22);
            bierLab = null;
            if (negated) {
                bierIdx = 0;
                bierMax = 0;
                bierLen = 0;
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            bierLen = tabLabelBier.normalizeBsl(bits.str2num(cmd.word()));
            bierMax = bits.str2num(cmd.word());
            bierIdx = bits.str2num(cmd.word());
            bierLab = tabLabel.allocate(22, (bierMax + bierLen - 1) / bierLen);
            needFull.add(1);
            compute.wakeup();
            return false;
        }
        if (s.equals("graceful-restart")) {
            restartTime = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("nexthop")) {
            s = cmd.word();
            if (s.equals("recursion")) {
                recursion = bits.str2num(cmd.word());
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("route-map")) {
                if (negated) {
                    nhtRoumap = null;
                    needFull.add(1);
                    compute.wakeup();
                    return false;
                }
                cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such route map");
                    return false;
                }
                nhtRoumap = ntry.roumap;
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("route-policy")) {
                if (negated) {
                    nhtRouplc = null;
                    needFull.add(1);
                    compute.wakeup();
                    return false;
                }
                cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such route map");
                    return false;
                }
                nhtRouplc = ntry.rouplc;
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("prefix-list")) {
                if (negated) {
                    nhtPfxlst = null;
                    needFull.add(1);
                    compute.wakeup();
                    return false;
                }
                cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such prefix list");
                    return false;
                }
                nhtPfxlst = ntry.prflst;
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            return true;
        }
        if (s.equals("afi-links")) {
            rtrBgpLnkst ls = new rtrBgpLnkst();
            tabRouteAttr.routeType rt = cfgRtr.name2num(cmd.word());
            if (rt == null) {
                cmd.error("bad protocol");
                return false;
            }
            cfgRtr rtr = cfgAll.rtrFind(rt, bits.str2num(cmd.word()), false);
            if (rtr == null) {
                cmd.error("no such router");
                return false;
            }
            ls.rtr = rtr.getRouter();
            if (ls.rtr == null) {
                cmd.error("not initialized");
                return false;
            }
            ls.par = bits.str2num(cmd.word());
            if (negated) {
                linkStates.del(ls);
            } else {
                linkStates.put(ls);
            }
            needFull.add(1);
            compute.wakeup();
            return false;
        }
        if (s.equals("flowspec-install")) {
            flowInst = !negated;
            if (negated) {
                fwdCore.flowspec = null;
            }
            needFull.add(1);
            compute.wakeup();
            return false;
        }
        if (s.equals("flowspec-advert")) {
            if (negated) {
                flowSpec = null;
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            cfgPlymp ntry = cfgAll.plmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such policy map");
                return false;
            }
            flowSpec = ntry.plcmap;
            needFull.add(1);
            compute.wakeup();
            return false;
        }
        if (s.equals("afi-other")) {
            s = cmd.word();
            if (s.equals("enable")) {
                if (negated) {
                    other.unregister2ip();
                } else {
                    other.register2ip();
                }
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("vpn-mode")) {
                other.routerVpn = !negated;
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("distance")) {
                other.distance = bits.str2num(cmd.word());
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("srv6")) {
                if (negated) {
                    other.srv6 = null;
                } else {
                    other.srv6 = cfgAll.ifcFind(cmd.word(), false);
                }
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("flowspec-install")) {
                other.flowInst = !negated;
                if (negated) {
                    other.fwd.flowspec = null;
                }
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("flowspec-advert")) {
                if (negated) {
                    other.flowSpec = null;
                    needFull.add(1);
                    compute.wakeup();
                    return false;
                }
                cfgPlymp ntry = cfgAll.plmpFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such policy map");
                    return false;
                }
                other.flowSpec = ntry.plcmap;
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (cfgRtr.doCfgRedist(other, negated, s, cmd)) {
                cmd.badCmd();
            }
            needFull.add(1);
            compute.wakeup();
            return false;
        }
        if (s.equals("afi-vrf")) {
            cfgVrf cfv = cfgAll.vrfFind(cmd.word(), false);
            if (cfv == null) {
                cmd.error("no such vrf");
                return false;
            }
            rtrBgpVrf cur = new rtrBgpVrf(this, cfv, false);
            s = cmd.word();
            if (s.equals("enable")) {
                rtrBgpVrf old = vrfs.find(cur);
                if (old != null) {
                    cur = old;
                    if (negated) {
                        old.doer.unregister2ip();
                        vrfs.del(old);
                        return false;
                    }
                    return false;
                }
                if (negated) {
                    return false;
                }
                cur.doer.register2ip();
                vrfs.put(cur);
                return false;
            }
            cur = vrfs.find(cur);
            if (cur == null) {
                cmd.error("vrf not enabled");
                return false;
            }
            if (s.equals("distance")) {
                cur.doer.distance = bits.str2num(cmd.word());
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("mvpn")) {
                if (negated) {
                    cur.doer.mvpn = null;
                } else {
                    cur.doer.mvpn = cfgAll.ifcFind(cmd.word(), false);
                }
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("srv6")) {
                if (negated) {
                    cur.doer.srv6 = null;
                } else {
                    cur.doer.srv6 = cfgAll.ifcFind(cmd.word(), false);
                }
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("default-originate")) {
                cur.doer.defRou = !negated;
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("flowspec-install")) {
                cur.doer.flowInst = !negated;
                if (negated) {
                    cur.doer.fwd.flowspec = null;
                }
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("flowspec-advert")) {
                if (negated) {
                    cur.doer.flowSpec = null;
                    needFull.add(1);
                    compute.wakeup();
                    return false;
                }
                cfgPlymp ntry = cfgAll.plmpFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such policy map");
                    return false;
                }
                cur.doer.flowSpec = ntry.plcmap;
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (cfgRtr.doCfgRedist(cur.doer, negated, s, cmd)) {
                cmd.badCmd();
            }
            needFull.add(1);
            compute.wakeup();
            return false;
        }
        if (s.equals("afi-ovrf")) {
            cfgVrf cfv = cfgAll.vrfFind(cmd.word(), false);
            if (cfv == null) {
                cmd.error("no such vrf");
                return false;
            }
            rtrBgpVrf cur = new rtrBgpVrf(this, cfv, true);
            s = cmd.word();
            if (s.equals("enable")) {
                rtrBgpVrf old = ovrfs.find(cur);
                if (old != null) {
                    cur = old;
                    if (negated) {
                        old.doer.unregister2ip();
                        ovrfs.del(old);
                        return false;
                    }
                    return false;
                }
                if (negated) {
                    return false;
                }
                cur.doer.register2ip();
                ovrfs.put(cur);
                return false;
            }
            cur = ovrfs.find(cur);
            if (cur == null) {
                cmd.error("vrf not enabled");
                return false;
            }
            if (s.equals("distance")) {
                cur.doer.distance = bits.str2num(cmd.word());
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("mvpn")) {
                if (negated) {
                    cur.doer.mvpn = null;
                } else {
                    cur.doer.mvpn = cfgAll.ifcFind(cmd.word(), false);
                }
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("srv6")) {
                if (negated) {
                    cur.doer.srv6 = null;
                } else {
                    cur.doer.srv6 = cfgAll.ifcFind(cmd.word(), false);
                }
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("default-originate")) {
                cur.doer.defRou = !negated;
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("flowspec-install")) {
                cur.doer.flowInst = !negated;
                if (negated) {
                    cur.doer.fwd.flowspec = null;
                }
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("flowspec-advert")) {
                if (negated) {
                    cur.doer.flowSpec = null;
                    needFull.add(1);
                    compute.wakeup();
                    return false;
                }
                cfgPlymp ntry = cfgAll.plmpFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such policy map");
                    return false;
                }
                cur.doer.flowSpec = ntry.plcmap;
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (cfgRtr.doCfgRedist(cur.doer, negated, s, cmd)) {
                cmd.badCmd();
            }
            needFull.add(1);
            compute.wakeup();
            return false;
        }
        if (s.equals("afi-vpls")) {
            rtrBgpVpls cur = new rtrBgpVpls(this);
            cur.id = tabRtrmapN.string2rd(cmd.word());
            s = cmd.word();
            if (s.equals("bridge-group")) {
                rtrBgpVpls old = vpls.del(cur);
                if (old != null) {
                    old.doStop();
                }
                if (negated) {
                    needFull.add(1);
                    compute.wakeup();
                    return false;
                }
                cur.bridge = cfgAll.brdgFind(cmd.word(), false);
                if (cur.bridge == null) {
                    cmd.error("no such bridge");
                    return false;
                }
                vpls.add(cur);
                return false;
            }
            cur = vpls.find(cur);
            if (cur == null) {
                cmd.error("vpls not enabled");
                return false;
            }
            if (s.equals("ve-id")) {
                cur.veId = bits.str2num(cmd.word());
                cur.veMax = bits.str2num(cmd.word());
                if (negated) {
                    cur.veId = 0;
                    cur.veMax = 0;
                }
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("update-source")) {
                if (negated) {
                    cur.iface = null;
                } else {
                    cur.iface = cfgAll.ifcFind(cmd.word(), false);
                }
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            return false;
        }
        if (s.equals("afi-evpn")) {
            rtrBgpEvpn cur = new rtrBgpEvpn(this);
            cur.id = bits.str2num(cmd.word());
            s = cmd.word();
            if (s.equals("bridge-group")) {
                rtrBgpEvpn old = evpn.del(cur);
                if (old != null) {
                    old.doStop();
                }
                if (negated) {
                    needFull.add(1);
                    compute.wakeup();
                    return false;
                }
                cur.bridge = cfgAll.brdgFind(cmd.word(), false);
                if (cur.bridge == null) {
                    cmd.error("no such bridge");
                    return false;
                }
                cur.bridge.bridgeHed.macRouter = cur;
                cur.bbmac = addrMac.getRandom();
                cur.bcmac = ifcDot1ah.dstBmac4flood(cur.id);
                cur.encap = rtrBgpEvpn.encapType.pbb;
                evpn.add(cur);
                return false;
            }
            cur = evpn.find(cur);
            if (cur == null) {
                cmd.error("evpn not enabled");
                return false;
            }
            if (s.equals("bmac")) {
                cur.bbmac.fromString(cmd.word());
                return false;
            }
            if (s.equals("srv6")) {
                if (negated) {
                    cur.srv6 = null;
                } else {
                    cur.srv6 = cfgAll.ifcFind(cmd.word(), false);
                }
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("update-source")) {
                if (negated) {
                    cur.iface = null;
                } else {
                    cur.iface = cfgAll.ifcFind(cmd.word(), false);
                }
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("encapsulation")) {
                s = cmd.word();
                if (s.equals("pbb")) {
                    cur.encap = rtrBgpEvpn.encapType.pbb;
                }
                if (s.equals("vxlan")) {
                    cur.encap = rtrBgpEvpn.encapType.vxlan;
                }
                if (s.equals("cmac")) {
                    cur.encap = rtrBgpEvpn.encapType.cmac;
                }
                if (s.equals("vpws")) {
                    cur.encap = rtrBgpEvpn.encapType.vpws;
                }
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            return false;
        }
        if (s.equals("dump")) {
            rtrBgpMrt dmp = new rtrBgpMrt(cmd.word());
            if (negated) {
                dmp = dmps.del(dmp);
                if (dmp == null) {
                    return false;
                }
                dmp.fileHandle.close();
                return false;
            }
            rtrBgpMrt old = dmps.add(dmp);
            if (old != null) {
                old.fileHandle.close();
                dmp = old;
            }
            dmp.fileHandle = new logFil(cmd.word());
            int tim = bits.str2num(cmd.word());
            String bck = cmd.word();
            int siz = bits.str2num(cmd.word());
            dmp.fileHandle.rotate(bck, siz, tim, 0);
            dmp.fileHandle.open(true);
            return false;
        }
        if (s.equals("monitor")) {
            rtrBgpMon mon = new rtrBgpMon(this, cmd.word());
            if (negated) {
                mon = mons.del(mon);
                if (mon == null) {
                    return false;
                }
                mon.stopNow();
                return false;
            }
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return false;
            }
            mon.proxy = prx.proxy;
            mon.server = cmd.word();
            mon.port = bits.str2num(cmd.word());
            mon.startNow();
            mons.add(mon);
            return false;
        }
        if (s.equals("rpki")) {
            rtrBgpRpki rpki = new rtrBgpRpki(this);
            rpki.rpkiName = cmd.word();
            if (negated) {
                rpki = rpkis.del(rpki);
                if (rpki == null) {
                    return false;
                }
                rpki.stopNow();
                return false;
            }
            cfgProxy prx = cfgAll.proxyFind(cmd.word(), false);
            if (prx == null) {
                cmd.error("no such proxy");
                return false;
            }
            rpki.proxy = prx.proxy;
            rpki.server = cmd.word();
            rpki.port = bits.str2num(cmd.word());
            rpki.startNow();
            rpkis.add(rpki);
            return false;
        }
        if (s.equals("listen")) {
            tcpCore.listenStop(lstnIfc, port, null, 0);
            if (negated) {
                lstnAcl = null;
                lstnTmp = null;
                lstnIfc = null;
                return false;
            }
            cfgAceslst acl = cfgAll.aclsFind(cmd.word(), false);
            if (acl == null) {
                cmd.error("no such acl");
                return true;
            }
            lstnTmp = findTemp(cmd.word());
            if (lstnTmp == null) {
                cmd.error("no such template");
                return true;
            }
            lstnIfc = null;
            if (lstnTmp.srcIface != null) {
                if (afiUni == rtrBgpUtil.safiIp4uni) {
                    lstnIfc = lstnTmp.srcIface.fwdIf4;
                } else {
                    lstnIfc = lstnTmp.srcIface.fwdIf6;
                }
            }
            lstnAcl = acl.aceslst;
            tcpCore.streamListen(this, new pipeLine(lstnTmp.bufferSize, false), lstnIfc, port, null, 0, "bgp", lstnTmp.passwd, lstnTmp.ttlSecurity);
            return false;
        }
        if (s.equals("template")) {
            rtrBgpTemp ntry = new rtrBgpTemp(this);
            ntry.tempName = cmd.word();
            rtrBgpTemp old = temps.add(ntry);
            if (old != null) {
                ntry = old;
            }
            negated = ntry.setParamCfg(cmd, negated);
            needFull.add(1);
            compute.wakeup();
            if (ntry.remoteAs != 0) {
                return negated;
            }
            temps.del(ntry);
            return negated;
        }
        if (!s.equals("neighbor")) {
            return true;
        }
        rtrBgpNeigh ntry = new rtrBgpNeigh(this);
        if (ntry.peerAddr.fromString(cmd.word())) {
            cmd.error("bad address");
            return false;
        }
        rtrBgpNeigh old = neighs.add(ntry);
        if (old == null) {
            ntry.startNow();
        } else {
            ntry = old;
        }
        negated = ntry.setParamCfg(cmd, negated);
        ntry.updatePeer();
        needFull.add(1);
        compute.wakeup();
        if (ntry.remoteAs != 0) {
            return negated;
        }
        ntry.stopNow();
        neighs.del(ntry);
        return negated;
    }

    /**
     * template configuration
     *
     * @param temp template interface
     * @param cmd command to parse
     * @param negated negated
     */
    public void templateConfig(rtrBgpTemp temp, String cmd, boolean negated) {
        for (int i = 0; i < neighs.size(); i++) {
            templateConfig(neighs.get(i), temp, cmd, negated);
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            templateConfig(lstnNei.get(i), temp, cmd, negated);
        }
    }

    private void templateConfig(rtrBgpNeigh nei, rtrBgpTemp temp, String cmd, boolean negated) {
        if (nei == null) {
            return;
        }
        if (nei.template == null) {
            return;
        }
        if (!temp.tempName.equals(nei.template.tempName)) {
            return;
        }
        nei.setParamCfg(new cmds("template", cmd), negated);
        nei.updatePeer();
    }

    /**
     * list neighbors
     *
     * @return list of neighbors
     */
    public userFormat showRpkiNei() {
        userFormat l = new userFormat("|", "learn|neighbor");
        for (int i = 0; i < rpkis.size(); i++) {
            rtrBgpRpki ntry = rpkis.get(i);
            l.add(ntry.table.size() + "|" + ntry.server);
        }
        return l;
    }

    /**
     * list neighbor summary
     *
     * @param safi safi to query
     * @return list of neighbors
     */
    public userFormat showNeighs(int safi) {
        userFormat l = new userFormat("|", "neighbor|as|learn|accept|will|done|uptime");
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpNeigh ntry = neighs.get(i);
            if (ntry == null) {
                continue;
            }
            l.add(ntry.showNeighs(safi));
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            rtrBgpNeigh ntry = lstnNei.get(i);
            if (ntry == null) {
                continue;
            }
            l.add(ntry.showNeighs(safi));
        }
        return l;
    }

    /**
     * list neighbor summary
     *
     * @param mod mode: 1=afi, 2=groups, 3=nexthops, 4=graceful, 5=addpath,
     * 6=routerid, 7=buffer, 8=description, 9=hostname, 10=compress, 11=connect,
     * 12=resolve, 13=summary, 14=multilab
     * @return list of neighbors
     */
    public userFormat showSummary(int mod) {
        userFormat l = null;
        switch (mod) {
            case 1:
                l = new userFormat("|", "neighbor|as|open|norem|noloc");
                break;
            case 2:
                l = new userFormat("|", "neighbor|as|group|uptime");
                break;
            case 3:
                l = new userFormat("|", "neighbor|as|reach|chg|num|uptime");
                break;
            case 4:
                l = new userFormat("|", "neighbor|as|rx|tx");
                break;
            case 5:
                l = new userFormat("|", "neighbor|as|rx|tx|rx|tx|rx|tx", "2|2open|2norem|2noloc");
                break;
            case 6:
                l = new userFormat("|", "neighbor|as|router|wideas|refresh|extop|extup|type|role");
                break;
            case 7:
                l = new userFormat("|", "neighbor|as|buffer|over|ver|incr|full|need");
                break;
            case 8:
                l = new userFormat("|", "neighbor|as|description");
                break;
            case 9:
                l = new userFormat("|", "neighbor|as|hostname|domain");
                break;
            case 10:
                l = new userFormat("|", "neighbor|as|rx|tx|rx|tx", "2|2operate|2ratio");
                break;
            case 11:
                l = new userFormat("|", "neighbor|as|rx|tx|rx|tx|rx|tx", "2|2update|2byte|2refresh");
                break;
            case 12:
                l = new userFormat("|", "neighbor|as|domain");
                break;
            case 13:
                l = new userFormat("|", "neighbor|as|ready|learn|sent|uptime");
                break;
            case 14:
                l = new userFormat("|", "neighbor|as|rx|tx");
                break;
            default:
                return null;
        }
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpNeigh ntry = neighs.get(i);
            if (ntry == null) {
                continue;
            }
            l.add(ntry.showSummary(mod));
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            rtrBgpNeigh ntry = lstnNei.get(i);
            if (ntry == null) {
                continue;
            }
            l.add(ntry.showSummary(mod));
        }
        return l;
    }

    /**
     * find peer
     *
     * @param adr address to find
     * @return neighbor, null if not found
     */
    public rtrBgpNeigh findPeer(addrIP adr) {
        rtrBgpNeigh ntry = new rtrBgpNeigh(this);
        ntry.peerAddr = adr.copyBytes();
        rtrBgpNeigh res = neighs.find(ntry);
        if (res != null) {
            return res;
        }
        return lstnNei.find(ntry);
    }

    private String findPeers(int mod, rtrBgpNeigh ntry) {
        switch (mod) {
            case 1:
                return "" + ntry.remoteAs;
            case 2:
                return "" + ntry.peerAddr;
            case 3:
                return "" + (ntry.localAs == ntry.remoteAs);
            default:
                return "";
        }
    }

    /**
     * find peers
     *
     * @param mod mode: 1=asn, 2=addr, 3=ibgp
     * @param reg regexp
     * @return list of peers
     */
    public List<rtrBgpNeigh> findPeers(int mod, String reg) {
        List<rtrBgpNeigh> res = new ArrayList<rtrBgpNeigh>();
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpNeigh ntry = neighs.get(i);
            String a = findPeers(mod, ntry);
            if (a.matches(reg)) {
                res.add(ntry);
            }
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            rtrBgpNeigh ntry = lstnNei.get(i);
            String a = findPeers(mod, ntry);
            if (a.matches(reg)) {
                res.add(ntry);
            }
        }
        return res;
    }

    /**
     * find group
     *
     * @param num number of group
     * @return group, null if not found
     */
    public rtrBgpGroup findGroup(int num) {
        if (num < 0) {
            return null;
        }
        if (num >= groups.size()) {
            return null;
        }
        return groups.get(num);
    }

    /**
     * find template
     *
     * @param nam name to find
     * @return template, null if not found
     */
    public rtrBgpTemp findTemp(String nam) {
        rtrBgpTemp ntry = new rtrBgpTemp(this);
        ntry.tempName = "" + nam;
        return temps.find(ntry);
    }

    /**
     * get neighbor count
     *
     * @return count
     */
    public int routerNeighCount() {
        return neighs.size() + lstnNei.size();
    }

    /**
     * neighbor list
     *
     * @param tab list
     */
    public void routerNeighList(tabRoute<addrIP> tab) {
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = new addrPrefix<addrIP>(nei.peerAddr, addrIP.size * 8);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, tab, afiUni, 0, ntry, true, null, null, routerAutoMesh);
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            rtrBgpNeigh nei = lstnNei.get(i);
            if (nei == null) {
                continue;
            }
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = new addrPrefix<addrIP>(nei.peerAddr, addrIP.size * 8);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, tab, afiUni, 0, ntry, true, null, null, routerAutoMesh);
        }
        other.getPeerList(tab);
        for (int i = 0; i < vrfs.size(); i++) {
            vrfs.get(i).doer.getPeerList(tab);
        }
        for (int i = 0; i < ovrfs.size(); i++) {
            ovrfs.get(i).doer.getPeerList(tab);
        }
        for (int i = 0; i < vpls.size(); i++) {
            vpls.get(i).getPeerList(tab);
        }
        for (int i = 0; i < evpn.size(); i++) {
            evpn.get(i).getPeerList(tab);
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
        return recursion;
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
     * get all routes
     *
     * @param safi safi to query
     * @param prf prefix to find
     * @return list of routes
     */
    public userFormat getAllRoutes(int safi, tabRouteEntry<addrIP> prf) {
        userFormat lst = new userFormat("|", "category|value");
        for (int i = 0; i < neighs.size(); i++) {
            getAllRoutes(lst, neighs.get(i), safi, prf);
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            getAllRoutes(lst, lstnNei.get(i), safi, prf);
        }
        return lst;
    }

    private void getAllRoutes(userFormat lst, rtrBgpNeigh nei, int safi, tabRouteEntry<addrIP> prf) {
        if (nei == null) {
            return;
        }
        tabRoute<addrIP> tab = nei.conn.getLearned(safi);
        if (tab == null) {
            return;
        }
        tabRouteEntry<addrIP> res = tab.find(prf);
        if (res == null) {
            return;
        }
        lst.add("peer|" + nei.peerAddr);
        lst.add(res.fullDump(fwdCore));
    }

    /**
     * get flap stats
     *
     * @param afi afi
     * @param num minimum flap count
     * @return list of statistics
     */
    public userFormat getFlapstat(int afi, int num) {
        userFormat l = new userFormat("|", "prefix|count|paths|ago|last");
        if (flaps == null) {
            return l;
        }
        for (int i = 0; i < flaps.size(); i++) {
            rtrBgpFlap ntry = flaps.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.afi != afi) {
                continue;
            }
            if (ntry.count < num) {
                continue;
            }
            l.add(ntry + "");
        }
        return l;
    }

    /**
     * get flap paths
     *
     * @param afi afi
     * @param rd rd
     * @param prf prefix
     * @param rev reverse path
     * @return list of paths
     */
    public userFormat getFlappath(int afi, long rd, addrPrefix<addrIP> prf, boolean rev) {
        if (flaps == null) {
            return null;
        }
        rtrBgpFlap ntry = new rtrBgpFlap();
        ntry.afi = afi;
        ntry.rd = rd;
        ntry.prefix = prf.copyBytes();
        ntry = flaps.find(ntry);
        if (ntry == null) {
            return null;
        }
        userFormat l = new userFormat("|", "count|ago|last|path");
        for (int i = 0; i < ntry.paths.size(); i++) {
            l.add("" + ntry.paths.get(i).dump(rev));
        }
        return l;
    }

    /**
     * originating as
     *
     * @param safi safi to query
     * @return text
     */
    public userFormat getAsOrigin(int safi) {
        tabGen<rtrBgpFlapath> lst = new tabGen<rtrBgpFlapath>();
        tabRoute<addrIP> rou = getDatabase(safi);
        for (int i = 0; i < rou.size(); i++) {
            tabRouteEntry<addrIP> ntry = rou.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.best.pathSeq == null) {
                continue;
            }
            if (ntry.best.pathSeq.size() < 1) {
                continue;
            }
            getAsOrigin(lst, ntry.best.pathSeq.get(ntry.best.pathSeq.size() - 1));
        }
        userFormat res = new userFormat("|", "as|nets");
        for (int i = 0; i < lst.size(); i++) {
            rtrBgpFlapath ntry = lst.get(i);
            res.add(ntry.path + "|" + ntry.count);
        }
        return res;
    }

    /**
     * transiting as
     *
     * @param safi safi to query
     * @return text
     */
    public userFormat getAsTransit(int safi) {
        tabGen<rtrBgpFlapath> lst = new tabGen<rtrBgpFlapath>();
        tabRoute<addrIP> rou = getDatabase(safi);
        for (int i = 0; i < rou.size(); i++) {
            tabRouteEntry<addrIP> ntry = rou.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.best.pathSeq == null) {
                continue;
            }
            for (int o = 0; o < (ntry.best.pathSeq.size() - 1); o++) {
                getAsOrigin(lst, ntry.best.pathSeq.get(o));
            }
        }
        userFormat res = new userFormat("|", "as|nets");
        for (int i = 0; i < lst.size(); i++) {
            rtrBgpFlapath ntry = lst.get(i);
            res.add(ntry.path + "|" + ntry.count);
        }
        return res;
    }

    private void getAsOrigin(tabGen<rtrBgpFlapath> lst, int as) {
        rtrBgpFlapath res = new rtrBgpFlapath();
        res.path = bits.num2str(as);
        res.count = 1;
        rtrBgpFlapath old = lst.add(res);
        if (old == null) {
            return;
        }
        old.count++;
    }

    /**
     * as path graph
     *
     * @param safi safi to query
     * @return text
     */
    public List<String> getAsGraph(int safi) {
        tabGen<rtrBgpFlapath> lst = new tabGen<rtrBgpFlapath>();
        for (int i = 0; i < neighs.size(); i++) {
            getAsGraph(lst, neighs.get(i), safi);
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            getAsGraph(lst, lstnNei.get(i), safi);
        }
        int o = 0;
        for (int i = 0; i < lst.size(); i++) {
            rtrBgpFlapath ntry = lst.get(i);
            if (o < ntry.count) {
                o = ntry.count;
            }
        }
        o += 2;
        List<String> res = new ArrayList<String>();
        res.add(shrtPthFrst.graphBeg1);
        res.add(shrtPthFrst.graphBeg2);
        for (int i = 0; i < lst.size(); i++) {
            rtrBgpFlapath ntry = lst.get(i);
            res.add(ntry.path + " [weight=" + (o - ntry.count) + "]");
        }
        res.add(shrtPthFrst.graphEnd1);
        res.add(shrtPthFrst.graphEnd2);
        return res;
    }

    private void getAsGraph(tabGen<rtrBgpFlapath> lst, rtrBgpNeigh nei, int safi) {
        if (nei == null) {
            return;
        }
        tabRoute<addrIP> tab = nei.conn.getLearned(safi);
        if (tab == null) {
            return;
        }
        cmds cmd;
        for (int i = 0; i < tab.size(); i++) {
            tabRouteEntry<addrIP> prf = tab.get(i);
            if (prf == null) {
                continue;
            }
            cmd = new cmds("path", prf.best.asPathStr());
            String prv = cmd.word();
            for (;;) {
                String a = cmd.word();
                if (a.length() < 1) {
                    break;
                }
                rtrBgpFlapath ntry = new rtrBgpFlapath();
                ntry.path = prv + " -- " + a;
                rtrBgpFlapath old = lst.add(ntry);
                if (old != null) {
                    ntry = old;
                }
                ntry.count++;
                prv = a;
            }
        }
    }

    /**
     * as connections
     *
     * @param safi safi to query
     * @return text
     */
    public userFormat getAsConns(int safi) {
        tabGen<rtrBgpFlapath> lst = new tabGen<rtrBgpFlapath>();
        for (int i = 0; i < neighs.size(); i++) {
            getAsGraph(lst, neighs.get(i), safi);
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            getAsGraph(lst, lstnNei.get(i), safi);
        }
        userFormat res = new userFormat("|", "as|conn|net|peers");
        int conns = -1;
        int prefs = -1;
        String peers = "";
        String curr = "none";
        for (int i = 0; i < lst.size(); i++) {
            rtrBgpFlapath ntry = lst.get(i);
            String a = ntry.path;
            int o = a.indexOf(" ");
            String b = a.substring(0, o);
            o = a.lastIndexOf(" ");
            a = a.substring(o + 1, a.length());
            if (b.equals(curr)) {
                peers += " " + a;
                conns++;
                prefs += ntry.count;
                continue;
            }
            if (conns > 0) {
                res.add(curr + "|" + conns + "|" + prefs + "|" + peers);
            }
            curr = b;
            peers = a;
            conns = 1;
            prefs = ntry.count;
        }
        if (conns > 0) {
            res.add(curr + "|" + conns + "|" + prefs + "|" + peers);
        }
        return res;
    }

    /**
     * inconsistent next hops
     *
     * @param safi safi to query
     * @param mtch matcher
     * @return text
     */
    public userFormat getNhIncons(int safi, tabIntMatcher mtch) {
        tabGen<rtrBgpFlap> lst = new tabGen<rtrBgpFlap>();
        for (int i = 0; i < neighs.size(); i++) {
            getNhIncons(lst, neighs.get(i), safi);
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            getNhIncons(lst, lstnNei.get(i), safi);
        }
        userFormat res = new userFormat("|", "path|nexthops");
        for (int i = 0; i < lst.size(); i++) {
            rtrBgpFlap ntry = lst.get(i);
            if (!mtch.matches(ntry.paths.size())) {
                continue;
            }
            res.add("" + ntry.toIncons());
        }
        return res;
    }

    private void getNhIncons(tabGen<rtrBgpFlap> lst, rtrBgpNeigh nei, int safi) {
        if (nei == null) {
            return;
        }
        tabRoute<addrIP> tab = nei.conn.getLearned(safi);
        if (tab == null) {
            return;
        }
        for (int i = 0; i < tab.size(); i++) {
            tabRouteEntry<addrIP> prf = tab.get(i);
            if (prf == null) {
                continue;
            }
            rtrBgpFlap ntry = new rtrBgpFlap();
            ntry.rd = prf.rouDst;
            ntry.prefix = prf.prefix.copyBytes();
            rtrBgpFlap old = lst.add(ntry);
            if (old != null) {
                ntry = old;
            }
            String a = "" + prf.best.nextHop;
            rtrBgpFlapath pth = new rtrBgpFlapath();
            pth.path = a;
            ntry.paths.add(pth);
        }
    }

    /**
     * inconsistent as paths
     *
     * @param safi safi to query
     * @param mtch matcher
     * @return text
     */
    public userFormat getAsIncons(int safi, tabIntMatcher mtch) {
        tabGen<rtrBgpFlap> lst = new tabGen<rtrBgpFlap>();
        for (int i = 0; i < neighs.size(); i++) {
            getAsIncons(lst, neighs.get(i), safi);
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            getAsIncons(lst, lstnNei.get(i), safi);
        }
        userFormat res = new userFormat("|", "path|ases");
        for (int i = 0; i < lst.size(); i++) {
            rtrBgpFlap ntry = lst.get(i);
            if (!mtch.matches(ntry.paths.size())) {
                continue;
            }
            res.add("" + ntry.toIncons());
        }
        return res;
    }

    private void getAsIncons(tabGen<rtrBgpFlap> lst, rtrBgpNeigh nei, int safi) {
        if (nei == null) {
            return;
        }
        tabRoute<addrIP> tab = nei.conn.getLearned(safi);
        if (tab == null) {
            return;
        }
        for (int i = 0; i < tab.size(); i++) {
            tabRouteEntry<addrIP> prf = tab.get(i);
            if (prf == null) {
                continue;
            }
            rtrBgpFlap ntry = new rtrBgpFlap();
            ntry.rd = prf.rouDst;
            ntry.prefix = prf.prefix.copyBytes();
            rtrBgpFlap old = lst.add(ntry);
            if (old != null) {
                ntry = old;
            }
            String a = prf.best.asPathStr();
            int o = a.lastIndexOf(" ");
            if (o >= 0) {
                a = a.substring(o + 1, a.length());
            }
            rtrBgpFlapath pth = new rtrBgpFlapath();
            pth.path = a;
            ntry.paths.add(pth);
        }
    }

    /**
     * get bestpath stats
     *
     * @return list of statistics
     */
    public userFormat getBestpath() {
        userFormat l = new userFormat("|", "category|value|addition");
        l.add("version|" + compRound);
        l.add("full run|" + fullCount + "|times");
        l.add("full last|" + bits.timePast(fullLast) + "|" + bits.time2str(cfgAll.timeZoneName, fullLast + cfgAll.timeServerOffset, 3));
        l.add("full time|" + fullTime + "|ms");
        l.add("incr run|" + incrCount + "|times");
        l.add("incr last|" + bits.timePast(incrLast) + "|" + bits.time2str(cfgAll.timeZoneName, incrLast + cfgAll.timeServerOffset, 3));
        l.add("incr time|" + incrTime + "|ms");
        l.add("changes all|" + changedTot);
        l.add("changes now|" + changedCur);
        l.add("static peers|" + neighs.size());
        l.add("dynamic peers|" + lstnNei.size());
        l.add("groups|" + groups.size() + "|" + groupMin + ".." + groupMax);
        l.add("rpki table|" + computedRpki.size());
        l.add("unicast table|" + routerComputedU.size() + "|" + changedUni.size());
        l.add("multicast table|" + routerComputedM.size() + "|" + changedMlt.size());
        l.add("ouni table|" + computedOtrU.size() + "|" + changedOtrU.size());
        l.add("omlt table|" + computedOtrM.size() + "|" + changedOtrM.size());
        l.add("oflw table|" + computedOtrF.size() + "|" + changedOtrF.size());
        l.add("osrt table|" + computedOtrS.size() + "|" + changedOtrS.size());
        l.add("flowspec table|" + routerComputedF.size() + "|" + changedFlw.size());
        l.add("vpnuni table|" + computedVpnU.size() + "|" + changedVpnU.size());
        l.add("vpnmlt table|" + computedVpnM.size() + "|" + changedVpnM.size());
        l.add("vpnflw table|" + computedVpnF.size() + "|" + changedVpnF.size());
        l.add("ovpnuni table|" + computedVpoU.size() + "|" + changedVpoU.size());
        l.add("ovpnmlt table|" + computedVpoM.size() + "|" + changedVpoM.size());
        l.add("ovpnflw table|" + computedVpoF.size() + "|" + changedVpoF.size());
        l.add("vpls table|" + computedVpls.size() + "|" + changedVpls.size());
        l.add("mspw table|" + computedMspw.size() + "|" + changedMspw.size());
        l.add("evpn table|" + computedEvpn.size() + "|" + changedEvpn.size());
        l.add("mdt table|" + computedMdt.size() + "|" + changedMdt.size());
        l.add("nsh table|" + computedNsh.size() + "|" + changedNsh.size());
        l.add("srte table|" + computedSrte.size() + "|" + changedSrte.size());
        l.add("linkstate table|" + computedLnks.size() + "|" + changedLnks.size());
        l.add("mvpn table|" + computedMvpn.size() + "|" + changedMvpn.size());
        l.add("omvpn table|" + computedMvpo.size() + "|" + changedMvpo.size());
        return l;
    }

}
