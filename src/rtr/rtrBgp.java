package rtr;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrMac;
import addr.addrPrefix;
import cfg.cfgAceslst;
import cfg.cfgAll;
import cfg.cfgInit;
import cfg.cfgPlymp;
import cfg.cfgPrfxlst;
import cfg.cfgProxy;
import cfg.cfgRoump;
import cfg.cfgRouplc;
import cfg.cfgRtr;
import cfg.cfgVrf;
import ifc.ifcDot1ah;
import ip.ipCor4;
import ip.ipCor6;
import ip.ipFwd;
import ip.ipFwdIface;
import ip.ipFwdTab;
import ip.ipRtr;
import java.math.BigInteger;
import java.util.List;
import java.util.ArrayList;
import pipe.pipeLine;
import pipe.pipeSide;
import prt.prtGenConn;
import prt.prtServS;
import prt.prtTcp;
import tab.tabListing;
import tab.tabAceslstN;
import tab.tabGen;
import tab.tabLabel;
import tab.tabLabelBier;
import tab.tabLabelBierN;
import tab.tabLabelNtry;
import tab.tabPlcmapN;
import tab.tabPrfxlstN;
import tab.tabRoute;
import tab.tabRouteEntry;
import tab.tabRtrmapN;
import tab.tabRtrplcN;
import user.userFormat;
import user.userHelping;
import util.bits;
import util.cmds;
import util.debugger;
import util.logger;
import util.notifier;
import util.shrtPthFrst;
import util.syncInt;

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
     * segment routing index
     */
    public int segrouIdx = 0;

    /**
     * segment routing maximum
     */
    public int segrouMax = 0;

    /**
     * segment routing labels
     */
    protected tabLabelNtry[] segrouLab;

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
    protected tabLabelNtry[] bierLab;

    /**
     * scan time interval
     */
    public int scanTime;

    /**
     * initial delay
     */
    public int scanDelay;

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
    protected final tabRouteEntry.routeType rouTyp;

    /**
     * unicast afi
     */
    protected int afiUni;

    /**
     * labeled unicast afi
     */
    protected int afiLab;

    /**
     * multicast afi
     */
    protected int afiMlt;

    /**
     * other afi
     */
    protected int afiOtr;

    /**
     * flow specification afi
     */
    protected int afiFlw;

    /**
     * unicast vpn afi
     */
    protected int afiVpnU;

    /**
     * multicast vpn afi
     */
    protected int afiVpnM;

    /**
     * flowspec vpn afi
     */
    protected int afiVpnF;

    /**
     * other unicast vpn afi
     */
    protected int afiVpoU;

    /**
     * other multicast vpn afi
     */
    protected int afiVpoM;

    /**
     * other flowspec vpn afi
     */
    protected int afiVpoF;

    /**
     * vpls afi
     */
    protected int afiVpls;

    /**
     * mspw afi
     */
    protected int afiMspw;

    /**
     * evpn afi
     */
    protected int afiEvpn;

    /**
     * mdt afi
     */
    protected int afiMdt;

    /**
     * srte afi
     */
    protected int afiSrte;

    /**
     * mvpn afi
     */
    protected int afiMvpn;

    /**
     * other mvpn afi
     */
    protected int afiMvpo;

    /**
     * router number
     */
    protected final int rtrNum;

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
     * list of rpkis
     */
    protected tabGen<rtrBgpRpki> rpkis;

    /**
     * the computed rpki routes
     */
    public tabRoute<addrIP> computedRpki = new tabRoute<addrIP>("rx");

    /**
     * the computed other routes
     */
    public tabRoute<addrIP> computedOtr = new tabRoute<addrIP>("rx");

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
     * the computed srte routes
     */
    public tabRoute<addrIP> computedSrte = new tabRoute<addrIP>("rx");

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
     * the changed other routes
     */
    public final tabRoute<addrIP> changedOtr = new tabRoute<addrIP>("rx");

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
     * the changed srte routes
     */
    public final tabRoute<addrIP> changedSrte = new tabRoute<addrIP>("rx");

    /**
     * the changed mvpn routes
     */
    public final tabRoute<addrIP> changedMvpn = new tabRoute<addrIP>("rx");

    /**
     * the changed other mvpn routes
     */
    public final tabRoute<addrIP> changedMvpo = new tabRoute<addrIP>("rx");

    /**
     * the originated other routes
     */
    public tabRoute<addrIP> origntedOtr = new tabRoute<addrIP>("tx");

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
     * the originated srte routes
     */
    public tabRoute<addrIP> origntedSrte = new tabRoute<addrIP>("tx");

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
    protected tabGen<rtrBgpFlap> flaps;

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
    protected tabLabelNtry evpnUni;

    /**
     * evpn multicast label
     */
    protected tabLabelNtry evpnMul;

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
        addrFams = rtrBgpParam.mskUni;
        rtrNum = id;
        switch (fwdCore.ipVersion) {
            case ipCor4.protocolVersion:
                rouTyp = tabRouteEntry.routeType.bgp4;
                afiUni = rtrBgpUtil.safiIp4uni;
                afiLab = rtrBgpUtil.safiIp4lab;
                afiMlt = rtrBgpUtil.safiIp4multi;
                afiOtr = rtrBgpUtil.safiIp6lab;
                afiFlw = rtrBgpUtil.safiIp4flow;
                afiVpnU = rtrBgpUtil.safiIp4vpnU;
                afiVpnM = rtrBgpUtil.safiIp4vpnM;
                afiVpnF = rtrBgpUtil.safiIp4vpnF;
                afiVpoU = rtrBgpUtil.safiIp6vpnU;
                afiVpoM = rtrBgpUtil.safiIp6vpnM;
                afiVpoF = rtrBgpUtil.safiIp6vpnF;
                afiVpls = rtrBgpUtil.safiVpls4;
                afiMspw = rtrBgpUtil.safiMspw4;
                afiEvpn = rtrBgpUtil.safiEvpn4;
                afiMdt = rtrBgpUtil.safiIp4mdt;
                afiSrte = rtrBgpUtil.safiIp4srte;
                afiMvpn = rtrBgpUtil.safiIp4mvpn;
                afiMvpo = rtrBgpUtil.safiIp6mvpn;
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteEntry.routeType.bgp6;
                afiUni = rtrBgpUtil.safiIp6uni;
                afiLab = rtrBgpUtil.safiIp6lab;
                afiMlt = rtrBgpUtil.safiIp6multi;
                afiOtr = rtrBgpUtil.safiIp4lab;
                afiFlw = rtrBgpUtil.safiIp6flow;
                afiVpnU = rtrBgpUtil.safiIp6vpnU;
                afiVpnM = rtrBgpUtil.safiIp6vpnM;
                afiVpnF = rtrBgpUtil.safiIp6vpnF;
                afiVpoU = rtrBgpUtil.safiIp4vpnU;
                afiVpoM = rtrBgpUtil.safiIp4vpnM;
                afiVpoF = rtrBgpUtil.safiIp4vpnF;
                afiVpls = rtrBgpUtil.safiVpls6;
                afiMspw = rtrBgpUtil.safiMspw6;
                afiEvpn = rtrBgpUtil.safiEvpn6;
                afiMdt = rtrBgpUtil.safiIp6mdt;
                afiSrte = rtrBgpUtil.safiIp6srte;
                afiMvpn = rtrBgpUtil.safiIp6mvpn;
                afiMvpo = rtrBgpUtil.safiIp4mvpn;
                break;
            default:
                rouTyp = null;
                afiUni = 0;
                afiLab = 0;
                afiMlt = 0;
                afiOtr = 0;
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
                afiSrte = 0;
                afiMvpn = 0;
                afiMvpo = 0;
                break;
        }
        incrLimit = 1000;
        conquer = false;
        flaps = null;
        scanTime = 1000;
        scanDelay = 1000;
        restartTime = 60 * 1000;
        distantExt = 20;
        distantInt = 200;
        distantLoc = 200;
        lstnNei = new tabGen<rtrBgpNeigh>();
        neighs = new tabGen<rtrBgpNeigh>();
        mons = new tabGen<rtrBgpMon>();
        dmps = new tabGen<rtrBgpMrt>();
        rpkis = new tabGen<rtrBgpRpki>();
        temps = new tabGen<rtrBgpTemp>();
        routerComputedU = new tabRoute<addrIP>("rx");
        routerComputedM = new tabRoute<addrIP>("rx");
        routerComputedF = new tabRoute<addrIP>("rx");
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
        if (safi == afiOtr) {
            return rtrBgpParam.mskOtr;
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
        if (safi == afiSrte) {
            return rtrBgpParam.mskSrte;
        }
        if (safi == afiMvpn) {
            return rtrBgpParam.mskMvpn;
        }
        if (safi == afiMvpo) {
            return rtrBgpParam.mskMvpo;
        }
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
            case rtrBgpParam.mskOtr:
                return afiOtr;
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
            case rtrBgpParam.mskSrte:
                return afiSrte;
            case rtrBgpParam.mskMvpn:
                return afiMvpn;
            case rtrBgpParam.mskMvpo:
                return afiMvpo;
            default:
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
        if ((mask & rtrBgpParam.mskOtr) != 0) {
            safis.add(afiOtr);
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
        if ((mask & rtrBgpParam.mskSrte) != 0) {
            safis.add(afiSrte);
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
        if (safi == afiOtr) {
            return computedOtr;
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
        if (safi == afiSrte) {
            return computedSrte;
        }
        if (safi == afiMvpn) {
            return computedMvpn;
        }
        if (safi == afiMvpo) {
            return computedMvpo;
        }
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
        if (safi == afiOtr) {
            return changedOtr;
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
        if (safi == afiSrte) {
            return changedSrte;
        }
        if (safi == afiMvpn) {
            return changedMvpn;
        }
        if (safi == afiMvpo) {
            return changedMvpo;
        }
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
        needFull.add(1);
        compute.wakeup();
    }

    /**
     * others changed
     */
    public void routerOthersChanged() {
        if ((nhtRoumap == null) && (nhtRouplc == null) && (nhtPfxlst == null)) {
            return;
        }
        compute.wakeup();
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
        changedOtr.clear();
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
        changedSrte.clear();
        changedMvpn.clear();
        changedMvpo.clear();
        tabRoute<addrIP> nUni = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nMlt = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nOtr = new tabRoute<addrIP>("bst");
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
        tabRoute<addrIP> nSrte = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nMvpn = new tabRoute<addrIP>("bst");
        tabRoute<addrIP> nMvpo = new tabRoute<addrIP>("bst");
        if (flowSpec != null) {
            rtrBgpFlow.doAdvertise(nFlw, flowSpec, new tabRouteEntry<addrIP>(), afiUni == rtrBgpUtil.safiIp6uni, localAs);
        }
        for (int i = 0; i < routerRedistedF.size(); i++) {
            tabRouteEntry<addrIP> ntry = routerRedistedF.get(i);
            ntry = ntry.copyBytes();
            ntry.rouTyp = rouTyp;
            ntry.protoNum = rtrNum;
            ntry.distance = distantLoc;
            nFlw.add(tabRoute.addType.better, ntry, false, false);
        }
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
        origntedOtr = new tabRoute<addrIP>(nOtr);
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
        origntedSrte = new tabRoute<addrIP>(nSrte);
        origntedMvpn = new tabRoute<addrIP>(nMvpn);
        origntedMvpo = new tabRoute<addrIP>(nMvpo);
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " rpki");
        }
        computedRpki = new tabRoute<addrIP>("bgp");
        for (int i = 0; i < rpkis.size(); i++) {
            computedRpki.mergeFrom(tabRoute.addType.better, rpkis.get(i).table, null, true, tabRouteEntry.distanLim);
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
            nei.setMerge(nUni, nMlt, nOtr, nFlw, nVpnU, nVpnM, nVpnF, nVpoU, nVpoM, nVpoF, nVpls, nMspw, nEvpn, nMdt, nSrte, nMvpn, nMvpo);
        }
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            nei.setAccepted();
            nei.setGroup();
            nei.setValidity();
            nei.setMerge(nUni, nMlt, nOtr, nFlw, nVpnU, nVpnM, nVpnF, nVpoU, nVpoM, nVpoF, nVpls, nMspw, nEvpn, nMdt, nSrte, nMvpn, nMvpo);
        }
        if (conquer) {
            if (debugger.rtrBgpComp) {
                logger.debug("round " + compRound + " counquer");
            }
            computeConquerTable(routerComputedU, nUni);
            computeConquerTable(routerComputedM, nMlt);
            computeConquerTable(computedOtr, nOtr);
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
            computeConquerTable(computedSrte, nSrte);
            computeConquerTable(computedMvpn, nMvpn);
            computeConquerTable(computedMvpo, nMvpo);
        }
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " groups");
        }
        for (int i = 0; i < groups.size(); i++) {
            groups.get(i).createNeeded(nUni, nMlt, nOtr, nFlw, nVpnU, nVpnM, nVpnF, nVpoU, nVpoM, nVpoF, nVpls, nMspw, nEvpn, nMdt, nSrte, nMvpn, nMvpo);
        }
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " neigroups");
        }
        routerComputedU = nUni;
        routerComputedM = nMlt;
        computedOtr = nOtr;
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
        computedSrte = nSrte;
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
            boolean[] segrouUsd = new boolean[segrouMax];
            for (int i = 0; i < nUni.size(); i++) {
                tabRouteEntry<addrIP> ntry = nUni.get(i);
                if (ntry == null) {
                    continue;
                }
                if (ntry.segrouBeg < 1) {
                    continue;
                }
                if ((ntry.segrouIdx <= 0) || (ntry.segrouIdx >= segrouMax)) {
                    continue;
                }
                rtrBgpNeigh nei = findPeer(ntry.nextHop);
                if (nei == null) {
                    continue;
                }
                List<Integer> lab = tabLabel.int2labels(ntry.segrouBeg + ntry.segrouIdx);
                segrouLab[ntry.segrouIdx].setFwdMpls(13, fwdCore, nei.localIfc, nei.peerAddr, lab);
                segrouUsd[ntry.segrouIdx] = true;
            }
            segrouUsd[segrouIdx] = true;
            segrouLab[segrouIdx].setFwdCommon(13, fwdCore);
            for (int i = 0; i < segrouLab.length; i++) {
                if (segrouUsd[i]) {
                    continue;
                }
                segrouLab[i].setFwdDrop(13);
            }
        }
        if (bierLab != null) {
            if (debugger.rtrBgpComp) {
                logger.debug("round " + compRound + " bier");
            }
            tabLabelBier res = new tabLabelBier();
            res.base = bierLab[0].getValue();
            res.fwdr = fwdCore;
            res.bsl = tabLabelBier.num2bsl(bierLen);
            res.idx = bierIdx;
            for (int i = 0; i < nUni.size(); i++) {
                tabRouteEntry<addrIP> ntry = nUni.get(i);
                if (ntry == null) {
                    continue;
                }
                if (ntry.bierBeg < 1) {
                    continue;
                }
                if ((ntry.bierIdx <= 0) || (ntry.bierIdx >= bierMax)) {
                    continue;
                }
                rtrBgpNeigh nei = findPeer(ntry.nextHop);
                if (nei == null) {
                    continue;
                }
                tabLabelBierN per = new tabLabelBierN(nei.localIfc, nei.peerAddr, ntry.bierBeg);
                per.ned = BigInteger.ZERO;
                tabLabelBierN old = res.peers.add(per);
                if (old != null) {
                    per = old;
                }
                per.ned = per.ned.setBit(ntry.bierIdx);
            }
            for (int i = 0; i < res.peers.size(); i++) {
                tabLabelBierN ntry = res.peers.get(i);
                ntry.ned = ntry.ned.shiftRight(1);
            }
            for (int i = 0; i < bierLab.length; i++) {
                bierLab[i].setBierMpls(22, fwdCore, res);
            }
        }
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " export");
        }
        for (int i = 0; i < vrfs.size(); i++) {
            vrfs.get(i).doer.doPeers(nVpnU, nVpnM, nVpnF);
        }
        for (int i = 0; i < ovrfs.size(); i++) {
            ovrfs.get(i).doer.doPeers(nVpoU, nVpoM, nVpoF);
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
            return ntry;
        }
        if (!best.isOtherBetter(ntry)) {
            return best;
        }
        return ntry;
    }

    private void computeIncrEntry(int afi, tabRouteEntry<addrIP> curr, tabRoute<addrIP> cmp, tabRoute<addrIP> org) {
        if (debugger.rtrBgpIncr) {
            logger.debug("bestpath for " + tabRtrmapN.rd2string(curr.rouDst) + " " + curr.prefix + " in " + rtrBgpUtil.safi2string(afi));
        }
        tabRouteEntry<addrIP> best = org.find(curr);
        if (best != null) {
            best = best.copyBytes();
            best.rouSrc = rtrBgpUtil.peerOriginate;
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            best = computeIncrBest(afi, lstnNei.get(i), best, curr);
        }
        for (int i = 0; i < neighs.size(); i++) {
            best = computeIncrBest(afi, neighs.get(i), best, curr);
        }
        if (best == null) {
            cmp.del(curr);
            curr.version = compRound.get() + 1;
            for (int i = 0; i < groups.size(); i++) {
                rtrBgpGroup grp = groups.get(i);
                tabRoute<addrIP> wil = grp.getWilling(afi);
                tabRoute<addrIP> chg = grp.getChanged(afi);
                if ((wil == null) || (chg == null)) {
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
        best.version = compRound.get() + 1;
        if (conquer) {
            tabRouteEntry<addrIP> res = computeConquerEntry(cmp, best);
            if (res != null) {
                best = res;
            }
        }
        if ((best.rouSrc == rtrBgpUtil.peerOriginate) && ((afi == afiUni) || (afi == afiMlt))) {
            cmp.del(best);
        } else {
            cmp.add(tabRoute.addType.always, best, false, false);
        }
        for (int i = 0; i < groups.size(); i++) {
            rtrBgpGroup grp = groups.get(i);
            tabRoute<addrIP> wil = grp.getWilling(afi);
            tabRoute<addrIP> chg = grp.getChanged(afi);
            if ((wil == null) || (chg == null)) {
                needFull.add(1);
                continue;
            }
            tabRouteEntry<addrIP> ntry = best.copyBytes();
            tabRouteEntry<addrIP> old = wil.find(ntry);
            if (ntry.rouSrc == rtrBgpUtil.peerOriginate) {
                grp.originatePrefix(ntry);
            } else if (grp.readvertPrefix(afi == afiUni, ntry)) {
                ntry = null;
            }
            if ((afi == afiUni) || (afi == afiMlt) || (afi == afiOtr)) {
                ntry = tabRoute.doUpdateEntry(afi, ntry, grp.roumapOut, grp.roupolOut, grp.prflstOut);
            } else {
                ntry = tabRoute.doUpdateEntry(afi, ntry, grp.voumapOut, grp.voupolOut, null);
            }
            if ((ntry == null) && (old == null)) {
                continue;
            }
            if (ntry == null) {
                wil.del(best);
                chg.add(tabRoute.addType.always, best, false, false);
                continue;
            }
            if (!ntry.differs(old)) {
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
            if (ntry.version >= ver) {
                continue;
            }
            chg.del(ntry);
        }
    }

    private boolean computeIncr() {
        long tim = bits.getTime();
        if ((segrouLab != null) || (bierLab != null)) {
            return true;
        }
        for (int i = 0; i < groups.size(); i++) {
            rtrBgpGroup grp = groups.get(i);
            if (grp.sendDefRou) {
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
        for (int i = 0; i < groups.size(); i++) {
            rtrBgpGroup grp = groups.get(i);
            computeIncrPurge(grp.minversion, grp.chgUni);
            computeIncrPurge(grp.minversion, grp.chgMlt);
            computeIncrPurge(grp.minversion, grp.chgOtr);
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
            computeIncrPurge(grp.minversion, grp.chgSrte);
            computeIncrPurge(grp.minversion, grp.chgMvpn);
            computeIncrPurge(grp.minversion, grp.chgMvpo);
        }
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " changes");
        }
        computeIncrUpdate(afiUni, changedUni, routerComputedU, routerRedistedU);
        computeIncrUpdate(afiMlt, changedMlt, routerComputedM, routerRedistedM);
        computeIncrUpdate(afiOtr, changedOtr, computedOtr, origntedOtr);
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
        computeIncrUpdate(afiSrte, changedSrte, computedSrte, origntedSrte);
        computeIncrUpdate(afiMvpn, changedMvpn, computedMvpn, origntedMvpn);
        computeIncrUpdate(afiMvpo, changedMvpo, computedMvpo, origntedMvpo);
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " export");
        }
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
        if (best.nextHop == null) {
            return null;
        }
        tabRouteEntry<addrIP> old = cmp.find(best);
        if (old == null) {
            return null;
        }
        if (old.nextHop == null) {
            return null;
        }
        best = best.copyBytes();
        if (best.locPref < old.locPref) {
            best.locPref = old.locPref;
        }
        if (old.nextHop.compare(old.nextHop, best.nextHop) != 0) {
            best.locPref++;
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
        if (res == null) {
            ntry.validity = 2;
            return;
        }
        if (ntry.pathSeq == null) {
            ntry.validity = 3;
            return;
        }
        int i = ntry.pathSeq.size();
        if (i < 1) {
            ntry.validity = 3;
            return;
        }
        if (ntry.pathSeq.get(i - 1) != res.rouSrc) {
            ntry.validity = 3;
            return;
        }
        ntry.validity = 1;
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
        int chg = changedUni.size() + changedMlt.size() + changedOtr.size() + changedFlw.size()
                + changedVpnU.size() + changedVpnM.size() + changedVpnF.size()
                + changedVpoU.size() + changedVpoM.size() + changedVpoF.size()
                + changedVpls.size() + changedMspw.size() + changedEvpn.size()
                + changedMdt.size() + changedSrte.size() + changedMvpn.size() + changedMvpo.size();
        if (chg > incrLimit) {
            needFull.add(1);
        }
        if (oldAggr) {
            needFull.add(1);
        }
        if (routerAggregating.size() > 0) {
            needFull.add(1);
            oldAggr = true;
        } else {
            oldAggr = false;
        }
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
        changedCur = chg;
        changedTot += chg;
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
            ntry.stopNow();
        }
        if (lstnTmp != null) {
            tcpCore.listenStop(lstnIfc, port, null, 0, 0);
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
        l.add("1 2   address-family              specify address families");
        rtrBgpParam.getAfiList(l, "2 2,.", "to use", true);
        l.add("1 2   local-as                    specify local as number");
        l.add("2 .     <num>                     autonomous system number");
        l.add("1 .   conquer                     conquer bestpath advertisements");
        l.add("1 .   flapstat                    count flap statistics");
        l.add("1 2   incremental                 limit on incremental bestpath calculation");
        l.add("2 .     <num>                     maximum prefixes");
        l.add("1 2   router-id                   specify router id");
        l.add("2 .     <addr>                    router id");
        l.add("1 2   scantime                    scan time interval");
        l.add("2 .     <num>                     ms between scans");
        l.add("1 2   scandelay                   initial scan time delay");
        l.add("2 .     <num>                     ms before scan");
        l.add("1 2   graceful-restart            graceful restart interval");
        l.add("2 .     <num>                     ms to recover");
        l.add("1 2   template                    specify template parameters");
        l.add("2 3     <name>                    name of template");
        rtrBgpParam.getParamHelp(l);
        l.add("1 2   nexthop                     specify next hop tracking parameter");
        l.add("2 3     route-map                 filter next hops");
        l.add("3 .       <name>                  name of route map");
        l.add("2 3     route-policy              filter next hops");
        l.add("3 .       <name>                  name of route policy");
        l.add("2 3     prefix-list               filter next hops");
        l.add("3 .       <name>                  name of prefix list");
        l.add("1 2   segrout                     segment routing parameters");
        l.add("2 3     <num>                     maximum index");
        l.add("3 .       <num>                   this node index");
        l.add("1 2   bier                        bier parameters");
        l.add("2 3     <num>                     bitstring length");
        l.add("3 4       <num>                   maximum index");
        l.add("4 .         <num>                 this node index");
        l.add("1 2   flowspec                    specify flowspec parameter");
        l.add("2 .     <name>                    name of policy map");
        l.add("1 2   neighbor                    specify neighbor parameters");
        l.add("2 3     <addr>                    address of peer");
        l.add("3 4       template                get configuration from template");
        l.add("4 .         <name>                name of source template");
        rtrBgpParam.getParamHelp(l);
        l.add("1 2   distance                    specify default distance");
        l.add("2 3     <num>                     external peer distance");
        l.add("3 4       <num>                   internal peer distance");
        l.add("4 .         <num>                 locally generated distance");
        l.add("1 2   listen                      passively listen for clients");
        l.add("2 3     <name>                    access list name");
        l.add("3 .       <name>                  template name");
        l.add("1 2   dump                        setup bgp dump file");
        l.add("2 3     <name>                    name of mrt");
        l.add("3 4,.     <file>                  name of file");
        l.add("4 5         <num>                 ms between backup");
        l.add("5 .           <file>              name of backup");
        l.add("1 2   monitor                     setup bgp monitor protocol server");
        l.add("2 3     <name>                    name of bmp");
        l.add("3 4       <name>                  proxy profile");
        l.add("4 5         <name>                hostname");
        l.add("5 .           <num>               port number");
        l.add("1 2   rpki                        setup resource public key infrastructure server");
        l.add("2 3     <name>                    name of bmp");
        l.add("3 4       <name>                  proxy profile");
        l.add("4 5         <name>                hostname");
        l.add("5 .           <num>               port number");
        l.add("1 2   afi-vrf                     select vrf to advertise");
        l.add("2 3     <vrf>                     name of routing table");
        l.add("3 .       enable                  enable processing");
        l.add("3 4       mvpn                    mvpn advertisement");
        l.add("4 .         <name>                select source to advertise");
        l.add("3 4       distance                set import distance");
        l.add("4 .         <num>                 distance");
        l.add("3 4       flowspec                specify flowspec parameter");
        l.add("4 .         <name>                name of policy map");
        cfgRtr.getRedistHelp(l, 2);
        l.add("1 2   afi-ovrf                    select other vrf to advertise");
        l.add("2 3     <vrf>                     name of routing table");
        l.add("3 .       enable                  enable processing");
        l.add("3 4       mvpn                    mvpn advertisement");
        l.add("4 .         <name>                select source to advertise");
        l.add("3 4       distance                set import distance");
        l.add("4 .         <num>                 distance");
        l.add("3 4       flowspec                specify flowspec parameter");
        l.add("4 .         <name>                name of policy map");
        cfgRtr.getRedistHelp(l, 2);
        l.add("1 2   afi-vpls                    select vpls to advertise");
        l.add("2 3     <id>                      vpls id in ASnum:IDnum format");
        l.add("3 4       bridge-group            enable processing");
        l.add("4 .         <name>                bridge group number");
        l.add("3 4       update-source           select source to advertise");
        l.add("4 .         <name>                name of interface");
        l.add("3 4       ve-id                   specify ve id");
        l.add("4 5         <num>                 ve id number");
        l.add("5 .           <num>               ve maximum number");
        l.add("1 2   afi-evpn                    select evpn to advertise");
        l.add("2 3     <id>                      evpn id");
        l.add("3 4       bridge-group            enable processing");
        l.add("4 .         <name>                bridge group number");
        l.add("3 4       bmac                    set backbone mac");
        l.add("4 .         <addr>                mac address");
        l.add("3 4       update-source           select source to advertise");
        l.add("4 .         <name>                name of interface");
        l.add("3 4       encapsulation           specify encapsulation to use");
        l.add("4 .         pbb                   pbb");
        l.add("4 .         vxlan                 vxlan");
        l.add("4 .         cmac                  cmac");
    }

    /**
     * get config
     *
     * @param l list
     * @param beg beginning
     * @param filter filter
     */
    public void routerGetConfig(List<String> l, String beg, boolean filter) {
        l.add(beg + "local-as " + bits.num2str(localAs));
        l.add(beg + "router-id " + routerID);
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
        cmds.cfgLine(l, segrouMax < 1, beg, "segrout", "" + segrouMax + " " + segrouIdx);
        cmds.cfgLine(l, bierMax < 1, beg, "bier", bierLen + " " + bierMax + " " + bierIdx);
        cmds.cfgLine(l, flowSpec == null, beg, "flowspec", "" + flowSpec);
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
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            segrouMax = bits.str2num(cmd.word());
            segrouIdx = bits.str2num(cmd.word());
            segrouLab = tabLabel.allocate(13, segrouMax);
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
            if (s.equals("route-map")) {
                if (negated) {
                    nhtRoumap = null;
                    return false;
                }
                cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such route map");
                    return false;
                }
                nhtRoumap = ntry.roumap;
                return false;
            }
            if (s.equals("route-policy")) {
                if (negated) {
                    nhtRouplc = null;
                    return false;
                }
                cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such route map");
                    return false;
                }
                nhtRouplc = ntry.rouplc;
                return false;
            }
            if (s.equals("prefix-list")) {
                if (negated) {
                    nhtPfxlst = null;
                    return false;
                }
                cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such prefix list");
                    return false;
                }
                nhtPfxlst = ntry.prflst;
                return false;
            }
            return true;
        }
        if (s.equals("flowspec")) {
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
            if (s.equals("flowspec")) {
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
            if (s.equals("flowspec")) {
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
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            return false;
        }
        if (s.equals("dump")) {
            rtrBgpMrt dmp = new rtrBgpMrt();
            dmp.dumpName = cmd.word();
            if (negated) {
                dmp = dmps.del(dmp);
                if (dmp == null) {
                    return false;
                }
                dmp.stopNow();
                return false;
            }
            rtrBgpMrt old = dmps.add(dmp);
            if (old != null) {
                old.stopNow();
                dmp = old;
            }
            dmp.fileName = cmd.word();
            dmp.backupTime = bits.str2num(cmd.word());
            dmp.backupName = cmd.word();
            dmp.startNow();
            return false;
        }
        if (s.equals("monitor")) {
            rtrBgpMon mon = new rtrBgpMon();
            mon.monName = cmd.word();
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
            tcpCore.listenStop(lstnIfc, port, null, 0, 0);
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
            tcpCore.streamListen(this, new pipeLine(lstnTmp.bufferSize, false), lstnIfc, port, null, 0, 0, "bgp", lstnTmp.passwd, lstnTmp.ttlSecurity);
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
        userFormat l = new userFormat("|", "as|learn|accept|will|done|neighbor|uptime");
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
     * @param mod mode: 1=summary, 2=groups, 3=nexthops, 4=graceful, 5=addpath,
     * 6=routerid, 7=buffer, 8=description, 9=hostname, 10=compress, 11=connect,
     * 12=resolve
     * @return list of neighbors
     */
    public userFormat showSummary(int mod) {
        userFormat l = null;
        switch (mod) {
            case 1:
                l = new userFormat("|", "as|afis|ready|neighbor|uptime");
                break;
            case 2:
                l = new userFormat("|", "as|group|neighbor|uptime");
                break;
            case 3:
                l = new userFormat("|", "as|reach|chg|num|neighbor|uptime");
                break;
            case 4:
                l = new userFormat("|", "as|rx|tx|neighbor");
                break;
            case 5:
                l = new userFormat("|", "as|rx|tx|neighbor");
                break;
            case 6:
                l = new userFormat("|", "as|router|wideas|refresh|neighbor");
                break;
            case 7:
                l = new userFormat("|", "as|buffer|over|ver|incr|full|need|neighbor");
                break;
            case 8:
                l = new userFormat("|", "as|neighbor|description");
                break;
            case 9:
                l = new userFormat("|", "as|neighbor|hostname");
                break;
            case 10:
                l = new userFormat("|", "as|rx|tx|rx|tx|neighbor");
                break;
            case 11:
                l = new userFormat("|", "as|prx|ptx|brx|btx|rrx|rtx|neighbor");
                break;
            case 12:
                l = new userFormat("|", "as|neighbor|domain|uptime");
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
     * @return
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
            tabRoute.addUpdatedEntry(tabRoute.addType.better, tab, afiUni, ntry, null, null, routerAutoMesh);
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            rtrBgpNeigh nei = lstnNei.get(i);
            if (nei == null) {
                continue;
            }
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = new addrPrefix<addrIP>(nei.peerAddr, addrIP.size * 8);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, tab, afiUni, ntry, null, null, routerAutoMesh);
        }
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
     * get all routes
     *
     * @param safi safi to query
     * @param prf prefix to find
     * @return list of routes
     */
    public List<String> getAllRoutes(int safi, tabRouteEntry<addrIP> prf) {
        List<String> lst = new ArrayList<String>();
        for (int i = 0; i < neighs.size(); i++) {
            getAllRoutes(lst, neighs.get(i), safi, prf);
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            getAllRoutes(lst, lstnNei.get(i), safi, prf);
        }
        return lst;
    }

    private void getAllRoutes(List<String> lst, rtrBgpNeigh nei, int safi, tabRouteEntry<addrIP> prf) {
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
        lst.add("*************************** " + nei.peerAddr);
        lst.addAll(res.fullDump(fwdCore));
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
     * @return list of paths
     */
    public userFormat getFlappath(int afi, long rd, addrPrefix<addrIP> prf) {
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
            l.add("" + ntry.paths.get(i));
        }
        return l;
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
        res.add(shrtPthFrst.graphBeg);
        for (int i = 0; i < lst.size(); i++) {
            rtrBgpFlapath ntry = lst.get(i);
            res.add(ntry.path + " [weight=" + (o - ntry.count) + "]");
        }
        res.add(shrtPthFrst.graphEnd);
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
            cmd = new cmds("path", prf.asPathStr());
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
     * inconsistent as paths
     *
     * @param safi safi to query
     * @return text
     */
    public userFormat getAsIncons(int safi) {
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
            if (ntry.paths.size() < 2) {
                continue;
            }
            res.add(ntry.prefix + " " + tabRtrmapN.rd2string(ntry.rd) + "|" + ntry.getPaths());
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
            String a = prf.asPathStr();
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
        userFormat l = new userFormat("|", "category|value");
        l.add("version|" + compRound);
        l.add("full run|" + fullCount + " times");
        l.add("full last|" + bits.time2str(cfgAll.timeZoneName, fullLast + cfgAll.timeServerOffset, 3) + " (" + bits.timePast(fullLast) + " ago)");
        l.add("full time|" + fullTime + " ms");
        l.add("incr run|" + incrCount + " times");
        l.add("incr last|" + bits.time2str(cfgAll.timeZoneName, incrLast + cfgAll.timeServerOffset, 3) + " (" + bits.timePast(incrLast) + " ago)");
        l.add("incr time|" + incrTime + " ms");
        l.add("changes|" + changedCur + ", total=" + changedTot);
        l.add("rpki table|" + computedRpki.size());
        l.add("unicast table|" + routerComputedU.size() + ", list=" + changedUni.size());
        l.add("multicast table|" + routerComputedM.size() + ", list=" + changedMlt.size());
        l.add("other table|" + computedOtr.size() + ", list=" + changedOtr.size());
        l.add("flowspec table|" + routerComputedF.size() + ", list=" + changedFlw.size());
        l.add("vpnuni table|" + computedVpnU.size() + ", list=" + changedVpnU.size());
        l.add("vpnmlt table|" + computedVpnM.size() + ", list=" + changedVpnM.size());
        l.add("vpnflw table|" + computedVpnF.size() + ", list=" + changedVpnF.size());
        l.add("ovpnuni table|" + computedVpoU.size() + ", list=" + changedVpoU.size());
        l.add("ovpnmlt table|" + computedVpoM.size() + ", list=" + changedVpoM.size());
        l.add("ovpnflw table|" + computedVpoF.size() + ", list=" + changedVpoF.size());
        l.add("vpls table|" + computedVpls.size() + ", list=" + changedVpls.size());
        l.add("mspw table|" + computedMspw.size() + ", list=" + changedMspw.size());
        l.add("evpn table|" + computedEvpn.size() + ", list=" + changedEvpn.size());
        l.add("mdt table|" + computedMdt.size() + ", list=" + changedMdt.size());
        l.add("srte table|" + computedSrte.size() + ", list=" + changedSrte.size());
        l.add("mvpn table|" + computedMvpn.size() + ", list=" + changedMvpn.size());
        l.add("omvpn table|" + computedMvpo.size() + ", list=" + changedMvpo.size());
        return l;
    }

}
