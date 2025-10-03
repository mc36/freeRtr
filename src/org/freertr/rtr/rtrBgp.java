package org.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAceslst;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgInit;
import org.freertr.cfg.cfgPlymp;
import org.freertr.cfg.cfgPrfxlst;
import org.freertr.cfg.cfgProxy;
import org.freertr.cfg.cfgRoump;
import org.freertr.cfg.cfgRouplc;
import org.freertr.cfg.cfgRtr;
import org.freertr.cfg.cfgVrf;
import org.freertr.clnt.clntWhois;
import org.freertr.ifc.ifcDot1ah;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipFwdIface;
import org.freertr.ip.ipRtr;
import org.freertr.pack.packHolder;
import org.freertr.pipe.pipeLine;
import org.freertr.pipe.pipeSide;
import org.freertr.prt.prtGenConn;
import org.freertr.prt.prtServS;
import org.freertr.prt.prtTcp;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.tab.tabIntMatcher;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabLabelBier;
import org.freertr.tab.tabLabelBierN;
import org.freertr.tab.tabLabelEntry;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabPlcmapN;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.tab.tabQos;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRouteUtil;
import org.freertr.tab.tabRtrmapN;
import org.freertr.tab.tabRtrplcN;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.debugger;
import org.freertr.util.logFil;
import org.freertr.util.logger;
import org.freertr.util.notifier;
import org.freertr.spf.spfCalc;
import org.freertr.spf.spfLnkst;
import org.freertr.tab.tabRpkiAspa;
import org.freertr.tab.tabRpkiRoa;
import org.freertr.util.counter;
import org.freertr.util.syncInt;

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
    public boolean[] addrFams;

    /**
     * router id
     */
    public addrIPv4 routerID;

    /**
     * safe ebgp
     */
    public boolean safeEbgp;

    /**
     * client reflection
     */
    public boolean clientReflect;

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
     * bier subdomain
     */
    public int bierSub = 0;

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
     * default information originate
     */
    public boolean defRou;

    /**
     * restart time
     */
    public int restartTime;

    /**
     * long lived restart time
     */
    public int llRestartTime;

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
     * listen configurations
     */
    public tabGen<rtrBgpLstn> lstnTmp = new tabGen<rtrBgpLstn>();

    /**
     * list of dynamic neighbors
     */
    protected tabGen<rtrBgpNeigh> lstnNei;

    /**
     * dummy neighbor
     */
    protected rtrBgpNeigh dummyNei;

    /**
     * dummy speaker
     */
    protected rtrBgpSpeak dummySpk;

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
     * classful transport plane afi
     */
    protected final int afiCtp;

    /**
     * color aware routing afi
     */
    protected final int afiCar;

    /**
     * multicast afi
     */
    protected final int afiMlt;

    /**
     * other labeled unicast afi
     */
    protected final int afiOlab;

    /**
     * other classful transport plane afi
     */
    protected final int afiOctp;

    /**
     * other color aware routing afi
     */
    protected final int afiOcar;

    /**
     * other unicast afi
     */
    protected final int afiOuni;

    /**
     * other multicast afi
     */
    protected final int afiOmlt;

    /**
     * other flowspec afi
     */
    protected final int afiOflw;

    /**
     * other srte afi
     */
    protected final int afiOsrt;

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
     * rpd afi
     */
    protected final int afiRpd;

    /**
     * sdwan afi
     */
    protected final int afiSdw;

    /**
     * spf afi
     */
    protected final int afiSpf;

    /**
     * rtfilter afi
     */
    protected final int afiRtf;

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
     * mtree afi
     */
    protected final int afiMtre;

    /**
     * other mtree afi
     */
    protected final int afiMtro;

    /**
     * router number
     */
    protected final int rtrNum;

    /**
     * other changes trigger full computation
     */
    protected boolean otherTrigger;

    /**
     * have route reflector client
     */
    protected boolean have2reflect;

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
     * rpki type configured
     */
    protected tabRouteAttr.routeType rpkiT;

    /**
     * rpki number configured
     */
    protected int rpkiN;

    /**
     * rpki process
     */
    protected rtrRpki rpkiR;

    /**
     * rpki table
     */
    protected tabGen<tabRpkiRoa> rpkiA = new tabGen<tabRpkiRoa>();

    /**
     * other rpki table
     */
    protected tabGen<tabRpkiRoa> rpkiO = new tabGen<tabRpkiRoa>();

    /**
     * provider rpki table
     */
    protected tabGen<tabRpkiAspa> rpkiP = new tabGen<tabRpkiAspa>();

    /**
     * the computed other unicast routes
     */
    public tabRoute<addrIP> computedOuni = new tabRoute<addrIP>("rx");

    /**
     * the computed other multicast routes
     */
    public tabRoute<addrIP> computedOmlt = new tabRoute<addrIP>("rx");

    /**
     * the computed other flowspec routes
     */
    public tabRoute<addrIP> computedOflw = new tabRoute<addrIP>("rx");

    /**
     * the computed other srte routes
     */
    public tabRoute<addrIP> computedOsrt = new tabRoute<addrIP>("rx");

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
     * the computed rpd routes
     */
    public tabRoute<addrIP> computedRpd = new tabRoute<addrIP>("rx");

    /**
     * the computed sdwan routes
     */
    public tabRoute<addrIP> computedSdw = new tabRoute<addrIP>("rx");

    /**
     * the computed spf routes
     */
    public tabRoute<addrIP> computedSpf = new tabRoute<addrIP>("rx");

    /**
     * the computed rtfilter routes
     */
    public tabRoute<addrIP> computedRtf = new tabRoute<addrIP>("rx");

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
     * the computed mtree routes
     */
    public tabRoute<addrIP> computedMtre = new tabRoute<addrIP>("rx");

    /**
     * the computed other mtree routes
     */
    public tabRoute<addrIP> computedMtro = new tabRoute<addrIP>("rx");

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
    public final tabRoute<addrIP> changedOuni = new tabRoute<addrIP>("rx");

    /**
     * the changed other multicast routes
     */
    public final tabRoute<addrIP> changedOmlt = new tabRoute<addrIP>("rx");

    /**
     * the changed other flowspec routes
     */
    public final tabRoute<addrIP> changedOflw = new tabRoute<addrIP>("rx");

    /**
     * the changed other srte routes
     */
    public final tabRoute<addrIP> changedOsrt = new tabRoute<addrIP>("rx");

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
     * the changed rpd routes
     */
    public final tabRoute<addrIP> changedRpd = new tabRoute<addrIP>("rx");

    /**
     * the changed sdwan routes
     */
    public final tabRoute<addrIP> changedSdw = new tabRoute<addrIP>("rx");

    /**
     * the changed spf routes
     */
    public final tabRoute<addrIP> changedSpf = new tabRoute<addrIP>("rx");

    /**
     * the changed rtfilter routes
     */
    public final tabRoute<addrIP> changedRtf = new tabRoute<addrIP>("rx");

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
     * the changed mvpn routes
     */
    public final tabRoute<addrIP> changedMtre = new tabRoute<addrIP>("rx");

    /**
     * the changed other mvpn routes
     */
    public final tabRoute<addrIP> changedMtro = new tabRoute<addrIP>("rx");

    /**
     * the newly computed unicast routes
     */
    public tabRoute<addrIP> newlyUni = new tabRoute<addrIP>("bst");

    /**
     * the newly computed multicast routes
     */
    public tabRoute<addrIP> newlyMlt = new tabRoute<addrIP>("bst");

    /**
     * the newly computed other unicast routes
     */
    public tabRoute<addrIP> newlyOuni = new tabRoute<addrIP>("bst");

    /**
     * the newly computed other multicast routes
     */
    public tabRoute<addrIP> newlyOmlt = new tabRoute<addrIP>("bst");

    /**
     * the newly computed other flowspec routes
     */
    public tabRoute<addrIP> newlyOflw = new tabRoute<addrIP>("bst");

    /**
     * the newly computed other srte routes
     */
    public tabRoute<addrIP> newlyOsrt = new tabRoute<addrIP>("bst");

    /**
     * the newly computed flowspec routes
     */
    public tabRoute<addrIP> newlyFlw = new tabRoute<addrIP>("bst");

    /**
     * the newly computed vpnuni routes
     */
    public tabRoute<addrIP> newlyVpnU = new tabRoute<addrIP>("bst");

    /**
     * the newly computed vpnmlt routes
     */
    public tabRoute<addrIP> newlyVpnM = new tabRoute<addrIP>("bst");

    /**
     * the newly computed vpnflw routes
     */
    public tabRoute<addrIP> newlyVpnF = new tabRoute<addrIP>("bst");

    /**
     * the newly computed other vpnuni routes
     */
    public tabRoute<addrIP> newlyVpoU = new tabRoute<addrIP>("bst");

    /**
     * the newly computed other vpnmlt routes
     */
    public tabRoute<addrIP> newlyVpoM = new tabRoute<addrIP>("bst");

    /**
     * the newly computed other vpnflw routes
     */
    public tabRoute<addrIP> newlyVpoF = new tabRoute<addrIP>("bst");

    /**
     * the newly computed vpls routes
     */
    public tabRoute<addrIP> newlyVpls = new tabRoute<addrIP>("bst");

    /**
     * the newly computed mspw routes
     */
    public tabRoute<addrIP> newlyMspw = new tabRoute<addrIP>("bst");

    /**
     * the newly computed evpn routes
     */
    public tabRoute<addrIP> newlyEvpn = new tabRoute<addrIP>("bst");

    /**
     * the newly computed mdt routes
     */
    public tabRoute<addrIP> newlyMdt = new tabRoute<addrIP>("bst");

    /**
     * the newly computed nsh routes
     */
    public tabRoute<addrIP> newlyNsh = new tabRoute<addrIP>("bst");

    /**
     * the newly computed rpd routes
     */
    public tabRoute<addrIP> newlyRpd = new tabRoute<addrIP>("bst");

    /**
     * the newly computed sdwan routes
     */
    public tabRoute<addrIP> newlySdw = new tabRoute<addrIP>("bst");

    /**
     * the newly computed spf routes
     */
    public tabRoute<addrIP> newlySpf = new tabRoute<addrIP>("bst");

    /**
     * the newly computed rtfilter routes
     */
    public tabRoute<addrIP> newlyRtf = new tabRoute<addrIP>("bst");

    /**
     * the newly computed srte routes
     */
    public tabRoute<addrIP> newlySrte = new tabRoute<addrIP>("bst");

    /**
     * the newly computed lnkst routes
     */
    public tabRoute<addrIP> newlyLnks = new tabRoute<addrIP>("bst");

    /**
     * the newly computed mvpn routes
     */
    public tabRoute<addrIP> newlyMvpn = new tabRoute<addrIP>("bst");

    /**
     * the newly computed other mvpn routes
     */
    public tabRoute<addrIP> newlyMvpo = new tabRoute<addrIP>("bst");

    /**
     * the newly computed mtree routes
     */
    public tabRoute<addrIP> newlyMtre = new tabRoute<addrIP>("bst");

    /**
     * the newly computed other mtree routes
     */
    public tabRoute<addrIP> newlyMtro = new tabRoute<addrIP>("bst");

    /**
     * the originated other unicast routes
     */
    public tabRoute<addrIP> origntedOuni = new tabRoute<addrIP>("tx");

    /**
     * the originated other multicast routes
     */
    public tabRoute<addrIP> origntedOmlt = new tabRoute<addrIP>("tx");

    /**
     * the originated other flowspec routes
     */
    public tabRoute<addrIP> origntedOflw = new tabRoute<addrIP>("tx");

    /**
     * the originated other srte routes
     */
    public tabRoute<addrIP> origntedOsrt = new tabRoute<addrIP>("tx");

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
     * the originated rpd routes
     */
    public tabRoute<addrIP> origntedRpd = new tabRoute<addrIP>("tx");

    /**
     * the originated sdwan routes
     */
    public tabRoute<addrIP> origntedSdw = new tabRoute<addrIP>("tx");

    /**
     * the originated spf routes
     */
    public tabRoute<addrIP> origntedSpf = new tabRoute<addrIP>("tx");

    /**
     * the originated rtfilter routes
     */
    public tabRoute<addrIP> origntedRtf = new tabRoute<addrIP>("tx");

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
     * the originated mtree routes
     */
    public tabRoute<addrIP> origntedMtre = new tabRoute<addrIP>("tx");

    /**
     * the originated other mtree routes
     */
    public tabRoute<addrIP> origntedMtro = new tabRoute<addrIP>("tx");

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
    public tabGen<rtrBgpFlapStat> flaps;

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
     * spf afi router
     */
    protected rtrBgpSpf lspf;

    /**
     * list of vrfs
     */
    protected tabGen<rtrBgpVrf> vrfs;

    /**
     * list of other vrfs
     */
    protected tabGen<rtrBgpVrf> ovrfs;

    /**
     * list of colors
     */
    protected tabGen<rtrBgpVrf> clrs;

    /**
     * list of other colors
     */
    protected tabGen<rtrBgpVrf> oclrs;

    /**
     * list of l3evpn
     */
    protected tabGen<rtrBgpVrf> l3es;

    /**
     * list of other l3evpn
     */
    protected tabGen<rtrBgpVrf> ol3es;

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
     * accept statistics
     */
    public final counter accptStat = new counter();

    /**
     * reachability statistics
     */
    public final counter reachabStat = new counter();

    /**
     * unreachability statistics
     */
    public final counter unreachStat = new counter();

    /**
     * message types received
     */
    public final counter[] msgStats = new counter[256];

    /**
     * attribute types received
     */
    public final counter[] attrStats = new counter[256];

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
     * changed prefixes peak
     */
    public long changedMax;

    /**
     * changed prefixes peak
     */
    public long changedPek;

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
        clrs = new tabGen<rtrBgpVrf>();
        oclrs = new tabGen<rtrBgpVrf>();
        l3es = new tabGen<rtrBgpVrf>();
        ol3es = new tabGen<rtrBgpVrf>();
        vpls = new tabGen<rtrBgpVpls>();
        evpn = new tabGen<rtrBgpEvpn>();
        evpnRcv = new rtrBgpEvpnPbb(this);
        routerID = new addrIPv4();
        safeEbgp = true;
        clientReflect = true;
        addrFams = rtrBgpParam.boolsSet(false);
        addrFams[rtrBgpParam.idxUni] = true;
        rtrNum = id;
        for (int i = 0; i < msgStats.length; i++) {
            msgStats[i] = new counter();
        }
        for (int i = 0; i < attrStats.length; i++) {
            attrStats[i] = new counter();
        }
        switch (fwdCore.ipVersion) {
            case ipCor4.protocolVersion:
                rouTyp = tabRouteAttr.routeType.bgp4;
                afiUni = rtrBgpUtil.safiIp4uni;
                afiLab = rtrBgpUtil.safiIp4lab;
                afiCtp = rtrBgpUtil.safiIp4ctp;
                afiCar = rtrBgpUtil.safiIp4car;
                afiMlt = rtrBgpUtil.safiIp4multi;
                afiOlab = rtrBgpUtil.safiIp6lab;
                afiOctp = rtrBgpUtil.safiIp6ctp;
                afiOcar = rtrBgpUtil.safiIp6car;
                afiOuni = rtrBgpUtil.safiIp6uni;
                afiOmlt = rtrBgpUtil.safiIp6multi;
                afiOflw = rtrBgpUtil.safiIp6flow;
                afiOsrt = rtrBgpUtil.safiIp6srte;
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
                afiRpd = rtrBgpUtil.safiRpd46;
                afiSdw = rtrBgpUtil.safiIp4sdwan;
                afiSpf = rtrBgpUtil.safiIp46spf;
                afiRtf = rtrBgpUtil.safiRtf46;
                afiLnks = rtrBgpUtil.safiIp46lnks;
                afiSrte = rtrBgpUtil.safiIp4srte;
                afiMvpn = rtrBgpUtil.safiIp4mvpn;
                afiMvpo = rtrBgpUtil.safiIp6mvpn;
                afiMtre = rtrBgpUtil.safiIp4mtree;
                afiMtro = rtrBgpUtil.safiIp6mtree;
                other = new rtrBgpOther(this, vrfCore.fwd6);
                lspf = new rtrBgpSpf(this);
                break;
            case ipCor6.protocolVersion:
                rouTyp = tabRouteAttr.routeType.bgp6;
                afiUni = rtrBgpUtil.safiIp6uni;
                afiLab = rtrBgpUtil.safiIp6lab;
                afiCtp = rtrBgpUtil.safiIp6ctp;
                afiCar = rtrBgpUtil.safiIp6car;
                afiMlt = rtrBgpUtil.safiIp6multi;
                afiOlab = rtrBgpUtil.safiIp4lab;
                afiOctp = rtrBgpUtil.safiIp4ctp;
                afiOcar = rtrBgpUtil.safiIp4car;
                afiOuni = rtrBgpUtil.safiIp4uni;
                afiOmlt = rtrBgpUtil.safiIp4multi;
                afiOflw = rtrBgpUtil.safiIp4flow;
                afiOsrt = rtrBgpUtil.safiIp4srte;
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
                afiRpd = rtrBgpUtil.safiRpd46;
                afiSdw = rtrBgpUtil.safiIp6sdwan;
                afiSpf = rtrBgpUtil.safiIp46spf;
                afiRtf = rtrBgpUtil.safiRtf46;
                afiLnks = rtrBgpUtil.safiIp46lnks;
                afiSrte = rtrBgpUtil.safiIp6srte;
                afiMvpn = rtrBgpUtil.safiIp6mvpn;
                afiMvpo = rtrBgpUtil.safiIp4mvpn;
                afiMtre = rtrBgpUtil.safiIp6mtree;
                afiMtro = rtrBgpUtil.safiIp4mtree;
                other = new rtrBgpOther(this, vrfCore.fwd4);
                lspf = new rtrBgpSpf(this);
                break;
            default:
                rouTyp = null;
                afiUni = 0;
                afiLab = 0;
                afiCtp = 0;
                afiCar = 0;
                afiMlt = 0;
                afiOlab = 0;
                afiOctp = 0;
                afiOcar = 0;
                afiOuni = 0;
                afiOmlt = 0;
                afiOflw = 0;
                afiOsrt = 0;
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
                afiRpd = 0;
                afiSdw = 0;
                afiSpf = 0;
                afiRtf = 0;
                afiLnks = 0;
                afiSrte = 0;
                afiMvpn = 0;
                afiMvpo = 0;
                afiMtre = 0;
                afiMtro = 0;
                other = new rtrBgpOther(this, null);
                lspf = new rtrBgpSpf(this);
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
        temps = new tabGen<rtrBgpTemp>();
        routerComputedU = new tabRoute<addrIP>("rx");
        routerComputedM = new tabRoute<addrIP>("rx");
        routerComputedF = new tabRoute<addrIP>("rx");
        dummyNei = new rtrBgpNeigh(this, new addrIP());
        dummySpk = new rtrBgpSpeak(this, dummyNei, null, 0);
        if (tcpCore == null) {
            return;
        }
        routerComputedI = new tabGen<tabIndex<addrIP>>();
        evpnUni = tabLabel.allocate(tabLabelEntry.owner.evpnPbb);
        evpnMul = tabLabel.allocate(tabLabelEntry.owner.evpnPbb);
        evpnUni.setFwdPwe(tabLabelEntry.owner.evpnPbb, fwdCore, evpnRcv, 0, null);
        evpnMul.setFwdPwe(tabLabelEntry.owner.evpnPbb, fwdCore, evpnRcv, 0, null);
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
     * convert safi to index
     *
     * @param safi safi
     * @return mask
     */
    public int safi2idx(int safi) {
        if (safi == afiUni) {
            return rtrBgpParam.idxUni;
        }
        if (safi == afiLab) {
            return rtrBgpParam.idxLab;
        }
        if (safi == afiCtp) {
            return rtrBgpParam.idxCtp;
        }
        if (safi == afiCar) {
            return rtrBgpParam.idxCar;
        }
        if (safi == afiMlt) {
            return rtrBgpParam.idxMlt;
        }
        if (safi == afiOlab) {
            return rtrBgpParam.idxOlab;
        }
        if (safi == afiOctp) {
            return rtrBgpParam.idxOctp;
        }
        if (safi == afiOcar) {
            return rtrBgpParam.idxOcar;
        }
        if (safi == afiOuni) {
            return rtrBgpParam.idxOuni;
        }
        if (safi == afiOmlt) {
            return rtrBgpParam.idxOmlt;
        }
        if (safi == afiOflw) {
            return rtrBgpParam.idxOflw;
        }
        if (safi == afiOsrt) {
            return rtrBgpParam.idxOsrt;
        }
        if (safi == afiFlw) {
            return rtrBgpParam.idxFlw;
        }
        if (safi == afiVpnU) {
            return rtrBgpParam.idxVpnU;
        }
        if (safi == afiVpnM) {
            return rtrBgpParam.idxVpnM;
        }
        if (safi == afiVpnF) {
            return rtrBgpParam.idxVpnF;
        }
        if (safi == afiVpoU) {
            return rtrBgpParam.idxVpoU;
        }
        if (safi == afiVpoM) {
            return rtrBgpParam.idxVpoM;
        }
        if (safi == afiVpoF) {
            return rtrBgpParam.idxVpoF;
        }
        if (safi == afiVpls) {
            return rtrBgpParam.idxVpls;
        }
        if (safi == afiMspw) {
            return rtrBgpParam.idxMspw;
        }
        if (safi == afiEvpn) {
            return rtrBgpParam.idxEvpn;
        }
        if (safi == afiMdt) {
            return rtrBgpParam.idxMdt;
        }
        if (safi == afiNsh) {
            return rtrBgpParam.idxNsh;
        }
        if (safi == afiRpd) {
            return rtrBgpParam.idxRpd;
        }
        if (safi == afiSdw) {
            return rtrBgpParam.idxSdw;
        }
        if (safi == afiSpf) {
            return rtrBgpParam.idxSpf;
        }
        if (safi == afiRtf) {
            return rtrBgpParam.idxRtf;
        }
        if (safi == afiSrte) {
            return rtrBgpParam.idxSrte;
        }
        if (safi == afiLnks) {
            return rtrBgpParam.idxLnks;
        }
        if (safi == afiMvpn) {
            return rtrBgpParam.idxMvpn;
        }
        if (safi == afiMvpo) {
            return rtrBgpParam.idxMvpo;
        }
        if (safi == afiMtre) {
            return rtrBgpParam.idxMtre;
        }
        if (safi == afiMtro) {
            return rtrBgpParam.idxMtro;
        }
        logger.info("unknown safi (" + safi + ") requested");
        return -1;
    }

    /**
     * convert index to safi
     *
     * @param idx mask
     * @return safi
     */
    public int idx2safi(int idx) {
        if (idx == rtrBgpParam.idxUni) {
            return afiUni;
        }
        if (idx == rtrBgpParam.idxLab) {
            return afiLab;
        }
        if (idx == rtrBgpParam.idxCtp) {
            return afiCtp;
        }
        if (idx == rtrBgpParam.idxCar) {
            return afiCar;
        }
        if (idx == rtrBgpParam.idxMlt) {
            return afiMlt;
        }
        if (idx == rtrBgpParam.idxOlab) {
            return afiOlab;
        }
        if (idx == rtrBgpParam.idxOctp) {
            return afiOctp;
        }
        if (idx == rtrBgpParam.idxOcar) {
            return afiOcar;
        }
        if (idx == rtrBgpParam.idxOuni) {
            return afiOuni;
        }
        if (idx == rtrBgpParam.idxOmlt) {
            return afiOmlt;
        }
        if (idx == rtrBgpParam.idxOflw) {
            return afiOflw;
        }
        if (idx == rtrBgpParam.idxOsrt) {
            return afiOsrt;
        }
        if (idx == rtrBgpParam.idxFlw) {
            return afiFlw;
        }
        if (idx == rtrBgpParam.idxVpnU) {
            return afiVpnU;
        }
        if (idx == rtrBgpParam.idxVpnM) {
            return afiVpnM;
        }
        if (idx == rtrBgpParam.idxVpnF) {
            return afiVpnF;
        }
        if (idx == rtrBgpParam.idxVpoU) {
            return afiVpoU;
        }
        if (idx == rtrBgpParam.idxVpoM) {
            return afiVpoM;
        }
        if (idx == rtrBgpParam.idxVpoF) {
            return afiVpoF;
        }
        if (idx == rtrBgpParam.idxVpls) {
            return afiVpls;
        }
        if (idx == rtrBgpParam.idxMspw) {
            return afiMspw;
        }
        if (idx == rtrBgpParam.idxEvpn) {
            return afiEvpn;
        }
        if (idx == rtrBgpParam.idxMdt) {
            return afiMdt;
        }
        if (idx == rtrBgpParam.idxNsh) {
            return afiNsh;
        }
        if (idx == rtrBgpParam.idxRpd) {
            return afiRpd;
        }
        if (idx == rtrBgpParam.idxSdw) {
            return afiSdw;
        }
        if (idx == rtrBgpParam.idxSpf) {
            return afiSpf;
        }
        if (idx == rtrBgpParam.idxRtf) {
            return afiRtf;
        }
        if (idx == rtrBgpParam.idxSrte) {
            return afiSrte;
        }
        if (idx == rtrBgpParam.idxLnks) {
            return afiLnks;
        }
        if (idx == rtrBgpParam.idxMvpn) {
            return afiMvpn;
        }
        if (idx == rtrBgpParam.idxMvpo) {
            return afiMvpo;
        }
        if (idx == rtrBgpParam.idxMtre) {
            return afiMtre;
        }
        if (idx == rtrBgpParam.idxMtro) {
            return afiMtro;
        }
        logger.info("unknown safi (" + idx + ") requested");
        return -1;
    }

    /**
     * clear flap statistics
     */
    public void doClearFlaps() {
        if (flaps == null) {
            return;
        }
        flaps.clear();
    }

    /**
     * clear peak statistics
     */
    public void doClearPeaks() {
        changedMax = 0;
        changedPek = 0;
    }

    /**
     * clear tiny counters
     */
    public void doClearTinys() {
        reachabStat.clear();
        unreachStat.clear();
        accptStat.clear();
    }

    /**
     * clear msg statistics
     */
    public void doClearMsgs() {
        for (int i = 0; i < msgStats.length; i++) {
            msgStats[i].clear();
        }
    }

    /**
     * clear msg statistics
     */
    public void doClearAttrs() {
        for (int i = 0; i < attrStats.length; i++) {
            attrStats[i].clear();
        }
    }

    /**
     * get database
     *
     * @param idx safi to query
     * @param mask safi to query
     * @param safi safi to query
     * @return table
     */
    public tabRoute<addrIP> getDatabase(int idx, long mask, int safi) {
        if (idx == rtrBgpParam.idxUni) {
            return routerComputedU;
        }
        if (idx == rtrBgpParam.idxLab) {
            return routerComputedU;
        }
        if (idx == rtrBgpParam.idxCtp) {
            return routerComputedU;
        }
        if (idx == rtrBgpParam.idxCar) {
            return routerComputedU;
        }
        if (idx == rtrBgpParam.idxMlt) {
            return routerComputedM;
        }
        if (idx == rtrBgpParam.idxOlab) {
            return computedOuni;
        }
        if (idx == rtrBgpParam.idxOctp) {
            return computedOuni;
        }
        if (idx == rtrBgpParam.idxOcar) {
            return computedOuni;
        }
        if (idx == rtrBgpParam.idxOuni) {
            return computedOuni;
        }
        if (idx == rtrBgpParam.idxOmlt) {
            return computedOmlt;
        }
        if (idx == rtrBgpParam.idxOflw) {
            return computedOflw;
        }
        if (idx == rtrBgpParam.idxOsrt) {
            return computedOsrt;
        }
        if (idx == rtrBgpParam.idxFlw) {
            return routerComputedF;
        }
        if (idx == rtrBgpParam.idxVpnU) {
            return computedVpnU;
        }
        if (idx == rtrBgpParam.idxVpnM) {
            return computedVpnM;
        }
        if (idx == rtrBgpParam.idxVpnF) {
            return computedVpnF;
        }
        if (idx == rtrBgpParam.idxVpoU) {
            return computedVpoU;
        }
        if (idx == rtrBgpParam.idxVpoM) {
            return computedVpoM;
        }
        if (idx == rtrBgpParam.idxVpoF) {
            return computedVpoF;
        }
        if (idx == rtrBgpParam.idxVpls) {
            return computedVpls;
        }
        if (idx == rtrBgpParam.idxMspw) {
            return computedMspw;
        }
        if (idx == rtrBgpParam.idxEvpn) {
            return computedEvpn;
        }
        if (idx == rtrBgpParam.idxMdt) {
            return computedMdt;
        }
        if (idx == rtrBgpParam.idxNsh) {
            return computedNsh;
        }
        if (idx == rtrBgpParam.idxRpd) {
            return computedRpd;
        }
        if (idx == rtrBgpParam.idxSdw) {
            return computedSdw;
        }
        if (idx == rtrBgpParam.idxSpf) {
            return computedSpf;
        }
        if (idx == rtrBgpParam.idxRtf) {
            return computedRtf;
        }
        if (idx == rtrBgpParam.idxSrte) {
            return computedSrte;
        }
        if (idx == rtrBgpParam.idxLnks) {
            return computedLnks;
        }
        if (idx == rtrBgpParam.idxMvpn) {
            return computedMvpn;
        }
        if (idx == rtrBgpParam.idxMvpo) {
            return computedMvpo;
        }
        if (idx == rtrBgpParam.idxMtre) {
            return computedMtre;
        }
        if (idx == rtrBgpParam.idxMtro) {
            return computedMtro;
        }
        logger.info("unknown safi (" + safi + ") requested");
        return null;
    }

    /**
     * get changed
     *
     * @param idx safi to query
     * @param mask safi to query
     * @param safi safi to query
     * @return table
     */
    public tabRoute<addrIP> getChanged(int idx, long mask, int safi) {
        if (idx == rtrBgpParam.idxUni) {
            return changedUni;
        }
        if (idx == rtrBgpParam.idxLab) {
            return changedUni;
        }
        if (idx == rtrBgpParam.idxCtp) {
            return changedUni;
        }
        if (idx == rtrBgpParam.idxCar) {
            return changedUni;
        }
        if (idx == rtrBgpParam.idxMlt) {
            return changedMlt;
        }
        if (idx == rtrBgpParam.idxOlab) {
            return changedOuni;
        }
        if (idx == rtrBgpParam.idxOctp) {
            return changedOuni;
        }
        if (idx == rtrBgpParam.idxOcar) {
            return changedOuni;
        }
        if (idx == rtrBgpParam.idxOuni) {
            return changedOuni;
        }
        if (idx == rtrBgpParam.idxOmlt) {
            return changedOmlt;
        }
        if (idx == rtrBgpParam.idxOflw) {
            return changedOflw;
        }
        if (idx == rtrBgpParam.idxOsrt) {
            return changedOsrt;
        }
        if (idx == rtrBgpParam.idxFlw) {
            return changedFlw;
        }
        if (idx == rtrBgpParam.idxVpnU) {
            return changedVpnU;
        }
        if (idx == rtrBgpParam.idxVpnM) {
            return changedVpnM;
        }
        if (idx == rtrBgpParam.idxVpnF) {
            return changedVpnF;
        }
        if (idx == rtrBgpParam.idxVpoU) {
            return changedVpoU;
        }
        if (idx == rtrBgpParam.idxVpoM) {
            return changedVpoM;
        }
        if (idx == rtrBgpParam.idxVpoF) {
            return changedVpoF;
        }
        if (idx == rtrBgpParam.idxVpls) {
            return changedVpls;
        }
        if (idx == rtrBgpParam.idxMspw) {
            return changedMspw;
        }
        if (idx == rtrBgpParam.idxEvpn) {
            return changedEvpn;
        }
        if (idx == rtrBgpParam.idxMdt) {
            return changedMdt;
        }
        if (idx == rtrBgpParam.idxNsh) {
            return changedNsh;
        }
        if (idx == rtrBgpParam.idxRpd) {
            return changedRpd;
        }
        if (idx == rtrBgpParam.idxSdw) {
            return changedSdw;
        }
        if (idx == rtrBgpParam.idxSpf) {
            return changedSpf;
        }
        if (idx == rtrBgpParam.idxRtf) {
            return changedRtf;
        }
        if (idx == rtrBgpParam.idxSrte) {
            return changedSrte;
        }
        if (idx == rtrBgpParam.idxLnks) {
            return changedLnks;
        }
        if (idx == rtrBgpParam.idxMvpn) {
            return changedMvpn;
        }
        if (idx == rtrBgpParam.idxMvpo) {
            return changedMvpo;
        }
        if (idx == rtrBgpParam.idxMtre) {
            return changedMtre;
        }
        if (idx == rtrBgpParam.idxMtro) {
            return changedMtro;
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
        needFull.add(1);
        routerCreateComputed();
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
        packHolder pckCnt = new packHolder(true, true);
        accptStat.rx(pckCnt);
        rtrBgpLstn lstn = null;
        for (int i = 0; i < lstnTmp.size(); i++) {
            rtrBgpLstn ntry = lstnTmp.get(i);
            if (!ntry.acl.matches(id)) {
                continue;
            }
            lstn = ntry;
            break;
        }
        if (lstn == null) {
            accptStat.drop(pckCnt, counter.reasons.notInTab);
            return true;
        }
        if (lstn.temp.maxClones > 0) {
            int i = countClones(neighs, lstn.temp);
            i += countClones(lstnNei, lstn.temp);
            if (i > lstn.temp.maxClones) {
                accptStat.drop(pckCnt, counter.reasons.noBuffer);
                return true;
            }
        }
        id.changeSecurity(lstn.temp.keyId, lstn.temp.passwd, lstn.temp.ttlSecurity, lstn.temp.tosValue);
        rtrBgpNeigh ntry = new rtrBgpNeigh(this, id.peerAddr);
        ntry.updateAddr(id.iface);
        if (neighs.find(ntry) != null) {
            accptStat.drop(pckCnt, counter.reasons.notUp);
            return true;
        }
        ntry.copyFrom(lstn.temp);
        ntry.template = lstn.temp;
        ntry.updatePeer();
        rtrBgpNeigh res = lstnNei.add(ntry);
        if (res != null) {
            accptStat.drop(pckCnt, counter.reasons.noBuffer);
            return true;
        }
        logger.info("accepting dynamic " + id.peerAddr + " " + id.portRem + " as " + lstn.temp);
        ntry.conn = new rtrBgpSpeak(this, ntry, pipe, 1);
        ntry.socketMode = 4;
        ntry.startNow();
        accptStat.tx(pckCnt);
        return false;
    }

    private final int countClones(tabGen<rtrBgpNeigh> lst, rtrBgpTemp tmp) {
        int o = 0;
        for (int i = lst.size() - 1; i >= 0; i--) {
            rtrBgpNeigh ntry = lst.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.template != tmp) {
                continue;
            }
            o++;
        }
        return o;
    }

    /**
     * add listen peer
     *
     * @param peer peer address
     * @param from from address
     * @param temp template to use
     * @return neighbor instance
     */
    public rtrBgpNeigh addListenPeer(addrIP peer, addrIP from, rtrBgpTemp temp) {
        rtrBgpNeigh ntry = new rtrBgpNeigh(this, peer);
        ntry.localAddr = from.copyBytes();
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
        tabGen<rtrBgpNeigh> lstn = new tabGen<rtrBgpNeigh>(lstnNei);
        routerChangedU = null;
        routerChangedM = null;
        routerChangedF = null;
        other.routerChangedU = null;
        other.routerChangedM = null;
        other.routerChangedF = null;
        changedUni.clear();
        changedMlt.clear();
        changedOuni.clear();
        changedOmlt.clear();
        changedOflw.clear();
        changedOsrt.clear();
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
        changedRpd.clear();
        changedSdw.clear();
        changedSpf.clear();
        changedRtf.clear();
        changedSrte.clear();
        changedLnks.clear();
        changedMvpn.clear();
        changedMvpo.clear();
        changedMtre.clear();
        changedMtro.clear();
        newlyUni = new tabRoute<addrIP>("bst");
        newlyMlt = new tabRoute<addrIP>("bst");
        newlyOuni = new tabRoute<addrIP>("bst");
        newlyOmlt = new tabRoute<addrIP>("bst");
        newlyOflw = new tabRoute<addrIP>("bst");
        newlyOsrt = new tabRoute<addrIP>("bst");
        newlyFlw = new tabRoute<addrIP>("bst");
        newlyVpnU = new tabRoute<addrIP>("bst");
        newlyVpnM = new tabRoute<addrIP>("bst");
        newlyVpnF = new tabRoute<addrIP>("bst");
        newlyVpoU = new tabRoute<addrIP>("bst");
        newlyVpoM = new tabRoute<addrIP>("bst");
        newlyVpoF = new tabRoute<addrIP>("bst");
        newlyVpls = new tabRoute<addrIP>("bst");
        newlyMspw = new tabRoute<addrIP>("bst");
        newlyEvpn = new tabRoute<addrIP>("bst");
        newlyMdt = new tabRoute<addrIP>("bst");
        newlyNsh = new tabRoute<addrIP>("bst");
        newlyRpd = new tabRoute<addrIP>("bst");
        newlySdw = new tabRoute<addrIP>("bst");
        newlySpf = new tabRoute<addrIP>("bst");
        newlyRtf = new tabRoute<addrIP>("bst");
        newlySrte = new tabRoute<addrIP>("bst");
        newlyLnks = new tabRoute<addrIP>("bst");
        newlyMvpn = new tabRoute<addrIP>("bst");
        newlyMvpo = new tabRoute<addrIP>("bst");
        newlyMtre = new tabRoute<addrIP>("bst");
        newlyMtro = new tabRoute<addrIP>("bst");
        if (flowSpec != null) {
            rtrBgpFlow.doAdvertise(newlyFlw, flowSpec, new tabRouteEntry<addrIP>(), afiUni == rtrBgpUtil.safiIp6uni, localAs);
        }
        for (int i = 0; i < linkStates.size(); i++) {
            rtrBgpLnkst ls = linkStates.get(i);
            ls.rtr.routerLinkStates(newlyLnks, ls.par, localAs, routerID);
        }
        for (int i = 0; i < routerRedistedF.size(); i++) {
            tabRouteEntry<addrIP> ntry = routerRedistedF.get(i);
            if (ntry == null) {
                continue;
            }
            ntry = ntry.copyBytes(tabRoute.addType.notyet);
            ntry.best.rouTyp = rouTyp;
            ntry.best.protoNum = rtrNum;
            ntry.best.distance = distantLoc;
            newlyFlw.add(tabRoute.addType.better, ntry, false, false);
        }
        other.doAdvertise();
        lspf.doAdvertise();
        for (int i = 0; i < vrfs.size(); i++) {
            vrfs.get(i).doer.doAdvertise(rtrBgpUtil.sfiUnicast, newlyVpnU, newlyVpnM, newlyVpnF, newlyMvpn);
        }
        for (int i = 0; i < ovrfs.size(); i++) {
            ovrfs.get(i).doer.doAdvertise(rtrBgpUtil.sfiUnicast, newlyVpoU, newlyVpoM, newlyVpoF, newlyMvpo);
        }
        for (int i = 0; i < clrs.size(); i++) {
            clrs.get(i).doer.doAdvertise(rtrBgpUtil.sfiUnicast, newlyUni, newlyMlt, newlyFlw, newlyMvpn);
        }
        for (int i = 0; i < oclrs.size(); i++) {
            oclrs.get(i).doer.doAdvertise(rtrBgpUtil.sfiUnicast, newlyOuni, newlyOmlt, newlyOflw, newlyMvpo);
        }
        for (int i = 0; i < l3es.size(); i++) {
            l3es.get(i).doer.doAdvertise(rtrBgpUtil.sfiEthVpn, newlyEvpn, newlyVpnM, newlyVpnF, newlyMvpn);
        }
        for (int i = 0; i < ol3es.size(); i++) {
            ol3es.get(i).doer.doAdvertise(rtrBgpUtil.sfiEthVpn, newlyEvpn, newlyVpoM, newlyVpoF, newlyMvpo);
        }
        for (int i = 0; i < vpls.size(); i++) {
            vpls.get(i).doAdvertise();
        }
        for (int i = 0; i < evpn.size(); i++) {
            evpn.get(i).doAdvertise();
        }
        origntedOuni = new tabRoute<addrIP>(newlyOuni);
        origntedOmlt = new tabRoute<addrIP>(newlyOmlt);
        origntedOflw = new tabRoute<addrIP>(newlyOflw);
        origntedOsrt = new tabRoute<addrIP>(newlyOsrt);
        origntedFlw = new tabRoute<addrIP>(newlyFlw);
        origntedVpnU = new tabRoute<addrIP>(newlyVpnU);
        origntedVpnM = new tabRoute<addrIP>(newlyVpnM);
        origntedVpnF = new tabRoute<addrIP>(newlyVpnF);
        origntedVpoU = new tabRoute<addrIP>(newlyVpoU);
        origntedVpoM = new tabRoute<addrIP>(newlyVpoM);
        origntedVpoF = new tabRoute<addrIP>(newlyVpoF);
        origntedVpls = new tabRoute<addrIP>(newlyVpls);
        origntedMspw = new tabRoute<addrIP>(newlyMspw);
        origntedEvpn = new tabRoute<addrIP>(newlyEvpn);
        origntedMdt = new tabRoute<addrIP>(newlyMdt);
        origntedNsh = new tabRoute<addrIP>(newlyNsh);
        origntedRpd = new tabRoute<addrIP>(newlyRpd);
        origntedSdw = new tabRoute<addrIP>(newlySdw);
        origntedSpf = new tabRoute<addrIP>(newlySpf);
        origntedRtf = new tabRoute<addrIP>(newlyRtf);
        origntedSrte = new tabRoute<addrIP>(newlySrte);
        origntedLnks = new tabRoute<addrIP>(newlyLnks);
        origntedMvpn = new tabRoute<addrIP>(newlyMvpn);
        origntedMvpo = new tabRoute<addrIP>(newlyMvpo);
        origntedMtre = new tabRoute<addrIP>(newlyMtre);
        origntedMtro = new tabRoute<addrIP>(newlyMtro);
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " rpki");
        }
        rpkiR = null;
        if (rpkiT != null) {
            cfgRtr rtrCfg = cfgAll.rtrFind(rpkiT, rpkiN, false);
            if (rtrCfg != null) {
                rpkiR = (rtrRpki) rtrCfg.getRouter();
            }
        }
        if (rpkiR != null) {
            rpkiA = rpkiR.getFinalTabRoa(fwdCore.ipVersion);
            rpkiO = rpkiR.getFinalTabRoa(other.fwd.ipVersion);
            rpkiP = rpkiR.getFinalTabAspa();
        } else {
            rpkiA = new tabGen<tabRpkiRoa>();
            rpkiO = new tabGen<tabRpkiRoa>();
            rpkiP = new tabGen<tabRpkiAspa>();
        }
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " neighbors");
        }
        groups = new ArrayList<rtrBgpGroup>();
        have2reflect = false;
        for (int i = 0; i < lstn.size(); i++) {
            rtrBgpNeigh nei = lstn.get(i);
            if (nei == null) {
                continue;
            }
            nei.setAccepted();
            nei.setGroup();
            nei.setMerge();
        }
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            nei.setAccepted();
            nei.setGroup();
            nei.setMerge();
        }
        if (have2reflect) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = new addrPrefix<addrIP>(new addrIP(), 0);
            ntry.best.rouSrc = rtrBgpUtil.peerOriginate;
            newlyRtf.add(tabRoute.addType.always, ntry, true, true);
            origntedRtf.add(tabRoute.addType.always, ntry, true, true);
        }
        if (conquer) {
            if (debugger.rtrBgpComp) {
                logger.debug("round " + compRound + " counquer");
            }
            computeConquerTable(routerComputedU, newlyUni);
            computeConquerTable(routerComputedM, newlyMlt);
            computeConquerTable(computedOuni, newlyOuni);
            computeConquerTable(computedOmlt, newlyOmlt);
            computeConquerTable(computedOflw, newlyOflw);
            computeConquerTable(computedOsrt, newlyOsrt);
            computeConquerTable(routerComputedF, newlyFlw);
            computeConquerTable(computedVpnU, newlyVpnU);
            computeConquerTable(computedVpnM, newlyVpnM);
            computeConquerTable(computedVpnF, newlyVpnF);
            computeConquerTable(computedVpoU, newlyVpoU);
            computeConquerTable(computedVpoM, newlyVpoM);
            computeConquerTable(computedVpoF, newlyVpoF);
            computeConquerTable(computedVpls, newlyVpls);
            computeConquerTable(computedMspw, newlyMspw);
            computeConquerTable(computedEvpn, newlyEvpn);
            computeConquerTable(computedMdt, newlyMdt);
            computeConquerTable(computedNsh, newlyNsh);
            computeConquerTable(computedRpd, newlyRpd);
            computeConquerTable(computedSdw, newlySdw);
            computeConquerTable(computedSpf, newlySpf);
            computeConquerTable(computedRtf, newlyRtf);
            computeConquerTable(computedSrte, newlySrte);
            computeConquerTable(computedLnks, newlyLnks);
            computeConquerTable(computedMvpn, newlyMvpn);
            computeConquerTable(computedMvpo, newlyMvpo);
            computeConquerTable(computedMtre, newlyMtre);
            computeConquerTable(computedMtro, newlyMtro);
        }
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " groups");
        }
        for (int i = 0; i < groups.size(); i++) {
            groups.get(i).createNeeded();
        }
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " neigroups");
        }
        lspf.doPeersFull();
        boolean diffs = newlyUni.differs(tabRoute.addType.alters, routerComputedU) || newlyMlt.differs(tabRoute.addType.alters, routerComputedM) || newlyFlw.differs(tabRoute.addType.alters, routerComputedF);
        routerComputedU = newlyUni;
        routerComputedM = newlyMlt;
        computedOuni = newlyOuni;
        computedOmlt = newlyOmlt;
        computedOflw = newlyOflw;
        computedOsrt = newlyOsrt;
        routerComputedF = newlyFlw;
        computedVpnU = newlyVpnU;
        computedVpnM = newlyVpnM;
        computedVpnF = newlyVpnF;
        computedVpoU = newlyVpoU;
        computedVpoM = newlyVpoM;
        computedVpoF = newlyVpoF;
        computedVpls = newlyVpls;
        computedMspw = newlyMspw;
        computedEvpn = newlyEvpn;
        computedMdt = newlyMdt;
        computedNsh = newlyNsh;
        computedRpd = newlyRpd;
        computedSdw = newlySdw;
        computedSpf = newlySpf;
        computedRtf = newlyRtf;
        computedSrte = newlySrte;
        computedLnks = newlyLnks;
        computedMvpn = newlyMvpn;
        computedMvpo = newlyMvpo;
        computedMtre = newlyMtre;
        computedMtro = newlyMtro;
        if (diffs) {
            fwdCore.routerChg(this, true);
        }
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
        if (!lspf.enabled && (segrouLab != null)) {
            if (debugger.rtrBgpComp) {
                logger.debug("round " + compRound + " segrou");
            }
            tabGen<tabIndex<addrIP>> segrouUsd = new tabGen<tabIndex<addrIP>>();
            for (int i = 0; i < newlyUni.size(); i++) {
                tabRouteEntry<addrIP> ntry = newlyUni.get(i);
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
                segrouLab[ntry.best.segrouIdx].setFwdMpls(tabLabelEntry.owner.bgpSrgb, fwdCore, nei.localIfc, nei.peerAddr, lab);
                tabIndex.add2table(segrouUsd, new tabIndex<addrIP>(ntry.best.segrouIdx, ntry.prefix));
            }
            tabIndex.add2table(segrouUsd, new tabIndex<addrIP>(segrouIdx, new addrPrefix<addrIP>(new addrIP(), 0)));
            segrouLab[segrouIdx].setFwdCommon(tabLabelEntry.owner.bgpSrgb, fwdCore);
            for (int i = 0; i < segrouLab.length; i++) {
                if (segrouUsd.find(new tabIndex<addrIP>(i, null)) != null) {
                    continue;
                }
                segrouLab[i].setFwdDrop(tabLabelEntry.owner.bgpSrgb);
            }
            routerComputedI = segrouUsd;
        }
        if (!lspf.enabled && (bierLab != null)) {
            if (debugger.rtrBgpComp) {
                logger.debug("round " + compRound + " bier");
            }
            tabLabelBier res = new tabLabelBier(bierLab[0].label, tabLabelBier.num2bsl(bierLen));
            res.idx = bierIdx;
            for (int i = 0; i < newlyUni.size(); i++) {
                tabRouteEntry<addrIP> ntry = newlyUni.get(i);
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
                tabLabelBierN per = new tabLabelBierN(fwdCore, nei.localIfc, nei.peerAddr, ntry.best.bierBeg, 0);
                tabLabelBierN old = res.peers.add(per);
                if (old != null) {
                    per = old;
                }
                per.setBit(ntry.best.bierIdx - 1);
            }
            for (int i = 0; i < bierLab.length; i++) {
                bierLab[i].setBierMpls(tabLabelEntry.owner.bgpBier, fwdCore, res);
            }
        }
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " export");
        }
        otherTrigger = addrFams[rtrBgpParam.idxLab];
        otherTrigger |= addrFams[rtrBgpParam.idxCtp];
        otherTrigger |= addrFams[rtrBgpParam.idxCar];
        otherTrigger |= linkStates.size() > 0;
        if (flowInst) {
            fwdCore.flowspec = tabQos.convertPolicy(rtrBgpFlow.doDecode(routerComputedF, afiUni == rtrBgpUtil.safiIp6uni));
        }
        other.doPeersFull();
        for (int i = 0; i < vrfs.size(); i++) {
            otherTrigger |= vrfs.get(i).doer.doPeersFull(rtrBgpUtil.sfiUnicast, newlyVpnU, newlyVpnM, newlyVpnF);
        }
        for (int i = 0; i < ovrfs.size(); i++) {
            otherTrigger |= ovrfs.get(i).doer.doPeersFull(rtrBgpUtil.sfiUnicast, newlyVpoU, newlyVpoM, newlyVpoF);
        }
        for (int i = 0; i < clrs.size(); i++) {
            otherTrigger |= clrs.get(i).doer.doPeersFull(rtrBgpUtil.sfiUnicast, newlyUni, newlyMlt, newlyFlw);
        }
        for (int i = 0; i < oclrs.size(); i++) {
            otherTrigger |= oclrs.get(i).doer.doPeersFull(rtrBgpUtil.sfiUnicast, newlyOuni, newlyOmlt, newlyOflw);
        }
        for (int i = 0; i < l3es.size(); i++) {
            otherTrigger |= l3es.get(i).doer.doPeersFull(rtrBgpUtil.sfiEthVpn, newlyEvpn, newlyVpnM, newlyVpnF);
        }
        for (int i = 0; i < ol3es.size(); i++) {
            otherTrigger |= ol3es.get(i).doer.doPeersFull(rtrBgpUtil.sfiEthVpn, newlyEvpn, newlyVpoM, newlyVpoF);
        }
        for (int i = 0; i < vpls.size(); i++) {
            vpls.get(i).doPeers();
        }
        for (int i = 0; i < evpn.size(); i++) {
            evpn.get(i).doPeers();
        }
        fullLast = bits.getTime();
        fullTime = (int) (fullLast - tim);
        fullCount++;
    }

    private tabRouteEntry<addrIP> computeIncrBest(int idx, long mask, int afi, rtrBgpNeigh nei, tabRouteEntry<addrIP> best, tabRouteEntry<addrIP> curr) {
        if (nei == null) {
            return best;
        }
        if (!nei.reachable) {
            return best;
        }
        tabRoute<addrIP> acc = nei.getAccepted(idx, mask, afi);
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
        if (best == null) {
            return ntry.copyBytes(tabRoute.addType.lnkEcmp);
        }
        if (best.best.isOtherBetter(ntry.best)) {
            return ntry.copyBytes(tabRoute.addType.lnkEcmp);
        }
        if (ntry.best.isOtherBetter(best.best)) {
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

    private void computeIncrEntry(int idx, long mask, int afi, tabRouteEntry<addrIP> curr, tabRoute<addrIP> cmp, tabRoute<addrIP> org) {
        if (debugger.rtrBgpIncr) {
            logger.debug("bestpath for " + tabRouteUtil.rd2string(curr.rouDst) + " " + curr.prefix + " in " + rtrBgpUtil.safi2string(afi));
        }
        tabRouteEntry<addrIP> best = org.find(curr);
        if (best != null) {
            best = best.copyBytes(tabRoute.addType.altEcmp);
            best.best.rouSrc = rtrBgpUtil.peerOriginate;
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            best = computeIncrBest(idx, mask, afi, lstnNei.get(i), best, curr);
        }
        for (int i = 0; i < neighs.size(); i++) {
            best = computeIncrBest(idx, mask, afi, neighs.get(i), best, curr);
        }
        if (best == null) {
            cmp.del(curr);
            computeIncrVersion(curr);
            for (int i = 0; i < groups.size(); i++) {
                rtrBgpGroup grp = groups.get(i);
                tabRoute<addrIP> wil = grp.getWilling(idx, mask, afi);
                tabRoute<addrIP> chg = grp.getChanged(idx, mask, afi);
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
            tabRoute<addrIP> wil = grp.getWilling(idx, mask, afi);
            tabRoute<addrIP> chg = grp.getChanged(idx, mask, afi);
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
            } else if ((afi == afiOuni) || (afi == afiOmlt)) {
                ntry = tabRoute.doUpdateEntry(afi, grp.remoteAs, ntry, grp.oroumapOut, grp.oroupolOut, grp.oprflstOut);
            } else if ((afi == afiOflw) || (afi == afiOsrt) || (afi == afiMvpo) || (afi == afiVpoU) || (afi == afiVpoM) || (afi == afiVpoF)) {
                ntry = tabRoute.doUpdateEntry(afi, grp.remoteAs, ntry, grp.wroumapOut, grp.wroupolOut, null);
            } else if ((afi == afiEvpn) || (afi == afiVpls)) {
                ntry = tabRoute.doUpdateEntry(afi, grp.remoteAs, ntry, grp.eroumapOut, grp.eroupolOut, null);
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
            if (ntry.differs(tabRoute.addType.alters, old) == 0) {
                continue;
            }
            wil.add(tabRoute.addType.always, ntry, false, false);
            chg.add(tabRoute.addType.always, ntry, false, false);
        }
    }

    private int computeIncrUpdate(int idx, int afi, long msk, tabRoute<addrIP> don, tabRoute<addrIP> chg, tabRoute<addrIP> cmp, tabRoute<addrIP> org) {
        int res = 0;
        if (don == null) {
            don = new tabRoute<addrIP>("chg");
        }
        for (int i = chg.size() - 1; i >= 0; i--) {
            tabRouteEntry<addrIP> ntry = chg.get(i);
            chg.del(ntry);
            don.add(tabRoute.addType.always, ntry, false, false);
            computeIncrEntry(idx, msk, afi, ntry, cmp, org);
            res++;
        }
        return res;
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
        if (lspf.enabled && (changedSpf.size() > 0)) {
            if (debugger.rtrBgpFull) {
                logger.debug("spf");
            }
            return true;
        }
        if (routerAutoSummary || (routerAggregating.size() > 0)) {
            if (debugger.rtrBgpFull) {
                logger.debug("aggregation");
            }
            oldAggr = true;
            return true;
        }
        if (oldAggr) {
            if (debugger.rtrBgpFull) {
                logger.debug("old aggregation");
            }
            oldAggr = false;
            return true;
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
        boolean labPer = routerAutoMesh != null;
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
            labPer |= nei.conn.peerAfis[rtrBgpParam.idxLab] | nei.conn.peerAfis[rtrBgpParam.idxCtp] | nei.conn.peerAfis[rtrBgpParam.idxCar];
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
            labPer |= nei.conn.peerAfis[rtrBgpParam.idxLab] | nei.conn.peerAfis[rtrBgpParam.idxCtp] | nei.conn.peerAfis[rtrBgpParam.idxCar];
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
            computeIncrPurge(grp.minversion, grp.chgOuni);
            computeIncrPurge(grp.minversion, grp.chgOmlt);
            computeIncrPurge(grp.minversion, grp.chgOflw);
            computeIncrPurge(grp.minversion, grp.chgOsrt);
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
            computeIncrPurge(grp.minversion, grp.chgRpd);
            computeIncrPurge(grp.minversion, grp.chgSdw);
            computeIncrPurge(grp.minversion, grp.chgSpf);
            computeIncrPurge(grp.minversion, grp.chgRtf);
            computeIncrPurge(grp.minversion, grp.chgSrte);
            computeIncrPurge(grp.minversion, grp.chgLnks);
            computeIncrPurge(grp.minversion, grp.chgMvpn);
            computeIncrPurge(grp.minversion, grp.chgMvpo);
            computeIncrPurge(grp.minversion, grp.chgMtre);
            computeIncrPurge(grp.minversion, grp.chgMtro);
        }
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " changes");
        }
        routerChangedU = new tabRoute<addrIP>("chg");
        routerChangedM = new tabRoute<addrIP>("chg");
        routerChangedF = new tabRoute<addrIP>("chg");
        other.routerChangedU = new tabRoute<addrIP>("chg");
        other.routerChangedM = new tabRoute<addrIP>("chg");
        other.routerChangedF = new tabRoute<addrIP>("chg");
        tabRoute<addrIP> chgVpnU = new tabRoute<addrIP>("chg");
        tabRoute<addrIP> chgVpnM = new tabRoute<addrIP>("chg");
        tabRoute<addrIP> chgVpnF = new tabRoute<addrIP>("chg");
        tabRoute<addrIP> chgVpoU = new tabRoute<addrIP>("chg");
        tabRoute<addrIP> chgVpoM = new tabRoute<addrIP>("chg");
        tabRoute<addrIP> chgVpoF = new tabRoute<addrIP>("chg");
        tabRoute<addrIP> chgEvpn = new tabRoute<addrIP>("chg");
        int cntGlb = computeIncrUpdate(rtrBgpParam.idxUni, afiUni, rtrBgpParam.mskUni, routerChangedU, changedUni, routerComputedU, routerRedistedU);
        cntGlb += computeIncrUpdate(rtrBgpParam.idxMlt, afiMlt, rtrBgpParam.mskMlt, routerChangedM, changedMlt, routerComputedM, routerRedistedM);
        computeIncrUpdate(rtrBgpParam.idxOuni, afiOuni, rtrBgpParam.mskOuni, other.routerChangedU, changedOuni, computedOuni, origntedOuni);
        computeIncrUpdate(rtrBgpParam.idxOmlt, afiOmlt, rtrBgpParam.mskOmlt, other.routerChangedM, changedOmlt, computedOmlt, origntedOmlt);
        computeIncrUpdate(rtrBgpParam.idxOflw, afiOflw, rtrBgpParam.mskOflw, other.routerChangedF, changedOflw, computedOflw, origntedOflw);
        computeIncrUpdate(rtrBgpParam.idxOsrt, afiOsrt, rtrBgpParam.mskOsrt, null, changedOsrt, computedOsrt, origntedOsrt);
        int cntFlw = computeIncrUpdate(rtrBgpParam.idxFlw, afiFlw, rtrBgpParam.mskFlw, routerChangedF, changedFlw, routerComputedF, origntedFlw);
        computeIncrUpdate(rtrBgpParam.idxVpnU, afiVpnU, rtrBgpParam.mskVpnU, chgVpnU, changedVpnU, computedVpnU, origntedVpnU);
        computeIncrUpdate(rtrBgpParam.idxVpnM, afiVpnM, rtrBgpParam.mskVpnM, chgVpnM, changedVpnM, computedVpnM, origntedVpnM);
        computeIncrUpdate(rtrBgpParam.idxVpnF, afiVpnF, rtrBgpParam.mskVpnF, chgVpnF, changedVpnF, computedVpnF, origntedVpnF);
        computeIncrUpdate(rtrBgpParam.idxVpoU, afiVpoU, rtrBgpParam.mskVpoU, chgVpoU, changedVpoU, computedVpoU, origntedVpoU);
        computeIncrUpdate(rtrBgpParam.idxVpoM, afiVpoM, rtrBgpParam.mskVpoM, chgVpoM, changedVpoM, computedVpoM, origntedVpoM);
        computeIncrUpdate(rtrBgpParam.idxVpoF, afiVpoF, rtrBgpParam.mskVpoF, chgVpoF, changedVpoF, computedVpoF, origntedVpoF);
        int cntVpls = computeIncrUpdate(rtrBgpParam.idxVpls, afiVpls, rtrBgpParam.mskVpls, null, changedVpls, computedVpls, origntedVpls);
        computeIncrUpdate(rtrBgpParam.idxMspw, afiMspw, rtrBgpParam.mskMspw, null, changedMspw, computedMspw, origntedMspw);
        int cntEvpn = computeIncrUpdate(rtrBgpParam.idxEvpn, afiEvpn, rtrBgpParam.mskEvpn, chgEvpn, changedEvpn, computedEvpn, origntedEvpn);
        computeIncrUpdate(rtrBgpParam.idxMdt, afiMdt, rtrBgpParam.mskMdt, null, changedMdt, computedMdt, origntedMdt);
        computeIncrUpdate(rtrBgpParam.idxNsh, afiNsh, rtrBgpParam.mskNsh, null, changedNsh, computedNsh, origntedNsh);
        computeIncrUpdate(rtrBgpParam.idxRpd, afiRpd, rtrBgpParam.mskRpd, null, changedRpd, computedRpd, origntedRpd);
        computeIncrUpdate(rtrBgpParam.idxSdw, afiSdw, rtrBgpParam.mskSdw, null, changedSdw, computedSdw, origntedSdw);
        computeIncrUpdate(rtrBgpParam.idxSpf, afiSpf, rtrBgpParam.mskSpf, null, changedSpf, computedSpf, origntedSpf);
        computeIncrUpdate(rtrBgpParam.idxRtf, afiRtf, rtrBgpParam.mskRtf, null, changedRtf, computedRtf, origntedRtf);
        computeIncrUpdate(rtrBgpParam.idxSrte, afiSrte, rtrBgpParam.mskSrte, null, changedSrte, computedSrte, origntedSrte);
        computeIncrUpdate(rtrBgpParam.idxLnks, afiLnks, rtrBgpParam.mskLnks, null, changedLnks, computedLnks, origntedLnks);
        computeIncrUpdate(rtrBgpParam.idxMvpn, afiMvpn, rtrBgpParam.mskMvpn, null, changedMvpn, computedMvpn, origntedMvpn);
        computeIncrUpdate(rtrBgpParam.idxMvpo, afiMvpo, rtrBgpParam.mskMvpo, null, changedMvpo, computedMvpo, origntedMvpo);
        computeIncrUpdate(rtrBgpParam.idxMtre, afiMtre, rtrBgpParam.mskMtre, null, changedMtre, computedMtre, origntedMtre);
        computeIncrUpdate(rtrBgpParam.idxMtro, afiMtro, rtrBgpParam.mskMtro, null, changedMtro, computedMtro, origntedMtro);
        lspf.doPeersIncr();
        if (labPer || ((cntGlb + cntFlw) > 0)) {
            fwdCore.routerChg(this, labPer);
        }
        if (debugger.rtrBgpComp) {
            logger.debug("round " + compRound + " export");
        }
        if (flowInst && (cntFlw > 0)) {
            fwdCore.flowspec = tabQos.convertPolicy(rtrBgpFlow.doDecode(routerComputedF, afiUni == rtrBgpUtil.safiIp6uni));
        }
        other.doPeersIncr();
        for (int i = 0; i < vrfs.size(); i++) {
            vrfs.get(i).doer.doPeersIncr(rtrBgpUtil.sfiUnicast, computedVpnU, computedVpnM, computedVpnF, chgVpnU, chgVpnM, chgVpnF);
        }
        for (int i = 0; i < ovrfs.size(); i++) {
            ovrfs.get(i).doer.doPeersIncr(rtrBgpUtil.sfiUnicast, computedVpoU, computedVpoM, computedVpoF, chgVpoU, chgVpoM, chgVpoF);
        }
        for (int i = 0; i < clrs.size(); i++) {
            clrs.get(i).doer.doPeersIncr(rtrBgpUtil.sfiUnicast, routerComputedU, routerComputedM, routerComputedF, routerChangedU, routerChangedM, routerChangedF);
        }
        for (int i = 0; i < oclrs.size(); i++) {
            oclrs.get(i).doer.doPeersIncr(rtrBgpUtil.sfiUnicast, computedOuni, computedOmlt, computedOflw, other.routerChangedU, other.routerChangedM, other.routerChangedF);
        }
        for (int i = 0; i < l3es.size(); i++) {
            l3es.get(i).doer.doPeersIncr(rtrBgpUtil.sfiEthVpn, computedEvpn, computedVpnM, computedVpnF, chgEvpn, chgVpnM, chgVpnF);
        }
        for (int i = 0; i < ol3es.size(); i++) {
            ol3es.get(i).doer.doPeersIncr(rtrBgpUtil.sfiEthVpn, computedEvpn, computedVpoM, computedVpoF, chgEvpn, chgVpoM, chgVpoF);
        }
        if (cntVpls > 0) {
            for (int i = 0; i < vpls.size(); i++) {
                vpls.get(i).doPeers();
            }
        }
        if (cntEvpn > 0) {
            for (int i = 0; i < evpn.size(); i++) {
                evpn.get(i).doPeers();
            }
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
        if (old.best.nextHop.compareTo(best.best.nextHop) != 0) {
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
     * update flap statistics
     *
     * @param idx afi
     * @param mask afi
     * @param afi afi
     * @param rd rd
     * @param prf prefix
     * @param pth path
     */
    protected void prefixFlapped(int idx, long mask, int afi, long rd, addrPrefix<addrIP> prf, List<Integer> pth) {
        if (pth == null) {
            pth = new ArrayList<Integer>();
        }
        rtrBgpFlapStat ntry = new rtrBgpFlapStat(idx, mask, afi, rd, prf);
        rtrBgpFlapStat old = flaps.add(ntry);
        if (old != null) {
            ntry = old;
        }
        ntry.count++;
        ntry.last = bits.getTime();
        rtrBgpFlapLst pe = new rtrBgpFlapLst(pth);
        rtrBgpFlapLst op = ntry.paths.add(pe);
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
        changedCur = changedUni.size() + changedMlt.size() + changedOuni.size()
                + changedOmlt.size() + changedOflw.size() + changedOsrt.size()
                + changedFlw.size() + changedVpnU.size() + changedVpnM.size()
                + changedVpnF.size() + changedVpoU.size() + changedVpoM.size()
                + changedVpoF.size() + changedVpls.size() + changedMspw.size()
                + changedEvpn.size() + changedMdt.size() + changedNsh.size()
                + changedRpd.size() + changedSdw.size() + changedSpf.size()
                + changedSrte.size() + changedLnks.size() + changedRtf.size()
                + changedMvpn.size() + changedMvpo.size() + changedMtre.size()
                + changedMtro.size();
        changedTot += changedCur;
        if (changedCur > changedMax) {
            changedMax = changedCur;
            changedPek = bits.getTime();
        }
        if (needFull.set(0) > 0) {
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
        for (int i = 0; i < mons.size(); i++) {
            rtrBgpMon ntry = mons.get(i);
            ntry.stopNow();
        }
        for (int i = 0; i < dmps.size(); i++) {
            rtrBgpMrt ntry = dmps.get(i);
            ntry.fileHandle.close();
        }
        for (int i = 0; i < lstnTmp.size(); i++) {
            rtrBgpLstn ntry = lstnTmp.get(i);
            tcpCore.listenStop(ntry.iface, port, null, 0);
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
        for (int i = 0; i < clrs.size(); i++) {
            clrs.get(i).doer.unregister2ip();
        }
        for (int i = 0; i < oclrs.size(); i++) {
            oclrs.get(i).doer.unregister2ip();
        }
        for (int i = 0; i < l3es.size(); i++) {
            l3es.get(i).doer.unregister2ip();
        }
        for (int i = 0; i < ol3es.size(); i++) {
            ol3es.get(i).doer.unregister2ip();
        }
        for (int i = 0; i < vpls.size(); i++) {
            vpls.get(i).doStop();
        }
        for (int i = 0; i < evpn.size(); i++) {
            evpn.get(i).doStop();
        }
        tabLabel.release(evpnUni, tabLabelEntry.owner.evpnPbb);
        tabLabel.release(evpnMul, tabLabelEntry.owner.evpnPbb);
        tabLabel.release(segrouLab, tabLabelEntry.owner.bgpSrgb);
        tabLabel.release(bierLab, tabLabelEntry.owner.bgpBier);
        fwdCore.routerDel(this);
    }

    /**
     * get help
     *
     * @param l list
     */
    public void routerGetHelp(userHelp l) {
        List<String> tmps = new ArrayList<String>();
        for (int i = 0; i < temps.size(); i++) {
            rtrBgpTemp ntry = temps.get(i);
            tmps.add(ntry.tempName);
        }
        List<String> neis = new ArrayList<String>();
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpNeigh ntry = neighs.get(i);
            neis.add("" + ntry.peerAddr);
        }
        l.add(null, false, 1, new int[]{2}, "address-family", "specify address families");
        rtrBgpParam.getAfiList(l, 2, new int[]{2, -1}, "to use", true);
        l.add(null, false, 1, new int[]{2}, "local-as", "specify local as number");
        l.add(null, false, 2, new int[]{-1}, "<num>", "autonomous system number");
        l.add(null, false, 1, new int[]{-1}, "default-originate", "advertise default route");
        l.add(null, false, 1, new int[]{-1}, "conquer", "conquer bestpath advertisements");
        l.add(null, false, 1, new int[]{-1}, "flapstat", "count flap statistics");
        l.add(null, false, 1, new int[]{-1}, "safe-ebgp", "enforce safe ebgp policy");
        l.add(null, false, 1, new int[]{-1}, "client-reflect", "perform client to client reflection");
        l.add(null, false, 1, new int[]{2}, "incremental", "limit on incremental bestpath calculation");
        l.add(null, false, 2, new int[]{-1}, "<num>", "maximum prefixes");
        l.add(null, false, 1, new int[]{2}, "router-id", "specify router id");
        l.add(null, false, 2, new int[]{-1}, "<addr>", "router id");
        l.add(null, false, 1, new int[]{2}, "scantime", "scan time interval");
        l.add(null, false, 2, new int[]{-1}, "<num>", "ms between scans");
        l.add(null, false, 1, new int[]{2}, "scandelay", "initial scan time delay");
        l.add(null, false, 2, new int[]{-1}, "<num>", "ms before scan");
        l.add(null, false, 1, new int[]{2}, "graceful-restart", "graceful restart interval");
        l.add(null, false, 2, new int[]{-1}, "<num>", "ms to recover");
        l.add(null, false, 1, new int[]{2}, "longlived-graceful", "long lived graceful restart interval");
        l.add(null, false, 2, new int[]{-1}, "<num>", "ms to recover");
        l.add(null, false, 1, new int[]{2}, "template", "specify template parameters");
        l.add(tmps, false, 2, new int[]{3}, "<name:loc>", "name of template");
        rtrBgpParam.getParamHelp(l);
        l.add(null, false, 1, new int[]{2}, "nexthop", "specify next hop tracking parameter");
        l.add(null, false, 2, new int[]{3}, "recursion", "specify recursion depth");
        l.add(null, false, 3, new int[]{-1}, "<num>", "maximum rounds");
        l.add(null, false, 2, new int[]{3}, "route-map", "filter next hops");
        l.add(null, false, 3, new int[]{-1}, "<name:rm>", "name of route map");
        l.add(null, false, 2, new int[]{3}, "route-policy", "filter next hops");
        l.add(null, false, 3, new int[]{-1}, "<name:rpl>", "name of route policy");
        l.add(null, false, 2, new int[]{3}, "prefix-list", "filter next hops");
        l.add(null, false, 3, new int[]{-1}, "<name:pl>", "name of prefix list");
        l.add(null, false, 1, new int[]{2}, "segrout", "segment routing parameters");
        l.add(null, false, 2, new int[]{3}, "<num>", "maximum index");
        l.add(null, false, 3, new int[]{4, -1}, "<num>", "this node index");
        l.add(null, false, 4, new int[]{5}, "base", "specify base");
        l.add(null, false, 5, new int[]{4, -1}, "<num>", "label base");
        l.add(null, false, 1, new int[]{2}, "bier", "bier parameters");
        l.add(null, false, 2, new int[]{3}, "<num>", "bitstring length");
        l.add(null, false, 3, new int[]{4}, "<num>", "maximum index");
        l.add(null, false, 4, new int[]{5, -1}, "<num>", "node index");
        l.add(null, false, 5, new int[]{-1}, "<num>", "subdomain");
        l.add(null, false, 1, new int[]{2}, "afi-links", "specify link state parameter");
        cfgRtr.getRouterList(l, 0, " to advertise");
        l.add(null, false, 3, new int[]{4}, "<num:rtr>", "process id");
        l.add(null, false, 4, new int[]{-1}, "<num>", "area/level number");
        l.add(null, false, 1, new int[]{-1}, "flowspec-install", "specify flowspec installation");
        l.add(null, false, 1, new int[]{2}, "flowspec-advert", "specify flowspec parameter");
        l.add(null, false, 2, new int[]{-1}, "<name:pm>", "name of policy map");
        l.add(null, false, 1, new int[]{2}, "neighbor", "specify neighbor parameters");
        l.add(neis, false, 2, new int[]{3}, "<addr:loc>", "address of peer");
        l.add(null, false, 3, new int[]{4}, "template", "get configuration from template");
        l.add(tmps, false, 4, new int[]{5, -1}, "<name:loc>", "name of source template");
        l.add(null, false, 5, new int[]{-1}, "shutdown", "connection disabled for this peer");
        rtrBgpParam.getParamHelp(l);
        l.add(null, false, 1, new int[]{2}, "distance", "specify default distance");
        l.add(null, false, 2, new int[]{3}, "<num>", "external peer distance");
        l.add(null, false, 3, new int[]{4}, "<num>", "internal peer distance");
        l.add(null, false, 4, new int[]{-1}, "<num>", "locally generated distance");
        l.add(null, false, 1, new int[]{2}, "listen", "passively listen for clients");
        l.add(null, false, 2, new int[]{3}, "<name:acl>", "access list name");
        l.add(tmps, false, 3, new int[]{-1}, "<name:loc>", "template name");
        l.add(null, false, 1, new int[]{2}, "dump", "setup bgp dump file");
        l.add(null, false, 2, new int[]{3}, "<str>", "name of mrt");
        l.add(null, false, 3, new int[]{4, -1}, "<file>", "name of file");
        l.add(null, false, 4, new int[]{5}, "<num>", "ms between backup");
        l.add(null, false, 5, new int[]{6, -1}, "<file>", "name of backup");
        l.add(null, false, 6, new int[]{-1}, "<num>", "maximum size of backup");
        l.add(null, false, 1, new int[]{2}, "monitor", "setup bgp monitor protocol server");
        l.add(null, false, 2, new int[]{3}, "<str>", "name of bmp");
        l.add(null, false, 3, new int[]{4}, "<name:prx>", "proxy profile");
        l.add(null, false, 4, new int[]{5}, "<str>", "hostname");
        l.add(null, false, 5, new int[]{-1}, "<num>", "port number");
        l.add(null, false, 1, new int[]{2}, "rpki", "setup resource public key infrastructure");
        cfgRtr.getRouterList(l, 0, "");
        l.add(null, false, 3, new int[]{-1}, "<num>", "process number");
        l.add(null, false, 1, new int[]{2}, "afi-spf", "select spf to advertise");
        l.add(null, false, 2, new int[]{-1}, "enable", "enable processing");
        l.add(null, false, 2, new int[]{-1}, "hostname", "advertise hostname");
        l.add(null, false, 2, new int[]{-1}, "stub", "stub router");
        l.add(null, false, 2, new int[]{3}, "distance", "set import distance");
        l.add(null, false, 3, new int[]{-1}, "<num>", "distance");
        l.add(null, false, 2, new int[]{-1}, "default-originate", "advertise default route");
        l.add(null, false, 2, new int[]{3}, "route-map", "process prefixes");
        l.add(null, false, 3, new int[]{-1}, "<name:rm>", "name of route map");
        l.add(null, false, 2, new int[]{3}, "route-policy", "process prefixes");
        l.add(null, false, 3, new int[]{-1}, "<name:rpl>", "name of route policy");
        l.add(null, false, 2, new int[]{3}, "prefix-list", "filter prefixes");
        l.add(null, false, 3, new int[]{-1}, "<name:pl>", "name of prefix list");
        l.add(null, false, 2, new int[]{-1}, "spf-bidir", "spf bidir check");
        l.add(null, false, 2, new int[]{3, -1}, "spf-topolog", "spf topology logging");
        l.add(null, false, 3, new int[]{3, -1}, "noappear", "exclude node (dis)appearance");
        l.add(null, false, 3, new int[]{3, -1}, "noconnect", "exclude link (dis)connection");
        l.add(null, false, 3, new int[]{3, -1}, "noforward", "exclude forward (un)willingness");
        l.add(null, false, 3, new int[]{3, -1}, "noreachable", "exclude node (un)reachable");
        l.add(null, false, 3, new int[]{3, -1}, "nometric", "exclude link metric change");
        l.add(null, false, 3, new int[]{3, -1}, "noprefix", "exclude prefix change");
        l.add(null, false, 2, new int[]{-1}, "spf-hops", "spf hops disallow");
        l.add(null, false, 2, new int[]{-1}, "spf-ecmp", "spf ecmp allow");
        l.add(null, false, 2, new int[]{3}, "spf-log", "spf log size");
        l.add(null, false, 3, new int[]{-1}, "<num>", "number of entries");
        l.add(null, false, 1, new int[]{2}, "afi-other", "select other to advertise");
        l.add(null, false, 2, new int[]{-1}, "vpn-mode", "enable vpn mode");
        rtrBgpVrfRtr.getHelp(l, 2);
        cfgRtr.getRedistHelp(l, 1);
        l.add(null, false, 1, new int[]{2}, "afi-vrf", "select vrf to advertise");
        l.add(null, false, 2, new int[]{3}, "<name:vrf>", "name of routing table");
        rtrBgpVrfRtr.getHelp(l, 3);
        cfgRtr.getRedistHelp(l, 2);
        l.add(null, false, 1, new int[]{2}, "afi-ovrf", "select other vrf to advertise");
        l.add(null, false, 2, new int[]{3}, "<name:vrf>", "name of routing table");
        rtrBgpVrfRtr.getHelp(l, 3);
        cfgRtr.getRedistHelp(l, 2);
        l.add(null, false, 1, new int[]{2}, "afi-clr", "select vrf to advertise");
        l.add(null, false, 2, new int[]{3}, "<name:vrf>", "name of routing table");
        rtrBgpVrfRtr.getHelp(l, 3);
        cfgRtr.getRedistHelp(l, 2);
        l.add(null, false, 1, new int[]{2}, "afi-oclr", "select other vrf to advertise");
        l.add(null, false, 2, new int[]{3}, "<name:vrf>", "name of routing table");
        rtrBgpVrfRtr.getHelp(l, 3);
        cfgRtr.getRedistHelp(l, 2);
        l.add(null, false, 1, new int[]{2}, "afi-l3e", "select vrf to advertise");
        l.add(null, false, 2, new int[]{3}, "<name:vrf>", "name of routing table");
        rtrBgpVrfRtr.getHelp(l, 3);
        cfgRtr.getRedistHelp(l, 2);
        l.add(null, false, 1, new int[]{2}, "afi-ol3e", "select other vrf to advertise");
        l.add(null, false, 2, new int[]{3}, "<name:vrf>", "name of routing table");
        rtrBgpVrfRtr.getHelp(l, 3);
        cfgRtr.getRedistHelp(l, 2);
        l.add(null, false, 1, new int[]{2}, "afi-vpls", "select vpls to advertise");
        l.add(null, false, 2, new int[]{3}, "<id>", "vpls id in ASnum:IDnum format");
        l.add(null, false, 3, new int[]{4}, "bridge-group", "enable processing");
        l.add(null, false, 4, new int[]{-1}, "<num>", "bridge group number");
        l.add(null, false, 3, new int[]{4}, "update-source", "select source to advertise");
        l.add(null, false, 4, new int[]{-1}, "<name:ifc>", "name of interface");
        l.add(null, false, 3, new int[]{-1}, "control-word", "specify control word");
        l.add(null, false, 3, new int[]{4}, "ve-id", "specify ve id");
        l.add(null, false, 4, new int[]{5}, "<num>", "ve id number");
        l.add(null, false, 5, new int[]{-1}, "<num>", "ve maximum number");
        l.add(null, false, 1, new int[]{2}, "afi-evpn", "select evpn to advertise");
        l.add(null, false, 2, new int[]{3}, "<id>", "evpn id");
        l.add(null, false, 3, new int[]{4}, "bridge-group", "enable processing");
        l.add(null, false, 4, new int[]{-1}, "<num>", "bridge group number");
        l.add(null, false, 3, new int[]{4}, "srv6", "srv6 advertisement");
        l.add(null, false, 4, new int[]{-1}, "<name:ifc>", "select source to advertise");
        l.add(null, false, 3, new int[]{4}, "bmac", "set backbone mac");
        l.add(null, false, 4, new int[]{-1}, "<addr>", "mac address");
        l.add(null, false, 3, new int[]{4}, "update-source", "select source to advertise");
        l.add(null, false, 4, new int[]{-1}, "<name:ifc>", "name of interface");
        l.add(null, false, 3, new int[]{4}, "encapsulation", "specify encapsulation to use");
        l.add(null, false, 4, new int[]{-1}, "pbb", "pbb");
        l.add(null, false, 4, new int[]{-1}, "vxlan", "vxlan");
        l.add(null, false, 4, new int[]{-1}, "vpws", "vpws");
        l.add(null, false, 4, new int[]{-1}, "cmac", "cmac");
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
        cmds.cfgLine(l, !clientReflect, beg, "client-reflect", "");
        l.add(beg + "address-family" + rtrBgpParam.bools2string(addrFams));
        l.add(beg + "distance " + distantExt + " " + distantInt + " " + distantLoc);
        l.add(beg + "scantime " + scanTime);
        l.add(beg + "scandelay " + scanDelay);
        l.add(beg + "incremental " + incrLimit);
        l.add(beg + "graceful-restart " + restartTime);
        l.add(beg + "longlived-graceful " + llRestartTime);
        cmds.cfgLine(l, !defRou, beg, "default-originate", "");
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
        cmds.cfgLine(l, bierMax < 1, beg, "bier", bierLen + " " + bierMax + " " + bierIdx + " " + bierSub);
        cmds.cfgLine(l, !flowInst, beg, "flowspec-install", "");
        cmds.cfgLine(l, flowSpec == null, beg, "flowspec-advert", "" + flowSpec);
        if (rpkiT == null) {
            l.add(beg + "no rpki");
        } else {
            l.add(beg + "rpki " + cfgRtr.num2name(rpkiT) + " " + rpkiN);
        }
        for (int i = 0; i < mons.size(); i++) {
            mons.get(i).getConfig(l, beg);
        }
        for (int i = 0; i < dmps.size(); i++) {
            dmps.get(i).getConfig(l, beg);
        }
        l.add(beg + cmds.comment);
        for (int i = 0; i < temps.size(); i++) {
            temps.get(i).getConfig(l, beg, filter);
        }
        for (int i = 0; i < lstnTmp.size(); i++) {
            lstnTmp.get(i).getConfig(l, beg);
        }
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            nei.getConfig(l, beg, filter);
        }
        other.getConfig(l, beg + "afi-other ");
        lspf.getConfig(l, beg, "afi-spf ");
        for (int i = 0; i < vrfs.size(); i++) {
            vrfs.get(i).doer.getConfig(l, beg, "afi-vrf ");
        }
        for (int i = 0; i < ovrfs.size(); i++) {
            ovrfs.get(i).doer.getConfig(l, beg, "afi-ovrf ");
        }
        for (int i = 0; i < clrs.size(); i++) {
            clrs.get(i).doer.getConfig(l, beg, "afi-clr ");
        }
        for (int i = 0; i < oclrs.size(); i++) {
            oclrs.get(i).doer.getConfig(l, beg, "afi-oclr ");
        }
        for (int i = 0; i < l3es.size(); i++) {
            l3es.get(i).doer.getConfig(l, beg, "afi-l3e ");
        }
        for (int i = 0; i < ol3es.size(); i++) {
            ol3es.get(i).doer.getConfig(l, beg, "afi-ol3e ");
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
        l.add(beg + cmds.comment);
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
        if (s.equals(cmds.negated)) {
            s = cmd.word();
            negated = true;
        }
        if (s.equals("local-as")) {
            localAs = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("router-id")) {
            s = cmd.word();
            routerID.fromString(s);
            cfgIfc ifc = cfgAll.ifcFind(s, 0);
            if (ifc != null) {
                if (ifc.addr4 != null) {
                    routerID.setAddr(ifc.addr4);
                }
            }
            if (negated) {
                routerID = new addrIPv4();
            }
            return false;
        }
        if (s.equals("safe-ebgp")) {
            safeEbgp = !negated;
            return false;
        }
        if (s.equals("client-reflect")) {
            clientReflect = !negated;
            needFull.add(1);
            compute.wakeup();
            return false;
        }
        if (s.equals("address-family")) {
            addrFams = rtrBgpParam.string2bools(cmd);
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
                flaps = new tabGen<rtrBgpFlapStat>();
            }
            return false;
        }
        if (s.equals("segrout")) {
            tabLabel.release(segrouLab, tabLabelEntry.owner.bgpSrgb);
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
            segrouLab = tabLabel.allocate(tabLabelEntry.owner.bgpSrgb, segrouBase, segrouMax);
            needFull.add(1);
            compute.wakeup();
            return false;
        }
        if (s.equals("bier")) {
            tabLabel.release(bierLab, tabLabelEntry.owner.bgpBier);
            bierLab = null;
            if (negated) {
                bierIdx = 0;
                bierSub = 0;
                bierMax = 0;
                bierLen = 0;
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            bierLen = tabLabelBier.normalizeBsl(bits.str2num(cmd.word()));
            bierMax = bits.str2num(cmd.word());
            bierIdx = bits.str2num(cmd.word());
            bierSub = bits.str2num(cmd.word());
            bierLab = tabLabel.allocate(tabLabelEntry.owner.bgpBier, (bierMax + bierLen - 1) / bierLen);
            needFull.add(1);
            compute.wakeup();
            return false;
        }
        if (s.equals("default-originate")) {
            defRou = !negated;
            needFull.add(1);
            compute.wakeup();
            return false;
        }
        if (s.equals("graceful-restart")) {
            restartTime = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("longlived-graceful")) {
            llRestartTime = bits.str2num(cmd.word());
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
        if (s.equals("afi-spf")) {
            s = cmd.word();
            if (s.equals("enable")) {
                lspf.enabled = !negated;
                routerIgp = lspf.enabled;
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("distance")) {
                lspf.distance = bits.str2num(cmd.word());
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("default-originate")) {
                lspf.defRou = !negated;
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("hostname")) {
                lspf.hostname = !negated;
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("stub")) {
                lspf.stub = !negated;
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("prefix-list")) {
                if (negated) {
                    lspf.prflstIn = null;
                    needFull.add(1);
                    compute.wakeup();
                    return false;
                }
                cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such prefix list");
                    return false;
                }
                lspf.prflstIn = ntry.prflst;
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("route-map")) {
                if (negated) {
                    lspf.roumapIn = null;
                    needFull.add(1);
                    compute.wakeup();
                    return false;
                }
                cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such route map");
                    return false;
                }
                lspf.roumapIn = ntry.roumap;
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("route-policy")) {
                if (negated) {
                    lspf.roupolIn = null;
                    needFull.add(1);
                    compute.wakeup();
                    return false;
                }
                cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
                if (ntry == null) {
                    cmd.error("no such route policy");
                    return false;
                }
                lspf.roupolIn = ntry.rouplc;
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("spf-log")) {
                lspf.lastSpf.logSize.set(bits.str2num(cmd.word()));
                if (negated) {
                    lspf.lastSpf.logSize.set(0);
                }
                return false;
            }
            if (s.equals("spf-topolog")) {
                if (negated) {
                    lspf.lastSpf.topoLog.set(0);
                    return false;
                }
                lspf.lastSpf.setTopoLogMode(cmd);
                return false;
            }
            if (s.equals("spf-bidir")) {
                if (negated) {
                    lspf.lastSpf.bidir.set(0);
                } else {
                    lspf.lastSpf.bidir.set(1);
                }
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("spf-hops")) {
                if (negated) {
                    lspf.lastSpf.hops.set(0);
                } else {
                    lspf.lastSpf.hops.set(1);
                }
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("spf-ecmp")) {
                if (negated) {
                    lspf.lastSpf.ecmp.set(0);
                } else {
                    lspf.lastSpf.ecmp.set(1);
                }
                needFull.add(1);
                compute.wakeup();
                return false;
            }
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
            if (s.equals("default-originate")) {
                other.defRou = !negated;
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
                    other.srv6 = cfgAll.ifcFind(cmd.word(), 0);
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
            if (cfgRtr.doCfgRedist(other, other.fwd, negated, s, cmd)) {
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
                    if (!negated) {
                        return false;
                    }
                    old.doer.unregister2ip();
                    vrfs.del(old);
                    needFull.add(1);
                    compute.wakeup();
                    return false;
                }
                if (negated) {
                    return false;
                }
                cur.doer.register2ip();
                vrfs.put(cur);
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            cur = vrfs.find(cur);
            if (cur == null) {
                cmd.error("vrf not enabled");
                return false;
            }
            cur.doer.doConfig(negated, cmd, s);
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
                    if (!negated) {
                        return false;
                    }
                    old.doer.unregister2ip();
                    ovrfs.del(old);
                    needFull.add(1);
                    compute.wakeup();
                    return false;
                }
                if (negated) {
                    return false;
                }
                cur.doer.register2ip();
                ovrfs.put(cur);
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            cur = ovrfs.find(cur);
            if (cur == null) {
                cmd.error("vrf not enabled");
                return false;
            }
            cur.doer.doConfig(negated, cmd, s);
            return false;
        }
        if (s.equals("afi-clr")) {
            cfgVrf cfv = cfgAll.vrfFind(cmd.word(), false);
            if (cfv == null) {
                cmd.error("no such vrf");
                return false;
            }
            rtrBgpVrf cur = new rtrBgpVrf(this, cfv, false);
            s = cmd.word();
            if (s.equals("enable")) {
                rtrBgpVrf old = clrs.find(cur);
                if (old != null) {
                    if (!negated) {
                        return false;
                    }
                    old.doer.unregister2ip();
                    clrs.del(old);
                    needFull.add(1);
                    compute.wakeup();
                    return false;
                }
                if (negated) {
                    return false;
                }
                cur.doer.register2ip();
                clrs.put(cur);
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            cur = clrs.find(cur);
            if (cur == null) {
                cmd.error("vrf not enabled");
                return false;
            }
            cur.doer.doConfig(negated, cmd, s);
            return false;
        }
        if (s.equals("afi-oclr")) {
            cfgVrf cfv = cfgAll.vrfFind(cmd.word(), false);
            if (cfv == null) {
                cmd.error("no such vrf");
                return false;
            }
            rtrBgpVrf cur = new rtrBgpVrf(this, cfv, true);
            s = cmd.word();
            if (s.equals("enable")) {
                rtrBgpVrf old = oclrs.find(cur);
                if (old != null) {
                    if (!negated) {
                        return false;
                    }
                    old.doer.unregister2ip();
                    oclrs.del(old);
                    needFull.add(1);
                    compute.wakeup();
                    return false;
                }
                if (negated) {
                    return false;
                }
                cur.doer.register2ip();
                oclrs.put(cur);
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            cur = oclrs.find(cur);
            if (cur == null) {
                cmd.error("vrf not enabled");
                return false;
            }
            cur.doer.doConfig(negated, cmd, s);
            return false;
        }
        if (s.equals("afi-l3e")) {
            cfgVrf cfv = cfgAll.vrfFind(cmd.word(), false);
            if (cfv == null) {
                cmd.error("no such vrf");
                return false;
            }
            rtrBgpVrf cur = new rtrBgpVrf(this, cfv, false);
            s = cmd.word();
            if (s.equals("enable")) {
                rtrBgpVrf old = l3es.find(cur);
                if (old != null) {
                    if (!negated) {
                        return false;
                    }
                    old.doer.unregister2ip();
                    l3es.del(old);
                    needFull.add(1);
                    compute.wakeup();
                    return false;
                }
                if (negated) {
                    return false;
                }
                cur.doer.register2ip();
                l3es.put(cur);
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            cur = l3es.find(cur);
            if (cur == null) {
                cmd.error("vrf not enabled");
                return false;
            }
            cur.doer.doConfig(negated, cmd, s);
            return false;
        }
        if (s.equals("afi-ol3e")) {
            cfgVrf cfv = cfgAll.vrfFind(cmd.word(), false);
            if (cfv == null) {
                cmd.error("no such vrf");
                return false;
            }
            rtrBgpVrf cur = new rtrBgpVrf(this, cfv, true);
            s = cmd.word();
            if (s.equals("enable")) {
                rtrBgpVrf old = ol3es.find(cur);
                if (old != null) {
                    if (!negated) {
                        return false;
                    }
                    old.doer.unregister2ip();
                    ol3es.del(old);
                    needFull.add(1);
                    compute.wakeup();
                    return false;
                }
                if (negated) {
                    return false;
                }
                cur.doer.register2ip();
                ol3es.put(cur);
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            cur = ol3es.find(cur);
            if (cur == null) {
                cmd.error("vrf not enabled");
                return false;
            }
            cur.doer.doConfig(negated, cmd, s);
            return false;
        }
        if (s.equals("afi-vpls")) {
            rtrBgpVpls cur = new rtrBgpVpls(this);
            cur.id = tabRouteUtil.string2rd(cmd.word());
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
            if (s.equals("control-word")) {
                cur.ctrlWrd = !negated;
                needFull.add(1);
                compute.wakeup();
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
                    needFull.add(1);
                    compute.wakeup();
                    return false;
                }
                cfgIfc res = cfgAll.ifcFind(cmd.word(), 0);
                if (res == null) {
                    cmd.error("no such interface");
                    return false;
                }
                if (res.vrfFor != vrfCore) {
                    cmd.error("in other vrf");
                    return false;
                }
                cur.iface = res;
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
                    cur.srv6 = cfgAll.ifcFind(cmd.word(), 0);
                }
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            if (s.equals("update-source")) {
                if (negated) {
                    cur.iface = null;
                    needFull.add(1);
                    compute.wakeup();
                    return false;
                }
                cfgIfc res = cfgAll.ifcFind(cmd.word(), 0);
                if (res == null) {
                    cmd.error("no such interface");
                    return false;
                }
                if (res.vrfFor != vrfCore) {
                    cmd.error("in other vrf");
                    return false;
                }
                cur.iface = res;
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
            if (negated) {
                rpkiT = null;
                rpkiN = 0;
                needFull.add(1);
                compute.wakeup();
                return false;
            }
            rpkiT = cfgRtr.name2num(cmd.word());
            rpkiN = bits.str2num(cmd.word());
            if (ipRtr.isRPKI(rpkiT) < 0) {
                cmd.error("not an rpki process");
                rpkiT = null;
                rpkiN = 0;
                return false;
            }
            needFull.add(1);
            compute.wakeup();
            return false;
        }
        if (s.equals("listen")) {
            rtrBgpLstn ntry = new rtrBgpLstn();
            cfgAceslst acl = cfgAll.aclsFind(cmd.word(), false);
            if (acl == null) {
                cmd.error("no such acl");
                return true;
            }
            ntry.acl = acl.aceslst;
            rtrBgpLstn old = lstnTmp.del(ntry);
            if (old != null) {
                tcpCore.listenStop(old.iface, port, null, 0);
            }
            if (negated) {
                return false;
            }
            ntry.temp = findTemp(cmd.word());
            if (ntry.temp == null) {
                cmd.error("no such template");
                return true;
            }
            ntry.iface = null;
            if (ntry.temp.srcIface != null) {
                if (afiUni == rtrBgpUtil.safiIp4uni) {
                    ntry.iface = ntry.temp.srcIface.fwdIf4;
                } else {
                    ntry.iface = ntry.temp.srcIface.fwdIf6;
                }
            }
            lstnTmp.put(ntry);
            tcpCore.streamListen(this, new pipeLine(ntry.temp.bufferSize, false), ntry.iface, port, null, 0, "bgp", ntry.temp.keyId, ntry.temp.passwd, ntry.temp.ttlSecurity, ntry.temp.tosValue);
            return false;
        }
        if (s.equals("template")) {
            s = cmd.word();
            rtrBgpTemp ntry = new rtrBgpTemp(this, s);
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
        s = cmd.word().trim();
        addrIP adr = cfgRtr.string2addr(rouTyp, s, null);
        if (adr == null) {
            cmd.error("bad address");
            return false;
        }
        rtrBgpNeigh ntry = new rtrBgpNeigh(this, adr);
        rtrBgpNeigh old = neighs.add(ntry);
        if (old == null) {
            if (!s.equals("" + ntry.peerAddr)) {
                ntry.description = s;
            } else {
                ntry.description = cfgRtr.addr2string(rouTyp, ntry.peerAddr, null);
            }
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
     * list neighbor summary
     *
     * @param idx safi to query
     * @param mask safi to query
     * @param safi safi to query
     * @return list of neighbors
     */
    public userFormat showNeighs(int idx, long mask, int safi) {
        userFormat l = new userFormat("|", "neighbor|as|learn|accept|will|done|uptime");
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpNeigh ntry = neighs.get(i);
            if (ntry == null) {
                continue;
            }
            l.add(ntry.showNeighs(idx, mask, safi));
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            rtrBgpNeigh ntry = lstnNei.get(i);
            if (ntry == null) {
                continue;
            }
            l.add(ntry.showNeighs(idx, mask, safi));
        }
        return l;
    }

    /**
     * list neighbor summary
     *
     * @param mod mode: 1=afi, 2=groups, 3=nexthops, 4=graceful, 5=addpath,
     * 6=routerid, 7=buffer, 8=description, 9=hostname, 10=compress, 11=connect,
     * 12=resolve, 13=summary, 14=multilab, 15=longlived, 16=software, 17=desum
     * 18=unknowns, 19=asnsum, 20=pfxsummary
     * @return list of neighbors
     */
    public userFormat showSummary(int mod) {
        userFormat l = null;
        switch (mod) {
            case 1:
                l = new userFormat("|", "neighbor|as|open|norem|noloc");
                break;
            case 2:
                l = new userFormat("|", "neighbor|as|group|mode|uptime");
                break;
            case 3:
                l = new userFormat("|", "neighbor|as|reach|chg|num|sess|uptime");
                break;
            case 4:
            case 15:
                l = new userFormat("|", "neighbor|as|rx|tx");
                break;
            case 5:
                l = new userFormat("|", "neighbor|as|rx|tx|rx|tx|rx|tx", "2|2open|2norem|2noloc");
                break;
            case 6:
                l = new userFormat("|", "neighbor|as|router|wideas|refresh|dyncap|extop|extup|type|role");
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
                l = new userFormat("|", "neighbor|as|rx|tx|rx|tx|rx|tx|rx|tx", "2|2update|2byte|2refresh|2capa");
                break;
            case 12:
                l = new userFormat("|", "neighbor|as|domain");
                break;
            case 13:
                l = new userFormat("|", "neighbor|as|state|learn|sent|uptime");
                break;
            case 14:
                l = new userFormat("|", "neighbor|as|rx|tx");
                break;
            case 16:
                l = new userFormat("|", "neighbor|as|software");
                break;
            case 17:
                l = new userFormat("|", "neighbor|as|state|learn|sent|uptim|descr");
                break;
            case 18:
                l = new userFormat("|", "neighbor|as|updates|bytes|ago");
                break;
            case 19:
                l = new userFormat("|", "neighbor|as|state|learn|sent|uptim|asname|asinfo");
                break;
            case 20:
                l = new userFormat("|", "neighbor|as|rx|tx|rx|tx|rx|tx", "2|2reach|2unrea|2ago");
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
        rtrBgpNeigh ntry = new rtrBgpNeigh(this, adr);
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
        rtrBgpTemp ntry = new rtrBgpTemp(this, nam);
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
        for (int i = 0; i < clrs.size(); i++) {
            clrs.get(i).doer.getPeerList(tab);
        }
        for (int i = 0; i < oclrs.size(); i++) {
            oclrs.get(i).doer.getPeerList(tab);
        }
        for (int i = 0; i < l3es.size(); i++) {
            l3es.get(i).doer.getPeerList(tab);
        }
        for (int i = 0; i < ol3es.size(); i++) {
            ol3es.get(i).doer.getPeerList(tab);
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
        lspf.lastSpf.listLinkStates(tab, spfLnkst.protoBgp, -1, asn, adv, addrIPv4.size, 4);
    }

    /**
     * get state information
     *
     * @param lst list to append
     */
    public void routerStateGet(List<String> lst) {
        String a = routerGetName() + " ";
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpNeigh nei = neighs.get(i);
            if (nei == null) {
                continue;
            }
            String b = nei.stateGet();
            if (b == null) {
                continue;
            }
            lst.add(a + b);
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            rtrBgpNeigh nei = lstnNei.get(i);
            if (nei == null) {
                continue;
            }
            String b = nei.stateGet();
            if (b == null) {
                continue;
            }
            lst.add(a + b);
        }
    }

    /**
     * set state information
     *
     * @param cmd string to append
     * @return true on error, false on success
     */
    public boolean routerStateSet(cmds cmd) {
        addrIP adr = new addrIP();
        if (adr.fromString(cmd.word())) {
            return true;
        }
        rtrBgpNeigh ntry = new rtrBgpNeigh(this, adr);
        rtrBgpTemp temp = findTemp(cmd.word());
        rtrBgpNeigh old = neighs.find(ntry);
        if (old != null) {
            ntry = old;
        } else {
            if (temp == null) {
                return true;
            }
            ntry.copyFrom(temp);
            ntry.template = temp;
            ntry.socketMode = 4;
        }
        ntry.updatePeer();
        if (ntry.stateSet(cmd)) {
            return true;
        }
        if (old == null) {
            lstnNei.add(ntry);
        }
        ntry.startNow();
        return false;
    }

    /**
     * get all routes
     *
     * @param idx safi to query
     * @param mask safi to query
     * @param safi safi to query
     * @param prf prefix to find
     * @return list of routes
     */
    public userFormat getAllRoutes(int idx, long mask, int safi, tabRouteEntry<addrIP> prf) {
        userFormat lst = new userFormat("|", "id|category|value");
        for (int i = 0; i < neighs.size(); i++) {
            getAllRoutes(lst, neighs.get(i), idx, mask, safi, prf);
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            getAllRoutes(lst, lstnNei.get(i), idx, mask, safi, prf);
        }
        return lst;
    }

    private void getAllRoutes(userFormat lst, rtrBgpNeigh nei, int idx, long mask, int safi, tabRouteEntry<addrIP> prf) {
        if (nei == null) {
            return;
        }
        tabRoute<addrIP> tab = nei.conn.getLearned(idx, mask, safi);
        if (tab == null) {
            return;
        }
        tabRouteEntry<addrIP> res = tab.find(prf);
        if (res == null) {
            return;
        }
        lst.add("|peer|" + nei.peerAddr);
        lst.add(res.fullDump("" + nei.peerAddr, fwdCore));
    }

    /**
     * get flap stats
     *
     * @param mask afi
     * @param afi afi
     * @param num minimum flap count
     * @return list of statistics
     */
    public userFormat getFlapstat(long mask, int afi, int num) {
        userFormat l = new userFormat("|", "prefix|count|paths|ago|last");
        if (flaps == null) {
            return l;
        }
        for (int i = 0; i < flaps.size(); i++) {
            rtrBgpFlapStat ntry = flaps.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.mask != mask) {
                continue;
            }
            if (ntry.count < num) {
                continue;
            }
            l.add(ntry.toFlaps());
        }
        return l;
    }

    /**
     * get flap paths
     *
     * @param idx afi
     * @param mask afi
     * @param afi afi
     * @param rd rd
     * @param prf prefix
     * @param rev reverse path
     * @return list of paths
     */
    public userFormat getFlappath(int idx, long mask, int afi, long rd, addrPrefix<addrIP> prf, boolean rev) {
        if (flaps == null) {
            return null;
        }
        rtrBgpFlapStat ntry = new rtrBgpFlapStat(idx, mask, afi, rd, prf);
        ntry = flaps.find(ntry);
        if (ntry == null) {
            return null;
        }
        userFormat l = new userFormat("|", "count|ago|last|path");
        for (int i = 0; i < ntry.paths.size(); i++) {
            l.add("" + ntry.paths.get(i).dumpFlap(rev));
        }
        return l;
    }

    /**
     * uplinking as
     *
     * @param idx safi to query
     * @param mask safi to query
     * @param safi safi to query
     * @return text
     */
    public userFormat getAsUplink(int idx, long mask, int safi) {
        tabGen<rtrBgpFlapAsn> lst = new tabGen<rtrBgpFlapAsn>();
        tabRoute<addrIP> rou = getDatabase(idx, mask, safi);
        for (int i = 0; i < rou.size(); i++) {
            tabRouteEntry<addrIP> ntry = rou.get(i);
            if (ntry == null) {
                continue;
            }
            int o = ntry.best.asPathBeg();
            if (o == -1) {
                rtrBgpDump.updateAsOrigin(lst, localAs);
            } else {
                rtrBgpDump.updateAsOrigin(lst, o);
            }
        }
        userFormat res = new userFormat("|", "asnum|asnam|nets|asinfo");
        for (int i = 0; i < lst.size(); i++) {
            rtrBgpFlapAsn ntry = lst.get(i);
            res.add("" + ntry);
        }
        return res;
    }

    /**
     * originating as
     *
     * @param idx safi to query
     * @param mask safi to query
     * @param safi safi to query
     * @return text
     */
    public userFormat getAsOrigin(int idx, long mask, int safi) {
        tabGen<rtrBgpFlapAsn> lst = new tabGen<rtrBgpFlapAsn>();
        tabRoute<addrIP> rou = getDatabase(idx, mask, safi);
        for (int i = 0; i < rou.size(); i++) {
            tabRouteEntry<addrIP> ntry = rou.get(i);
            if (ntry == null) {
                continue;
            }
            int o = ntry.best.asPathEnd();
            if (o == -1) {
                rtrBgpDump.updateAsOrigin(lst, localAs);
            } else {
                rtrBgpDump.updateAsOrigin(lst, o);
            }
        }
        userFormat res = new userFormat("|", "asnum|asnam|nets|asinfo");
        for (int i = 0; i < lst.size(); i++) {
            rtrBgpFlapAsn ntry = lst.get(i);
            res.add("" + ntry);
        }
        return res;
    }

    /**
     * transiting as
     *
     * @param idx safi to query
     * @param mask safi to query
     * @param safi safi to query
     * @return text
     */
    public userFormat getAsTransit(int idx, long mask, int safi) {
        tabGen<rtrBgpFlapAsn> lst = new tabGen<rtrBgpFlapAsn>();
        tabRoute<addrIP> rou = getDatabase(idx, mask, safi);
        for (int i = 0; i < rou.size(); i++) {
            tabRouteEntry<addrIP> ntry = rou.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.best.pathSeq == null) {
                continue;
            }
            List<Integer> res = ntry.best.asPathInts(localAs);
            rtrBgpDump.updateAsOrigin(lst, localAs);
            int p = res.size() - 1;
            for (int o = 0; o < p; o++) {
                rtrBgpDump.updateAsOrigin(lst, res.get(o));
            }
        }
        userFormat res = new userFormat("|", "asnum|asnam|nets|asinfo");
        for (int i = 0; i < lst.size(); i++) {
            rtrBgpFlapAsn ntry = lst.get(i);
            res.add("" + ntry);
        }
        return res;
    }

    /**
     * as path graph
     *
     * @param idx safi to refresh
     * @param mask safi to query
     * @param safi safi to query
     * @return text
     */
    public List<String> getAsGraph(int idx, long mask, int safi) {
        tabGen<rtrBgpFlapAsn> lst = new tabGen<rtrBgpFlapAsn>();
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpDump.updateAsGraph(localAs, lst, neighs.get(i), idx, mask, safi);
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            rtrBgpDump.updateAsGraph(localAs, lst, lstnNei.get(i), idx, mask, safi);
        }
        int o = 0;
        for (int i = 0; i < lst.size(); i++) {
            rtrBgpFlapAsn ntry = lst.get(i);
            if (o < ntry.count) {
                o = ntry.count;
            }
        }
        o += 2;
        List<String> res = new ArrayList<String>();
        res.add(spfCalc.graphBeg1);
        res.add(spfCalc.graphBeg2);
        res.add(spfCalc.graphBeg3);
        for (int i = 0; i < lst.size(); i++) {
            rtrBgpFlapAsn ntry = lst.get(i);
            res.add("\"" + clntWhois.asn2mixed(ntry.prev, true) + "\" -- \"" + clntWhois.asn2mixed(ntry.asn, true) + "\" [weight=" + (o - ntry.count) + "]");
        }
        res.add(spfCalc.graphEnd1);
        res.add(spfCalc.graphEnd2);
        return res;
    }

    /**
     * as path tree
     *
     * @param idx safi to refresh
     * @param mask safi to query
     * @param safi safi to query
     * @param asn asn to start from
     * @return text
     */
    public List<String> getAsTree(int idx, long mask, int safi, int asn) {
        tabGen<rtrBgpFlapAsn> lst = new tabGen<rtrBgpFlapAsn>();
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpDump.updateAsGraph(localAs, lst, neighs.get(i), idx, mask, safi);
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            rtrBgpDump.updateAsGraph(localAs, lst, lstnNei.get(i), idx, mask, safi);
        }
        if (asn == 0) {
            asn = localAs;
        }
        List<String> res = new ArrayList<String>();
        rtrBgpDump.drawAsTree(res, lst, asn, "");
        return res;
    }

    /**
     * as path statistics
     *
     * @param idx safi to refresh
     * @param mask safi to query
     * @param safi safi to query
     * @param asn asn to query
     * @return text
     */
    public userFormat getPathAround(int idx, long mask, int safi, int asn) {
        tabGen<rtrBgpFlapAsn> res = new tabGen<rtrBgpFlapAsn>();
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpDump.updatePathAround(localAs, asn, res, neighs.get(i), idx, mask, safi);
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            rtrBgpDump.updatePathAround(localAs, asn, res, lstnNei.get(i), idx, mask, safi);
        }
        userFormat lst = new userFormat("|", "asnum|asnam|nets|asinfo");
        for (int i = 0; i < res.size(); i++) {
            lst.add("" + res.get(i));
        }
        return lst;
    }

    /**
     * as path statistics
     *
     * @param idx safi to refresh
     * @param mask safi to query
     * @param safi safi to query
     * @param asn asn to query
     * @return text
     */
    public userFormat getPathContain(int idx, long mask, int safi, int asn) {
        tabGen<rtrBgpFlapLst> res = new tabGen<rtrBgpFlapLst>();
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpDump.updatePathContain(asn, res, neighs.get(i), idx, mask, safi);
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            rtrBgpDump.updatePathContain(asn, res, lstnNei.get(i), idx, mask, safi);
        }
        userFormat lst = new userFormat("|", "count|path");
        for (int i = 0; i < res.size(); i++) {
            lst.add(res.get(i).dumpContain());
        }
        return lst;
    }

    /**
     * as path statistics
     *
     * @param idx safi to query
     * @param mask safi to query
     * @param safi safi to query
     * @return text
     */
    public userFormat getPathStat(int idx, long mask, int safi) {
        tabRoute<addrIP> rou = getDatabase(idx, mask, safi);
        List<Integer> res = new ArrayList<Integer>();
        for (int i = 0; i < rou.size(); i++) {
            tabRouteEntry<addrIP> ntry = rou.get(i);
            if (ntry == null) {
                continue;
            }
            int o = ntry.best.asPathLen();
            o++;
            for (int p = res.size(); p < o; p++) {
                res.add(0);
            }
            o--;
            res.set(o, 1 + res.get(o));
        }
        userFormat lst = new userFormat("|", "len|count");
        for (int i = 0; i < res.size(); i++) {
            lst.add(i + "|" + res.get(i));
        }
        return lst;
    }

    /**
     * as connections
     *
     * @param idx safi to refresh
     * @param mask safi to query
     * @param safi safi to query
     * @return text
     */
    public userFormat getAsConns(int idx, long mask, int safi) {
        tabGen<rtrBgpFlapAsn> lst = new tabGen<rtrBgpFlapAsn>();
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpDump.updateAsGraph(localAs, lst, neighs.get(i), idx, mask, safi);
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            rtrBgpDump.updateAsGraph(localAs, lst, lstnNei.get(i), idx, mask, safi);
        }
        userFormat res = new userFormat("|", "asnum|asnam|conn|net|peers");
        int conns = -1;
        int prefs = -1;
        String peers = "";
        int curr = 0;
        for (int i = 0; i < lst.size(); i++) {
            rtrBgpFlapAsn ntry = lst.get(i);
            if (curr == ntry.prev) {
                peers += " " + clntWhois.asn2mixed(ntry.asn, true);
                conns++;
                prefs += ntry.count;
                continue;
            }
            if (conns > 0) {
                res.add(bits.num2str(curr) + "|" + clntWhois.asn2name(curr, true) + "|" + conns + "|" + prefs + "|" + peers);
            }
            curr = ntry.prev;
            peers = clntWhois.asn2mixed(ntry.asn, true);
            conns = 1;
            prefs = ntry.count;
        }
        if (conns > 0) {
            res.add(curr + "|" + clntWhois.asn2name(curr, true) + "|" + conns + "|" + prefs + "|" + peers);
        }
        return res;
    }

    /**
     * usage of next hops
     *
     * @param idx safi to refresh
     * @param mask safi to query
     * @param safi safi to query
     * @return text
     */
    public userFormat getNhPrfxes(int idx, long mask, int safi) {
        tabGen<rtrBgpFlapStat> lst = new tabGen<rtrBgpFlapStat>();
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpDump.updateNhPrfxes(lst, neighs.get(i), idx, mask, safi);
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            rtrBgpDump.updateNhPrfxes(lst, lstnNei.get(i), idx, mask, safi);
        }
        userFormat res = new userFormat("|", "nexthop|prefixes");
        for (int i = 0; i < lst.size(); i++) {
            rtrBgpFlapStat ntry = lst.get(i);
            res.add(ntry.toNhPrfxes());
        }
        return res;
    }

    /**
     * usage of next hops
     *
     * @param idx safi to refresh
     * @param mask safi to query
     * @param safi safi to query
     * @return text
     */
    public userFormat getNhTrnsit(int idx, long mask, int safi) {
        tabGen<rtrBgpFlapStat> lst = new tabGen<rtrBgpFlapStat>();
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpDump.updateNhTrnsit(lst, neighs.get(i), idx, mask, safi);
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            rtrBgpDump.updateNhTrnsit(lst, lstnNei.get(i), idx, mask, safi);
        }
        userFormat res = new userFormat("|", "nexthop|count|transits");
        for (int i = 0; i < lst.size(); i++) {
            rtrBgpFlapStat ntry = lst.get(i);
            res.add(ntry.toNhTrnsit());
        }
        return res;
    }

    /**
     * usage of next hops
     *
     * @param idx safi to refresh
     * @param mask safi to query
     * @param safi safi to query
     * @return text
     */
    public userFormat getNhOrigin(int idx, long mask, int safi) {
        tabGen<rtrBgpFlapStat> lst = new tabGen<rtrBgpFlapStat>();
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpDump.updateNhOrigin(lst, neighs.get(i), idx, mask, safi);
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            rtrBgpDump.updateNhOrigin(lst, lstnNei.get(i), idx, mask, safi);
        }
        userFormat res = new userFormat("|", "nexthop|count|origins");
        for (int i = 0; i < lst.size(); i++) {
            rtrBgpFlapStat ntry = lst.get(i);
            res.add(ntry.toNhTrnsit());
        }
        return res;
    }

    /**
     * inconsistent next hops
     *
     * @param idx safi to refresh
     * @param mask safi to query
     * @param safi safi to query
     * @param mtch matcher
     * @return text
     */
    public userFormat getNhIncons(int idx, long mask, int safi, tabIntMatcher mtch) {
        tabGen<rtrBgpFlapStat> lst = new tabGen<rtrBgpFlapStat>();
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpDump.updateNhIncons(lst, neighs.get(i), idx, mask, safi);
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            rtrBgpDump.updateNhIncons(lst, lstnNei.get(i), idx, mask, safi);
        }
        userFormat res = new userFormat("|", "path|nexthops");
        for (int i = 0; i < lst.size(); i++) {
            rtrBgpFlapStat ntry = lst.get(i);
            if (!mtch.matches(ntry.infos.size())) {
                continue;
            }
            res.add(ntry.toInconsStr());
        }
        return res;
    }

    /**
     * inconsistent as paths
     *
     * @param idx safi to refresh
     * @param mask safi to query
     * @param safi safi to query
     * @param mtch matcher
     * @return text
     */
    public userFormat getAsIncons(int idx, long mask, int safi, tabIntMatcher mtch) {
        tabGen<rtrBgpFlapStat> lst = new tabGen<rtrBgpFlapStat>();
        for (int i = 0; i < neighs.size(); i++) {
            rtrBgpDump.updateAsIncons(lst, neighs.get(i), idx, mask, safi);
        }
        for (int i = 0; i < lstnNei.size(); i++) {
            rtrBgpDump.updateAsIncons(lst, lstnNei.get(i), idx, mask, safi);
        }
        userFormat res = new userFormat("|", "path|ases");
        for (int i = 0; i < lst.size(); i++) {
            rtrBgpFlapStat ntry = lst.get(i);
            if (!mtch.matches(ntry.paths.size())) {
                continue;
            }
            res.add(ntry.toInconsPth());
        }
        return res;
    }

    /**
     * get message statistics
     *
     * @return list of statistics
     */
    public userFormat getMsgStats() {
        return rtrBgpDump.getMsgStats(msgStats);
    }

    /**
     * get message statistics
     *
     * @return list of statistics
     */
    public userFormat getAttrStats() {
        return rtrBgpDump.getAttrStats(attrStats);
    }

    /**
     * show spf
     *
     * @return log of spf
     */
    public userFormat getSpfStat() {
        return lspf.lastSpf.listStatistics();
    }

    /**
     * show topology
     *
     * @param cmd entry to find
     * @return log of spf
     */
    public userFormat getSpfTopo(cmds cmd) {
        return lspf.lastSpf.listTopology(new addrIPv4(), cmd);
    }

    /**
     * show log
     *
     * @return log of spf
     */
    public userFormat getSpfLog() {
        return lspf.lastSpf.listUsages();
    }

    /**
     * show tree
     *
     * @param cmd entry to find
     * @return tree of spf
     */
    public List<String> getSpfTree(cmds cmd) {
        return lspf.lastSpf.listTree(cmd);
    }

    /**
     * show tree
     *
     * @param cmd entry to find
     * @return tree of spf
     */
    public List<String> getSpfOtherTree(cmds cmd) {
        spfCalc<addrIPv4> spf = lspf.lastSpf.copyBytes();
        addrIPv4 ned = new addrIPv4();
        ned.fromString(cmd.word());
        spf.doWork(ned);
        return spf.listTree(cmd);
    }

    /**
     * show topology
     *
     * @param cmd entry to find
     * @return log of spf
     */
    public userFormat getSpfOtherTopo(cmds cmd) {
        spfCalc<addrIPv4> spf = lspf.lastSpf.copyBytes();
        addrIPv4 ned = new addrIPv4();
        ned.fromString(cmd.word());
        spf.doWork(ned);
        return spf.listTopology(new addrIPv4(), cmd);
    }

    /**
     * show graph
     *
     * @param cmd masks
     * @return graph of spf
     */
    public List<String> getSpfGraph(cmds cmd) {
        return lspf.lastSpf.listGraphviz(cmd);
    }

    /**
     * show nh inconsistency
     *
     * @param mtch matcher
     * @return inconsistency list
     */
    public userFormat getNhIncons(tabIntMatcher mtch) {
        return lspf.lastSpf.listNhIncons(mtch);
    }

    /**
     * show met inconsistency
     *
     * @param mtch matcher
     * @return inconsistency list
     */
    public userFormat getMetIncons(tabIntMatcher mtch) {
        return lspf.lastSpf.listMetIncons(mtch);
    }

    /**
     * show non redundant nodes
     *
     * @return necessity list
     */
    public userFormat showNonRedundant() {
        return lspf.lastSpf.listNonRedundant();
    }

    /**
     * show route
     *
     * @return routes of spf
     */
    public tabRoute<addrIP> getSpfRoute() {
        return lspf.routes;
    }

    /**
     * get bestpath stats
     *
     * @return list of statistics
     */
    public userFormat getBestpath() {
        userFormat l = new userFormat("|", "category|value|addition");
        l.add("self|" + this);
        l.add("other|" + other);
        l.add("asn|" + clntWhois.asn2mixed(localAs, true));
        l.add("routerid|" + routerID);
        l.add("version|" + compRound);
        l.add("full run|" + fullCount + "|times");
        l.add("full last|" + bits.timePast(fullLast) + "|" + bits.time2str(cfgAll.timeZoneName, fullLast + cfgAll.timeServerOffset, 3));
        l.add("full time|" + fullTime + "|ms");
        l.add("incr run|" + incrCount + "|times");
        l.add("incr last|" + bits.timePast(incrLast) + "|" + bits.time2str(cfgAll.timeZoneName, incrLast + cfgAll.timeServerOffset, 3));
        l.add("incr time|" + incrTime + "|ms");
        rtrBgpDump.getMsgStats(l, rtrBgpUtil.msgOpen, msgStats, "|", "|");
        rtrBgpDump.getMsgStats(l, rtrBgpUtil.msgUpdate, msgStats, "|", "|");
        rtrBgpDump.getMsgStats(l, rtrBgpUtil.msgRefrsh, msgStats, "|", "|");
        rtrBgpDump.getMsgStats(l, rtrBgpUtil.msgCapability, msgStats, "|", "|");
        rtrBgpDump.getMsgStats(l, rtrBgpUtil.msgNotify, msgStats, "|", "|");
        rtrBgpDump.getUnReachStats(l, reachabStat, unreachStat, "|", "|");
        rtrBgpDump.getUnknwSum(l, false, msgStats, "|", "|");
        rtrBgpDump.getUnknwSum(l, true, attrStats, "|", "|");
        l.add("listen accepts|" + accptStat.packTx + "|" + accptStat.packTx + " " + accptStat.packDr);
        l.add("changes all|" + changedTot);
        l.add("changes now|" + changedCur);
        l.add("changes max|" + changedMax);
        l.add("changes peak|" + bits.timePast(changedPek) + "|" + bits.time2str(cfgAll.timeZoneName, changedPek + cfgAll.timeServerOffset, 3));
        l.add("static peers|" + rtrBgpUtil.tabSiz2str(neighs));
        l.add("dynamic peers|" + rtrBgpUtil.tabSiz2str(lstnNei));
        l.add("dynamic templates|" + rtrBgpUtil.tabSiz2str(lstnTmp));
        l.add("templates|" + rtrBgpUtil.tabSiz2str(temps));
        l.add("linkstates|" + rtrBgpUtil.tabSiz2str(linkStates));
        l.add("flapstats|" + rtrBgpUtil.tabSiz2str(flaps));
        l.add("monitors|" + rtrBgpUtil.tabSiz2str(mons));
        l.add("dumps|" + rtrBgpUtil.tabSiz2str(dmps));
        l.add("vrfs|" + rtrBgpUtil.tabSiz2str(vrfs));
        l.add("other vrfs|" + rtrBgpUtil.tabSiz2str(ovrfs));
        l.add("colors|" + rtrBgpUtil.tabSiz2str(clrs));
        l.add("other colors|" + rtrBgpUtil.tabSiz2str(oclrs));
        l.add("l3evpn|" + rtrBgpUtil.tabSiz2str(l3es));
        l.add("other l3evpn|" + rtrBgpUtil.tabSiz2str(ol3es));
        l.add("vplses|" + rtrBgpUtil.tabSiz2str(vpls));
        l.add("evpns|" + rtrBgpUtil.tabSiz2str(evpn));
        l.add("groups|" + groups.size() + "|" + groupMin + ".." + groupMax);
        l.add("roa table|" + rpkiA.size() + "|" + rpkiO.size());
        l.add("aspa table|" + rpkiP.size());
        l.add("unicast table|" + routerComputedU.size() + "|" + changedUni.size());
        l.add("multicast table|" + routerComputedM.size() + "|" + changedMlt.size());
        l.add("ouni table|" + computedOuni.size() + "|" + changedOuni.size());
        l.add("omlt table|" + computedOmlt.size() + "|" + changedOmlt.size());
        l.add("oflw table|" + computedOflw.size() + "|" + changedOflw.size());
        l.add("osrt table|" + computedOsrt.size() + "|" + changedOsrt.size());
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
        l.add("rpd table|" + computedRpd.size() + "|" + changedRpd.size());
        l.add("sdwan table|" + computedSdw.size() + "|" + changedSdw.size());
        l.add("spf table|" + computedSpf.size() + "|" + changedSpf.size());
        l.add("rtfilter table|" + computedRtf.size() + "|" + changedRtf.size());
        l.add("srte table|" + computedSrte.size() + "|" + changedSrte.size());
        l.add("linkstate table|" + computedLnks.size() + "|" + changedLnks.size());
        l.add("mvpn table|" + computedMvpn.size() + "|" + changedMvpn.size());
        l.add("omvpn table|" + computedMvpo.size() + "|" + changedMvpo.size());
        l.add("mtree table|" + computedMtre.size() + "|" + changedMtre.size());
        l.add("omtree table|" + computedMtro.size() + "|" + changedMtro.size());
        return l;
    }

}
