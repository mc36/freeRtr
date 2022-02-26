package net.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.auth.authLocal;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgPrfxlst;
import net.freertr.cfg.cfgRoump;
import net.freertr.cfg.cfgRouplc;
import net.freertr.cfg.cfgRtr;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabPrfxlstN;
import net.freertr.tab.tabRtrmapN;
import net.freertr.tab.tabRtrplcN;
import net.freertr.user.userFilter;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * bgp4 parameters
 *
 * @author matecsaba
 */
public abstract class rtrBgpParam {

    /**
     * lower
     */
    public final rtrBgp lower;

    /**
     * true if template, false if neighbor
     */
    public final boolean isTemplate;

    /**
     * as of peer
     */
    public int remoteAs;

    /**
     * local as
     */
    public int localAs;

    /**
     * address families
     */
    public int addrFams;

    /**
     * source template
     */
    public rtrBgpTemp template;

    /**
     * password
     */
    public String passwd;

    /**
     * remote description
     */
    public String description;

    /**
     * check neighbor route
     */
    public boolean fallOver;

    /**
     * consider remote asn in group membership
     */
    public boolean ungrpRemAs;

    /**
     * confederation peer
     */
    public boolean remoteConfed;

    /**
     * send accumulated igp
     */
    public boolean accIgp;

    /**
     * send traffic engineering
     */
    public boolean traffEng;

    /**
     * send pmsi tunnel
     */
    public boolean pmsiTun;

    /**
     * send tunnel encapsulation
     */
    public boolean tunEnc;

    /**
     * send link state
     */
    public boolean lnkSta;

    /**
     * send attribute set
     */
    public boolean attribSet;

    /**
     * send segment routing
     */
    public boolean segRout;

    /**
     * send bier
     */
    public boolean bier;

    /**
     * egress engineering index
     */
    public int egressEng;

    /**
     * leak prevention role
     */
    public int leakRole;

    /**
     * leak prevention attribute
     */
    public boolean leakAttr;

    /**
     * leak prevention enforced
     */
    public boolean leakForce;

    /**
     * advertise pop label
     */
    public boolean labelPop;

    /**
     * capability negotiation
     */
    public boolean capaNego;

    /**
     * track nexthops
     */
    public boolean trackNxthop;

    /**
     * route reflector client
     */
    public boolean reflectClnt;

    /**
     * dmz link bandwidth
     */
    public int dmzLinkBw;

    /**
     * distance
     */
    public int distance;

    /**
     * keep alive
     */
    public int keepAlive;

    /**
     * hold timer
     */
    public int holdTimer;

    /**
     * source interface
     */
    public cfgIfc srcIface;

    /**
     * peer disabled
     */
    public boolean shutdown;

    /**
     * bfd enabled
     */
    public boolean bfdTrigger;

    /**
     * soft reconfiguration
     */
    public boolean softReconfig;

    /**
     * graceful restart
     */
    public int graceRestart;

    /**
     * multiple labels
     */
    public int multiLabel;

    /**
     * extended nexthop current afi
     */
    public int extNextCur;

    /**
     * extended nexthop other afi
     */
    public int extNextOtr;

    /**
     * hostname
     */
    public int hostname;

    /**
     * extended open
     */
    public boolean extOpen;

    /**
     * extended update
     */
    public boolean extUpdate;

    /**
     * not transmit during receive
     */
    public boolean unidirection;

    /**
     * connection mode 1=active, 2=passive, 3=both, 4=dynamic
     */
    public int socketMode;

    /**
     * compression mode 0=none, 1=receive, 2=send, 3=both
     */
    public int compressMode;

    /**
     * buffer size
     */
    public int bufferSize;

    /**
     * ttl security
     */
    public int ttlSecurity;

    /**
     * additional path receive(1) mode
     */
    public int addpathRmode;

    /**
     * additional path transmit(2) mode
     */
    public int addpathTmode;

    /**
     * default information originate
     */
    public boolean sendDefRou;

    /**
     * other default information originate
     */
    public boolean sendOtrDefRou;

    /**
     * propagate next hop
     */
    public boolean nxtHopUnchgd;

    /**
     * rewrite next hop
     */
    public boolean nxtHopPeer;

    /**
     * rewrite next hop
     */
    public boolean nxtHopSelf;

    /**
     * propagate community 0=none, 1=std, 2=ext, 4=lrg, 7=all
     */
    public int sendCommunity;

    /**
     * preserve attributes
     */
    public boolean intVpnClnt;

    /**
     * allow my as relearn
     */
    public boolean allowAsIn;

    /**
     * allow peer as advertisement
     */
    public boolean allowAsOut;

    /**
     * transmit advertisement interval
     */
    public int advertIntTx;

    /**
     * receive advertisement interval
     */
    public int advertIntRx;

    /**
     * dampening prefixes
     */
    public tabGen<rtrBgpDamp> dampenPfxs;

    /**
     * dampening withdarw penalty
     */
    public int dampenWthd;

    /**
     * dampening announce penalty
     */
    public int dampenAnno;

    /**
     * dampening minimum penalty
     */
    public int dampenMinp;

    /**
     * dampening maximum penalty
     */
    public int dampenMaxp;

    /**
     * dampening suppress threshold
     */
    public int dampenSupp;

    /**
     * dampening reuse threshold
     */
    public int dampenReus;

    /**
     * dampening half life time
     */
    public int dampenHalf;

    /**
     * max prefix count
     */
    public int maxPrefixCnt;

    /**
     * max prefix percent
     */
    public int maxPrefixPrc;

    /**
     * enforce first as
     */
    public boolean enforceFirst;

    /**
     * route server client
     */
    public boolean serverClnt;

    /**
     * remove private as
     */
    public boolean removePrivAsOut;

    /**
     * remove private as
     */
    public boolean removePrivAsIn;

    /**
     * override peer as
     */
    public boolean overridePeerOut;

    /**
     * override peer as
     */
    public boolean overridePeerIn;

    /**
     * monitor to use
     */
    public rtrBgpMon monitor;

    /**
     * dump to use
     */
    public rtrBgpMrt dump;

    /**
     * other address
     */
    public addrIP otherAdr;

    /**
     * ingress prefix list
     */
    public tabListing<tabPrfxlstN, addrIP> prflstIn;

    /**
     * egress prefix list
     */
    public tabListing<tabPrfxlstN, addrIP> prflstOut;

    /**
     * ingress route map
     */
    public tabListing<tabRtrmapN, addrIP> roumapIn;

    /**
     * egress route map
     */
    public tabListing<tabRtrmapN, addrIP> roumapOut;

    /**
     * ingress route policy
     */
    public tabListing<tabRtrplcN, addrIP> roupolIn;

    /**
     * egress route policy
     */
    public tabListing<tabRtrplcN, addrIP> roupolOut;

    /**
     * other ingress prefix list
     */
    public tabListing<tabPrfxlstN, addrIP> oprflstIn;

    /**
     * other egress prefix list
     */
    public tabListing<tabPrfxlstN, addrIP> oprflstOut;

    /**
     * other ingress route map
     */
    public tabListing<tabRtrmapN, addrIP> oroumapIn;

    /**
     * other egress route map
     */
    public tabListing<tabRtrmapN, addrIP> oroumapOut;

    /**
     * other ingress route policy
     */
    public tabListing<tabRtrplcN, addrIP> oroupolIn;

    /**
     * other egress route policy
     */
    public tabListing<tabRtrplcN, addrIP> oroupolOut;

    /**
     * ingress vpn route map
     */
    public tabListing<tabRtrmapN, addrIP> vroumapIn;

    /**
     * egress vpn route map
     */
    public tabListing<tabRtrmapN, addrIP> vroumapOut;

    /**
     * ingress vpn route policy
     */
    public tabListing<tabRtrplcN, addrIP> vroupolIn;

    /**
     * egress vpn route policy
     */
    public tabListing<tabRtrplcN, addrIP> vroupolOut;

    /**
     * ingress other vpn route map
     */
    public tabListing<tabRtrmapN, addrIP> wroumapIn;

    /**
     * egress other vpn route map
     */
    public tabListing<tabRtrmapN, addrIP> wroumapOut;

    /**
     * ingress other vpn route policy
     */
    public tabListing<tabRtrplcN, addrIP> wroupolIn;

    /**
     * egress other vpn route policy
     */
    public tabListing<tabRtrplcN, addrIP> wroupolOut;

    /**
     * unicast
     */
    public final static int mskUni = 0x01;

    /**
     * labeled unicast
     */
    public final static int mskLab = 0x02;

    /**
     * multicast
     */
    public final static int mskMlt = 0x04;

    /**
     * vpn unicast
     */
    public final static int mskVpnU = 0x08;

    /**
     * vpn multicast
     */
    public final static int mskVpnM = 0x10;

    /**
     * vpls
     */
    public final static int mskVpls = 0x20;

    /**
     * evpn
     */
    public final static int mskEvpn = 0x40;

    /**
     * mdt
     */
    public final static int mskMdt = 0x80;

    /**
     * flowspec
     */
    public final static int mskFlw = 0x100;

    /**
     * vpn flowspec
     */
    public final static int mskVpnF = 0x200;

    /**
     * other vpn unicast
     */
    public final static int mskVpoU = 0x400;

    /**
     * other vpn multicast
     */
    public final static int mskVpoM = 0x800;

    /**
     * other vpn flowspec
     */
    public final static int mskVpoF = 0x1000;

    /**
     * mvpn
     */
    public final static int mskMvpn = 0x2000;

    /**
     * other mvpn
     */
    public final static int mskMvpo = 0x4000;

    /**
     * other labeled unicast
     */
    public final static int mskOtrL = 0x8000;

    /**
     * mspw
     */
    public final static int mskMspw = 0x10000;

    /**
     * srte
     */
    public final static int mskSrte = 0x20000;

    /**
     * link state
     */
    public final static int mskLnks = 0x40000;

    /**
     * other unicast
     */
    public final static int mskOtrU = 0x80000;

    /**
     * other multicast
     */
    public final static int mskOtrM = 0x100000;

    /**
     * other flowspec
     */
    public final static int mskOtrF = 0x200000;

    /**
     * other srte
     */
    public final static int mskOtrS = 0x400000;

    /**
     * nsh
     */
    public final static int mskNsh = 0x800000;

    /**
     * all
     */
    public final static int mskAll = mskUni | mskLab | mskMlt | mskVpnU | mskVpnM | mskVpls | mskEvpn | mskMdt | mskSrte | mskLnks | mskFlw | mskVpnF | mskVpoU | mskVpoM | mskVpoF | mskMvpn | mskMvpo | mskOtrL | mskOtrU | mskOtrM | mskOtrF | mskOtrS | mskMspw | mskNsh;

    /**
     * string to afi mask
     *
     * @param s string
     * @return afi mask
     */
    public static final int string2mask(String s) {
        cmds c = new cmds("afi", s);
        return string2mask(c);
    }

    /**
     * string to afi mask
     *
     * @param c string
     * @return afi mask
     */
    public static final int string2mask(cmds c) {
        int i = 0;
        for (;;) {
            String a = c.word();
            if (a.length() < 1) {
                break;
            }
            if (a.equals("all")) {
                i |= mskAll;
            }
            if (a.equals("none")) {
                i = 0;
            }
            if (a.equals("unicast")) {
                i |= mskUni;
            }
            if (a.equals("labeled")) {
                i |= mskLab;
            }
            if (a.equals("multicast")) {
                i |= mskMlt;
            }
            if (a.equals("flowspec")) {
                i |= mskFlw;
            }
            if (a.equals("vpnuni")) {
                i |= mskVpnU;
            }
            if (a.equals("vpnmlt")) {
                i |= mskVpnM;
            }
            if (a.equals("vpnflw")) {
                i |= mskVpnF;
            }
            if (a.equals("vpls")) {
                i |= mskVpls;
            }
            if (a.equals("mspw")) {
                i |= mskMspw;
            }
            if (a.equals("evpn")) {
                i |= mskEvpn;
            }
            if (a.equals("mdt")) {
                i |= mskMdt;
            }
            if (a.equals("nsh")) {
                i |= mskNsh;
            }
            if (a.equals("srte")) {
                i |= mskSrte;
            }
            if (a.equals("linkstate")) {
                i |= mskLnks;
            }
            if (a.equals("mvpn")) {
                i |= mskMvpn;
            }
            if (a.equals("omvpn")) {
                i |= mskMvpo;
            }
            if (a.equals("ovpnuni")) {
                i |= mskVpoU;
            }
            if (a.equals("ovpnmlt")) {
                i |= mskVpoM;
            }
            if (a.equals("ovpnflw")) {
                i |= mskVpoF;
            }
            if (a.equals("olab")) {
                i |= mskOtrL;
            }
            if (a.equals("ouni")) {
                i |= mskOtrU;
            }
            if (a.equals("omlt")) {
                i |= mskOtrM;
            }
            if (a.equals("oflw")) {
                i |= mskOtrF;
            }
            if (a.equals("osrt")) {
                i |= mskOtrS;
            }
        }
        int bth = mskUni | mskLab;
        if ((i & bth) == bth) {
            i -= rtrBgpParam.mskUni;
        }
        bth = mskOtrU | mskOtrL;
        if ((i & bth) == bth) {
            i -= rtrBgpParam.mskOtrU;
        }
        return i;
    }

    /**
     * afi mask to string
     *
     * @param i afi mask
     * @return string
     */
    public static final String mask2string(int i) {
        String a = "";
        if ((i & mskUni) != 0) {
            a += " unicast";
        }
        if ((i & mskLab) != 0) {
            a += " labeled";
        }
        if ((i & mskMlt) != 0) {
            a += " multicast";
        }
        if ((i & mskOtrL) != 0) {
            a += " olab";
        }
        if ((i & mskOtrU) != 0) {
            a += " ouni";
        }
        if ((i & mskOtrM) != 0) {
            a += " omlt";
        }
        if ((i & mskFlw) != 0) {
            a += " flowspec";
        }
        if ((i & mskOtrF) != 0) {
            a += " oflw";
        }
        if ((i & mskVpnU) != 0) {
            a += " vpnuni";
        }
        if ((i & mskVpnM) != 0) {
            a += " vpnmlt";
        }
        if ((i & mskVpnF) != 0) {
            a += " vpnflw";
        }
        if ((i & mskVpoU) != 0) {
            a += " ovpnuni";
        }
        if ((i & mskVpoM) != 0) {
            a += " ovpnmlt";
        }
        if ((i & mskVpoF) != 0) {
            a += " ovpnflw";
        }
        if ((i & mskVpls) != 0) {
            a += " vpls";
        }
        if ((i & mskMspw) != 0) {
            a += " mspw";
        }
        if ((i & mskEvpn) != 0) {
            a += " evpn";
        }
        if ((i & mskMdt) != 0) {
            a += " mdt";
        }
        if ((i & mskNsh) != 0) {
            a += " nsh";
        }
        if ((i & mskSrte) != 0) {
            a += " srte";
        }
        if ((i & mskOtrS) != 0) {
            a += " osrt";
        }
        if ((i & mskLnks) != 0) {
            a += " linkstate";
        }
        if ((i & mskMvpn) != 0) {
            a += " mvpn";
        }
        if ((i & mskMvpo) != 0) {
            a += " omvpn";
        }
        return a;
    }

    /**
     * get list of address families
     *
     * @param hl helping
     * @param beg beginning
     * @param end ending
     * @param all all, none
     */
    public static void getAfiList(userHelping hl, String beg, String end, boolean all) {
        if (all) {
            hl.add(null, beg + "  all           all address family to " + end);
            hl.add(null, beg + "  none          no address family to " + end);
        }
        hl.add(null, beg + "  unicast       address family to " + end);
        hl.add(null, beg + "  labeled       address family to " + end);
        hl.add(null, beg + "  olab          address family to " + end);
        hl.add(null, beg + "  ouni          address family to " + end);
        hl.add(null, beg + "  omlt          address family to " + end);
        hl.add(null, beg + "  oflw          address family to " + end);
        hl.add(null, beg + "  osrt          address family to " + end);
        hl.add(null, beg + "  multicast     address family to " + end);
        hl.add(null, beg + "  flowspec      address family to " + end);
        hl.add(null, beg + "  vpnuni        address family to " + end);
        hl.add(null, beg + "  vpnmlt        address family to " + end);
        hl.add(null, beg + "  vpnflw        address family to " + end);
        hl.add(null, beg + "  ovpnuni       address family to " + end);
        hl.add(null, beg + "  ovpnmlt       address family to " + end);
        hl.add(null, beg + "  ovpnflw       address family to " + end);
        hl.add(null, beg + "  vpls          address family to " + end);
        hl.add(null, beg + "  mspw          address family to " + end);
        hl.add(null, beg + "  evpn          address family to " + end);
        hl.add(null, beg + "  mdt           address family to " + end);
        hl.add(null, beg + "  nsh           address family to " + end);
        hl.add(null, beg + "  srte          address family to " + end);
        hl.add(null, beg + "  linkstate     address family to " + end);
        hl.add(null, beg + "  mvpn          address family to " + end);
        hl.add(null, beg + "  omvpn         address family to " + end);
    }

    /**
     * create parameter container
     *
     * @param parent bgp process
     * @param temp template or not
     */
    public rtrBgpParam(rtrBgp parent, boolean temp) {
        lower = parent;
        isTemplate = temp;
        if (lower != null) {
            localAs = lower.localAs;
            addrFams = lower.addrFams;
        }
        allowAsOut = true;
        dmzLinkBw = -1;
        socketMode = 3;
        bufferSize = 65536;
        ttlSecurity = -1;
        leakRole = -1;
        passwd = null;
        capaNego = true;
        trackNxthop = true;
        keepAlive = 60 * 1000;
        holdTimer = keepAlive * 3;
    }

    /**
     * copy parameters
     *
     * @param src source where from copy
     */
    public void copyFrom(rtrBgpParam src) {
        remoteAs = src.remoteAs;
        localAs = src.localAs;
        addrFams = src.addrFams;
        template = src.template;
        description = src.description;
        fallOver = src.fallOver;
        remoteConfed = src.remoteConfed;
        reflectClnt = src.reflectClnt;
        dmzLinkBw = src.dmzLinkBw;
        distance = src.distance;
        keepAlive = src.keepAlive;
        holdTimer = src.holdTimer;
        srcIface = src.srcIface;
        shutdown = src.shutdown;
        monitor = src.monitor;
        dump = src.dump;
        otherAdr = src.otherAdr;
        passwd = src.passwd;
        accIgp = src.accIgp;
        traffEng = src.traffEng;
        pmsiTun = src.pmsiTun;
        tunEnc = src.tunEnc;
        lnkSta = src.lnkSta;
        attribSet = src.attribSet;
        segRout = src.segRout;
        bier = src.bier;
        egressEng = src.egressEng;
        leakRole = src.leakRole;
        leakAttr = src.leakAttr;
        leakForce = src.leakForce;
        labelPop = src.labelPop;
        capaNego = src.capaNego;
        trackNxthop = src.trackNxthop;
        bfdTrigger = src.bfdTrigger;
        softReconfig = src.softReconfig;
        graceRestart = src.graceRestart;
        multiLabel = src.multiLabel;
        extNextCur = src.extNextCur;
        extNextOtr = src.extNextOtr;
        hostname = src.hostname;
        extOpen = src.extOpen;
        extUpdate = src.extUpdate;
        unidirection = src.unidirection;
        compressMode = src.compressMode;
        socketMode = src.socketMode;
        bufferSize = src.bufferSize;
        ttlSecurity = src.ttlSecurity;
        addpathRmode = src.addpathRmode;
        addpathTmode = src.addpathTmode;
        sendDefRou = src.sendDefRou;
        sendOtrDefRou = src.sendOtrDefRou;
        nxtHopUnchgd = src.nxtHopUnchgd;
        nxtHopPeer = src.nxtHopPeer;
        nxtHopSelf = src.nxtHopSelf;
        sendCommunity = src.sendCommunity;
        intVpnClnt = src.intVpnClnt;
        allowAsIn = src.allowAsIn;
        allowAsOut = src.allowAsOut;
        advertIntTx = src.advertIntTx;
        advertIntRx = src.advertIntRx;
        serverClnt = src.serverClnt;
        dampenWthd = src.dampenWthd;
        dampenAnno = src.dampenAnno;
        dampenMinp = src.dampenMinp;
        dampenMaxp = src.dampenMaxp;
        dampenSupp = src.dampenSupp;
        dampenReus = src.dampenReus;
        dampenHalf = src.dampenHalf;
        if (src.dampenPfxs == null) {
            dampenPfxs = null;
        } else {
            if (dampenPfxs == null) {
                dampenPfxs = new tabGen<rtrBgpDamp>();
            }
        }
        maxPrefixCnt = src.maxPrefixCnt;
        maxPrefixPrc = src.maxPrefixPrc;
        enforceFirst = src.enforceFirst;
        removePrivAsOut = src.removePrivAsOut;
        removePrivAsIn = src.removePrivAsIn;
        ungrpRemAs = src.ungrpRemAs;
        overridePeerOut = src.overridePeerOut;
        overridePeerIn = src.overridePeerIn;
        prflstIn = src.prflstIn;
        prflstOut = src.prflstOut;
        roumapIn = src.roumapIn;
        roumapOut = src.roumapOut;
        roupolIn = src.roupolIn;
        roupolOut = src.roupolOut;
        oprflstIn = src.oprflstIn;
        oprflstOut = src.oprflstOut;
        oroumapIn = src.oroumapIn;
        oroumapOut = src.oroumapOut;
        oroupolIn = src.oroupolIn;
        oroupolOut = src.oroupolOut;
        vroumapIn = src.vroumapIn;
        vroumapOut = src.vroumapOut;
        vroupolIn = src.vroupolIn;
        vroupolOut = src.vroupolOut;
        wroumapIn = src.wroumapIn;
        wroumapOut = src.wroumapOut;
        wroupolIn = src.wroupolIn;
        wroupolOut = src.wroupolOut;
    }

    /**
     * check if same output will produced
     *
     * @param src where to compare
     * @return false if same, true if differs
     */
    public boolean sameOutput(rtrBgpParam src) {
        if (!allowAsOut) {
            if (remoteAs != src.remoteAs) {
                return true;
            }
        }
        if (overridePeerOut) {
            if (remoteAs != src.remoteAs) {
                return true;
            }
        }
        if (ungrpRemAs) {
            if (remoteAs != src.remoteAs) {
                return true;
            }
        }
        if (localAs != src.localAs) {
            return true;
        }
        if (addrFams != src.addrFams) {
            return true;
        }
        if (leakAttr != src.leakAttr) {
            return true;
        }
        if (leakRole != src.leakRole) {
            return true;
        }
        if (remoteConfed != src.remoteConfed) {
            return true;
        }
        if (reflectClnt != src.reflectClnt) {
            return true;
        }
        if (serverClnt != src.serverClnt) {
            return true;
        }
        if (intVpnClnt != src.intVpnClnt) {
            return true;
        }
        if (accIgp != src.accIgp) {
            return true;
        }
        if (traffEng != src.traffEng) {
            return true;
        }
        if (pmsiTun != src.pmsiTun) {
            return true;
        }
        if (tunEnc != src.tunEnc) {
            return true;
        }
        if (lnkSta != src.lnkSta) {
            return true;
        }
        if (attribSet != src.attribSet) {
            return true;
        }
        if (labelPop != src.labelPop) {
            return true;
        }
        if (segRout != src.segRout) {
            return true;
        }
        if (bier != src.bier) {
            return true;
        }
        if (addpathTmode != src.addpathTmode) {
            return true;
        }
        if (sendOtrDefRou != src.sendOtrDefRou) {
            return true;
        }
        if (sendDefRou != src.sendDefRou) {
            return true;
        }
        if (nxtHopUnchgd != src.nxtHopUnchgd) {
            return true;
        }
        if (nxtHopSelf != src.nxtHopSelf) {
            return true;
        }
        if (sendCommunity != src.sendCommunity) {
            return true;
        }
        if (allowAsOut != src.allowAsOut) {
            return true;
        }
        if (removePrivAsOut != src.removePrivAsOut) {
            return true;
        }
        if (overridePeerOut != src.overridePeerOut) {
            return true;
        }
        if (prflstOut == null) {
            if (src.prflstOut != null) {
                return true;
            }
        } else {
            if (src.prflstOut == null) {
                return true;
            }
            if (!prflstOut.listName.equals(src.prflstOut.listName)) {
                return true;
            }
        }
        if (roumapOut == null) {
            if (src.roumapOut != null) {
                return true;
            }
        } else {
            if (src.roumapOut == null) {
                return true;
            }
            if (!roumapOut.listName.equals(src.roumapOut.listName)) {
                return true;
            }
        }
        if (roupolOut == null) {
            if (src.roupolOut != null) {
                return true;
            }
        } else {
            if (src.roupolOut == null) {
                return true;
            }
            if (!roupolOut.listName.equals(src.roupolOut.listName)) {
                return true;
            }
        }
        if (oprflstOut == null) {
            if (src.oprflstOut != null) {
                return true;
            }
        } else {
            if (src.oprflstOut == null) {
                return true;
            }
            if (!oprflstOut.listName.equals(src.oprflstOut.listName)) {
                return true;
            }
        }
        if (oroumapOut == null) {
            if (src.oroumapOut != null) {
                return true;
            }
        } else {
            if (src.oroumapOut == null) {
                return true;
            }
            if (!oroumapOut.listName.equals(src.oroumapOut.listName)) {
                return true;
            }
        }
        if (oroupolOut == null) {
            if (src.oroupolOut != null) {
                return true;
            }
        } else {
            if (src.oroupolOut == null) {
                return true;
            }
            if (!oroupolOut.listName.equals(src.oroupolOut.listName)) {
                return true;
            }
        }
        if (vroumapOut == null) {
            if (src.vroumapOut != null) {
                return true;
            }
        } else {
            if (src.vroumapOut == null) {
                return true;
            }
            if (!vroumapOut.listName.equals(src.vroumapOut.listName)) {
                return true;
            }
        }
        if (vroupolOut == null) {
            if (src.vroupolOut != null) {
                return true;
            }
        } else {
            if (src.vroupolOut == null) {
                return true;
            }
            if (!vroupolOut.listName.equals(src.vroupolOut.listName)) {
                return true;
            }
        }
        if (wroumapOut == null) {
            if (src.wroumapOut != null) {
                return true;
            }
        } else {
            if (src.wroumapOut == null) {
                return true;
            }
            if (!wroumapOut.listName.equals(src.wroumapOut.listName)) {
                return true;
            }
        }
        if (wroupolOut == null) {
            if (src.wroupolOut != null) {
                return true;
            }
        } else {
            if (src.wroupolOut == null) {
                return true;
            }
            if (!wroupolOut.listName.equals(src.wroupolOut.listName)) {
                return true;
            }
        }
        return false;
    }

    /**
     * get parameter help
     *
     * @param l list to append
     */
    public static void getParamHelp(userHelping l) {
        l.add(null, "3  4       remote-as                   remote as number");
        l.add(null, "4  5,.       <num>                     autonomous system number");
        l.add(null, "5  .           shutdown                connection disabled for this peer");
        l.add(null, "3  4       password                    set session password");
        l.add(null, "4  .         <text>                    password to use");
        l.add(null, "3  .       shutdown                    connection disabled for this peer");
        l.add(null, "3  4       description                 describe this neighbor");
        l.add(null, "4  4,.       <str>                     description of neighbor");
        l.add(null, "3  4       update-source               connection source for this peer");
        l.add(null, "4  .         <name:ifc>                name of interface");
        l.add(null, "3  4       address-family              specify address families");
        getAfiList(l, "4  4,.", "use", true);
        l.add(null, "3  4       monitor                     bgp monitor protocol for this peer");
        l.add(null, "4  .         <name>                    name of bmp");
        l.add(null, "3  4       other-address               address of peer in the other afi");
        l.add(null, "4  .         <addr>                    other address");
        l.add(null, "3  4       dump                        bgp dump for this peer");
        l.add(null, "4  .         <name>                    name of mrt");
        l.add(null, "3  4       buffer-size                 size of buffer");
        l.add(null, "4  .         <num>                     bytes in buffer");
        l.add(null, "3  4       ttl-security                sending ttl value");
        l.add(null, "4  .         <num>                     ttl value");
        l.add(null, "3  4       egress-engineering          set egress engineering");
        l.add(null, "4  .         <num>                     index value");
        l.add(null, "3  4       role                        leak prevention role");
        l.add(null, "4  5,.       disabled                  disable processing");
        l.add(null, "4  5,.       attrib                    only send otc attribute");
        l.add(null, "4  5,.       provider                  provider");
        l.add(null, "4  5,.       ix-server                 route server");
        l.add(null, "4  5,.       ix-client                 route server client");
        l.add(null, "4  5,.       customer                  customer");
        l.add(null, "4  5,.       peer                      peer");
        l.add(null, "5  .           enforce                 enforce negotiation");
        l.add(null, "3  .       capability-negotiation      perform capability negosiation");
        l.add(null, "3  .       track-next-hop              perform next hop tracking");
        l.add(null, "3  4       connection-mode             connection mode allowed");
        l.add(null, "4  .         active                    this router will initiate session");
        l.add(null, "4  .         passive                   remote router will initiate session");
        l.add(null, "4  .         both                      both modes allowed");
        l.add(null, "3  4       compression                 compression mode allowed");
        l.add(null, "4  .         none                      not allowed");
        l.add(null, "4  .         receive                   receive direction");
        l.add(null, "4  .         transmit                  transmit direction");
        l.add(null, "4  .         both                      both directions");
        l.add(null, "3  4       additional-path-rx          additional path receive mode");
        getAfiList(l, "4  4,.", "use", true);
        l.add(null, "3  4       additional-path-tx          additional path transmit mode");
        getAfiList(l, "4  4,.", "use", true);
        l.add(null, "3  .       route-reflector-client      reflect routes to this client");
        l.add(null, "3  .       confederation-peer          confederation peer");
        l.add(null, "3  .       default-originate           send default route to peer");
        l.add(null, "3  .       other-default-originate     send other default route to peer");
        l.add(null, "3  .       aigp                        send accumulated igp attribute");
        l.add(null, "3  .       traffeng                    send traffic engineering attribute");
        l.add(null, "3  .       pmsitun                     send provider multicast service interface tunnel attribute");
        l.add(null, "3  .       tunenc                      send tunnel encapsulation attribute");
        l.add(null, "3  .       linkstate                   send link state attribute");
        l.add(null, "3  .       attribset                   send attribute set attribute");
        l.add(null, "3  .       label-pop                   advertise pop label");
        l.add(null, "3  .       segrout                     send segment routing attribute");
        l.add(null, "3  .       bier                        send bier attribute");
        l.add(null, "3  .       internal-vpn-client         preserve attributes from peer");
        l.add(null, "3  .       allow-as-in                 allow my as to relearn from peer");
        l.add(null, "3  .       allow-as-out                allow peer as to advertised out");
        l.add(null, "3  .       enforce-first-as            discard unprepended aspath from peer");
        l.add(null, "3  .       route-server-client         unmodified attributes to this client");
        l.add(null, "3  .       remove-private-as-out       remove private as to peer");
        l.add(null, "3  .       ungroup-remoteas            consider remote asn while grouping peers");
        l.add(null, "3  .       remove-private-as-in        remove private as from peer");
        l.add(null, "3  .       override-peer-as-out        replace peer as to peer");
        l.add(null, "3  .       override-peer-as-in         replace peer as from peer");
        l.add(null, "3  .       next-hop-unchanged          send next hop unchanged to peer");
        l.add(null, "3  .       next-hop-self               send next hop myself to peer");
        l.add(null, "3  .       next-hop-peer               set next hop to peer address");
        l.add(null, "3  .       bfd                         enable bfd triggered down");
        l.add(null, "3  4       multiple-labels             advertise multiple labels capability");
        getAfiList(l, "4  4,.", "use", true);
        l.add(null, "3  4       graceful-restart            advertise graceful restart capability");
        getAfiList(l, "4  4,.", "use", true);
        l.add(null, "3  4       extended-nexthop-current    advertise extended nexthop capability");
        getAfiList(l, "4  4,.", "use", true);
        l.add(null, "3  4       extended-nexthop-other      advertise extended nexthop capability");
        getAfiList(l, "4  4,.", "use", true);
        l.add(null, "3  4,.     hostname                    advertise hostname capability");
        l.add(null, "4  .         domain                    advertise domain too");
        l.add(null, "3  .       extended-open               send open in extended format");
        l.add(null, "3  .       extended-update             advertise extended update capability");
        l.add(null, "3  .       unidirection                not advertise when receiving");
        l.add(null, "3  .       fall-over                   track outgoing interface");
        l.add(null, "3  .       soft-reconfiguration        enable soft reconfiguration");
        l.add(null, "3  4       maximum-prefix              maximum number of accepted prefixes");
        l.add(null, "4  5         <num>                     prefix count");
        l.add(null, "5  .           <num>                   warning percent");
        l.add(null, "3  4       dampening                   route flap dampening of prefixes");
        l.add(null, "4  5         <num>                     withdraw penalty");
        l.add(null, "5  6           <num>                   announce penalty");
        l.add(null, "6  7             <num>                 minimum penalty");
        l.add(null, "7  8               <num>               maximum penalty");
        l.add(null, "8  9                 <num>             suppress threshold");
        l.add(null, "9  10                  <num>           reuse threshold");
        l.add(null, "10 .                     <num>         half life time in ms");
        l.add(null, "3  4       send-community              send community to peer");
        l.add(null, "4  4,.       standard                  send standard community");
        l.add(null, "4  4,.       extended                  send extended community");
        l.add(null, "4  4,.       large                     send large community");
        l.add(null, "4  4,.       both                      send std+ext communities");
        l.add(null, "4  4,.       all                       send std+ext+lrg communities");
        l.add(null, "4  4,.       none                      send no community");
        l.add(null, "3  4       local-as                    local as number");
        l.add(null, "4  .         <num>                     autonomous system number");
        l.add(null, "3  4       advertisement-interval-tx   time between sending updates");
        l.add(null, "3  4       advertisement-interval-rx   time between receiving updates");
        l.add(null, "4  .         <num>                     interval in ms");
        l.add(null, "3  4       dmz-link-bw                 set dmz link bandwidth");
        l.add(null, "4  .         <num>                     link bandwidth in kb");
        l.add(null, "3  4       timer                       neighbor keepalive times");
        l.add(null, "4  5         <num>                     keepalive in ms");
        l.add(null, "5  .           <num>                   hold time in ms");
        l.add(null, "3  4       distance                    administrative distance of routes");
        l.add(null, "4  .         <num>                     set administrative distance");
        l.add(null, "3  4       route-map-in                process prefixes in ingress updates");
        l.add(null, "4  .         <name:rm>                 name of route map");
        l.add(null, "3  4       route-map-out               process prefixes in egress updates");
        l.add(null, "4  .         <name:rm>                 name of route map");
        l.add(null, "3  4       route-policy-in             process prefixes in ingress updates");
        l.add(null, "4  .         <name:rpl>                name of route policy");
        l.add(null, "3  4       route-policy-out            process prefixes in egress updates");
        l.add(null, "4  .         <name:rpl>                name of route policy");
        l.add(null, "3  4       prefix-list-in              filter prefixes in ingress updates");
        l.add(null, "4  .         <name:pl>                 name of prefix list");
        l.add(null, "3  4       prefix-list-out             filter prefixes in egress updates");
        l.add(null, "4  .         <name:pl>                 name of prefix list");
        l.add(null, "3  4       other-route-map-in          process other prefixes in ingress updates");
        l.add(null, "4  .         <name:rm>                 name of route map");
        l.add(null, "3  4       other-route-map-out         process other prefixes in egress updates");
        l.add(null, "4  .         <name:rm>                 name of route map");
        l.add(null, "3  4       other-route-policy-in       process other prefixes in ingress updates");
        l.add(null, "4  .         <name:rpl>                name of route policy");
        l.add(null, "3  4       other-route-policy-out      process other prefixes in egress updates");
        l.add(null, "4  .         <name:rpl>                name of route policy");
        l.add(null, "3  4       other-prefix-list-in        filter other prefixes in ingress updates");
        l.add(null, "4  .         <name:pl>                 name of prefix list");
        l.add(null, "3  4       other-prefix-list-out       filter other prefixes in egress updates");
        l.add(null, "4  .         <name:pl>                 name of prefix list");
        l.add(null, "3  4       vpn-route-map-in            process vpn prefixes in ingress updates");
        l.add(null, "4  .         <name:rm>                 name of route map");
        l.add(null, "3  4       vpn-route-map-out           process vpn prefixes in egress updates");
        l.add(null, "4  .         <name:rm>                 name of route map");
        l.add(null, "3  4       vpn-route-policy-in         process vpn prefixes in ingress updates");
        l.add(null, "4  .         <name:rpl>                name of route policy");
        l.add(null, "3  4       vpn-route-policy-out        process vpn prefixes in egress updates");
        l.add(null, "4  .         <name:rpl>                name of route policy");
        l.add(null, "3  4       ovpn-route-map-in           process other vpn prefixes in ingress updates");
        l.add(null, "4  .         <name:rm>                 name of route map");
        l.add(null, "3  4       ovpn-route-map-out          process other vpn prefixes in egress updates");
        l.add(null, "4  .         <name:rm>                 name of route map");
        l.add(null, "3  4       ovpn-route-policy-in        process other vpn prefixes in ingress updates");
        l.add(null, "4  .         <name:rpl>                name of route policy");
        l.add(null, "3  4       ovpn-route-policy-out       process other vpn prefixes in egress updates");
        l.add(null, "4  .         <name:rpl>                name of route policy");
    }

    /**
     * get configuration
     *
     * @param beg beginning
     * @param nei prepending
     * @param filter filter defaults
     * @return list of text
     */
    public List<String> getParamCfg(String beg, String nei, int filter) {
        List<String> l = new ArrayList<String>();
        if (template == null) {
            l.add(beg + "no " + nei + "template");
        } else {
            l.add(beg + nei + "template " + template.tempName);
        }
        l.add(beg + nei + "remote-as " + bits.num2str(remoteAs));
        cmds.cfgLine(l, description == null, beg, nei + "description", description);
        cmds.cfgLine(l, passwd == null, beg, nei + "password", authLocal.passwdEncode(passwd, (filter & 2) != 0));
        l.add(beg + nei + "local-as " + bits.num2str(localAs));
        l.add(beg + nei + "advertisement-interval-tx " + advertIntTx);
        l.add(beg + nei + "advertisement-interval-rx " + advertIntRx);
        l.add(beg + nei + "address-family" + mask2string(addrFams));
        l.add(beg + nei + "distance " + distance);
        l.add(beg + nei + "timer " + keepAlive + " " + holdTimer);
        l.add(beg + nei + "dmz-link-bw " + dmzLinkBw);
        String s;
        switch (socketMode) {
            case 1:
                s = "active";
                break;
            case 2:
                s = "passive";
                break;
            default:
                s = "both";
                break;
        }
        l.add(beg + nei + "connection-mode " + s);
        switch (compressMode) {
            case 1:
                s = "receive";
                break;
            case 2:
                s = "transmit";
                break;
            case 3:
                s = "both";
                break;
            default:
                s = "none";
                break;
        }
        cmds.cfgLine(l, compressMode == 0, beg, nei + "compression", s);
        l.add(beg + nei + "buffer-size " + bufferSize);
        l.add(beg + nei + "ttl-security " + ttlSecurity);
        l.add(beg + nei + "additional-path-rx" + mask2string(addpathRmode));
        l.add(beg + nei + "additional-path-tx" + mask2string(addpathTmode));
        cmds.cfgLine(l, !shutdown, beg, nei + "shutdown", "");
        if (srcIface == null) {
            l.add(beg + "no " + nei + "update-source");
        } else {
            l.add(beg + nei + "update-source " + srcIface.name);
        }
        if (monitor == null) {
            l.add(beg + "no " + nei + "monitor");
        } else {
            l.add(beg + nei + "monitor " + monitor.monName);
        }
        if (dump == null) {
            l.add(beg + "no " + nei + "dump");
        } else {
            l.add(beg + nei + "dump " + dump.dumpName);
        }
        cmds.cfgLine(l, otherAdr == null, beg, nei + "other-address", "" + otherAdr);
        cmds.cfgLine(l, !bfdTrigger, beg, nei + "bfd", "");
        cmds.cfgLine(l, !ungrpRemAs, beg, nei + "ungroup-remoteas", "");
        cmds.cfgLine(l, !softReconfig, beg, nei + "soft-reconfiguration", "");
        l.add(beg + nei + "multiple-labels" + mask2string(multiLabel));
        l.add(beg + nei + "graceful-restart" + mask2string(graceRestart));
        l.add(beg + nei + "extended-nexthop-current" + mask2string(extNextCur));
        l.add(beg + nei + "extended-nexthop-other" + mask2string(extNextOtr));
        s = "";
        if (hostname > 1) {
            s = "domain";
        }
        cmds.cfgLine(l, hostname < 1, beg, nei + "hostname", s);
        cmds.cfgLine(l, !extOpen, beg, nei + "extended-open", "");
        cmds.cfgLine(l, !extUpdate, beg, nei + "extended-update", "");
        cmds.cfgLine(l, !unidirection, beg, nei + "unidirection", "");
        cmds.cfgLine(l, !fallOver, beg, nei + "fall-over", "");
        cmds.cfgLine(l, !sendDefRou, beg, nei + "default-originate", "");
        cmds.cfgLine(l, !sendOtrDefRou, beg, nei + "other-default-originate", "");
        cmds.cfgLine(l, !intVpnClnt, beg, nei + "internal-vpn-client", "");
        cmds.cfgLine(l, !allowAsIn, beg, nei + "allow-as-in", "");
        cmds.cfgLine(l, !allowAsOut, beg, nei + "allow-as-out", "");
        cmds.cfgLine(l, !enforceFirst, beg, nei + "enforce-first-as", "");
        cmds.cfgLine(l, maxPrefixCnt < 1, beg, nei + "maximum-prefix", maxPrefixCnt + " " + maxPrefixPrc);
        cmds.cfgLine(l, (dampenWthd + dampenAnno) < 1, beg, nei + "dampening", dampenWthd + " " + dampenAnno + " " + dampenMinp + " " + dampenMaxp + " " + dampenSupp + " " + dampenReus + " " + dampenHalf);
        cmds.cfgLine(l, !serverClnt, beg, nei + "route-server-client", "");
        cmds.cfgLine(l, !removePrivAsOut, beg, nei + "remove-private-as-out", "");
        cmds.cfgLine(l, !removePrivAsIn, beg, nei + "remove-private-as-in", "");
        cmds.cfgLine(l, !overridePeerOut, beg, nei + "override-peer-as-out", "");
        cmds.cfgLine(l, !overridePeerIn, beg, nei + "override-peer-as-in", "");
        cmds.cfgLine(l, !accIgp, beg, nei + "aigp", "");
        cmds.cfgLine(l, !traffEng, beg, nei + "traffeng", "");
        cmds.cfgLine(l, !pmsiTun, beg, nei + "pmsitun", "");
        cmds.cfgLine(l, !tunEnc, beg, nei + "tunenc", "");
        cmds.cfgLine(l, !lnkSta, beg, nei + "linkstate", "");
        cmds.cfgLine(l, !attribSet, beg, nei + "attribset", "");
        cmds.cfgLine(l, !segRout, beg, nei + "segrout", "");
        cmds.cfgLine(l, !bier, beg, nei + "bier", "");
        cmds.cfgLine(l, egressEng == 0, beg, nei + "egress-engineering", "" + egressEng);
        s = rtrBgpUtil.leakRole2string(leakRole, leakAttr);
        if (leakForce) {
            s += " enforce";
        }
        l.add(beg + nei + "role " + s);
        cmds.cfgLine(l, !labelPop, beg, nei + "label-pop", "");
        cmds.cfgLine(l, !capaNego, beg, nei + "capability-negotiation", "");
        cmds.cfgLine(l, !trackNxthop, beg, nei + "track-next-hop", "");
        cmds.cfgLine(l, !reflectClnt, beg, nei + "route-reflector-client", "");
        cmds.cfgLine(l, !remoteConfed, beg, nei + "confederation-peer", "");
        cmds.cfgLine(l, !nxtHopUnchgd, beg, nei + "next-hop-unchanged", "");
        cmds.cfgLine(l, !nxtHopSelf, beg, nei + "next-hop-self", "");
        cmds.cfgLine(l, !nxtHopPeer, beg, nei + "next-hop-peer", "");
        switch (sendCommunity) {
            case 1:
                s = "standard";
                break;
            case 2:
                s = "extended";
                break;
            case 3:
                s = "standard extended";
                break;
            case 4:
                s = "large";
                break;
            case 5:
                s = "standard large";
                break;
            case 6:
                s = "extended large";
                break;
            case 7:
                s = "all";
                break;
            default:
                s = "none";
                break;
        }
        cmds.cfgLine(l, sendCommunity == 0, beg, nei + "send-community", s);
        cmds.cfgLine(l, prflstIn == null, beg, nei + "prefix-list-in", "" + prflstIn);
        cmds.cfgLine(l, prflstOut == null, beg, nei + "prefix-list-out", "" + prflstOut);
        cmds.cfgLine(l, roumapIn == null, beg, nei + "route-map-in", "" + roumapIn);
        cmds.cfgLine(l, roumapOut == null, beg, nei + "route-map-out", "" + roumapOut);
        cmds.cfgLine(l, roupolIn == null, beg, nei + "route-policy-in", "" + roupolIn);
        cmds.cfgLine(l, roupolOut == null, beg, nei + "route-policy-out", "" + roupolOut);
        cmds.cfgLine(l, oprflstIn == null, beg, nei + "other-prefix-list-in", "" + oprflstIn);
        cmds.cfgLine(l, oprflstOut == null, beg, nei + "other-prefix-list-out", "" + oprflstOut);
        cmds.cfgLine(l, oroumapIn == null, beg, nei + "other-route-map-in", "" + oroumapIn);
        cmds.cfgLine(l, oroumapOut == null, beg, nei + "other-route-map-out", "" + oroumapOut);
        cmds.cfgLine(l, oroupolIn == null, beg, nei + "other-route-policy-in", "" + oroupolIn);
        cmds.cfgLine(l, oroupolOut == null, beg, nei + "other-route-policy-out", "" + oroupolOut);
        cmds.cfgLine(l, vroumapIn == null, beg, nei + "vpn-route-map-in", "" + vroumapIn);
        cmds.cfgLine(l, vroumapOut == null, beg, nei + "vpn-route-map-out", "" + vroumapOut);
        cmds.cfgLine(l, vroupolIn == null, beg, nei + "vpn-route-policy-in", "" + vroupolIn);
        cmds.cfgLine(l, vroupolOut == null, beg, nei + "vpn-route-policy-out", "" + vroupolOut);
        cmds.cfgLine(l, wroumapIn == null, beg, nei + "ovpn-route-map-in", "" + wroumapIn);
        cmds.cfgLine(l, wroumapOut == null, beg, nei + "ovpn-route-map-out", "" + wroumapOut);
        cmds.cfgLine(l, wroupolIn == null, beg, nei + "ovpn-route-policy-in", "" + wroupolIn);
        cmds.cfgLine(l, wroupolOut == null, beg, nei + "ovpn-route-policy-out", "" + wroupolOut);
        if ((filter & 1) == 0) {
            return l;
        }
        if (template == null) {
            return l;
        }
        List<String> t = template.getParamCfg(beg, nei, filter);
        t = userFilter.filterText(t, cfgRtr.defaultF);
        tabGen<userFilter> f = new tabGen<userFilter>();
        for (int i = 1; i < t.size(); i++) {
            String a = t.get(i);
            a = a.replaceAll("\\$", ".");
            f.add(new userFilter("", a, null));
        }
        l = userFilter.filterText(l, f);
        return l;
    }

    /**
     * flap connection
     */
    public abstract void flapBgpConn();

    /**
     * template configuration
     *
     * @param cmd command to do
     * @param negated negated
     */
    public abstract void doTempCfg(String cmd, boolean negated);

    /**
     * get configuration
     *
     * @param l list to append
     * @param beg beginning
     * @param filter filter defaults
     */
    public abstract void getConfig(List<String> l, String beg, int filter);

    /**
     * check safe ebgp policy
     *
     * @return true if violating, false if correct
     */
    public boolean checkSafeEbgp() {
        if (!lower.safeEbgp) {
            return false;
        }
        if (remoteAs == localAs) {
            return false;
        }
        if ((addrFams & mskUni) != 0) {
            if ((roumapIn == null) && (roupolIn == null) && (prflstIn == null)) {
                return true;
            }
            if ((roumapOut == null) && (roupolOut == null) && (prflstOut == null)) {
                return true;
            }
        }
        if ((addrFams & mskOtrU) != 0) {
            if ((oroumapIn == null) && (oroupolIn == null) && (oprflstIn == null)) {
                return true;
            }
            if ((oroumapOut == null) && (oroupolOut == null) && (oprflstOut == null)) {
                return true;
            }
        }
        return false;
    }

    /**
     * parse configuration command
     *
     * @param cmd command to parse
     * @param negated negated
     * @return false on success, true on error
     */
    public boolean setParamCfg(cmds cmd, boolean negated) {
        String s = cmd.word();
        if (s.equals("template")) {
            if (negated) {
                template = null;
                return false;
            }
            if (isTemplate) {
                return true;
            }
            rtrBgpTemp t = lower.findTemp(cmd.word());
            if (t == null) {
                cmd.error("no such template");
                return false;
            }
            copyFrom(t);
            template = t;
            shutdown |= cmd.word().equals("shutdown");
            shutdown |= checkSafeEbgp();
            return false;
        }
        if (isTemplate) {
            doTempCfg(s + " " + cmd.getRemaining(), negated);
        }
        if (s.equals("remote-as")) {
            remoteAs = bits.str2num(cmd.word());
            if (negated) {
                remoteAs = 0;
                return false;
            }
            if (remoteAs == localAs) {
                distance = lower.distantInt;
            } else {
                distance = lower.distantExt;
            }
            shutdown |= cmd.word().equals("shutdown");
            shutdown |= checkSafeEbgp();
            return false;
        }
        if (s.equals("local-as")) {
            localAs = bits.str2num(cmd.word());
            if (negated) {
                localAs = lower.localAs;
            }
            return false;
        }
        if (s.equals("address-family")) {
            addrFams = rtrBgpParam.string2mask(cmd);
            if (negated) {
                addrFams = lower.addrFams;
            }
            return false;
        }
        if (s.equals("update-source")) {
            if (negated) {
                srcIface = null;
                return false;
            }
            srcIface = cfgAll.ifcFind(cmd.word(), false);
            if (srcIface == null) {
                cmd.error("no such interface");
            }
            return false;
        }
        if (s.equals("other-address")) {
            if (negated) {
                otherAdr = null;
                return false;
            }
            otherAdr = new addrIP();
            otherAdr.fromString(cmd.word());
            return false;
        }
        if (s.equals("dump")) {
            if (negated) {
                dump = null;
                return false;
            }
            rtrBgpMrt mon = new rtrBgpMrt();
            mon.dumpName = cmd.word();
            dump = lower.dmps.find(mon);
            if (dump == null) {
                cmd.error("no such dump");
            }
            return false;
        }
        if (s.equals("monitor")) {
            if (negated) {
                monitor = null;
                return false;
            }
            rtrBgpMon mon = new rtrBgpMon(null);
            mon.monName = cmd.word();
            monitor = lower.mons.find(mon);
            if (monitor == null) {
                cmd.error("no such monitor");
            }
            return false;
        }
        if (s.equals("password")) {
            if (negated) {
                passwd = null;
                return false;
            }
            passwd = authLocal.passwdDecode(cmd.getRemaining());
            return false;
        }
        if (s.equals("description")) {
            if (negated) {
                description = null;
                return false;
            }
            description = cmd.getRemaining();
            return false;
        }
        if (s.equals("distance")) {
            distance = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("dmz-link-bw")) {
            dmzLinkBw = bits.str2num(cmd.word());
            if (negated) {
                dmzLinkBw = -1;
            }
            return false;
        }
        if (s.equals("timer")) {
            keepAlive = bits.str2num(cmd.word());
            holdTimer = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("bfd")) {
            bfdTrigger = !negated;
            return false;
        }
        if (s.equals("soft-reconfiguration")) {
            softReconfig = !negated;
            return false;
        }
        if (s.equals("extended-nexthop-current")) {
            extNextCur = rtrBgpParam.string2mask(cmd);
            if (negated) {
                extNextCur = 0;
            }
            return false;
        }
        if (s.equals("extended-nexthop-other")) {
            extNextOtr = rtrBgpParam.string2mask(cmd);
            if (negated) {
                extNextOtr = 0;
            }
            return false;
        }
        if (s.equals("multiple-labels")) {
            multiLabel = rtrBgpParam.string2mask(cmd);
            if (negated) {
                multiLabel = 0;
            }
            return false;
        }
        if (s.equals("graceful-restart")) {
            graceRestart = rtrBgpParam.string2mask(cmd);
            if (negated) {
                graceRestart = 0;
            }
            return false;
        }
        if (s.equals("hostname")) {
            if (negated) {
                hostname = 0;
                return false;
            }
            hostname = 1;
            s = cmd.word();
            if (s.equals("domain")) {
                hostname = 2;
            }
            return false;
        }
        if (s.equals("extended-open")) {
            extOpen = !negated;
            return false;
        }
        if (s.equals("extended-update")) {
            extUpdate = !negated;
            return false;
        }
        if (s.equals("unidirection")) {
            unidirection = !negated;
            return false;
        }
        if (s.equals("shutdown")) {
            shutdown = !negated;
            shutdown |= checkSafeEbgp();
            if (shutdown) {
                flapBgpConn();
            }
            return false;
        }
        if (s.equals("connection-mode")) {
            socketMode = 3;
            if (negated) {
                return false;
            }
            s = cmd.word();
            if (s.equals("active")) {
                socketMode = 1;
            }
            if (s.equals("passive")) {
                socketMode = 2;
            }
            return false;
        }
        if (s.equals("compression")) {
            compressMode = 0;
            if (negated) {
                return false;
            }
            s = cmd.word();
            if (s.equals("receive")) {
                compressMode = 1;
            }
            if (s.equals("transmit")) {
                compressMode = 2;
            }
            if (s.equals("both")) {
                compressMode = 3;
            }
            return false;
        }
        if (s.equals("advertisement-interval-tx")) {
            advertIntTx = bits.str2num(cmd.word());
            if (negated) {
                advertIntTx = 0;
            }
            return false;
        }
        if (s.equals("advertisement-interval-rx")) {
            advertIntRx = bits.str2num(cmd.word());
            if (negated) {
                advertIntRx = 0;
            }
            return false;
        }
        if (s.equals("ttl-security")) {
            ttlSecurity = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("buffer-size")) {
            bufferSize = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("additional-path-rx")) {
            addpathRmode = rtrBgpParam.string2mask(cmd);
            if (negated) {
                addpathRmode = 0;
                return false;
            }
            return false;
        }
        if (s.equals("additional-path-tx")) {
            addpathTmode = rtrBgpParam.string2mask(cmd);
            if (negated) {
                addpathTmode = 0;
                return false;
            }
            return false;
        }
        if (s.equals("internal-vpn-client")) {
            intVpnClnt = !negated;
            return false;
        }
        if (s.equals("allow-as-in")) {
            allowAsIn = !negated;
            return false;
        }
        if (s.equals("allow-as-out")) {
            allowAsOut = !negated;
            return false;
        }
        if (s.equals("enforce-first-as")) {
            enforceFirst = !negated;
            return false;
        }
        if (s.equals("dampening")) {
            dampenWthd = bits.str2num(cmd.word());
            dampenAnno = bits.str2num(cmd.word());
            dampenMinp = bits.str2num(cmd.word());
            dampenMaxp = bits.str2num(cmd.word());
            dampenSupp = bits.str2num(cmd.word());
            dampenReus = bits.str2num(cmd.word());
            dampenHalf = bits.str2num(cmd.word());
            dampenPfxs = new tabGen<rtrBgpDamp>();
            if (!negated) {
                return false;
            }
            dampenWthd = 0;
            dampenAnno = 0;
            dampenMinp = 0;
            dampenMaxp = 0;
            dampenSupp = 0;
            dampenReus = 0;
            dampenHalf = 0;
            dampenPfxs = null;
            return false;
        }
        if (s.equals("maximum-prefix")) {
            maxPrefixCnt = bits.str2num(cmd.word());
            maxPrefixPrc = bits.str2num(cmd.word());
            if (!negated) {
                return false;
            }
            maxPrefixCnt = 0;
            maxPrefixPrc = 0;
            return false;
        }
        if (s.equals("route-server-client")) {
            serverClnt = !negated;
            return false;
        }
        if (s.equals("aigp")) {
            accIgp = !negated;
            return false;
        }
        if (s.equals("traffeng")) {
            traffEng = !negated;
            return false;
        }
        if (s.equals("pmsitun")) {
            pmsiTun = !negated;
            return false;
        }
        if (s.equals("tunenc")) {
            tunEnc = !negated;
            return false;
        }
        if (s.equals("linkstate")) {
            lnkSta = !negated;
            return false;
        }
        if (s.equals("attribset")) {
            attribSet = !negated;
            return false;
        }
        if (s.equals("segrout")) {
            segRout = !negated;
            return false;
        }
        if (s.equals("bier")) {
            bier = !negated;
            return false;
        }
        if (s.equals("role")) {
            s = cmd.word();
            leakRole = -1;
            leakAttr = false;
            if (s.equals("disabled")) {
                leakRole = -1;
            }
            if (s.equals("provider")) {
                leakRole = rtrBgpUtil.roleProv;
            }
            if (s.equals("ix-server")) {
                leakRole = rtrBgpUtil.roleRs;
            }
            if (s.equals("ix-client")) {
                leakRole = rtrBgpUtil.roleRsc;
            }
            if (s.equals("customer")) {
                leakRole = rtrBgpUtil.roleCust;
            }
            if (s.equals("peer")) {
                leakRole = rtrBgpUtil.rolePeer;
            }
            if (s.equals("attrib")) {
                leakAttr = true;
            }
            leakAttr |= leakRole >= 0;
            s = cmd.word();
            leakForce = s.equals("enforce");
            if (!negated) {
                return false;
            }
            leakAttr = false;
            leakForce = false;
            leakRole = -1;
            return false;
        }
        if (s.equals("egress-engineering")) {
            egressEng = bits.str2num(cmd.word());
            if (!negated) {
                return false;
            }
            egressEng = 0;
            return false;
        }
        if (s.equals("label-pop")) {
            labelPop = !negated;
            return false;
        }
        if (s.equals("capability-negotiation")) {
            capaNego = !negated;
            return false;
        }
        if (s.equals("track-next-hop")) {
            trackNxthop = !negated;
            return false;
        }
        if (s.equals("ungroup-remoteas")) {
            ungrpRemAs = !negated;
            return false;
        }
        if (s.equals("remove-private-as-out")) {
            removePrivAsOut = !negated;
            return false;
        }
        if (s.equals("remove-private-as-in")) {
            removePrivAsIn = !negated;
            return false;
        }
        if (s.equals("override-peer-as-out")) {
            overridePeerOut = !negated;
            return false;
        }
        if (s.equals("override-peer-as-in")) {
            overridePeerIn = !negated;
            return false;
        }
        if (s.equals("fall-over")) {
            fallOver = !negated;
            return false;
        }
        if (s.equals("confederation-peer")) {
            remoteConfed = !negated;
            return false;
        }
        if (s.equals("next-hop-unchanged")) {
            nxtHopUnchgd = !negated;
            return false;
        }
        if (s.equals("next-hop-self")) {
            nxtHopSelf = !negated;
            return false;
        }
        if (s.equals("next-hop-peer")) {
            nxtHopPeer = !negated;
            return false;
        }
        if (s.equals("send-community")) {
            sendCommunity = 0;
            if (negated) {
                return false;
            }
            for (;;) {
                s = cmd.word();
                if (s.length() < 1) {
                    break;
                }
                if (s.equals("standard")) {
                    sendCommunity |= 1;
                    continue;
                }
                if (s.equals("extended")) {
                    sendCommunity |= 2;
                    continue;
                }
                if (s.equals("large")) {
                    sendCommunity |= 4;
                    continue;
                }
                if (s.equals("none")) {
                    sendCommunity = 0;
                    continue;
                }
                if (s.equals("both")) {
                    sendCommunity |= 3;
                    continue;
                }
                if (s.equals("all")) {
                    sendCommunity |= 7;
                    continue;
                }
            }
            return false;
        }
        if (s.equals("other-default-originate")) {
            sendOtrDefRou = !negated;
            return false;
        }
        if (s.equals("default-originate")) {
            sendDefRou = !negated;
            return false;
        }
        if (s.equals("route-reflector-client")) {
            reflectClnt = !negated;
            return false;
        }
        if (s.equals("prefix-list-in")) {
            if (negated) {
                prflstIn = null;
                return false;
            }
            cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such prefix list");
                return false;
            }
            prflstIn = ntry.prflst;
            return false;
        }
        if (s.equals("prefix-list-out")) {
            if (negated) {
                prflstOut = null;
                return false;
            }
            cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such prefix list");
                return false;
            }
            prflstOut = ntry.prflst;
            return false;
        }
        if (s.equals("route-map-in")) {
            if (negated) {
                roumapIn = null;
                return false;
            }
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return false;
            }
            roumapIn = ntry.roumap;
            return false;
        }
        if (s.equals("route-map-out")) {
            if (negated) {
                roumapOut = null;
                return false;
            }
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return false;
            }
            roumapOut = ntry.roumap;
            return false;
        }
        if (s.equals("route-policy-in")) {
            if (negated) {
                roupolIn = null;
                return false;
            }
            cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route policy");
                return false;
            }
            roupolIn = ntry.rouplc;
            return false;
        }
        if (s.equals("route-policy-out")) {
            if (negated) {
                roupolOut = null;
                return false;
            }
            cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route policy");
                return false;
            }
            roupolOut = ntry.rouplc;
            return false;
        }
        if (s.equals("other-prefix-list-in")) {
            if (negated) {
                oprflstIn = null;
                return false;
            }
            cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such prefix list");
                return false;
            }
            oprflstIn = ntry.prflst;
            return false;
        }
        if (s.equals("other-prefix-list-out")) {
            if (negated) {
                oprflstOut = null;
                return false;
            }
            cfgPrfxlst ntry = cfgAll.prfxFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such prefix list");
                return false;
            }
            oprflstOut = ntry.prflst;
            return false;
        }
        if (s.equals("other-route-map-in")) {
            if (negated) {
                oroumapIn = null;
                return false;
            }
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return false;
            }
            oroumapIn = ntry.roumap;
            return false;
        }
        if (s.equals("other-route-map-out")) {
            if (negated) {
                oroumapOut = null;
                return false;
            }
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return false;
            }
            oroumapOut = ntry.roumap;
            return false;
        }
        if (s.equals("other-route-policy-in")) {
            if (negated) {
                oroupolIn = null;
                return false;
            }
            cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route policy");
                return false;
            }
            oroupolIn = ntry.rouplc;
            return false;
        }
        if (s.equals("other-route-policy-out")) {
            if (negated) {
                oroupolOut = null;
                return false;
            }
            cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route policy");
                return false;
            }
            oroupolOut = ntry.rouplc;
            return false;
        }
        if (s.equals("vpn-route-map-in")) {
            if (negated) {
                vroumapIn = null;
                return false;
            }
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return false;
            }
            vroumapIn = ntry.roumap;
            return false;
        }
        if (s.equals("vpn-route-map-out")) {
            if (negated) {
                vroumapOut = null;
                return false;
            }
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return false;
            }
            vroumapOut = ntry.roumap;
            return false;
        }
        if (s.equals("vpn-route-policy-in")) {
            if (negated) {
                vroupolIn = null;
                return false;
            }
            cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route policy");
                return false;
            }
            vroupolIn = ntry.rouplc;
            return false;
        }
        if (s.equals("vpn-route-policy-out")) {
            if (negated) {
                vroupolOut = null;
                return false;
            }
            cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route policy");
                return false;
            }
            vroupolOut = ntry.rouplc;
            return false;
        }
        if (s.equals("ovpn-route-map-in")) {
            if (negated) {
                wroumapIn = null;
                return false;
            }
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return false;
            }
            wroumapIn = ntry.roumap;
            return false;
        }
        if (s.equals("ovpn-route-map-out")) {
            if (negated) {
                wroumapOut = null;
                return false;
            }
            cfgRoump ntry = cfgAll.rtmpFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route map");
                return false;
            }
            wroumapOut = ntry.roumap;
            return false;
        }
        if (s.equals("ovpn-route-policy-in")) {
            if (negated) {
                wroupolIn = null;
                return false;
            }
            cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route policy");
                return false;
            }
            wroupolIn = ntry.rouplc;
            return false;
        }
        if (s.equals("ovpn-route-policy-out")) {
            if (negated) {
                wroupolOut = null;
                return false;
            }
            cfgRouplc ntry = cfgAll.rtplFind(cmd.word(), false);
            if (ntry == null) {
                cmd.error("no such route policy");
                return false;
            }
            wroupolOut = ntry.rouplc;
            return false;
        }
        return true;
    }

}
