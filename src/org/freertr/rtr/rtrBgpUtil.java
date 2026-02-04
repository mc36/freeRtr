package org.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabRouteEntry;
import org.freertr.util.bits;
import org.freertr.enc.encTlv;
import org.freertr.ip.ipCor4;
import org.freertr.ip.ipCor6;
import org.freertr.tab.tabGen;
import org.freertr.util.counter;

/**
 * bgp4 utilities
 *
 * @author matecsaba
 */
public class rtrBgpUtil {

    private rtrBgpUtil() {
    }

    /**
     * marker value
     */
    public final static int markV = 255;

    /**
     * marker size
     */
    public final static int markS = 16;

    /**
     * header size
     */
    public final static int sizeU = 19;

    /**
     * compressed header size
     */
    public final static int sizeC = 3;

    /**
     * protocol version
     */
    public final static int version = 4;

    /**
     * ipv4 address family
     */
    public final static int afiIpv4 = 0x10000;

    /**
     * ipv6 address family
     */
    public final static int afiIpv6 = 0x20000;

    /**
     * nsap address family
     */
    public final static int afiNsap = 0x30000;

    /**
     * hdlc address family
     */
    public final static int afiHdlc = 0x40000;

    /**
     * ethernet address family
     */
    public final static int afiE802 = 0x60000;

    /**
     * telephony address family
     */
    public final static int afiE163 = 0x70000;

    /**
     * x25 address family
     */
    public final static int afiX25 = 0xa0000;

    /**
     * ipx address family
     */
    public final static int afiIpx = 0xb0000;

    /**
     * appletalk address family
     */
    public final static int afiApple = 0xc0000;

    /**
     * decnet address family
     */
    public final static int afiDecnet = 0xd0000;

    /**
     * banyanvines address family
     */
    public final static int afiBanyan = 0xe0000;

    /**
     * domain address family
     */
    public final static int afiDns = 0x100000;

    /**
     * name address family
     */
    public final static int afiName = 0x110000;

    /**
     * asn address family
     */
    public final static int afiAsn = 0x120000;

    /**
     * l2vpn address family
     */
    public final static int afiL2vpn = 0x190000;

    /**
     * mpls tp sel end id address family
     */
    public final static int afiMtpSel = 0x1a0000;

    /**
     * mpls tp lsp end id address family
     */
    public final static int afiMtpLsp = 0x1b0000;

    /**
     * mpls tp pwe end id address family
     */
    public final static int afiMtpPwe = 0x1c0000;

    /**
     * multi topology ipv4 address family
     */
    public final static int afiMtIp4 = 0x1d0000;

    /**
     * multi topology ipv6 address family
     */
    public final static int afiMtIp6 = 0x1e0000;

    /**
     * nsh address family
     */
    public final static int afiNsh = 0x1f0000;

    /**
     * linkstate address family
     */
    public final static int afiLnks = 0x40040000;

    /**
     * rpd address family
     */
    public final static int afiRpd = 0x400e0000;

    /**
     * mpls address family
     */
    public final static int afiMpls = 0x400f0000;

    /**
     * address family mask
     */
    public final static int afiMask = 0xffff0000;

    /**
     * sub address family mask
     */
    public final static int sfiMask = 0xffff;

    /**
     * refresh mask
     */
    public final static int frsMask = 0xffff00ff;

    /**
     * unicast address family
     */
    public final static int sfiUnicast = 0x01;

    /**
     * multicast address family
     */
    public final static int sfiMulticast = 0x02;

    /**
     * labeled address family
     */
    public final static int sfiLabeled = 0x04;

    /**
     * multicast vpn address family
     */
    public final static int sfiMvpn = 0x05;

    /**
     * dynamic multi segment pswudowires address family
     */
    public final static int sfiMspw = 0x06;

    /**
     * tunnel encapsulation address family
     */
    public final static int sfiEncap = 0x07;

    /**
     * multicast vpls address family
     */
    public final static int sfiMcastVpls = 0x08;

    /**
     * nsh address family
     */
    public final static int sfiNsh = 0x09;

    /**
     * tunnel address family
     */
    public final static int sfiTunnel = 0x40;

    /**
     * l2vpn address family
     */
    public final static int sfiVpls = 0x41;

    /**
     * mdt address family
     */
    public final static int sfiMdt = 0x42;

    /**
     * 4over6 address family
     */
    public final static int sfi4o6 = 0x43;

    /**
     * 6over4 address family
     */
    public final static int sfi6o4 = 0x44;

    /**
     * layer1 vpn address family
     */
    public final static int sfiL1vpn = 0x45;

    /**
     * ethernet vpn address family
     */
    public final static int sfiEthVpn = 0x46;

    /**
     * link state address family
     */
    public final static int sfiLnkSt = 0x47;

    /**
     * link state vpn address family
     */
    public final static int sfiVpnLnkSt = 0x48;

    /**
     * segment routing traffic engineering address family
     */
    public final static int sfiSrTe = 0x49;

    /**
     * sd-wan capability address family
     */
    public final static int sfiSdwan = 0x4a;

    /**
     * rpd address family
     */
    public final static int sfiRpd = 0x4b;

    /**
     * classful transport plane address family
     */
    public final static int sfiClsTrnPl = 0x4c;

    /**
     * tunneled flowspec address family
     */
    public final static int sfiTunFlw = 0x4d;

    /**
     * mcast tree address family
     */
    public final static int sfiMcsTre = 0x4e;

    /**
     * dynamic path selection address family
     */
    public final static int sfiDps = 0x4f;

    /**
     * shortest path first address family
     */
    public final static int sfiSpf = 0x50;

    /**
     * color aware routing address family
     */
    public final static int sfiClrAwRtg = 0x53;

    /**
     * car vpn address family
     */
    public final static int sfiCarVpn = 0x54;

    /**
     * mobile user plane address family
     */
    public final static int sfiMobUsrPln = 0x55;

    /**
     * vpn unicast address family
     */
    public final static int sfiMplsVpnU = 0x80;

    /**
     * vpn multicast address family
     */
    public final static int sfiMplsVpnM = 0x81;

    /**
     * route target constrain address family
     */
    public final static int sfiRtFltr = 0x84;

    /**
     * flow specification address family
     */
    public final static int sfiFlwSpc = 0x85;

    /**
     * mpls vpn flowspec address family
     */
    public final static int sfiVpnFlw = 0x86;

    /**
     * vpn auto discovery address family
     */
    public final static int sfiAutDsc = 0x8c;

    /**
     * ipv4 unicast address family
     */
    public final static int safiIp4uni = afiIpv4 | sfiUnicast;

    /**
     * ipv6 unicast address family
     */
    public final static int safiIp6uni = afiIpv6 | sfiUnicast;

    /**
     * ipv4 multicast address family
     */
    public final static int safiIp4multi = afiIpv4 | sfiMulticast;

    /**
     * ipv6 multicast address family
     */
    public final static int safiIp6multi = afiIpv6 | sfiMulticast;

    /**
     * ipv4 labeled address family
     */
    public final static int safiIp4lab = afiIpv4 | sfiLabeled;

    /**
     * ipv6 labeled address family
     */
    public final static int safiIp6lab = afiIpv6 | sfiLabeled;

    /**
     * ipv4 classful transport plane address family
     */
    public final static int safiIp4ctp = afiIpv4 | sfiClsTrnPl;

    /**
     * ipv6 classful transport plane address family
     */
    public final static int safiIp6ctp = afiIpv6 | sfiClsTrnPl;

    /**
     * ipv4 color aware routing address family
     */
    public final static int safiIp4car = afiIpv4 | sfiClrAwRtg;

    /**
     * ipv6 color aware routing plane address family
     */
    public final static int safiIp6car = afiIpv6 | sfiClrAwRtg;

    /**
     * ipv4 flowspec address family
     */
    public final static int safiIp4flow = afiIpv4 | sfiFlwSpc;

    /**
     * ipv6 flowspec address family
     */
    public final static int safiIp6flow = afiIpv6 | sfiFlwSpc;

    /**
     * ipv4 labeled vpn unicast address family
     */
    public final static int safiIp4vpnU = afiIpv4 | sfiMplsVpnU;

    /**
     * ipv6 labeled vpn unicast address family
     */
    public final static int safiIp6vpnU = afiIpv6 | sfiMplsVpnU;

    /**
     * ipv4 labeled vpn multicast address family
     */
    public final static int safiIp4vpnM = afiIpv4 | sfiMplsVpnM;

    /**
     * ipv6 labeled vpn multicast address family
     */
    public final static int safiIp6vpnM = afiIpv6 | sfiMplsVpnM;

    /**
     * ipv4 vpn flowspec address family
     */
    public final static int safiIp4vpnF = afiIpv4 | sfiVpnFlw;

    /**
     * ipv6 vpn flowspec address family
     */
    public final static int safiIp6vpnF = afiIpv6 | sfiVpnFlw;

    /**
     * nsh address family
     */
    public final static int safiNsh46 = afiNsh | sfiNsh;

    /**
     * rpd address family
     */
    public final static int safiRpd46 = afiRpd | sfiRpd;

    /**
     * rtfilter address family
     */
    public final static int safiRtf46 = afiIpv4 | sfiRtFltr;

    /**
     * ipv4 mdt address family
     */
    public final static int safiIp4mdt = afiIpv4 | sfiMdt;

    /**
     * ipv6 mdt address family
     */
    public final static int safiIp6mdt = afiIpv6 | sfiMdt;

    /**
     * ipv4 sdwan address family
     */
    public final static int safiIp4sdwan = afiIpv4 | sfiSdwan;

    /**
     * ipv6 sdwan address family
     */
    public final static int safiIp6sdwan = afiIpv6 | sfiSdwan;

    /**
     * ipv4 mup address family
     */
    public final static int safiIp4mup = afiIpv4 | sfiMobUsrPln;

    /**
     * ipv6 mup address family
     */
    public final static int safiIp6mup = afiIpv6 | sfiMobUsrPln;

    /**
     * ipv4 tunnel address family
     */
    public final static int safiIp4tun = afiIpv4 | sfiTunnel;

    /**
     * ipv6 tunnel address family
     */
    public final static int safiIp6tun = afiIpv6 | sfiTunnel;

    /**
     * ipv4 encap address family
     */
    public final static int safiIp4enc = afiIpv4 | sfiEncap;

    /**
     * ipv6 encap address family
     */
    public final static int safiIp6enc = afiIpv6 | sfiEncap;

    /**
     * ipv4/ipv6 link state address family
     */
    public final static int safiIp46lnks = afiLnks | sfiLnkSt;

    /**
     * ipv4/ipv6 vpn link state address family
     */
    public final static int safiIp46vpnL = afiLnks | sfiVpnLnkSt;

    /**
     * ipv4/ipv6 shortest path first address family
     */
    public final static int safiIp46spf = afiLnks | sfiSpf;

    /**
     * ipv4 srte address family
     */
    public final static int safiIp4srte = afiIpv4 | sfiSrTe;

    /**
     * ipv6 srte address family
     */
    public final static int safiIp6srte = afiIpv6 | sfiSrTe;

    /**
     * ipv4 mvpn address family
     */
    public final static int safiIp4mvpn = afiIpv4 | sfiMvpn;

    /**
     * ipv6 mvpn address family
     */
    public final static int safiIp6mvpn = afiIpv6 | sfiMvpn;

    /**
     * ipv4/ipv6 vpls address family
     */
    public final static int safiVpls46 = afiL2vpn | sfiVpls;

    /**
     * ipv4/ipv6 mspw address family
     */
    public final static int safiMspw46 = afiL2vpn | sfiMspw;

    /**
     * ipv4/ipv6 ethvpn address family
     */
    public final static int safiEvpn46 = afiL2vpn | sfiEthVpn;

    /**
     * ipv4 multicast tree address family
     */
    public final static int safiIp4mtree = afiIpv4 | sfiMcsTre;

    /**
     * ipv6 multicast tree address family
     */
    public final static int safiIp6mtree = afiIpv6 | sfiMcsTre;

    /**
     * self originate
     */
    public final static int peerOriginate = 255;

    /**
     * internal peer
     */
    public final static int peerIntrn = 1;

    /**
     * route reflector client
     */
    public final static int peerRflct = 2;

    /**
     * confederation peer
     */
    public final static int peerCnfed = 3;

    /**
     * external peer
     */
    public final static int peerExtrn = 4;

    /**
     * route server client
     */
    public final static int peerServr = 5;

    /**
     * provider
     */
    public final static int roleProv = 0;

    /**
     * route server
     */
    public final static int roleRs = 1;

    /**
     * route server client
     */
    public final static int roleRsc = 2;

    /**
     * customer
     */
    public final static int roleCust = 3;

    /**
     * peer
     */
    public final static int rolePeer = 4;

    /**
     * open
     */
    public final static int msgOpen = 1;

    /**
     * update
     */
    public final static int msgUpdate = 2;

    /**
     * notification
     */
    public final static int msgNotify = 3;

    /**
     * keep alive
     */
    public final static int msgKeepLiv = 4;

    /**
     * route refresh
     */
    public final static int msgRefrsh = 5;

    /**
     * capabilities
     */
    public final static int msgCapability = 6;

    /**
     * compression
     */
    public final static int msgCompress = 7;

    /**
     * optional
     */
    public final static int flagOptional = 0x80;

    /**
     * transitive
     */
    public final static int flagTransitive = 0x40;

    /**
     * complete
     */
    public final static int flagComplete = 0x20;

    /**
     * length
     */
    public final static int flagLength = 0x10;

    /**
     * origin type, 0=igp, 1=egp, 2=incomplete
     */
    public final static int attrOrigin = 1;

    /**
     * as path
     */
    public final static int attrAsPath = 2;

    /**
     * next hop
     */
    public final static int attrNextHop = 3;

    /**
     * multi exit discriminator
     */
    public final static int attrMetric = 4;

    /**
     * local preference
     */
    public final static int attrLocPref = 5;

    /**
     * atomic aggregate
     */
    public final static int attrAtomicAggr = 6;

    /**
     * aggregate
     */
    public final static int attrAggregator = 7;

    /**
     * standard community
     */
    public final static int attrStdComm = 8;

    /**
     * originator
     */
    public final static int attrOriginator = 9;

    /**
     * cluster list
     */
    public final static int attrClustList = 10;

    /**
     * destination preference
     */
    public final static int attrDestPref = 11;

    /**
     * advertiser
     */
    public final static int attrAdvertiser = 12;

    /**
     * clusterid
     */
    public final static int attrRcidPath = 13;

    /**
     * mp reach nlri
     */
    public final static int attrReachable = 14;

    /**
     * mp unreach nlri
     */
    public final static int attrUnReach = 15;

    /**
     * extended community
     */
    public final static int attrExtComm = 16;

    /**
     * as4 path
     */
    public final static int attrAs4path = 17;

    /**
     * as4 aggregator
     */
    public final static int attrAs4aggr = 18;

    /**
     * safi specific
     */
    public final static int attrSafiSpec = 19;

    /**
     * connector attribute
     */
    public final static int attrConnector = 20;

    /**
     * pathlimit
     */
    public final static int attrPathLimit = 21;

    /**
     * pmsi tunnel
     */
    public final static int attrPmsiTun = 22;

    /**
     * tunnel encapsulation
     */
    public final static int attrTunEnc = 23;

    /**
     * traffic engineering
     */
    public final static int attrTraffEng = 24;

    /**
     * ipv6 extended community
     */
    public final static int attrIpv6comm = 25;

    /**
     * accumulated igp
     */
    public final static int attrAccIgp = 26;

    /**
     * pe distinguisher label
     */
    public final static int attrPeDistLab = 27;

    /**
     * entropy label
     */
    public final static int attrEntropyLab = 28;

    /**
     * link state
     */
    public final static int attrLinkState = 29;

    /**
     * large community
     */
    public final static int attrLrgComm = 32;

    /**
     * bgpsec path
     */
    public final static int attrBgpSec = 33;

    /**
     * community container
     */
    public final static int attrCommCntnr = 34;

    /**
     * only to customer
     */
    public final static int attrOnlyCust = 35;

    /**
     * domain path
     */
    public final static int attrDomPath = 36;

    /**
     * nsh service chains
     */
    public final static int attrNshChain = 37;

    /**
     * bfd discriminator
     */
    public final static int attrBfdDisc = 38;

    /**
     * next hop capabilities
     */
    public final static int attrHopCapa = 39;

    /**
     * prefix sid
     */
    public final static int attrPrefSid = 40;

    /**
     * bier
     */
    public final static int attrBier = 41;

    /**
     * attribute set
     */
    public final static int attrAttribSet = 128;

    /**
     * multiprotocol bgp
     */
    public final static int capaMultiProto = 1;

    /**
     * route refresh
     */
    public final static int capaRouteRefresh = 2;

    /**
     * outbound route filter
     */
    public final static int capaRouteFilter = 3;

    /**
     * multiple routes
     */
    public final static int capaMultiRoute = 4;

    /**
     * extended next hop
     */
    public final static int capaExtNextHop = 5;

    /**
     * extended message
     */
    public final static int capaExtMessage = 6;

    /**
     * bgpsec
     */
    public final static int capaBgpSec = 7;

    /**
     * multiple labels
     */
    public final static int capaMultiLabel = 8;

    /**
     * bgp role
     */
    public final static int capaLeakRole = 9;

    /**
     * graceful restart
     */
    public final static int capaGraceRestart = 64;

    /**
     * 32bit as number
     */
    public final static int capa32bitAsNum = 65;

    /**
     * dynamic capability
     */
    public final static int capaDynamicCapa = 67;

    /**
     * multisession
     */
    public final static int capaMultisession = 68;

    /**
     * additional path
     */
    public final static int capaAdditionPath = 69;

    /**
     * enhanced route refresh
     */
    public final static int capaEnhancedRefresh = 70;

    /**
     * long lived graceful restart
     */
    public final static int capaLongGrace = 71;

    /**
     * routing policy distribution
     */
    public final static int capaRoutePolicy = 72;

    /**
     * hostname
     */
    public final static int capaHostname = 73;

    /**
     * strict bfd
     */
    public final static int capaStrictBfd = 74;

    /**
     * software version
     */
    public final static int capaSoftware = 75;

    /**
     * compression
     */
    public final static int capaCompress = 76;

    /**
     * link local
     */
    public final static int capaLinkLocal = 77;

    /**
     * no export community
     */
    public final static int commNoExport = 0xffffff01;

    /**
     * no advertise community
     */
    public final static int commNoAdvertise = 0xffffff02;

    /**
     * no sub confederation community
     */
    public final static int commNoConfed = 0xffffff03;

    /**
     * no peer community
     */
    public final static int commNoPeer = 0xffffff04;

    /**
     * graceful shutdown community
     */
    public final static int commGraceShut = 0xffff0000;

    /**
     * accept own community
     */
    public final static int commAcceptOwn = 0xffff0001;

    /**
     * long lived graceful restart community
     */
    public final static int commLlgrStale = 0xffff0006;

    /**
     * no llgr community
     */
    public final static int commNoLlgr = 0xffff0007;

    /**
     * accept own nexthop community
     */
    public final static int commAcceptHop = 0xffff0008;

    /**
     * blackhole community
     */
    public final static int commBlackhole = 0xffff029a;

    /**
     * roa validity extended community
     */
    public final static int commValidRoa = 0x43000000;

    /**
     * aspa validity extended community
     */
    public final static int commValidAspa = 0x43030000;

    /**
     * check if an unknown message type
     *
     * @param i number to check
     * @return true if yes, false if not
     */
    public static boolean isUnknownMsg(int i) {
        if (i < msgOpen) {
            return true;
        }
        if (i > msgCompress) {
            return true;
        }
        return false;
    }

    /**
     * check if an unknown attribute type
     *
     * @param i number to check
     * @return true if yes, false if not
     */
    public static boolean isUnknownAttr(int i) {
        switch (i) {
            case attrOrigin:
            case attrAsPath:
            case attrNextHop:
            case attrMetric:
            case attrLocPref:
            case attrAtomicAggr:
            case attrAggregator:
            case attrConnector:
            case attrPeDistLab:
            case attrPathLimit:
            case attrNshChain:
            case attrDomPath:
            case attrBfdDisc:
            case attrHopCapa:
            case attrStdComm:
            case attrOriginator:
            case attrClustList:
            case attrReachable:
            case attrUnReach:
            case attrExtComm:
            case attrPmsiTun:
            case attrTunEnc:
            case attrTraffEng:
            case attrAccIgp:
            case attrEntropyLab:
            case attrLinkState:
            case attrLrgComm:
            case attrOnlyCust:
            case attrPrefSid:
            case attrBier:
            case attrAttribSet:
                return false;
            default:
                return true;
        }
    }

    /**
     * convert type to string
     *
     * @param i type to convert
     * @return string
     */
    public static String msgType2string(int i) {
        switch (i) {
            case msgOpen:
                return "open";
            case msgUpdate:
                return "update";
            case msgNotify:
                return "notify";
            case msgKeepLiv:
                return "keepalive";
            case msgRefrsh:
                return "routerefresh";
            case msgCapability:
                return "capability";
            case msgCompress:
                return "compressed";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * convert attribute type to string
     *
     * @param i type
     * @return string
     */
    public static String attrType2string(int i) {
        switch (i) {
            case attrOrigin:
                return "origin";
            case attrAsPath:
                return "aspath";
            case attrNextHop:
                return "nexthop";
            case attrMetric:
                return "metric";
            case attrLocPref:
                return "locpref";
            case attrAtomicAggr:
                return "atomicAggr";
            case attrAggregator:
                return "aggregator";
            case attrStdComm:
                return "stdComm";
            case attrOriginator:
                return "originator";
            case attrClustList:
                return "clustList";
            case attrDestPref:
                return "destPref";
            case attrAdvertiser:
                return "adverter";
            case attrRcidPath:
                return "rcidPath";
            case attrReachable:
                return "reachable";
            case attrUnReach:
                return "unreachable";
            case attrExtComm:
                return "extComm";
            case attrAs4path:
                return "as4path";
            case attrAs4aggr:
                return "as4aggr";
            case attrSafiSpec:
                return "safiSpec";
            case attrConnector:
                return "connector";
            case attrPathLimit:
                return "pathLim";
            case attrPmsiTun:
                return "pmtiTun";
            case attrTunEnc:
                return "tunEnc";
            case attrTraffEng:
                return "traffEng";
            case attrIpv6comm:
                return "ipv6comm";
            case attrAccIgp:
                return "accIgp";
            case attrPeDistLab:
                return "peDistLab";
            case attrEntropyLab:
                return "entropyLab";
            case attrLinkState:
                return "linkStates";
            case attrLrgComm:
                return "lrgComm";
            case attrBgpSec:
                return "bgpSec";
            case attrCommCntnr:
                return "commCont";
            case attrOnlyCust:
                return "onlyCust";
            case attrDomPath:
                return "domPath";
            case attrHopCapa:
                return "hopCapa";
            case attrPrefSid:
                return "prefixSid";
            case attrBier:
                return "bier";
            case attrAttribSet:
                return "attribSet";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * decode notify codes
     *
     * @param err error
     * @param sub subcode
     * @return string
     */
    public static String notify2string(int err, int sub) {
        switch (err) {
            case 1: // message header
                switch (sub) {
                    case 1:
                        return "unsynced";
                    case 2:
                        return "badLen";
                    case 3:
                        return "badType";
                    default:
                        return "header/" + sub;
                }
            case 2: // open message
                switch (sub) {
                    case 1:
                        return "badVer";
                    case 2:
                        return "badAs";
                    case 3:
                        return "badId";
                    case 4:
                        return "badParam";
                    case 6:
                        return "badTimer";
                    case 7:
                        return "badCapa";
                    case 8:
                        return "badRole";
                    default:
                        return "open/" + sub;
                }
            case 3: // update message
                switch (sub) {
                    case 1:
                        return "malformedAttr";
                    case 2:
                        return "unrecognizedAttr";
                    case 3:
                        return "missingAttr";
                    case 4:
                        return "flagErr";
                    case 5:
                        return "lengthErr";
                    case 6:
                        return "badOrigin";
                    case 8:
                        return "badNextHop";
                    case 9:
                        return "attrErr";
                    case 10:
                        return "badNetwork";
                    case 11:
                        return "badAsPath";
                    default:
                        return "update/" + sub;
                }
            case 4: // hold timer
                return "timer/" + sub;
            case 5: // fsm
                switch (sub) {
                    case 1:
                        return "openSent";
                    case 2:
                        return "openConfirm";
                    case 3:
                        return "established";
                    default:
                        return "fsm/" + sub;
                }
            case 6: // cease
                switch (sub) {
                    case 1:
                        return "muchPrefix";
                    case 2:
                        return "shutdown";
                    case 3:
                        return "deconfig";
                    case 4:
                        return "adminReset";
                    case 5:
                        return "reject";
                    case 6:
                        return "chgConfig";
                    case 7:
                        return "collision";
                    case 8:
                        return "outOfMem";
                    case 9:
                        return "hardReset";
                    case 10:
                        return "decompress";
                    default:
                        return "cease/" + sub;
                }
            case 7: // refresh
                switch (sub) {
                    case 1:
                        return "badLen";
                    default:
                        return "refresh/" + sub;
                }
            case 8: // capamsg
                switch (sub) {
                    case 1:
                        return "badSeq";
                    case 2:
                        return "badLen";
                    case 3:
                        return "malfrmd";
                    case 4:
                        return "unsupp";
                    default:
                        return "capamsg/" + sub;
                }
            default:
                return err + "/" + sub;
        }
    }

    /**
     * convert leak to inverted leak
     *
     * @param old old role
     * @return inverted role, -1 on error
     */
    public static int leakInverter(int old) {
        switch (old) {
            case roleProv:
                return roleCust;
            case roleRs:
                return roleRsc;
            case roleRsc:
                return roleRs;
            case roleCust:
                return roleProv;
            case rolePeer:
                return rolePeer;
            default:
                return -1;
        }
    }

    /**
     * convert rpki mode to string
     *
     * @param mode mode to convert
     * @return converted string
     */
    public static String rpkiMode2string(int mode) {
        switch (mode) {
            case 0:
                return "transparent";
            case 1:
                return "accept";
            case 2:
                return "rewrite";
            case 3:
                return "onlymiss";
            case 4:
                return "fix-unset";
            case 5:
                return "fix-valid";
            case 6:
                return "fix-invalid";
            case 7:
                return "fix-unknown";
            default:
                return "unknown=" + mode;
        }
    }

    /**
     * convert string to rpki mode
     *
     * @param a string to convert
     * @return numerical value
     */
    public static int string2rpkiMode(String a) {
        if (a.equals("transparent")) {
            return 0;
        }
        if (a.equals("accept")) {
            return 1;
        }
        if (a.equals("rewrite")) {
            return 2;
        }
        if (a.equals("onlymiss")) {
            return 3;
        }
        if (a.equals("fix-unset")) {
            return 4;
        }
        if (a.equals("fix-valid")) {
            return 5;
        }
        if (a.equals("fix-invalid")) {
            return 6;
        }
        if (a.equals("fix-unknown")) {
            return 7;
        }
        return 0;
    }

    /**
     * convert peer role to string
     *
     * @param role role
     * @param attr attrib
     * @return string
     */
    public static String leakRole2string(int role, boolean attr) {
        switch (role) {
            case roleProv:
                return "provider";
            case roleRs:
                return "ix-server";
            case roleRsc:
                return "ix-client";
            case roleCust:
                return "customer";
            case rolePeer:
                return "peer";
            default:
                if (attr) {
                    return "attrib";
                } else {
                    return "disabled";
                }
        }
    }

    /**
     * convert peer type to string
     *
     * @param i peer type
     * @return string
     */
    public static String peerType2string(int i) {
        switch (i) {
            case peerIntrn:
                return "internal";
            case peerRflct:
                return "routeReflectorClient";
            case peerCnfed:
                return "confederation";
            case peerExtrn:
                return "external";
            case peerServr:
                return "routeServerClient";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * convert 32bit safi to 24bit safi
     *
     * @param i 32bit safi
     * @return 24bit safi
     */
    public static int safi2triplet(int i) {
        return (i & 0xffff0000) | ((i & 0xff) << 8);
    }

    /**
     * convert 24bit safi to 32bit safi
     *
     * @param i 24bit safi
     * @return 32bit safi
     */
    public static int triplet2safi(int i) {
        return (i & 0xffff0000) | ((i >>> 8) & 0xff);
    }

    /**
     * place one capability
     *
     * @param pck target packet
     * @param ext extended
     * @param typ type
     * @param buf content
     */
    public static void placeCapability(packHolder pck, boolean ext, int typ, byte[] buf) {
        encTlv tlv = getCapabilityTlv(ext);
        tlv.valDat[0] = (byte) typ;
        tlv.valDat[1] = (byte) buf.length;
        bits.byteCopy(buf, 0, tlv.valDat, 2, buf.length);
        tlv.valSiz = buf.length + 2;
        tlv.valTyp = 2;
        tlv.putThis(pck);
    }

    /**
     * get capability tlv
     *
     * @param ext extended
     * @return tlv
     */
    public static encTlv getCapabilityTlv(boolean ext) {
        if (ext) {
            return new encTlv(0, 8, 8, 16, 1, 0, 3, 1, 0, 512, true);
        } else {
            return new encTlv(0, 8, 8, 8, 1, 0, 2, 1, 0, 512, true);
        }
    }

    /**
     * get prefix sid tlv
     *
     * @return tlv
     */
    public static encTlv getPrefSidTlv() {
        return new encTlv(0, 8, 8, 16, 1, 0, 3, 1, 0, 512, true);
    }

    /**
     * get bier tlv
     *
     * @return tlv
     */
    public static encTlv getBierTlv() {
        return new encTlv(0, 8, 8, 16, 1, 0, 3, 1, 0, 512, true);
    }

    /**
     * decode attribute set
     *
     * @param spkr where to signal
     * @param ntry table entry
     */
    public static void decodeAttribSet(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.attribVal == null) {
            return;
        }
        spkr.currAdd.clear();
        List<tabRouteEntry<addrIP>> del = new ArrayList<tabRouteEntry<addrIP>>();
        packHolder pck = new packHolder(true, true);
        packHolder cur = new packHolder(true, true);
        pck.putCopy(ntry.best.attribVal, 0, 0, ntry.best.attribVal.length);
        pck.putSkip(ntry.best.attribVal.length);
        pck.merge2beg();
        ntry.best.attribVal = null;
        ntry.best.attribAs = 0;
        for (;;) {
            if (pck.dataSize() < 1) {
                break;
            }
            if (rtrBgpAttr.parseAttrib(pck, cur)) {
                break;
            }
            rtrBgpAttr.interpretAttribute(spkr, ntry, del, cur);
        }
    }

    /**
     * encode attribute set
     *
     * @param spkr where to signal
     * @param idx address family
     * @param as as number
     * @param ntry table entry
     */
    public static void encodeAttribSet(rtrBgpSpeak spkr, int idx, int as, tabRouteEntry<addrIP> ntry) {
        packHolder pck = new packHolder(true, true);
        packHolder hlp = new packHolder(true, true);
        rtrBgpAttr.placeUnknown(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeOrigin(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeAsPath(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeMetric(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeLocPref(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeEntropyLab(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeAtomicAggr(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeAggregator(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeConnector(spkr, pck, hlp, ntry);
        rtrBgpAttr.placePathLimit(spkr, pck, hlp, ntry);
        rtrBgpAttr.placePeDistLab(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeStdComm(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeExtComm(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeLrgComm(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeOriginator(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeClustList(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeTraffEng(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeAccIgp(spkr, pck, hlp, ntry);
        rtrBgpAttr.placePmsiTun(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeTunEnc(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeLnkSta(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeOnlyCust(spkr, pck, hlp, ntry);
        rtrBgpAttr.placePrefSid(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeBier(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeNshChain(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeDomainPath(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeBfdDiscr(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeHopCapa(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeAttribSet(spkr, pck, hlp, ntry);
        pck.merge2beg();
        ntry.best.attribAs = as;
        ntry.best.attribVal = pck.getCopy();
    }

    /**
     * create withdraw message
     *
     * @param spkr where to signal
     * @param pck packet to update
     * @param hlp helper packet
     * @param idx address family
     * @param addpath additional path
     * @param lst list of prefixes to withdraw
     */
    public static void createWithdraw(rtrBgpSpeak spkr, packHolder pck, packHolder hlp, int idx, boolean addpath, List<tabRouteEntry<addrIP>> lst) {
        if (spkr.parent.idx2safi[idx] == safiIp4uni) {
            for (int i = 0; i < lst.size(); i++) {
                tabRouteEntry<addrIP> ntry = lst.get(i);
                if (addpath) {
                    pck.msbPutD(0, ntry.best.ident);
                    pck.putSkip(4);
                }
                rtrBgpAfi.ipv4uni.writePrefix(true, pck, ntry);
            }
            pck.merge2beg();
            pck.msbPutW(0, pck.dataSize());
            pck.putSkip(2);
            pck.merge2beg();
            pck.msbPutW(0, 0);
            pck.putSkip(2);
            pck.merge2end();
            return;
        }
        rtrBgpAttr.placeUnreach(spkr, idx, addpath, pck, hlp, lst);
        pck.merge2beg();
        pck.msbPutW(0, pck.dataSize());
        pck.putSkip(2);
        pck.merge2beg();
        pck.msbPutW(0, 0);
        pck.putSkip(2);
        pck.merge2beg();
    }

    /**
     * check if valid header received
     *
     * @param pck packet to read
     * @return true if error, false if ok
     */
    public static boolean checkMarker(packHolder pck) {
        for (int i = 0; i < markS; i++) {
            if (pck.getByte(i) != markV) {
                return true;
            }
        }
        return false;
    }

    /**
     * check if valid header received
     *
     * @param pck packet to read
     * @return true if error, false if ok
     */
    public static boolean checkHeader(packHolder pck) {
        if (checkMarker(pck)) {
            return true;
        }
        pck.IPsiz = pck.msbGetW(16) - sizeU;
        pck.IPprt = pck.getByte(18);
        if (pck.IPsiz < 0) {
            return true;
        }
        if (pck.IPsiz > packHolder.maxData) {
            return true;
        }
        return false;
    }

    /**
     * create message header
     *
     * @param pck packet to update
     */
    public static void createMarker(packHolder pck) {
        for (int i = 0; i < markS; i++) {
            pck.putByte(i, markV);
        }
    }

    /**
     * create message header
     *
     * @param pck packet to update
     * @param typ message type
     */
    public static void createHeader(packHolder pck, int typ) {
        createMarker(pck);
        pck.msbPutW(16, pck.dataSize() + sizeU);
        pck.putByte(18, typ);
        pck.putSkip(sizeU);
        pck.merge2beg();
    }

    /**
     * create reachable message
     *
     * @param spkr where to signal
     * @param pck packet to update
     * @param hlp helper packet
     * @param idx address family
     * @param addpath additional path
     * @param lst list of prefixes to advertise
     */
    public static void createReachable(rtrBgpSpeak spkr, packHolder pck, packHolder hlp, int idx, boolean addpath, List<tabRouteEntry<addrIP>> lst) {
        tabRouteEntry<addrIP> ntry = lst.get(0);
        rtrBgpAttr.placeUnknown(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeOrigin(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeAsPath(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeMetric(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeLocPref(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeEntropyLab(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeAtomicAggr(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeAggregator(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeConnector(spkr, pck, hlp, ntry);
        rtrBgpAttr.placePathLimit(spkr, pck, hlp, ntry);
        rtrBgpAttr.placePeDistLab(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeStdComm(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeExtComm(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeLrgComm(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeOriginator(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeClustList(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeTraffEng(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeAccIgp(spkr, pck, hlp, ntry);
        rtrBgpAttr.placePmsiTun(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeTunEnc(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeLnkSta(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeOnlyCust(spkr, pck, hlp, ntry);
        rtrBgpAttr.placePrefSid(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeBier(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeNshChain(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeDomainPath(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeBfdDiscr(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeHopCapa(spkr, pck, hlp, ntry);
        rtrBgpAttr.placeAttribSet(spkr, pck, hlp, ntry);
        if (spkr.parent.idx2safi[idx] != safiIp4uni) {
            rtrBgpAttr.placeReachable(spkr, idx, addpath, pck, hlp, lst);
            pck.merge2beg();
            pck.msbPutW(0, 0);
            pck.msbPutW(2, pck.dataSize());
            pck.putSkip(4);
            pck.merge2beg();
            return;
        }
        if (!ntry.best.nextHop.isIPv4()) {
            rtrBgpAttr.placeReachable(spkr, idx, addpath, pck, hlp, lst);
            pck.merge2beg();
            pck.msbPutW(0, 0);
            pck.msbPutW(2, pck.dataSize());
            pck.putSkip(4);
            pck.merge2beg();
            return;
        }
        rtrBgpAttr.placeNextHop(spkr, pck, hlp, ntry);
        pck.merge2beg();
        pck.msbPutW(0, 0);
        pck.msbPutW(2, pck.dataSize());
        pck.putSkip(4);
        pck.merge2beg();
        for (int i = 0; i < lst.size(); i++) {
            ntry = lst.get(i);
            if (addpath) {
                pck.msbPutD(0, ntry.best.ident);
                pck.putSkip(4);
            }
            rtrBgpAfi.ipv4uni.writePrefix(true, pck, ntry);
        }
        pck.merge2end();
    }

    /**
     * create end of rib message
     *
     * @param spkr where to signal
     * @param pck packet to update
     * @param hlp helper packet
     * @param idx address family
     */
    public static void createEndOfRib(rtrBgpSpeak spkr, packHolder pck, packHolder hlp, int idx) {
        if (spkr.parent.idx2safi[idx] != safiIp4uni) {
            rtrBgpAttr.placeUnreach(spkr, idx, false, pck, hlp, new ArrayList<tabRouteEntry<addrIP>>());
        }
        pck.merge2beg();
        pck.msbPutW(0, pck.dataSize());
        pck.putSkip(2);
        pck.merge2beg();
        pck.msbPutW(0, 0);
        pck.putSkip(2);
        pck.merge2beg();
    }

    /**
     * get size of table
     *
     * @param <T> type of entries
     * @param tab table to check
     * @return string
     */
    public static <T extends Comparable<? super T>> String tabSiz2str(tabGen<T> tab) {
        if (tab == null) {
            return "n/a";
        } else {
            return "" + tab.size();
        }
    }

    /**
     * update counters
     *
     * @param d direction
     * @param c counters
     * @param t type
     * @param p packet
     */
    public static void updtStatsArr(boolean d, counter c[], int t, packHolder p) {
        t &= 0xff;
        if (d) {
            c[t].tx(p);
        } else {
            c[t].rx(p);
        }
    }

    /**
     * safi to ip version
     *
     * @param sfi safi to convert
     * @return ip version, -1 if nothing
     */
    public static int safi2ipVers(int sfi) {
        switch (sfi & afiMask) {
            case afiIpv4:
                return ipCor4.protocolVersion;
            case afiIpv6:
                return ipCor6.protocolVersion;
            default:
                return -1;
        }
    }

}
