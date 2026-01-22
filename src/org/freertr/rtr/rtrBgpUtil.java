package org.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrEmpty;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrMac;
import org.freertr.addr.addrPrefix;
import org.freertr.addr.addrType;
import org.freertr.cry.cryHashMd5;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabLargeComm;
import org.freertr.tab.tabRouteBlob;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRouteUtil;
import org.freertr.util.bits;
import org.freertr.util.logger;
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
     * l2vpn address family
     */
    public final static int afiL2vpn = 0x190000;

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
     * attributes dump
     */
    public final static int safiAttrib = -1;

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

    private static boolean readEvpn(packHolder pck, tabRouteEntry<addrIP> ntry, int typ) {
        ntry.rouDst = pck.msbGetQ(0);
        pck.getSkip(8);
        byte[] buf;
        switch (typ) {
            case 1: // ethernet auto discovery
                buf = new byte[addrIP.size];
                pck.getCopy(buf, 2, 0, 14); // esi + eti
                ntry.prefix.network.fromBuf(buf, 0);
                ntry.best.evpnLab = pck.msbGetD(14) >>> 8;
                return false;
            case 2: // mac ip advertisement
                buf = new byte[addrIP.size];
                pck.getCopy(buf, 0, 0, 10); // esi
                ntry.prefix.wildcard.fromBuf(buf, 0);
                buf = new byte[addrIP.size];
                pck.getCopy(buf, 2, 10, 4); // eti
                pck.getSkip(14);
                addrMac mac = new addrMac();
                if (pck.getByte(0) != mac.maxBits()) {
                    return true;
                }
                pck.getSkip(1);
                pck.getAddr(mac, 0);
                pck.getSkip(addrMac.size);
                mac.toBuffer(buf, 10);
                ntry.prefix.network.fromBuf(buf, 0);
                int i = pck.getByte(0);
                pck.getSkip(1);
                switch (i) {
                    case 0:
                        ntry.prefix.broadcast = new addrIP();
                        break;
                    case addrIPv4.size * 8:
                        ntry.prefix.broadcast = readAddress(afiIpv4, pck);
                        break;
                    case addrIPv6.size * 8:
                        ntry.prefix.broadcast = readAddress(afiIpv6, pck);
                        break;
                    default:
                        return true;
                }
                ntry.best.evpnLab = pck.msbGetD(0) >>> 8;
                return false;
            case 3: // inclusive multicast ethernet tag
                buf = new byte[addrIP.size];
                pck.getCopy(buf, 2, 0, 4); // eti
                pck.getSkip(4);
                ntry.prefix.network.fromBuf(buf, 0);
                i = pck.getByte(0);
                pck.getSkip(1);
                switch (i) {
                    case addrIPv4.size * 8:
                        ntry.prefix.broadcast = readAddress(afiIpv4, pck);
                        break;
                    case addrIPv6.size * 8:
                        ntry.prefix.broadcast = readAddress(afiIpv6, pck);
                        break;
                    default:
                        return true;
                }
                return false;
            case 4: // ethernet segment
                buf = new byte[addrIP.size];
                pck.getCopy(buf, 2, 0, 10); // esi
                pck.getSkip(10);
                ntry.prefix.network.fromBuf(buf, 0);
                i = pck.getByte(0);
                pck.getSkip(1);
                switch (i) {
                    case addrIPv4.size * 8:
                        ntry.prefix.broadcast = readAddress(afiIpv4, pck);
                        break;
                    case addrIPv6.size * 8:
                        ntry.prefix.broadcast = readAddress(afiIpv6, pck);
                        break;
                    default:
                        return true;
                }
                return false;
            case 5: // ip prefix
                buf = new byte[addrIP.size];
                pck.getCopy(buf, 0, 0, 10); // esi
                ntry.prefix.wildcard.fromBuf(buf, 0);
                buf = new byte[addrIP.size];
                pck.getCopy(buf, 2, 10, 4); // eti
                pck.getSkip(14);
                ntry.prefix.network.fromBuf(buf, 0);
                i = pck.getByte(0);
                pck.getSkip(1);
                switch (i) {
                    case addrIPv4.size * 8:
                        ntry.prefix.broadcast = readAddress(afiIpv4, pck);
                        ntry.prefix.mask = readAddress(afiIpv4, pck);
                        break;
                    case addrIPv6.size * 8:
                        ntry.prefix.broadcast = readAddress(afiIpv6, pck);
                        ntry.prefix.mask = readAddress(afiIpv6, pck);
                        break;
                    default:
                        return true;
                }
                ntry.best.evpnLab = pck.msbGetD(0) >>> 8;
                return false;
            default:
                return true;
        }
    }

    private static int writeEvpn(byte[] trg, tabRouteEntry<addrIP> ntry, int typ) {
        bits.msbPutQ(trg, 0, ntry.rouDst);
        int pos = 8;
        byte[] buf = new byte[addrIP.size];
        switch (typ) {
            case 1: // ethernet auto discovery
                ntry.prefix.network.toBuffer(buf, 0);
                bits.byteCopy(buf, 2, trg, pos, 14); // esi + eti
                pos += 14;
                bits.msbPutD(trg, pos, ntry.best.evpnLab << 8);
                pos += 3;
                return pos;
            case 2: // mac ip advertisement
                ntry.prefix.wildcard.toBuffer(buf, 0);
                bits.byteCopy(buf, 0, trg, pos, 10); // esi
                pos += 10;
                ntry.prefix.network.toBuffer(buf, 0);
                bits.byteCopy(buf, 2, trg, pos, 4); // eti
                pos += 4;
                trg[pos] = addrMac.size * 8; // size;
                pos++;
                bits.byteCopy(buf, 10, trg, pos, addrMac.size); // eti
                pos += addrMac.size;
                addrType adr;
                if (ntry.prefix.broadcast.isIPv4()) {
                    adr = ntry.prefix.broadcast.toIPv4();
                } else {
                    adr = ntry.prefix.broadcast.toIPv6();
                }
                if (ntry.prefix.broadcast.isEmpty()) {
                    adr = new addrEmpty();
                }
                trg[pos] = (byte) adr.maxBits();
                pos++;
                adr.toBuffer(trg, pos);
                pos += adr.getSize();
                bits.msbPutD(trg, pos, ntry.best.evpnLab << 8);
                pos += 3;
                return pos;
            case 3: // inclusive multicast ethernet tag
                ntry.prefix.network.toBuffer(buf, 0);
                bits.byteCopy(buf, 2, trg, pos, 4); // eti
                pos += 4;
                if (ntry.prefix.broadcast.isIPv4()) {
                    adr = ntry.prefix.broadcast.toIPv4();
                } else {
                    adr = ntry.prefix.broadcast.toIPv6();
                }
                trg[pos] = (byte) adr.maxBits();
                pos++;
                adr.toBuffer(trg, pos);
                pos += adr.getSize();
                return pos;
            case 4: // ethernet segment
                ntry.prefix.network.toBuffer(buf, 0);
                bits.byteCopy(buf, 2, trg, pos, 10); // esi
                pos += 10;
                if (ntry.prefix.broadcast.isIPv4()) {
                    adr = ntry.prefix.broadcast.toIPv4();
                } else {
                    adr = ntry.prefix.broadcast.toIPv6();
                }
                trg[pos] = (byte) adr.maxBits();
                pos++;
                adr.toBuffer(trg, pos);
                pos += adr.getSize();
                return pos;
            case 5: // ip prefix
                ntry.prefix.wildcard.toBuffer(buf, 0);
                bits.byteCopy(buf, 0, trg, pos, 10); // esi
                pos += 10;
                ntry.prefix.network.toBuffer(buf, 0);
                bits.byteCopy(buf, 2, trg, pos, 4); // eti
                pos += 4;
                if (ntry.prefix.broadcast.isIPv4()) {
                    adr = ntry.prefix.broadcast.toIPv4();
                } else {
                    adr = ntry.prefix.broadcast.toIPv6();
                }
                trg[pos] = (byte) adr.maxBits();
                pos++;
                adr.toBuffer(trg, pos);
                pos += adr.getSize();
                if (ntry.prefix.broadcast.isIPv4()) {
                    adr = ntry.prefix.mask.toIPv4();
                } else {
                    adr = ntry.prefix.mask.toIPv6();
                }
                adr.toBuffer(trg, pos);
                pos += adr.getSize();
                bits.msbPutD(trg, pos, ntry.best.evpnLab << 8);
                pos += 3;
                return pos;
            default:
                return -1;
        }
    }

    /**
     * read address from packet
     *
     * @param safi safi to read
     * @param oneLab just one label
     * @param pck packet to use
     * @return address read, null if nothing
     */
    public static tabRouteEntry<addrIP> readPrefix(int safi, boolean oneLab, packHolder pck) {
        int sfi = safi & sfiMask;
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        int i;
        int p = 0;
        switch (sfi) {
            case sfiClrAwRtg:
                i = pck.getByte(0);
                p = pck.getByte(1);
                i = pck.getByte(2);
                if (i == 2) {
                    byte[] buf = new byte[128];
                    pck.getCopy(buf, 0, 4, buf.length);
                    if (readPrefix(safi, ntry, pck.getByte(3), buf)) {
                        return null;
                    }
                    pck.getSkip(p + 3);
                    return ntry;
                }
                if (i != 1) {
                    return null;
                }
                byte[] buf = new byte[128];
                pck.getCopy(buf, 0, 4, buf.length);
                if (readPrefix(safi, ntry, pck.getByte(3), buf)) {
                    return null;
                }
                ntry.rouDst = pck.msbGetD(p - 1);
                pck.getSkip(p + 3);
                if (pck.getByte(0) != 1) {
                    return null;
                }
                i = pck.getByte(1);
                pck.getSkip(2);
                ntry.best.labelRem = new ArrayList<Integer>();
                for (;;) {
                    if (i < 3) {
                        break;
                    }
                    p = pck.msbGetD(0) >>> 8;
                    pck.getSkip(3);
                    i -= 3;
                    ntry.best.labelRem.add(p >>> 4);
                }
                return ntry;
            case sfiSdwan:
                if (pck.msbGetW(0) != 1) {
                    return null;
                }
                pck.getCopy(ntry.prefix.broadcast.getBytes(), 0, 4, 8);
                ntry.prefix = new addrPrefix<addrIP>(new addrIP(), addrIP.size * 8);
                if (pck.msbGetW(2) > (8 * 12)) {
                    addrIPv6 a6 = new addrIPv6();
                    pck.getAddr(a6, 12);
                    ntry.prefix.network.fromIPv6addr(a6);
                    pck.getSkip(28);
                } else {
                    addrIPv4 a4 = new addrIPv4();
                    pck.getAddr(a4, 12);
                    ntry.prefix.network.fromIPv4addr(a4);
                    pck.getSkip(16);
                }
                return ntry;
            case sfiMobUsrPln:
                if (pck.getByte(0) != 1) {
                    return null;
                }
                ntry.prefix = new addrPrefix<addrIP>(new addrIP(), addrIP.size * 8);
                p = pck.msbGetW(1);
                i = pck.getByte(3);
                ntry.rouDst = pck.msbGetQ(4);
                pck.getSkip(12);
                ntry.nlri = new byte[i + 2];
                bits.msbPutW(ntry.nlri, 0, p);
                pck.getCopy(ntry.nlri, 2, 0, i);
                pck.getSkip(i);
                addrIP adr = new addrIP();
                adr.fromBuf(cryHashMd5.compute(new cryHashMd5(), ntry.nlri), 0);
                ntry.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
                return ntry;
            case sfiSpf:
            case sfiVpnLnkSt:
            case sfiLnkSt:
                p = pck.msbGetW(0);
                i = pck.msbGetW(2);
                pck.getSkip(4);
                ntry.nlri = new byte[i + 2];
                bits.msbPutW(ntry.nlri, 0, p);
                pck.getCopy(ntry.nlri, 2, 0, i);
                pck.getSkip(i);
                adr = new addrIP();
                adr.fromBuf(cryHashMd5.compute(new cryHashMd5(), ntry.nlri), 0);
                ntry.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
                return ntry;
            case sfiEthVpn:
                ntry.prefix = new addrPrefix<addrIP>(new addrIP(), addrIP.size * 8);
                p = pck.getByte(0);
                i = pck.getByte(1);
                pck.getSkip(2);
                i = pck.dataSize() - i;
                if (readEvpn(pck, ntry, p)) {
                    ntry = null;
                } else {
                    ntry.prefix.network.getBytes()[0] = (byte) p;
                }
                pck.setBytesLeft(i);
                return ntry;
            case sfiNsh:
                ntry.prefix = new addrPrefix<addrIP>(new addrIP(), addrIP.size * 8);
                p = pck.msbGetW(0);
                i = pck.msbGetW(2);
                pck.getSkip(4);
                ntry.prefix.network.getBytes()[0] = (byte) p;
                ntry.prefix.network.getBytes()[1] = (byte) i;
                pck.getCopy(ntry.prefix.network.getBytes(), 2, 0, i);
                pck.getSkip(i);
                return ntry;
            case sfiRpd:
                ntry.prefix = new addrPrefix<addrIP>(new addrIP(), addrIP.size * 8);
                i = pck.getByte(0);
                ntry.prefix.wildcard.getBytes()[0] = (byte) pck.getByte(1);
                ntry.rouDst = pck.msbGetD(2);
                if (i > 9) {
                    addrIPv6 a6 = new addrIPv6();
                    pck.getAddr(a6, 6);
                    ntry.prefix.network.fromIPv6addr(a6);
                } else {
                    addrIPv4 a4 = new addrIPv4();
                    pck.getAddr(a4, 6);
                    ntry.prefix.network.fromIPv4addr(a4);
                }
                pck.getSkip(i + 1);
                return ntry;
            case sfiVpls:
                i = pck.msbGetW(0) * 8;
                pck.getSkip(2);
                break;
            case sfiFlwSpc:
            case sfiVpnFlw:
                i = pck.getByte(0);
                if (i >= 0xf0) {
                    i = pck.msbGetW(0) & 0xfff;
                    pck.getSkip(2);
                } else {
                    pck.getSkip(1);
                }
                i *= 8;
                break;
            case sfiMvpn:
            case sfiMcsTre:
                p = pck.getByte(0);
                i = pck.getByte(1) * 8;
                pck.getSkip(2);
                break;
            default:
                i = pck.getByte(0);
                pck.getSkip(1);
                break;
        }
        if ((sfi == sfiLabeled) || (sfi == sfiMplsVpnU) || (sfi == sfiClsTrnPl)) {
            ntry.best.labelRem = new ArrayList<Integer>();
            for (;;) {
                if (i < 24) {
                    return null;
                }
                p = pck.msbGetD(0) >>> 8;
                pck.getSkip(3);
                i -= 24;
                ntry.best.labelRem.add(p >>> 4);
                if (oneLab) {
                    break;
                }
                if ((p & 1) != 0) {
                    break;
                }
            }
        }
        if ((sfi == sfiMplsVpnU) || (sfi == sfiMplsVpnM) || (sfi == sfiClsTrnPl) || (sfi == sfiVpls) || (sfi == sfiMspw) || (sfi == sfiMdt) || (sfi == sfiSrTe) || (sfi == sfiVpnLnkSt) || (sfi == sfiVpnFlw) || (sfi == sfiMvpn) || (sfi == sfiMcsTre)) {
            ntry.rouDst = pck.msbGetQ(0);
            pck.getSkip(8);
            i -= 64;
        }
        int o = (i + 7) / 8;
        byte[] buf = new byte[128];
        if (o > buf.length) {
            logger.info("too long (" + o + ") nlri");
            return null;
        }
        bits.byteFill(buf, 0, buf.length, 0);
        pck.getCopy(buf, 0, 0, o);
        pck.getSkip(o);
        if ((sfi == sfiMvpn) || (sfi == sfiMcsTre)) {
            bits.byteCopy(buf, 0, buf, 1, o);
            buf[0] = (byte) p;
            o++;
        }
        if ((sfi == sfiFlwSpc) || (sfi == sfiVpnFlw) || (sfi == sfiMvpn) || (sfi == sfiMcsTre) || (sfi == sfiMspw)) {
            byte[] adr = new byte[addrIP.size];
            ntry.prefix = new addrPrefix<addrIP>(new addrIP(), adr.length * 8);
            adr[0] = (byte) o;
            bits.byteCopy(buf, 0, adr, 1, 15);
            ntry.prefix.network.fromBuf(adr, 0);
            bits.byteCopy(buf, 15, adr, 0, 16);
            ntry.prefix.broadcast.fromBuf(adr, 0);
            bits.byteCopy(buf, 31, adr, 0, 16);
            ntry.prefix.wildcard.fromBuf(adr, 0);
            bits.byteCopy(buf, 47, adr, 0, 16);
            ntry.prefix.mask.fromBuf(adr, 0);
            return ntry;
        }
        if ((sfi == sfiVpls) && (o == 9)) {
            byte[] adr = new byte[addrIP.size];
            ntry.prefix = new addrPrefix<addrIP>(new addrIP(), adr.length * 8);
            bits.byteCopy(buf, 0, adr, 0, 4);
            ntry.prefix.network.fromBuf(adr, 0);
            adr[0] = 5;
            bits.byteCopy(buf, 4, adr, 1, 5);
            ntry.prefix.wildcard.fromBuf(adr, 0);
            return ntry;
        }
        if (sfi == sfiMdt) {
            o = o / 2;
            byte[] adr = new byte[addrIP.size];
            ntry.prefix = new addrPrefix<addrIP>(new addrIP(), adr.length * 8);
            bits.byteCopy(buf, 0, adr, 0, o);
            ntry.prefix.network.fromBuf(adr, 0);
            bits.byteCopy(buf, o, adr, 0, o);
            ntry.prefix.broadcast.fromBuf(adr, 0);
            return ntry;
        }
        if (sfi == sfiRtFltr) {
            addrIP adr = new addrIP();
            adr.fromBuf(buf, 0);
            ntry.prefix = new addrPrefix<addrIP>(adr, i);
            return ntry;
        }
        if ((safi & afiMask) == afiL2vpn) {
            if (o >= addrIPv6.size) {
                addrIPv6 a6 = new addrIPv6();
                a6.fromBuf(buf, 0);
                ntry.prefix = addrPrefix.ip6toIP(new addrPrefix<addrIPv6>(a6, i));
            } else {
                addrIPv4 a4 = new addrIPv4();
                a4.fromBuf(buf, 0);
                ntry.prefix = addrPrefix.ip4toIP(new addrPrefix<addrIPv4>(a4, i));
            }
            return ntry;
        }
        if (readPrefix(safi, ntry, i, buf)) {
            return null;
        }
        return ntry;
    }

    private static boolean readPrefix(int safi, tabRouteEntry<addrIP> ntry, int siz, byte[] buf) {
        switch (safi & afiMask) {
            case afiIpv4:
                addrIPv4 a4 = new addrIPv4();
                a4.fromBuf(buf, 0);
                ntry.prefix = addrPrefix.ip4toIP(new addrPrefix<addrIPv4>(a4, siz));
                return false;
            case afiIpv6:
                addrIPv6 a6 = new addrIPv6();
                a6.fromBuf(buf, 0);
                ntry.prefix = addrPrefix.ip6toIP(new addrPrefix<addrIPv6>(a6, siz));
                return false;
            default:
                logger.info("unknown safi (" + safi + ") requested");
                return true;
        }
    }

    /**
     * write prefix
     *
     * @param safi safi to write
     * @param oneLab just one label
     * @param pck packet to use
     * @param ntry prefix to write
     */
    public static void writePrefix(int safi, boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        int sfi = safi & sfiMask;
        byte[] buf2;
        int i;
        switch (safi & afiMask) {
            case afiIpv4:
                addrPrefix<addrIPv4> a4 = addrPrefix.ip2ip4(ntry.prefix);
                i = a4.maskLen;
                buf2 = a4.network.getBytes();
                break;
            case afiIpv6:
                addrPrefix<addrIPv6> a6 = addrPrefix.ip2ip6(ntry.prefix);
                i = a6.maskLen;
                buf2 = a6.network.getBytes();
                break;
            case afiL2vpn:
                if (ntry.prefix.network.isIPv4()) {
                    a4 = addrPrefix.ip2ip4(ntry.prefix);
                    i = a4.maskLen;
                    buf2 = a4.network.getBytes();
                } else {
                    a6 = addrPrefix.ip2ip6(ntry.prefix);
                    i = a6.maskLen;
                    buf2 = a6.network.getBytes();
                }
                break;
            case afiLnks:
            case afiNsh:
                buf2 = new byte[0];
                i = 0;
                break;
            case afiRpd:
                buf2 = new byte[0];
                i = 0;
                break;
            default:
                pck.putByte(0, 0);
                pck.putSkip(1);
                return;
        }
        if ((sfi == sfiVpls) && (ntry.prefix.wildcard.getBytes()[0] == 5)) {
            buf2 = new byte[9];
            bits.byteCopy(ntry.prefix.network.getBytes(), 0, buf2, 0, 4);
            bits.byteCopy(ntry.prefix.wildcard.getBytes(), 1, buf2, 4, 5);
            i = buf2.length * 8;
        }
        if (sfi == sfiMdt) {
            int as = buf2.length;
            buf2 = new byte[as * 2];
            bits.byteCopy(ntry.prefix.network.getBytes(), 0, buf2, 0, as);
            bits.byteCopy(ntry.prefix.broadcast.getBytes(), 0, buf2, as, as);
            i = buf2.length * 8;
        }
        if ((sfi == sfiFlwSpc) || (sfi == sfiVpnFlw) || (sfi == sfiMvpn) || (sfi == sfiMcsTre) || (sfi == sfiMspw)) {
            buf2 = new byte[addrIP.size * 4];
            bits.byteCopy(ntry.prefix.network.getBytes(), 1, buf2, 0, 15);
            bits.byteCopy(ntry.prefix.broadcast.getBytes(), 0, buf2, 15, 16);
            bits.byteCopy(ntry.prefix.wildcard.getBytes(), 0, buf2, 31, 16);
            bits.byteCopy(ntry.prefix.mask.getBytes(), 0, buf2, 47, 16);
            i = ntry.prefix.network.getBytes()[0] * 8;
        }
        if (sfi == sfiRtFltr) {
            buf2 = new byte[addrIP.size];
            bits.byteCopy(ntry.prefix.network.getBytes(), 0, buf2, 0, 16);
            i = ntry.prefix.maskLen;
        }
        int o = (i + 7) / 8;
        byte[] buf1 = new byte[128];
        int p = 0;
        if ((sfi == sfiLabeled) || (sfi == sfiMplsVpnU) || (sfi == sfiClsTrnPl) || (sfi == sfiClrAwRtg)) {
            for (int q = 0; q < ntry.best.labelRem.size(); q++) {
                bits.msbPutD(buf1, p, ntry.best.labelRem.get(q) << 12);
                p += 3;
                i += 24;
                if (oneLab) {
                    break;
                }
            }
            buf1[p - 1] |= 1;
        }
        if ((sfi == sfiMplsVpnU) || (sfi == sfiMplsVpnM) || (sfi == sfiClsTrnPl) || (sfi == sfiVpls) || (sfi == sfiMspw) || (sfi == sfiMdt) || (sfi == sfiSrTe) || (sfi == sfiVpnLnkSt) || (sfi == sfiVpnFlw) || (sfi == sfiMvpn) || (sfi == sfiMcsTre)) {
            bits.msbPutQ(buf1, p, ntry.rouDst);
            p += 8;
            i += 64;
        }
        switch (sfi) {
            case sfiClrAwRtg:
                pck.putByte(0, o + p + 9);
                pck.putByte(1, o + 5);
                pck.putByte(2, 1);
                pck.putByte(3, i - (p * 8));
                pck.putSkip(4);
                pck.putCopy(buf2, 0, 0, o);
                pck.putSkip(o);
                pck.msbPutD(0, (int) ntry.rouDst);
                pck.putSkip(4);
                pck.putByte(0, 1);
                pck.putByte(1, p);
                pck.putSkip(2);
                pck.putCopy(buf1, 0, 0, p);
                pck.putSkip(p);
                return;
            case sfiSdwan:
                pck.msbPutW(0, 1);
                pck.putCopy(ntry.prefix.broadcast.getBytes(), 0, 4, 8);
                if (ntry.prefix.network.isIPv4()) {
                    pck.msbPutW(2, 8 * 12);
                    pck.putAddr(12, ntry.prefix.network.toIPv4());
                    pck.putSkip(16);
                } else {
                    pck.msbPutW(2, 8 * 24);
                    pck.putAddr(12, ntry.prefix.network.toIPv6());
                    pck.putSkip(28);
                }
                return;
            case sfiMobUsrPln:
                o = ntry.nlri.length - 2;
                pck.putByte(0, 1);
                pck.putCopy(ntry.nlri, 0, 1, 2);
                pck.putByte(3, o);
                pck.msbPutQ(4, ntry.rouDst);
                pck.putSkip(12);
                pck.putCopy(ntry.nlri, 2, 0, o);
                pck.putSkip(o);
                return;
            case sfiSpf:
            case sfiVpnLnkSt:
            case sfiLnkSt:
                o = ntry.nlri.length - 2;
                pck.putCopy(ntry.nlri, 0, 0, 2);
                pck.msbPutW(2, o);
                pck.putSkip(4);
                pck.putCopy(ntry.nlri, 2, 0, o);
                pck.putSkip(o);
                return;
            case sfiEthVpn:
                o = ntry.prefix.network.getBytes()[0];
                i = writeEvpn(buf1, ntry, o);
                if (i < 1) {
                    return;
                }
                pck.putByte(0, o);
                pck.putByte(1, i);
                pck.putSkip(2);
                pck.putCopy(buf1, 0, 0, i);
                pck.putSkip(i);
                return;
            case sfiNsh:
                o = ntry.prefix.network.getBytes()[0];
                i = ntry.prefix.network.getBytes()[0];
                pck.msbPutW(0, o);
                pck.msbPutW(2, i);
                pck.putSkip(4);
                pck.putCopy(ntry.prefix.network.getBytes(), 2, 0, i);
                pck.putSkip(i);
                return;
            case sfiRpd:
                pck.putByte(1, ntry.prefix.wildcard.getBytes()[0]);
                pck.msbPutD(2, (int) ntry.rouDst);
                if (ntry.prefix.network.isIPv4()) {
                    pck.putAddr(6, ntry.prefix.network.toIPv4());
                    i = addrIPv4.size;
                } else {
                    pck.putAddr(6, ntry.prefix.network.toIPv6());
                    i = addrIPv6.size;
                }
                pck.putByte(0, i + 5);
                pck.putSkip(i + 6);
                return;
            case sfiVpls:
                pck.msbPutW(0, o + p);
                pck.putSkip(2);
                break;
            case sfiFlwSpc:
            case sfiVpnFlw:
                i = o + p;
                if (i < 0xf0) {
                    pck.putByte(0, i);
                    pck.putSkip(1);
                } else {
                    pck.msbPutW(0, i | 0xf000);
                    pck.putSkip(2);
                }
                break;
            case sfiMvpn:
            case sfiMcsTre:
                o--;
                pck.putByte(0, buf2[0]);
                pck.putByte(1, o + p);
                pck.putSkip(2);
                bits.byteCopy(buf2, 1, buf2, 0, o);
                break;
            default:
                pck.putByte(0, i);
                pck.putSkip(1);
                break;
        }
        pck.putCopy(buf1, 0, 0, p);
        pck.putSkip(p);
        pck.putCopy(buf2, 0, 0, o);
        pck.putSkip(o);
    }

    /**
     * read address from packet
     *
     * @param safi safi to read
     * @param pck packet to use
     * @return address read, null if nothing
     */
    public static addrIP readAddress(int safi, packHolder pck) {
        addrIP ax = new addrIP();
        switch (safi & afiMask) {
            case afiIpv4:
                addrIPv4 a4 = new addrIPv4();
                pck.getAddr(a4, 0);
                pck.getSkip(addrIPv4.size);
                ax.fromIPv4addr(a4);
                return ax;
            case afiIpv6:
                addrIPv6 a6 = new addrIPv6();
                pck.getAddr(a6, 0);
                pck.getSkip(addrIPv6.size);
                ax.fromIPv6addr(a6);
                return ax;
            default:
                logger.info("unknown safi (" + safi + ") requested");
                return null;
        }
    }

    /**
     * write address to packet
     *
     * @param safi safi to write
     * @param pck packet to use
     * @param addr address to write
     */
    public static void writeAddress(int safi, packHolder pck, addrIP addr) {
        switch (safi & afiMask) {
            case afiIpv4:
                addrIPv4 a4 = addr.toIPv4();
                pck.putAddr(0, a4);
                pck.putSkip(addrIPv4.size);
                break;
            case afiIpv6:
                addrIPv6 a6 = addr.toIPv6();
                pck.putAddr(0, a6);
                pck.putSkip(addrIPv6.size);
                break;
            default:
                logger.info("unknown safi (" + safi + ") requested");
                break;
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
     * convert safi to string
     *
     * @param i safi to convert
     * @return converted string
     */
    public static String safi2string(int i) {
        switch (i) {
            case safiIp4uni:
                return "ip4unicast";
            case safiIp6uni:
                return "ip6unicast";
            case safiIp4multi:
                return "ip4multicast";
            case safiIp6multi:
                return "ip6multicast";
            case safiIp4lab:
                return "ip4labeled";
            case safiIp6lab:
                return "ip6labeled";
            case safiIp4ctp:
                return "ip4classful";
            case safiIp6ctp:
                return "ip6classful";
            case safiIp4car:
                return "ip4color";
            case safiIp6car:
                return "ip6color";
            case safiIp4flow:
                return "ip4flowspec";
            case safiIp6flow:
                return "ip6flowspec";
            case safiIp4vpnU:
                return "ip4vpnU";
            case safiIp6vpnU:
                return "ip6vpnU";
            case safiIp4vpnM:
                return "ip4vpnM";
            case safiIp6vpnM:
                return "ip6vpnM";
            case safiIp4vpnF:
                return "ip4vpnF";
            case safiIp6vpnF:
                return "ip6vpnF";
            case safiNsh46:
                return "nsh";
            case safiRpd46:
                return "rpd";
            case safiRtf46:
                return "rtfilter";
            case safiIp4mdt:
                return "mdt4";
            case safiIp6mdt:
                return "mdt6";
            case safiIp4srte:
                return "srte4";
            case safiIp6srte:
                return "srte6";
            case safiIp46lnks:
                return "linkstate";
            case safiIp46vpnL:
                return "vpnLnkst";
            case safiIp4mvpn:
                return "mvpn4";
            case safiIp6mvpn:
                return "mvpn6";
            case safiIp4mtree:
                return "mtree4";
            case safiIp6mtree:
                return "mtree6";
            case safiVpls46:
                return "vpls";
            case safiMspw46:
                return "mspw";
            case safiEvpn46:
                return "evpn";
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
     * parse origin type attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseOrigin(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.origin = pck.getByte(0);
    }

    private static void parseAsList(boolean longAs, List<Integer> lst, packHolder pck) {
        int o = pck.getByte(0);
        pck.getSkip(1);
        for (int i = 0; i < o; i++) {
            int p = 0;
            if (longAs) {
                p = pck.msbGetD(0);
                pck.getSkip(4);
            } else {
                p = pck.msbGetW(0);
                pck.getSkip(2);
            }
            lst.add(p);
        }
    }

    /**
     * parse as path attribute
     *
     * @param spkr where to signal
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseAsPath(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.pathSeq = new ArrayList<Integer>();
        ntry.best.pathSet = new ArrayList<Integer>();
        ntry.best.confSeq = new ArrayList<Integer>();
        ntry.best.confSet = new ArrayList<Integer>();
        for (; pck.dataSize() > 0;) {
            int i = pck.getByte(0);
            pck.getSkip(1);
            switch (i) {
                case 1: // as set
                    parseAsList(spkr.peer32bitAS, ntry.best.pathSet, pck);
                    break;
                case 2: // as seq
                    parseAsList(spkr.peer32bitAS, ntry.best.pathSeq, pck);
                    break;
                case 3: // confed seq
                    parseAsList(spkr.peer32bitAS, ntry.best.confSeq, pck);
                    break;
                case 4: // confed set
                    parseAsList(spkr.peer32bitAS, ntry.best.pathSet, pck);
                    break;
            }
        }
    }

    /**
     * parse next hop attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseNextHop(tabRouteEntry<addrIP> ntry, packHolder pck) {
        addrIPv4 as = new addrIPv4();
        pck.getAddr(as, 0);
        addrIP ax = new addrIP();
        ax.fromIPv4addr(as);
        ntry.best.nextHop = ax;
    }

    /**
     * parse metric attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseMetric(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.metric = pck.msbGetD(0);
    }

    /**
     * parse local preference attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseLocPref(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.locPref = pck.msbGetD(0);
    }

    /**
     * parse atomic aggregator attribute
     *
     * @param ntry table entry
     */
    public static void parseAtomicAggr(tabRouteEntry<addrIP> ntry) {
        ntry.best.atomicAggr = true;
    }

    /**
     * parse entropy label attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseEntropyLab(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.entropyLabel = pck.getCopy();
    }

    /**
     * parse connector attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseConnector(tabRouteEntry<addrIP> ntry, packHolder pck) {
        addrIPv4 as = new addrIPv4();
        pck.getAddr(as, 4);
        addrIP ax = new addrIP();
        ax.fromIPv4addr(as);
        ntry.best.connRtr = ax;
    }

    /**
     * parse path limit attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parsePathLimit(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.pathLim = pck.getByte(0);
        ntry.best.pathAsn = pck.msbGetD(1);
    }

    /**
     * parse pe distinguisher attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parsePeDistLab(tabRouteEntry<addrIP> ntry, packHolder pck) {
        int i;
        if (pck.dataSize() > 8) {
            addrIPv4 as = new addrIPv4();
            pck.getAddr(as, 0);
            addrIP ax = new addrIP();
            ax.fromIPv4addr(as);
            ntry.best.pediRtr = ax;
            i = addrIPv4.size;
        } else {
            addrIPv6 as = new addrIPv6();
            pck.getAddr(as, 0);
            addrIP ax = new addrIP();
            ax.fromIPv6addr(as);
            ntry.best.pediRtr = ax;
            i = addrIPv6.size;
        }
        ntry.best.pediLab = pck.msbGetD(0) >>> 12;
    }

    /**
     * parse aggregator attribute
     *
     * @param spkr where to signal
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseAggregator(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        if (spkr.peer32bitAS) {
            ntry.best.aggrAs = pck.msbGetD(0);
            pck.getSkip(4);
        } else {
            ntry.best.aggrAs = pck.msbGetW(0);
            pck.getSkip(2);
        }
        addrIPv4 as = new addrIPv4();
        pck.getAddr(as, 0);
        addrIP ax = new addrIP();
        ax.fromIPv4addr(as);
        ntry.best.aggrRtr = ax;
    }

    /**
     * parse standard community attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseStdComm(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.stdComm = new ArrayList<Integer>();
        for (; pck.dataSize() >= 4;) {
            ntry.best.stdComm.add(pck.msbGetD(0));
            pck.getSkip(4);
        }
    }

    /**
     * parse extended community attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseExtComm(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.extComm = new ArrayList<Long>();
        for (; pck.dataSize() >= 8;) {
            ntry.best.extComm.add(pck.msbGetQ(0));
            pck.getSkip(8);
        }
    }

    /**
     * parse large community attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseLrgComm(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.lrgComm = new ArrayList<tabLargeComm>();
        for (; pck.dataSize() >= 12;) {
            tabLargeComm d = new tabLargeComm();
            d.as = pck.msbGetD(0);
            d.d1 = pck.msbGetD(4);
            d.d2 = pck.msbGetD(8);
            ntry.best.lrgComm.add(d);
            pck.getSkip(12);
        }
    }

    /**
     * parse originator attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseOriginator(tabRouteEntry<addrIP> ntry, packHolder pck) {
        addrIPv4 as = new addrIPv4();
        pck.getAddr(as, 0);
        addrIP ax = new addrIP();
        ax.fromIPv4addr(as);
        ntry.best.originator = ax;
    }

    /**
     * parse accumulated igp attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseAccIgp(tabRouteEntry<addrIP> ntry, packHolder pck) {
        if (pck.getByte(0) != 1) {
            return;
        }
        if (pck.msbGetW(1) < 11) {
            return;
        }
        ntry.best.accIgp = (int) pck.msbGetQ(3);
    }

    /**
     * parse traffic engineering attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseTraffEng(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.bandwidth = ((Float) Float.intBitsToFloat(pck.msbGetD(4))).intValue() * 8;
    }

    /**
     * parse pmsi tunnel attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parsePmsiTun(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.pmsiTyp = pck.msbGetW(0);
        ntry.best.pmsiLab = pck.msbGetD(2) >>> 8;
        pck.getSkip(5);
        ntry.best.pmsiTun = pck.getCopy();
    }

    /**
     * parse tunnel encapsulation attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseTunEnc(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.tunelTyp = pck.msbGetW(0);
        int len = pck.msbGetW(2);
        pck.getSkip(4);
        if (pck.dataSize() < len) {
            return;
        }
        pck.setDataSize(len);
        ntry.best.tunelVal = pck.getCopy();
    }

    /**
     * parse link state attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseLnkSta(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.linkStat = pck.getCopy();
    }

    /**
     * parse attribute set attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseAttribSet(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.attribAs = pck.msbGetD(0);
        pck.getSkip(4);
        ntry.best.attribVal = pck.getCopy();
    }

    /**
     * parse nsh service chain attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseNshChain(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.nshChain = pck.getCopy();
    }

    /**
     * parse domain path attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseDomainPath(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.domainPath = pck.getCopy();
    }

    /**
     * parse bfd discriminator attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseBfdDiscr(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.bfdDiscr = pck.getCopy();
    }

    /**
     * parse next hop capabilities attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseHopCapa(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.hopCapa = pck.getCopy();
    }

    /**
     * parse prefix sid attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parsePrefSid(tabRouteEntry<addrIP> ntry, packHolder pck) {
        encTlv tlv = getPrefSidTlv();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case 1: // label index
                    ntry.best.segrouIdx = bits.msbGetD(tlv.valDat, 3); // index
                    break;
                case 3: // srgb
                    ntry.best.segrouBeg = bits.msbGetD(tlv.valDat, 2) >>> 8; // base
                    ntry.best.segrouSiz = bits.msbGetD(tlv.valDat, 5) >>> 8; // range
                    break;
                case 4: // prefix sid
                    addrIPv6 adr6 = new addrIPv6();
                    ntry.best.segrouPrf = new addrIP();
                    adr6.fromBuf(tlv.valDat, 3);
                    ntry.best.segrouPrf.fromIPv6addr(adr6);
                    break;
                case 5: // layer3 service
                case 6: // layer2 service
                    if (tlv.valDat[1] != 1) { // subtlv
                        break;
                    }
                    adr6 = new addrIPv6();
                    ntry.best.segrouPrf = new addrIP();
                    adr6.fromBuf(tlv.valDat, 5);
                    ntry.best.segrouPrf.fromIPv6addr(adr6);
                    if (tlv.valDat[25] != 1) { // sid structure
                        break;
                    }
                    ntry.best.segrouSiz = tlv.valDat[32] & 0xff; // transposition length
                    ntry.best.segrouOfs = tlv.valDat[33] & 0xff; // transposition offset
                    break;
            }
        }
    }

    /**
     * parse bier attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseBier(tabRouteEntry<addrIP> ntry, packHolder pck) {
        encTlv tlv = getBierTlv();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case 1: // bier
                    ntry.best.bierSub = tlv.valDat[0]; // subdomain
                    ntry.best.bierIdx = bits.msbGetW(tlv.valDat, 1); // bfr id
                    break;
                case 2: // mpls
                    ntry.best.bierBeg = bits.msbGetD(tlv.valDat, 0) & 0xfffff; // base
                    ntry.best.bierSiz = tlv.valDat[0] & 0xff; // range
                    ntry.best.bierHdr = (tlv.valDat[1] >>> 4) & 0xf; // bsl
                    break;
            }
        }
    }

    /**
     * parse cluster list attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseClustList(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.clustList = new ArrayList<addrIP>();
        for (; pck.dataSize() >= 4;) {
            addrIPv4 as = new addrIPv4();
            pck.getAddr(as, 0);
            pck.getSkip(addrIPv4.size);
            addrIP ax = new addrIP();
            ax.fromIPv4addr(as);
            ntry.best.clustList.add(ax);
        }
    }

    /**
     * parse only to customer attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseOnlyCust(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.onlyCust = pck.msbGetD(0);
    }

    /**
     * parse unknown attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseUnknown(tabRouteEntry<addrIP> ntry, packHolder pck) {
        if (ntry.best.unknown == null) {
            ntry.best.unknown = new ArrayList<tabRouteBlob>();
        }
        tabRouteBlob blb = new tabRouteBlob();
        blb.type = pck.ETHtype;
        blb.flag = pck.ETHcos;
        blb.data = pck.getCopy();
        ntry.best.unknown.add(blb);
    }

    /**
     * parse reachable attribute
     *
     * @param spkr where to signal
     * @param pfxs prefixes read
     * @param pck packet to parse
     */
    public static void parseReachable(rtrBgpSpeak spkr, List<tabRouteEntry<addrIP>> pfxs, packHolder pck) {
        int safi = triplet2safi(pck.msbGetD(0));
        int idx = spkr.parent.safi2idx(safi);
        if (idx < 0) {
            return;
        }
        int sfi = safi & sfiMask;
        int len = pck.getByte(3);
        boolean addpath = spkr.addpathRx[idx];
        boolean oneLab = !spkr.peerMltLab[idx];
        boolean v6nh = len >= addrIPv6.size;
        pck.getSkip(4);
        len = pck.dataSize() - len;
        addrIP nextHop = null;
        for (; pck.dataSize() > len;) {
            if ((sfi == sfiMplsVpnU) || (sfi == sfiMplsVpnM) || (sfi == sfiClsTrnPl)) {
                pck.getSkip(8); // rd
            }
            addrIP adr;
            if (v6nh) {
                adr = readAddress(afiIpv6, pck);
            } else {
                adr = readAddress(afiIpv4, pck);
            }
            if (adr == null) {
                continue;
            }
            if (nextHop == null) {
                nextHop = adr;
                continue;
            }
            if (!v6nh) {
                addrIPv4 adr4 = adr.toIPv4();
                if (adr4.isEmpty()) {
                    continue;
                }
            } else {
                addrIPv6 adr6 = adr.toIPv6();
                if (adr6.isEmpty()) {
                    continue;
                }
                if (adr6.isLinkLocal()) {
                    continue;
                }
            }
            nextHop = adr;
        }
        pck.setBytesLeft(len);
        len = pck.getByte(0);
        pck.getSkip(1);
        for (int i = 0; i < len; i++) {
            pck.getSkip(pck.getByte(0) + 1);
        }
        int ident = 0;
        for (; pck.dataSize() > 0;) {
            if (addpath) {
                ident = pck.msbGetD(0);
                pck.getSkip(4);
            }
            tabRouteEntry<addrIP> res = readPrefix(safi, oneLab, pck);
            if (res == null) {
                continue;
            }
            res.oldDst = idx;
            res.best.ident = ident;
            res.best.nextHop = nextHop;
            pfxs.add(res);
        }
    }

    /**
     * parse unreachable attribute
     *
     * @param spkr where to signal
     * @param pfxs prefixes read
     * @param pck packet to parse
     */
    public static void parseUnReach(rtrBgpSpeak spkr, List<tabRouteEntry<addrIP>> pfxs, packHolder pck) {
        pck.merge2beg();
        int safi = triplet2safi(pck.msbGetD(0));
        pck.getSkip(3);
        int idx = spkr.parent.safi2idx(safi);
        if (idx < 0) {
            return;
        }
        boolean addpath = spkr.addpathRx[idx];
        int ident = 0;
        for (; pck.dataSize() > 0;) {
            if (addpath) {
                ident = pck.msbGetD(0);
                pck.getSkip(4);
            }
            tabRouteEntry<addrIP> res = readPrefix(safi, true, pck);
            if (res == null) {
                continue;
            }
            res.oldDst = idx;
            res.best.ident = ident;
            pfxs.add(res);
        }
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
     * parse attribute
     *
     * @param src source where from read
     * @param attr attribute read to
     * @return false on success, true on error
     */
    public static boolean parseAttrib(packHolder src, packHolder attr) {
        int flg = src.getByte(0); // flags
        int typ = src.getByte(1); // type
        int len = 0;
        if ((flg & flagLength) == 0) {
            len = src.getByte(2);
            src.getSkip(3); // length
        } else {
            len = src.msbGetW(2);
            src.getSkip(4); // length
        }
        if (len > src.dataSize()) {
            return true;
        }
        byte[] buf = new byte[len];
        src.getCopy(buf, 0, 0, len);
        src.getSkip(len);
        attr.clear();
        attr.ETHcos = flg;
        attr.ETHtype = typ;
        attr.putCopy(buf, 0, 0, buf.length);
        attr.putSkip(buf.length);
        attr.merge2beg();
        return false;
    }

    /**
     * place one attribute
     *
     * @param spkr where to signal
     * @param flg flags
     * @param typ type
     * @param trg taget
     * @param attr attribute
     */
    public static void placeAttrib(rtrBgpSpeak spkr, int flg, int typ, packHolder trg, packHolder attr) {
        attr.merge2beg();
        updtStatsArr(true, spkr.parent.attrStats, typ, attr);
        updtStatsArr(true, spkr.neigh.attrStats, typ, attr);
        byte[] buf = attr.getCopy();
        if (buf.length > 0xff) {
            flg |= flagLength;
        }
        trg.putByte(0, flg); // flags
        trg.putByte(1, typ); // type
        if ((flg & flagLength) == 0) {
            trg.putByte(2, buf.length); // length
            trg.putSkip(3);
        } else {
            trg.msbPutW(2, buf.length); // length
            trg.putSkip(4);
        }
        trg.putCopy(buf, 0, 0, buf.length);
        trg.putSkip(buf.length);
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
        List<tabRouteEntry<addrIP>> add = new ArrayList<tabRouteEntry<addrIP>>();
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
            if (parseAttrib(pck, cur)) {
                break;
            }
            interpretAttribute(spkr, ntry, add, del, cur);
        }
    }

    /**
     * encode attribute set
     *
     * @param spkr where to signal
     * @param as as number
     * @param ntry table entry
     */
    public static void encodeAttribSet(rtrBgpSpeak spkr, int as, tabRouteEntry<addrIP> ntry) {
        List<tabRouteEntry<addrIP>> lst = new ArrayList<tabRouteEntry<addrIP>>();
        lst.add(ntry);
        packHolder pck = new packHolder(true, true);
        createReachable(spkr, pck, new packHolder(true, true), safiAttrib, false, false, lst);
        ntry.best.attribAs = as;
        ntry.best.attribVal = pck.getCopy();
    }

    /**
     * interpret attribute
     *
     * @param spkr where to signal
     * @param ntry table entry
     * @param add prefixes reachable
     * @param del prefixes unreachable
     * @param pck packet to parse
     */
    public static void interpretAttribute(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, List<tabRouteEntry<addrIP>> add, List<tabRouteEntry<addrIP>> del, packHolder pck) {
        updtStatsArr(false, spkr.parent.attrStats, pck.ETHtype, pck);
        updtStatsArr(false, spkr.neigh.attrStats, pck.ETHtype, pck);
        if (spkr.neigh.attribFilter != null) {
            if (spkr.neigh.attribFilter.matches(pck.ETHtype)) {
                logger.info("filtered attribute " + pck.ETHtype + " from " + spkr.neigh.peerAddr + " (" + pck.dump() + ")");
                return;
            }
        }
        switch (pck.ETHtype) {
            case attrReachable:
                parseReachable(spkr, add, pck);
                return;
            case attrUnReach:
                parseUnReach(spkr, del, pck);
                return;
            case attrOrigin:
                parseOrigin(ntry, pck);
                return;
            case attrAsPath:
                parseAsPath(spkr, ntry, pck);
                return;
            case attrNextHop:
                parseNextHop(ntry, pck);
                return;
            case attrMetric:
                parseMetric(ntry, pck);
                return;
            case attrLocPref:
                parseLocPref(ntry, pck);
                return;
            case attrAtomicAggr:
                parseAtomicAggr(ntry);
                return;
            case attrEntropyLab:
                parseEntropyLab(ntry, pck);
                return;
            case attrAggregator:
                parseAggregator(spkr, ntry, pck);
                return;
            case attrConnector:
                parseConnector(ntry, pck);
                return;
            case attrPathLimit:
                parsePathLimit(ntry, pck);
                return;
            case attrPeDistLab:
                parsePeDistLab(ntry, pck);
                return;
            case attrStdComm:
                parseStdComm(ntry, pck);
                return;
            case attrExtComm:
                parseExtComm(ntry, pck);
                return;
            case attrLrgComm:
                parseLrgComm(ntry, pck);
                return;
            case attrOriginator:
                parseOriginator(ntry, pck);
                return;
            case attrTraffEng:
                parseTraffEng(ntry, pck);
                return;
            case attrAccIgp:
                parseAccIgp(ntry, pck);
                return;
            case attrPmsiTun:
                parsePmsiTun(ntry, pck);
                return;
            case attrLinkState:
                parseLnkSta(ntry, pck);
                return;
            case attrTunEnc:
                parseTunEnc(ntry, pck);
                return;
            case attrAttribSet:
                parseAttribSet(ntry, pck);
                return;
            case attrNshChain:
                parseNshChain(ntry, pck);
                return;
            case attrDomPath:
                parseDomainPath(ntry, pck);
                return;
            case attrBfdDisc:
                parseBfdDiscr(ntry, pck);
                return;
            case attrHopCapa:
                parseHopCapa(ntry, pck);
                return;
            case attrPrefSid:
                parsePrefSid(ntry, pck);
                return;
            case attrBier:
                parseBier(ntry, pck);
                return;
            case attrClustList:
                parseClustList(ntry, pck);
                return;
            case attrOnlyCust:
                parseOnlyCust(ntry, pck);
                return;
            default:
                if (spkr.neigh.unknownsLog) {
                    logger.info("got unknown (" + pck.ETHtype + ") attribute " + spkr.neigh.peerAddr + " -> " + spkr.neigh.localAddr + " " + pck.dump());
                }
                parseUnknown(ntry, pck);
                return;
        }
    }

    /**
     * create withdraw message
     *
     * @param spkr where to signal
     * @param pck packet to update
     * @param hlp helper packet
     * @param safi address family
     * @param addpath additional path
     * @param lst list of prefixes to withdraw
     */
    public static void createWithdraw(rtrBgpSpeak spkr, packHolder pck, packHolder hlp, int safi, boolean addpath, List<tabRouteEntry<addrIP>> lst) {
        if (safi == safiIp4uni) {
            for (int i = 0; i < lst.size(); i++) {
                tabRouteEntry<addrIP> ntry = lst.get(i);
                if (addpath) {
                    pck.msbPutD(0, ntry.best.ident);
                    pck.putSkip(4);
                }
                writePrefix(safiIp4uni, true, pck, ntry);
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
        placeUnreach(spkr, safi, addpath, pck, hlp, lst);
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
     * @param safi address family
     * @param addpath additional path
     * @param oneLab just one label
     * @param lst list of prefixes to advertise
     */
    public static void createReachable(rtrBgpSpeak spkr, packHolder pck, packHolder hlp, int safi, boolean addpath, boolean oneLab, List<tabRouteEntry<addrIP>> lst) {
        tabRouteEntry<addrIP> ntry = lst.get(0);
        placeUnknown(spkr, pck, hlp, ntry);
        placeOrigin(spkr, pck, hlp, ntry);
        placeAsPath(spkr, pck, hlp, ntry);
        placeMetric(spkr, pck, hlp, ntry);
        placeLocPref(spkr, pck, hlp, ntry);
        placeEntropyLab(spkr, pck, hlp, ntry);
        placeAtomicAggr(spkr, pck, hlp, ntry);
        placeAggregator(spkr, pck, hlp, ntry);
        placeConnector(spkr, pck, hlp, ntry);
        placePathLimit(spkr, pck, hlp, ntry);
        placePeDistLab(spkr, pck, hlp, ntry);
        placeStdComm(spkr, pck, hlp, ntry);
        placeExtComm(spkr, pck, hlp, ntry);
        placeLrgComm(spkr, pck, hlp, ntry);
        placeOriginator(spkr, pck, hlp, ntry);
        placeClustList(spkr, pck, hlp, ntry);
        placeTraffEng(spkr, pck, hlp, ntry);
        placeAccIgp(spkr, pck, hlp, ntry);
        placePmsiTun(spkr, pck, hlp, ntry);
        placeTunEnc(spkr, pck, hlp, ntry);
        placeLnkSta(spkr, pck, hlp, ntry);
        placeOnlyCust(spkr, pck, hlp, ntry);
        placePrefSid(spkr, safi, pck, hlp, ntry);
        placeBier(spkr, pck, hlp, ntry);
        placeNshChain(spkr, pck, hlp, ntry);
        placeDomainPath(spkr, pck, hlp, ntry);
        placeBfdDiscr(spkr, pck, hlp, ntry);
        placeHopCapa(spkr, pck, hlp, ntry);
        placeAttribSet(spkr, pck, hlp, ntry);
        if (safi == safiAttrib) {
            pck.merge2beg();
            return;
        }
        if (safi != safiIp4uni) {
            placeReachable(spkr, safi, addpath, oneLab, pck, hlp, lst);
            pck.merge2beg();
            pck.msbPutW(0, 0);
            pck.msbPutW(2, pck.dataSize());
            pck.putSkip(4);
            pck.merge2beg();
            return;
        }
        if (!ntry.best.nextHop.isIPv4()) {
            placeReachable(spkr, safi, addpath, oneLab, pck, hlp, lst);
            pck.merge2beg();
            pck.msbPutW(0, 0);
            pck.msbPutW(2, pck.dataSize());
            pck.putSkip(4);
            pck.merge2beg();
            return;
        }
        placeNextHop(spkr, pck, hlp, ntry);
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
            writePrefix(safiIp4uni, oneLab, pck, ntry);
        }
        pck.merge2end();
    }

    /**
     * create end of rib message
     *
     * @param spkr where to signal
     * @param pck packet to update
     * @param hlp helper packet
     * @param safi address family
     */
    public static void createEndOfRib(rtrBgpSpeak spkr, packHolder pck, packHolder hlp, int safi) {
        if (safi != safiIp4uni) {
            placeUnreach(spkr, safi, false, pck, hlp, new ArrayList<tabRouteEntry<addrIP>>());
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
     * place unknown attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeUnknown(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.unknown == null) {
            return;
        }
        for (int i = 0; i < ntry.best.unknown.size(); i++) {
            tabRouteBlob blb = ntry.best.unknown.get(i);
            hlp.clear();
            hlp.putCopy(blb.data, 0, 0, blb.data.length);
            hlp.putSkip(blb.data.length);
            if (spkr.neigh.unknownsLog) {
                logger.info("sent unknown (" + blb.type + ") attribute " + spkr.neigh.peerAddr + " -> " + spkr.neigh.localAddr + " " + hlp.dump());
            }
            placeAttrib(spkr, blb.flag, blb.type, trg, hlp);
        }
    }

    /**
     * place origin type attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeOrigin(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        hlp.clear();
        hlp.putByte(0, ntry.best.origin % 3);
        hlp.putSkip(1);
        placeAttrib(spkr, flagTransitive, attrOrigin, trg, hlp);
    }

    private static void placeAsList(boolean longAs, packHolder pck, int typ, List<Integer> lst) {
        if (lst == null) {
            return;
        }
        int pos = 0;
        int max = lst.size();
        for (; pos < max;) {
            int end = pos + 255;
            if (end > max) {
                end = max;
            }
            pck.putByte(0, typ);
            pck.putByte(1, end - pos);
            pck.putSkip(2);
            for (; pos < end; pos++) {
                int i = lst.get(pos);
                if (longAs) {
                    pck.msbPutD(0, i);
                    pck.putSkip(4);
                } else {
                    pck.msbPutW(0, tabRouteUtil.asNum16bit(i));
                    pck.putSkip(2);
                }
            }
        }
    }

    /**
     * place as path attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeAsPath(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        hlp.clear();
        placeAsList(spkr.peer32bitAS, hlp, 3, ntry.best.confSeq); // confed seq
        placeAsList(spkr.peer32bitAS, hlp, 4, ntry.best.confSet); // confed set
        placeAsList(spkr.peer32bitAS, hlp, 2, ntry.best.pathSeq); // as seq
        placeAsList(spkr.peer32bitAS, hlp, 1, ntry.best.pathSet); // as set
        placeAttrib(spkr, flagTransitive, attrAsPath, trg, hlp);
    }

    /**
     * place next hop attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeNextHop(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        hlp.clear();
        hlp.putAddr(0, ntry.best.nextHop.toIPv4());
        hlp.putSkip(addrIPv4.size);
        placeAttrib(spkr, flagTransitive, attrNextHop, trg, hlp);
    }

    /**
     * place metric attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeMetric(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.metric < 1) {
            return;
        }
        hlp.clear();
        hlp.msbPutD(0, ntry.best.metric);
        hlp.putSkip(4);
        placeAttrib(spkr, flagOptional, attrMetric, trg, hlp);
    }

    /**
     * place local preference attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeLocPref(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.locPref < 1) {
            return;
        }
        hlp.clear();
        hlp.msbPutD(0, ntry.best.locPref);
        hlp.putSkip(4);
        placeAttrib(spkr, flagTransitive, attrLocPref, trg, hlp);
    }

    /**
     * place entropy label attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeEntropyLab(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.entropyLabel == null) {
            return;
        }
        if (ntry.best.entropyLabel.length < 1) {
            return;
        }
        hlp.clear();
        hlp.putCopy(ntry.best.entropyLabel, 0, 0, ntry.best.entropyLabel.length);
        hlp.putSkip(ntry.best.entropyLabel.length);
        placeAttrib(spkr, flagTransitive, attrEntropyLab, trg, hlp);
    }

    /**
     * place atomic aggregator attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeAtomicAggr(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (!ntry.best.atomicAggr) {
            return;
        }
        hlp.clear();
        placeAttrib(spkr, flagTransitive, attrAtomicAggr, trg, hlp);
    }

    /**
     * place aggregator attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeAggregator(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.aggrRtr == null) {
            return;
        }
        hlp.clear();
        if (spkr.peer32bitAS) {
            hlp.msbPutD(0, ntry.best.aggrAs);
            hlp.putSkip(4);
        } else {
            hlp.msbPutW(0, tabRouteUtil.asNum16bit(ntry.best.aggrAs));
            hlp.putSkip(2);
        }
        hlp.putAddr(0, ntry.best.aggrRtr.toIPv4());
        hlp.putSkip(addrIPv4.size);
        placeAttrib(spkr, flagOptional | flagTransitive, attrAggregator, trg, hlp);
    }

    /**
     * place connector attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeConnector(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.connRtr == null) {
            return;
        }
        hlp.clear();
        hlp.msbPutD(0, 1);
        hlp.putAddr(4, ntry.best.connRtr.toIPv4());
        hlp.putSkip(4 + addrIPv4.size);
        placeAttrib(spkr, flagOptional | flagTransitive, attrConnector, trg, hlp);
    }

    /**
     * place path limit attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placePathLimit(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.pathLim < 1) {
            return;
        }
        hlp.clear();
        hlp.putByte(0, ntry.best.pathLim);
        hlp.msbPutD(1, ntry.best.pathAsn);
        hlp.putSkip(5);
        placeAttrib(spkr, flagOptional | flagTransitive, attrPathLimit, trg, hlp);
    }

    /**
     * place pe distinguisher attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placePeDistLab(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.pediRtr == null) {
            return;
        }
        hlp.clear();
        if (ntry.best.pediRtr.isIPv4()) {
            hlp.putAddr(0, ntry.best.pediRtr.toIPv4());
            hlp.putSkip(addrIPv4.size);
        } else {
            hlp.putAddr(0, ntry.best.pediRtr.toIPv6());
            hlp.putSkip(addrIPv6.size);
        }
        hlp.msbPutD(0, ntry.best.pediLab << 12);
        hlp.putSkip(3);
        placeAttrib(spkr, flagOptional | flagTransitive, attrPeDistLab, trg, hlp);
    }

    /**
     * place standard community attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeStdComm(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.stdComm == null) {
            return;
        }
        if (ntry.best.stdComm.size() < 1) {
            return;
        }
        hlp.clear();
        for (int i = 0; i < ntry.best.stdComm.size(); i++) {
            hlp.msbPutD(0, ntry.best.stdComm.get(i));
            hlp.putSkip(4);
        }
        placeAttrib(spkr, flagOptional | flagTransitive, attrStdComm, trg, hlp);
    }

    /**
     * place extended community attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeExtComm(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.extComm == null) {
            return;
        }
        if (ntry.best.extComm.size() < 1) {
            return;
        }
        hlp.clear();
        for (int i = 0; i < ntry.best.extComm.size(); i++) {
            hlp.msbPutQ(0, ntry.best.extComm.get(i));
            hlp.putSkip(8);
        }
        placeAttrib(spkr, flagOptional | flagTransitive, attrExtComm, trg, hlp);
    }

    /**
     * place large community attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeLrgComm(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.lrgComm == null) {
            return;
        }
        if (ntry.best.lrgComm.size() < 1) {
            return;
        }
        hlp.clear();
        for (int i = 0; i < ntry.best.lrgComm.size(); i++) {
            tabLargeComm d = ntry.best.lrgComm.get(i);
            hlp.msbPutD(0, d.as);
            hlp.msbPutD(4, d.d1);
            hlp.msbPutD(8, d.d2);
            hlp.putSkip(12);
        }
        placeAttrib(spkr, flagOptional | flagTransitive, attrLrgComm, trg, hlp);
    }

    /**
     * place originator attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeOriginator(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.originator == null) {
            return;
        }
        hlp.clear();
        hlp.putAddr(0, ntry.best.originator.toIPv4());
        hlp.putSkip(addrIPv4.size);
        placeAttrib(spkr, flagOptional, attrOriginator, trg, hlp);
    }

    /**
     * place accumulated igp attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeAccIgp(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.accIgp < 1) {
            return;
        }
        hlp.clear();
        hlp.putByte(0, 1); // type
        hlp.msbPutW(1, 11); // length
        hlp.msbPutQ(3, ntry.best.accIgp); // value
        hlp.putSkip(11);
        placeAttrib(spkr, flagOptional, attrAccIgp, trg, hlp);
    }

    /**
     * place traffic engineering attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeTraffEng(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.bandwidth < 1) {
            return;
        }
        hlp.clear();
        hlp.putByte(0, 1); // psc1
        hlp.putByte(1, 1); // packet
        hlp.msbPutW(2, 0); // reserved
        int i = Float.floatToIntBits(ntry.best.bandwidth / 8);
        hlp.msbPutD(4, i); // pri0
        hlp.msbPutD(8, i); // pri1
        hlp.msbPutD(12, i); // pri2
        hlp.msbPutD(16, i); // pri3
        hlp.msbPutD(20, i); // pri4
        hlp.msbPutD(24, i); // pri5
        hlp.msbPutD(28, i); // pri6
        hlp.msbPutD(32, i); // pri7
        hlp.msbPutD(36, Float.floatToIntBits(1)); // minimum
        hlp.msbPutW(40, 1500); // mtu
        hlp.putSkip(42);
        placeAttrib(spkr, flagOptional, attrTraffEng, trg, hlp);
    }

    /**
     * place pmsi tunnel attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placePmsiTun(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.pmsiTun == null) {
            return;
        }
        hlp.clear();
        hlp.msbPutW(0, ntry.best.pmsiTyp);
        hlp.msbPutD(2, ntry.best.pmsiLab << 8);
        hlp.putSkip(5);
        hlp.putCopy(ntry.best.pmsiTun, 0, 0, ntry.best.pmsiTun.length);
        hlp.putSkip(ntry.best.pmsiTun.length);
        placeAttrib(spkr, flagOptional | flagTransitive, attrPmsiTun, trg, hlp);
    }

    /**
     * place tunnel encapsulation attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeTunEnc(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.tunelVal == null) {
            return;
        }
        hlp.clear();
        hlp.msbPutW(0, ntry.best.tunelTyp);
        hlp.msbPutW(2, ntry.best.tunelVal.length);
        hlp.putSkip(4);
        hlp.putCopy(ntry.best.tunelVal, 0, 0, ntry.best.tunelVal.length);
        hlp.putSkip(ntry.best.tunelVal.length);
        placeAttrib(spkr, flagOptional | flagTransitive, attrTunEnc, trg, hlp);
    }

    /**
     * place link state attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeLnkSta(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.linkStat == null) {
            return;
        }
        hlp.clear();
        hlp.putCopy(ntry.best.linkStat, 0, 0, ntry.best.linkStat.length);
        hlp.putSkip(ntry.best.linkStat.length);
        placeAttrib(spkr, flagOptional, attrLinkState, trg, hlp);
    }

    /**
     * place attribute set attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeAttribSet(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.attribVal == null) {
            return;
        }
        hlp.clear();
        hlp.msbPutD(0, ntry.best.attribAs);
        hlp.putSkip(4);
        hlp.putCopy(ntry.best.attribVal, 0, 0, ntry.best.attribVal.length);
        hlp.putSkip(ntry.best.attribVal.length);
        placeAttrib(spkr, flagOptional | flagTransitive, attrAttribSet, trg, hlp);
    }

    /**
     * place nsh chain attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeNshChain(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.nshChain == null) {
            return;
        }
        hlp.clear();
        hlp.putCopy(ntry.best.nshChain, 0, 0, ntry.best.nshChain.length);
        hlp.putSkip(ntry.best.nshChain.length);
        placeAttrib(spkr, flagOptional | flagTransitive, attrNshChain, trg, hlp);
    }

    /**
     * place domain path attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeDomainPath(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.domainPath == null) {
            return;
        }
        hlp.clear();
        hlp.putCopy(ntry.best.domainPath, 0, 0, ntry.best.domainPath.length);
        hlp.putSkip(ntry.best.domainPath.length);
        placeAttrib(spkr, flagOptional | flagTransitive, attrDomPath, trg, hlp);
    }

    /**
     * place bfd discr attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeBfdDiscr(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.bfdDiscr == null) {
            return;
        }
        hlp.clear();
        hlp.putCopy(ntry.best.bfdDiscr, 0, 0, ntry.best.bfdDiscr.length);
        hlp.putSkip(ntry.best.bfdDiscr.length);
        placeAttrib(spkr, flagOptional | flagTransitive, attrHopCapa, trg, hlp);
    }

    /**
     * place hop capability attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeHopCapa(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.hopCapa == null) {
            return;
        }
        hlp.clear();
        hlp.putCopy(ntry.best.hopCapa, 0, 0, ntry.best.hopCapa.length);
        hlp.putSkip(ntry.best.hopCapa.length);
        placeAttrib(spkr, flagOptional | flagTransitive, attrBfdDisc, trg, hlp);
    }

    /**
     * place prefix sid attribute
     *
     * @param spkr where to signal
     * @param safi sub address family
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placePrefSid(rtrBgpSpeak spkr, int safi, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        int afi = safi & afiMask;
        hlp.clear();
        encTlv tlv = getPrefSidTlv();
        if (ntry.best.segrouIdx != 0) {
            tlv.valDat[0] = 0; // reserved
            bits.msbPutW(tlv.valDat, 1, 0); // flags
            bits.msbPutD(tlv.valDat, 3, ntry.best.segrouIdx); // index
            tlv.putBytes(hlp, 1, 7, tlv.valDat);
        }
        if (ntry.best.segrouSiz != 0) {
            bits.msbPutW(tlv.valDat, 0, 0); // flags
            bits.msbPutD(tlv.valDat, 2, ntry.best.segrouBeg << 8); // base
            bits.msbPutD(tlv.valDat, 5, ntry.best.segrouSiz << 8); // range
            tlv.putBytes(hlp, 3, 8, tlv.valDat);
        }
        if (ntry.best.segrouPrf != null) {
            int i;
            int o;
            switch (afi) {
                case afiIpv4:
                    i = 0x13;
                    o = 5;
                    break;
                case afiIpv6:
                    i = 0x12;
                    o = 5;
                    break;
                case afiL2vpn:
                    i = 0x15;
                    o = 6;
                    break;
                default:
                    i = 0xffff;
                    o = 4;
                    break;
            }
            tlv.valDat[0] = 0; // reserved
            tlv.valDat[1] = 1; // subtlv type
            bits.msbPutW(tlv.valDat, 2, 30); // size
            tlv.valDat[4] = 0; // reserved
            ntry.best.segrouPrf.toIPv6().toBuffer(tlv.valDat, 5);
            tlv.valDat[21] = 0; // sid flags
            bits.msbPutW(tlv.valDat, 22, i); // behavior
            tlv.valDat[24] = 0; // reserved
            tlv.valDat[25] = 1; // sid structure
            bits.msbPutW(tlv.valDat, 26, 6); // subsubtlv length
            tlv.valDat[28] = 32; // locator block length
            tlv.valDat[29] = 16; // locator node length
            tlv.valDat[30] = 16; // locator function length
            tlv.valDat[31] = 0; // locator argument length
            tlv.valDat[32] = (byte) ntry.best.segrouSiz; // transposition length
            tlv.valDat[33] = (byte) ntry.best.segrouOfs; // transposition offset
            tlv.putBytes(hlp, o, 34, tlv.valDat);
        }
        if (hlp.headSize() < 1) {
            return;
        }
        placeAttrib(spkr, flagOptional | flagTransitive, attrPrefSid, trg, hlp);
    }

    /**
     * place bier attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeBier(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.bierIdx == 0) {
            return;
        }
        hlp.clear();
        encTlv tlv = getBierTlv();
        tlv.valDat[0] = (byte) ntry.best.bierSub; // subdomain
        bits.msbPutW(tlv.valDat, 1, ntry.best.bierIdx); // bfr id
        tlv.putBytes(hlp, 1, 4, tlv.valDat);
        if (ntry.best.bierSiz != 0) {
            bits.msbPutD(tlv.valDat, 0, ntry.best.bierBeg); // base + bsl
            tlv.valDat[1] |= (byte) (ntry.best.bierHdr << 4); // bsl
            tlv.valDat[0] = (byte) ntry.best.bierSiz; // range
            tlv.putBytes(hlp, 2, 4, tlv.valDat);
        }
        placeAttrib(spkr, flagOptional | flagTransitive, attrBier, trg, hlp);
    }

    /**
     * place cluster list attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeClustList(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.clustList == null) {
            return;
        }
        hlp.clear();
        for (int i = 0; i < ntry.best.clustList.size(); i++) {
            hlp.putAddr(0, ntry.best.clustList.get(i).toIPv4());
            hlp.putSkip(addrIPv4.size);
        }
        placeAttrib(spkr, flagOptional, attrClustList, trg, hlp);
    }

    /**
     * place only to customer attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeOnlyCust(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.onlyCust == 0) {
            return;
        }
        hlp.clear();
        hlp.msbPutD(0, ntry.best.onlyCust);
        hlp.putSkip(4);
        placeAttrib(spkr, flagOptional | flagTransitive, attrOnlyCust, trg, hlp);
    }

    /**
     * place reachable attribute
     *
     * @param spkr where to signal
     * @param safi sub address family
     * @param addpath additional path
     * @param oneLab just one label
     * @param trg target packet
     * @param hlp helper packet
     * @param lst list of table entries
     */
    public static void placeReachable(rtrBgpSpeak spkr, int safi, boolean addpath, boolean oneLab, packHolder trg, packHolder hlp, List<tabRouteEntry<addrIP>> lst) {
        int afi = safi & afiMask;
        int sfi = safi & sfiMask;
        addrIP nextHop = lst.get(0).best.nextHop;
        boolean v6nh = afi == afiIpv6;
        if (!v6nh) {
            v6nh = !nextHop.isIPv4();
        }
        int i = v6nh ? addrIPv6.size : addrIPv4.size;
        if ((sfi == sfiMplsVpnU) || (sfi == sfiMplsVpnM) || (sfi == sfiClsTrnPl)) {
            i += 8;
        }
        hlp.clear();
        hlp.msbPutD(0, safi2triplet(safi));
        hlp.putByte(3, i);
        hlp.putSkip(4);
        if ((sfi == sfiMplsVpnU) || (sfi == sfiMplsVpnM) || (sfi == sfiClsTrnPl)) {
            hlp.msbPutQ(0, 0); // rd
            hlp.putSkip(8);
        }
        if (v6nh) {
            writeAddress(afiIpv6, hlp, nextHop);
        } else {
            writeAddress(afiIpv4, hlp, nextHop);
        }
        hlp.putByte(0, 0);
        hlp.putSkip(1);
        for (i = 0; i < lst.size(); i++) {
            tabRouteEntry<addrIP> ntry = lst.get(i);
            if (addpath) {
                hlp.msbPutD(0, ntry.best.ident);
                hlp.putSkip(4);
            }
            writePrefix(safi, oneLab, hlp, ntry);
        }
        placeAttrib(spkr, flagOptional, attrReachable, trg, hlp);
    }

    /**
     * place unreachable attribute
     *
     * @param spkr where to signal
     * @param safi sub address family
     * @param addpath additional path
     * @param trg target packet
     * @param hlp helper packet
     * @param lst list of table entries
     */
    public static void placeUnreach(rtrBgpSpeak spkr, int safi, boolean addpath, packHolder trg, packHolder hlp, List<tabRouteEntry<addrIP>> lst) {
        hlp.clear();
        hlp.msbPutD(0, safi2triplet(safi));
        hlp.putSkip(3);
        for (int i = 0; i < lst.size(); i++) {
            tabRouteEntry<addrIP> ntry = lst.get(i);
            if (addpath) {
                hlp.msbPutD(0, ntry.best.ident);
                hlp.putSkip(4);
            }
            writePrefix(safi, true, hlp, ntry);
        }
        placeAttrib(spkr, flagOptional, attrUnReach, trg, hlp);
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
