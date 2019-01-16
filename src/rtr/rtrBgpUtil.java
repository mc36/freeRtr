package rtr;

import addr.addrEmpty;
import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrMac;
import addr.addrPrefix;
import addr.addrType;
import java.util.ArrayList;
import java.util.List;
import pack.packHolder;
import tab.tabLargeComm;
import tab.tabRouteEntry;
import util.bits;
import util.debugger;
import util.logger;
import util.typLenVal;

/**
 * bgp4 utilities
 *
 * @author matecsaba
 */
public class rtrBgpUtil {

    private rtrBgpUtil() {
    }

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
     * l2vpn4 address family
     */
    public final static int afiL2vpn4 = 0x190000;

    /**
     * l2vpn6 address family
     */
    public final static int afiL2vpn6 = 0x70190000;

    /**
     * address family mask
     */
    public final static int afiMask = 0xffff0000;

    /**
     * sub address family mask
     */
    public final static int safiMask = 0xffff;

    /**
     * unicast address family
     */
    public final static int safiUnicast = 0x01;

    /**
     * multicast address family
     */
    public final static int safiMulticast = 0x02;

    /**
     * labeled address family
     */
    public final static int safiLabeled = 0x04;

    /**
     * multicast vpn address family
     */
    public final static int safiMvpn = 0x05;

    /**
     * dynamic multi segment pswudowires address family
     */
    public final static int safiMspw = 0x06;

    /**
     * tunnel encapsulation address family
     */
    public final static int safiTunnel = 0x07;

    /**
     * multicast vpls address family
     */
    public final static int safiMcastVpls = 0x08;

    /**
     * l2vpn address family
     */
    public final static int safiVpls = 0x41;

    /**
     * mdt address family
     */
    public final static int safiMdt = 0x42;

    /**
     * 4over6 address family
     */
    public final static int safi4o6 = 0x43;

    /**
     * 6over4 address family
     */
    public final static int safi6o4 = 0x44;

    /**
     * layer1 vpn address family
     */
    public final static int safiL1vpn = 0x45;

    /**
     * ethernet vpn address family
     */
    public final static int safiEthVpn = 0x46;

    /**
     * link state address family
     */
    public final static int safiLnkSt = 0x47;

    /**
     * link state vpn address family
     */
    public final static int safiLnkStVpn = 0x48;

    /**
     * segment routing traffic engineering address family
     */
    public final static int safiSrTe = 0x49;

    /**
     * vpn unicast address family
     */
    public final static int safiMplsVpnU = 0x80;

    /**
     * vpn multicast address family
     */
    public final static int safiMplsVpnM = 0x81;

    /**
     * route target constrain
     */
    public final static int safiRtCnst = 0x84;

    /**
     * flow specification
     */
    public final static int safiFlwSpc = 0x85;

    /**
     * mpls vpn flowspec
     */
    public final static int safiVpnFlw = 0x86;

    /**
     * attributes dump
     */
    public final static int safiAttrib = -1;

    /**
     * ipv4 unicast address family
     */
    public final static int safiIp4uni = afiIpv4 | safiUnicast;

    /**
     * ipv6 unicast address family
     */
    public final static int safiIp6uni = afiIpv6 | safiUnicast;

    /**
     * ipv4 multicast address family
     */
    public final static int safiIp4multi = afiIpv4 | safiMulticast;

    /**
     * ipv6 multicast address family
     */
    public final static int safiIp6multi = afiIpv6 | safiMulticast;

    /**
     * ipv4 labeled address family
     */
    public final static int safiIp4lab = afiIpv4 | safiLabeled;

    /**
     * ipv6 labeled address family
     */
    public final static int safiIp6lab = afiIpv6 | safiLabeled;

    /**
     * ipv4 flowspec address family
     */
    public final static int safiIp4flow = afiIpv4 | safiFlwSpc;

    /**
     * ipv6 flowspec address family
     */
    public final static int safiIp6flow = afiIpv6 | safiFlwSpc;

    /**
     * ipv4 labeled vpn unicast address family
     */
    public final static int safiIp4vpnU = afiIpv4 | safiMplsVpnU;

    /**
     * ipv6 labeled vpn unicast address family
     */
    public final static int safiIp6vpnU = afiIpv6 | safiMplsVpnU;

    /**
     * ipv4 labeled vpn multicast address family
     */
    public final static int safiIp4vpnM = afiIpv4 | safiMplsVpnM;

    /**
     * ipv6 labeled vpn multicast address family
     */
    public final static int safiIp6vpnM = afiIpv6 | safiMplsVpnM;

    /**
     * ipv4 vpn flowspec address family
     */
    public final static int safiIp4vpnF = afiIpv4 | safiVpnFlw;

    /**
     * ipv6 vpn flowspec address family
     */
    public final static int safiIp6vpnF = afiIpv6 | safiVpnFlw;

    /**
     * ipv4 mdt address family
     */
    public final static int safiIp4mdt = afiIpv4 | safiMdt;

    /**
     * ipv6 mdt address family
     */
    public final static int safiIp6mdt = afiIpv6 | safiMdt;

    /**
     * ipv4 mvpn address family
     */
    public final static int safiIp4srte = afiIpv4 | safiSrTe;
    /**
     * ipv6 mvpn address family
     */
    public final static int safiIp6srte = afiIpv6 | safiSrTe;

    /**
     * ipv4 mvpn address family
     */
    public final static int safiIp4mvpn = afiIpv4 | safiMvpn;

    /**
     * ipv6 mvpn address family
     */
    public final static int safiIp6mvpn = afiIpv6 | safiMvpn;

    /**
     * ipv4 vpls address family
     */
    public final static int safiVpls4 = afiL2vpn4 | safiVpls;

    /**
     * ipv6 vpls address family
     */
    public final static int safiVpls6 = afiL2vpn6 | safiVpls;

    /**
     * ipv4 mspw address family
     */
    public final static int safiMspw4 = afiL2vpn4 | safiMspw;

    /**
     * ipv6 mspw address family
     */
    public final static int safiMspw6 = afiL2vpn6 | safiMspw;

    /**
     * ipv4 ethvpn address family
     */
    public final static int safiEvpn4 = afiL2vpn4 | safiEthVpn;

    /**
     * ipv6 ethvpn address family
     */
    public final static int safiEvpn6 = afiL2vpn6 | safiEthVpn;

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
     * open
     */
    public static final int msgOpen = 1;

    /**
     * update
     */
    public static final int msgUpdate = 2;

    /**
     * notification
     */
    public static final int msgNotify = 3;

    /**
     * keep alive
     */
    public static final int msgKeepLiv = 4;

    /**
     * route refresh
     */
    public static final int msgRefrsh = 5;

    /**
     * capabilities
     */
    public static final int msgCapability = 6;

    /**
     * compression
     */
    public static final int msgCompress = 7;

    /**
     * optional
     */
    public static final int flagOptional = 0x80;

    /**
     * transitive
     */
    public static final int flagTransitive = 0x40;

    /**
     * complete
     */
    public static final int flagComplete = 0x20;

    /**
     * length
     */
    public static final int flagLength = 0x10;

    /**
     * origin type, 0=igp, 1=egp, 2=incomplete
     */
    public static final int attrOrigin = 1;

    /**
     * as path
     */
    public static final int attrAsPath = 2;

    /**
     * next hop
     */
    public static final int attrNextHop = 3;

    /**
     * multi exit discriminator
     */
    public static final int attrMetric = 4;

    /**
     * local preference
     */
    public static final int attrLocPref = 5;

    /**
     * atomic aggregate
     */
    public static final int attrAtomicAggr = 6;

    /**
     * aggregate
     */
    public static final int attrAggregator = 7;

    /**
     * standard community
     */
    public static final int attrStdComm = 8;

    /**
     * originator
     */
    public static final int attrOriginator = 9;

    /**
     * cluster list
     */
    public static final int attrClustList = 10;

    /**
     * mp reach nlri
     */
    public static final int attrReachable = 14;

    /**
     * mp unreach nlri
     */
    public static final int attrUnReach = 15;

    /**
     * extended community
     */
    public static final int attrExtComm = 16;

    /**
     * as4 path
     */
    public static final int attrAs4path = 17;

    /**
     * as4 aggregator
     */
    public static final int attrAs4aggr = 18;

    /**
     * pmsi tunnel
     */
    public static final int attrPmsiTun = 22;

    /**
     * tunnel encapsulation
     */
    public static final int attrTunEnc = 23;

    /**
     * traffic engineering
     */
    public static final int attrTraffEng = 24;

    /**
     * ipv6 extended community
     */
    public static final int attrIpv6comm = 25;

    /**
     * accumulated igp
     */
    public static final int attrAccIgp = 26;

    /**
     * pe distinguisher label
     */
    public static final int attrPeDistLab = 27;

    /**
     * entropy label
     */
    public static final int attrEntropyLab = 28;

    /**
     * link state
     */
    public static final int attrLinkState = 29;

    /**
     * large community
     */
    public static final int attrLrgComm = 32;

    /**
     * bgpsec path
     */
    public static final int attrBgpSec = 33;

    /**
     * prefix sid
     */
    public static final int attrPrefSid = 40;

    /**
     * bier
     */
    public static final int attrBier = 44;

    /**
     * attribute set
     */
    public static final int attrAttribSet = 128;

    /**
     * multiprotocol bgp
     */
    public static final int capaMultiProto = 1;

    /**
     * route refresh
     */
    public static final int capaRouteRefresh = 2;

    /**
     * outbound route filter
     */
    public static final int capaRouteFilter = 3;

    /**
     * multiple routes
     */
    public static final int capaMultiRoute = 4;

    /**
     * extended next hop
     */
    public static final int capaExtNextHop = 5;

    /**
     * graceful restart
     */
    public static final int capaGraceRestart = 64;

    /**
     * 32bit as number
     */
    public static final int capa32bitAsNum = 65;

    /**
     * dynamic capability
     */
    public static final int capaDynamicCapa = 67;

    /**
     * multisession
     */
    public static final int capaMultisession = 68;

    /**
     * additional path
     */
    public static final int capaAdditionPath = 69;

    /**
     * enhanced route refresh
     */
    public static final int capaEnhancedRefresh = 70;

    /**
     * long lived graceful restart
     */
    public static final int capaLongGrace = 71;

    /**
     * covering prefixes outbound route filter
     */
    public static final int capaCoverFilter = 72;

    /**
     * hostname
     */
    public static final int capaHostname = 73;

    /**
     * compression
     */
    public static final int capaCompress = 76;

    /**
     * no export community
     */
    public static final int commNoExport = 0xffffff01;

    /**
     * no advertise community
     */
    public static final int commNoAdvertise = 0xffffff02;

    /**
     * no sub confederation community
     */
    public static final int commNoConfed = 0xffffff03;

    /**
     * no peer community
     */
    public static final int commNoPeer = 0xffffff04;

    /**
     * accept own community
     */
    public static final int commAcceptOwn = 0xffff0001;

    /**
     * blackhole community
     */
    public static final int commBlackhole = 0xffff029a;

    /**
     * convert type to string
     *
     * @param i type to convert
     * @return string
     */
    public static String type2string(int i) {
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
                ntry.evpnLab = pck.msbGetD(14) >>> 8;
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
                ntry.evpnLab = pck.msbGetD(0) >>> 8;
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
                ntry.evpnLab = pck.msbGetD(0) >>> 8;
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
                bits.msbPutD(trg, pos, ntry.evpnLab << 8);
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
                if (ntry.prefix.broadcast.isFilled(0)) {
                    adr = new addrEmpty();
                }
                trg[pos] = (byte) adr.maxBits();
                pos++;
                adr.toBuffer(trg, pos);
                pos += adr.getSize();
                bits.msbPutD(trg, pos, ntry.evpnLab << 8);
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
                bits.msbPutD(trg, pos, ntry.evpnLab << 8);
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
        final int sfi = safi & safiMask;
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        int i;
        int p = 0;
        switch (sfi) {
            case safiEthVpn:
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
            case safiVpls:
                i = pck.msbGetW(0) * 8;
                pck.getSkip(2);
                break;
            case safiFlwSpc:
            case safiVpnFlw:
                i = pck.getByte(0);
                if (i >= 0xf0) {
                    i = pck.msbGetW(0) & 0xfff;
                    pck.getSkip(2);
                } else {
                    pck.getSkip(1);
                }
                i *= 8;
                break;
            case safiMvpn:
                p = pck.getByte(0);
                i = pck.getByte(1) * 8;
                pck.getSkip(2);
                break;
            default:
                i = pck.getByte(0);
                pck.getSkip(1);
                break;
        }
        if ((sfi == safiLabeled) || (sfi == safiMplsVpnU)) {
            ntry.labelRem = new ArrayList<Integer>();
            p = 0;
            for (;;) {
                if ((p & 1) != 0) {
                    break;
                }
                if (i <= 24) {
                    return null;
                }
                p = pck.msbGetD(0) >>> 8;
                pck.getSkip(3);
                i -= 24;
                ntry.labelRem.add(p >>> 4);
                if (oneLab) {
                    break;
                }
            }
        }
        if ((sfi == safiMplsVpnU) || (sfi == safiMplsVpnM) || (sfi == safiVpls) || (sfi == safiMspw) || (sfi == safiMdt) || (sfi == safiSrTe) || (sfi == safiVpnFlw) || (sfi == safiMvpn)) {
            ntry.rouDst = pck.msbGetQ(0);
            pck.getSkip(8);
            i -= 64;
        }
        int o = (i + 7) / 8;
        byte[] buf = new byte[128];
        bits.byteFill(buf, 0, buf.length, 0);
        pck.getCopy(buf, 0, 0, o);
        pck.getSkip(o);
        if (sfi == safiMvpn) {
            bits.byteCopy(buf, 0, buf, 1, o);
            buf[0] = (byte) p;
            o++;
        }
        if ((sfi == safiFlwSpc) || (sfi == safiVpnFlw) || (sfi == safiMvpn) || (sfi == safiMspw)) {
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
        if ((sfi == safiVpls) && (o == 9)) {
            byte[] adr = new byte[addrIP.size];
            ntry.prefix = new addrPrefix<addrIP>(new addrIP(), adr.length * 8);
            bits.byteCopy(buf, 0, adr, 0, 4);
            ntry.prefix.network.fromBuf(adr, 0);
            adr[0] = 5;
            bits.byteCopy(buf, 4, adr, 1, 5);
            ntry.prefix.wildcard.fromBuf(adr, 0);
            return ntry;
        }
        if (sfi == safiMdt) {
            o = o / 2;
            byte[] adr = new byte[addrIP.size];
            ntry.prefix = new addrPrefix<addrIP>(new addrIP(), adr.length * 8);
            bits.byteCopy(buf, 0, adr, 0, o);
            ntry.prefix.network.fromBuf(adr, 0);
            bits.byteCopy(buf, o, adr, 0, o);
            ntry.prefix.broadcast.fromBuf(adr, 0);
            return ntry;
        }
        switch (safi & afiMask) {
            case afiIpv4:
            case afiL2vpn4:
                addrIPv4 a4 = new addrIPv4();
                a4.fromBuf(buf, 0);
                ntry.prefix = addrPrefix.ip4toIP(new addrPrefix<addrIPv4>(a4, i));
                return ntry;
            case afiIpv6:
            case afiL2vpn6:
                addrIPv6 a6 = new addrIPv6();
                a6.fromBuf(buf, 0);
                ntry.prefix = addrPrefix.ip6toIP(new addrPrefix<addrIPv6>(a6, i));
                return ntry;
            default:
                return null;
        }
    }

    /**
     * write prefix
     *
     * @param safi safi to write
     * @param pck packet to use
     * @param ntry prefix to write
     */
    public static void writePrefix(int safi, packHolder pck, tabRouteEntry<addrIP> ntry) {
        final int sfi = safi & safiMask;
        byte[] buf2;
        int i;
        switch (safi & afiMask) {
            case afiIpv4:
            case afiL2vpn4:
                addrPrefix<addrIPv4> a4 = addrPrefix.ip2ip4(ntry.prefix);
                i = a4.maskLen;
                buf2 = a4.network.getBytes();
                break;
            case afiIpv6:
            case afiL2vpn6:
                addrPrefix<addrIPv6> a6 = addrPrefix.ip2ip6(ntry.prefix);
                i = a6.maskLen;
                buf2 = a6.network.getBytes();
                break;
            default:
                pck.putByte(0, 0);
                pck.putSkip(1);
                return;
        }
        if ((sfi == safiVpls) && (ntry.prefix.wildcard.getBytes()[0] == 5)) {
            buf2 = new byte[9];
            bits.byteCopy(ntry.prefix.network.getBytes(), 0, buf2, 0, 4);
            bits.byteCopy(ntry.prefix.wildcard.getBytes(), 1, buf2, 4, 5);
            i = buf2.length * 8;
        }
        if (sfi == safiMdt) {
            int as = buf2.length;
            buf2 = new byte[as * 2];
            bits.byteCopy(ntry.prefix.network.getBytes(), 0, buf2, 0, as);
            bits.byteCopy(ntry.prefix.broadcast.getBytes(), 0, buf2, as, as);
            i = buf2.length * 8;
        }
        if ((sfi == safiFlwSpc) || (sfi == safiVpnFlw) || (sfi == safiMvpn) || (sfi == safiMspw)) {
            buf2 = new byte[addrIP.size * 4];
            bits.byteCopy(ntry.prefix.network.getBytes(), 1, buf2, 0, 15);
            bits.byteCopy(ntry.prefix.broadcast.getBytes(), 0, buf2, 15, 16);
            bits.byteCopy(ntry.prefix.wildcard.getBytes(), 0, buf2, 31, 16);
            bits.byteCopy(ntry.prefix.mask.getBytes(), 0, buf2, 47, 16);
            i = ntry.prefix.network.getBytes()[0] * 8;
        }
        int o = (i + 7) / 8;
        byte[] buf1 = new byte[128];
        int p = 0;
        if ((sfi == safiLabeled) || (sfi == safiMplsVpnU)) {
            for (int q = 0; q < ntry.labelRem.size(); q++) {
                bits.msbPutD(buf1, p, ntry.labelRem.get(q) << 12);
                p += 3;
                i += 24;
            }
            buf1[p - 1] |= 1;
        }
        if ((sfi == safiMplsVpnU) || (sfi == safiMplsVpnM) || (sfi == safiVpls) || (sfi == safiMspw) || (sfi == safiMdt) || (sfi == safiSrTe) || (sfi == safiVpnFlw) || (sfi == safiMvpn)) {
            bits.msbPutQ(buf1, p, ntry.rouDst);
            p += 8;
            i += 64;
        }
        switch (sfi) {
            case safiEthVpn:
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
            case safiVpls:
                pck.msbPutW(0, o + p);
                pck.putSkip(2);
                break;
            case safiFlwSpc:
            case safiVpnFlw:
                i = o + p;
                if (i < 0xf0) {
                    pck.putByte(0, i);
                    pck.putSkip(1);
                } else {
                    pck.msbPutW(0, i | 0xf000);
                    pck.putSkip(2);
                }
                break;
            case safiMvpn:
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
            case afiL2vpn4:
                addrIPv4 a4 = new addrIPv4();
                pck.getAddr(a4, 0);
                pck.getSkip(addrIPv4.size);
                ax.fromIPv4addr(a4);
                return ax;
            case afiIpv6:
            case afiL2vpn6:
                addrIPv6 a6 = new addrIPv6();
                pck.getAddr(a6, 0);
                pck.getSkip(addrIPv6.size);
                ax.fromIPv6addr(a6);
                return ax;
            default:
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
            case afiL2vpn4:
                addrIPv4 a4 = addr.toIPv4();
                pck.putAddr(0, a4);
                pck.putSkip(addrIPv4.size);
                break;
            case afiIpv6:
            case afiL2vpn6:
                addrIPv6 a6 = addr.toIPv6();
                pck.putAddr(0, a6);
                pck.putSkip(addrIPv6.size);
                break;
            default:
                break;
        }
    }

    /**
     * get size of address
     *
     * @param safi safi to use
     * @return size of address
     */
    public static int addressSize(int safi) {
        switch (safi & afiMask) {
            case afiIpv4:
            case afiL2vpn4:
                return addrIPv4.size;
            case afiIpv6:
            case afiL2vpn6:
                return addrIPv6.size;
            default:
                return 0;
        }
    }

    /**
     * get default route
     *
     * @param safi safi to use
     * @return default route prefix
     */
    public static addrPrefix<addrIP> defaultRoute(int safi) {
        switch (safi & afiMask) {
            case afiIpv4:
            case afiL2vpn4:
                return addrPrefix.ip4toIP(addrPrefix.defaultRoute4());
            case afiIpv6:
            case afiL2vpn6:
                return addrPrefix.ip6toIP(addrPrefix.defaultRoute6());
            default:
                return null;
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
     * convert as number to 16 bits
     *
     * @param i as number to convert
     * @return converted
     */
    public static int asNum16bit(int i) {
        if ((i & 0xffff) == i) {
            return i;
        }
        return 23456;
    }

    /**
     * test if private as number
     *
     * @param i as number to test
     * @return false if not, true if yes
     */
    public static boolean asNumPrivate(int i) {
        if ((i >= 64512) && (i <= 65534)) {
            return true;
        }
        if ((i >= -94967296) && (i < -2)) { // 4200000000 - 4294967294
            return true;
        }
        return false;
    }

    /**
     * test if documentary as number
     *
     * @param i as number to test
     * @return false if not, true if yes
     */
    public static boolean asNumDocumentary(int i) {
        if ((i >= 64496) && (i <= 64511)) {
            return true;
        }
        if ((i >= 65536) && (i <= 65551)) {
            return true;
        }
        return false;
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
            case safiIp4mdt:
                return "mdt4";
            case safiIp6mdt:
                return "mdt6";
            case safiIp4srte:
                return "srte4";
            case safiIp6srte:
                return "srte6";
            case safiIp4mvpn:
                return "mvpn4";
            case safiIp6mvpn:
                return "mvpn6";
            case safiVpls4:
                return "vpls4";
            case safiVpls6:
                return "vpls6";
            case safiMspw4:
                return "mspw4";
            case safiMspw6:
                return "mspw6";
            case safiEvpn4:
                return "evpn4";
            case safiEvpn6:
                return "evpn6";
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
        ntry.origin = pck.getByte(0);
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
     * @param longAs long as support
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseAsPath(boolean longAs, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.pathSeq = new ArrayList<Integer>();
        ntry.pathSet = new ArrayList<Integer>();
        ntry.confSeq = new ArrayList<Integer>();
        ntry.confSet = new ArrayList<Integer>();
        for (; pck.dataSize() > 0;) {
            int i = pck.getByte(0);
            pck.getSkip(1);
            switch (i) {
                case 1: // as set
                    parseAsList(longAs, ntry.pathSet, pck);
                    break;
                case 2: // as seq
                    parseAsList(longAs, ntry.pathSeq, pck);
                    break;
                case 3: // confed seq
                    parseAsList(longAs, ntry.confSeq, pck);
                    break;
                case 4: // confed set
                    parseAsList(longAs, ntry.pathSet, pck);
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
        ntry.nextHop = ax;
    }

    /**
     * parse metric attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseMetric(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.metric = pck.msbGetD(0);
    }

    /**
     * parse local preference attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseLocPref(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.locPref = pck.msbGetD(0);
    }

    /**
     * parse atomic aggregator attribute
     *
     * @param ntry table entry
     */
    public static void parseAtomicAggr(tabRouteEntry<addrIP> ntry) {
        ntry.atomicAggr = true;
    }

    /**
     * parse aggregator attribute
     *
     * @param longAs long as support
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseAggregator(boolean longAs, tabRouteEntry<addrIP> ntry, packHolder pck) {
        if (longAs) {
            ntry.aggrAs = pck.msbGetD(0);
            pck.getSkip(4);
        } else {
            ntry.aggrAs = pck.msbGetW(0);
            pck.getSkip(2);
        }
        addrIPv4 as = new addrIPv4();
        pck.getAddr(as, 0);
        addrIP ax = new addrIP();
        ax.fromIPv4addr(as);
        ntry.aggrRtr = ax;
    }

    /**
     * parse standard community attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseStdComm(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.stdComm = new ArrayList<Integer>();
        for (; pck.dataSize() >= 4;) {
            ntry.stdComm.add(pck.msbGetD(0));
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
        ntry.extComm = new ArrayList<Long>();
        for (; pck.dataSize() >= 8;) {
            ntry.extComm.add(pck.msbGetQ(0));
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
        ntry.lrgComm = new ArrayList<tabLargeComm>();
        for (; pck.dataSize() >= 12;) {
            tabLargeComm d = new tabLargeComm();
            d.as = pck.msbGetD(0);
            d.d1 = pck.msbGetD(4);
            d.d2 = pck.msbGetD(8);
            ntry.lrgComm.add(d);
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
        ntry.originator = ax;
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
        ntry.accIgp = (int) pck.msbGetQ(3);
    }

    /**
     * parse traffic engineering attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseTraffEng(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.bandwidth = ((Float) Float.intBitsToFloat(pck.msbGetD(4))).intValue() * 8;
    }

    /**
     * parse pmsi tunnel attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parsePmsiTun(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.pmsiTyp = pck.msbGetW(0);
        ntry.pmsiLab = pck.msbGetD(2) >>> 8;
        pck.getSkip(5);
        ntry.pmsiTun = pck.getCopy();
    }

    /**
     * parse tunnel encapsulation attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseTunEnc(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.tunelTyp = pck.msbGetW(0);
        int len = pck.msbGetW(2);
        pck.getSkip(4);
        if (pck.dataSize() < len) {
            return;
        }
        pck.setDataSize(len);
        ntry.tunelVal = pck.getCopy();
    }

    /**
     * parse attribute set attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseAttribSet(tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.attribAs = pck.msbGetD(0);
        pck.getSkip(4);
        ntry.attribVal = pck.getCopy();
    }

    /**
     * parse prefix sid attribute
     *
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parsePrefSid(tabRouteEntry<addrIP> ntry, packHolder pck) {
        typLenVal tlv = getPrefSidTlv();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case 1: // label index
                    ntry.segrouIdx = bits.msbGetD(tlv.valDat, 3); // index
                    break;
                case 3: // srgb
                    ntry.segrouBeg = bits.msbGetD(tlv.valDat, 2) >>> 8; // base
                    ntry.segrouSiz = bits.msbGetD(tlv.valDat, 5) >>> 8; // range
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
        typLenVal tlv = getBierTlv();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case 1: // bier
                    ntry.bierIdx = bits.msbGetW(tlv.valDat, 1); // bfr id
                    break;
                case 2: // mpls
                    ntry.bierBeg = bits.msbGetD(tlv.valDat, 0) >>> 8; // base
                    ntry.bierSiz = tlv.valDat[3] & 0xff; // range
                    ntry.bierHdr = (tlv.valDat[4] >>> 4) & 0xf; // bsl
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
        ntry.clustList = new ArrayList<addrIP>();
        for (; pck.dataSize() >= 4;) {
            addrIPv4 as = new addrIPv4();
            pck.getAddr(as, 0);
            pck.getSkip(addrIPv4.size);
            addrIP ax = new addrIP();
            ax.fromIPv4addr(as);
            ntry.clustList.add(ax);
        }
    }

    /**
     * parse reachable attribute
     *
     * @param lower where to signal
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseReachable(rtrBgpSpeak lower, tabRouteEntry<addrIP> ntry, packHolder pck) {
        int safi = triplet2safi(pck.msbGetD(0));
        int len = pck.getByte(3);
        pck.getSkip(4);
        len = pck.dataSize() - len;
        for (; pck.dataSize() > len;) {
            int sfi = safi & safiMask;
            if ((sfi == safiMplsVpnU) || (sfi == safiMplsVpnM)) {
                pck.getSkip(8); // rd
            }
            addrIP adr = readAddress(safi, pck);
            if (adr == null) {
                continue;
            }
            if ((safi & afiMask) == afiIpv6) {
                if (adr.toIPv6().isLinkLocal()) {
                    continue;
                }
            }
            ntry.nextHop = adr;
        }
        pck.setBytesLeft(len);
        len = pck.getByte(0);
        pck.getSkip(1);
        for (int i = 0; i < len; i++) {
            pck.getSkip(pck.getByte(0) + 1);
        }
        for (; pck.dataSize() > 0;) {
            if (lower.addPthRx(safi)) {
                pck.getSkip(4);
            }
            tabRouteEntry<addrIP> res = readPrefix(safi, false, pck);
            if (res == null) {
                continue;
            }
            ntry.rouDst = res.rouDst;
            ntry.prefix = res.prefix;
            ntry.labelRem = res.labelRem;
            ntry.evpnLab = res.evpnLab;
            lower.prefixReach(safi, ntry);
        }
    }

    /**
     * parse unreachable attribute
     *
     * @param lower where to signal
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void parseUnReach(rtrBgpSpeak lower, tabRouteEntry<addrIP> ntry, packHolder pck) {
        pck.merge2beg();
        int safi = triplet2safi(pck.msbGetD(0));
        pck.getSkip(3);
        for (; pck.dataSize() > 0;) {
            if (lower.addPthRx(safi)) {
                pck.getSkip(4);
            }
            tabRouteEntry<addrIP> res = readPrefix(safi, true, pck);
            if (res == null) {
                continue;
            }
            ntry.rouDst = res.rouDst;
            ntry.prefix = res.prefix;
            ntry.labelRem = res.labelRem;
            ntry.evpnLab = res.evpnLab;
            lower.prefixWithdraw(safi, ntry);
        }
    }

    /**
     * place one capability
     *
     * @param pck target packet
     * @param typ type
     * @param buf content
     */
    public static void placeCapability(packHolder pck, int typ, byte[] buf) {
        pck.putByte(0, 2); // type of parameter
        pck.putByte(1, buf.length + 2); // size of parameter
        pck.putByte(2, typ); // type of capability
        pck.putByte(3, buf.length); // size of capability
        pck.putCopy(buf, 0, 4, buf.length);
        pck.putSkip(4 + buf.length);
    }

    /**
     * get capability tlv
     *
     * @return tlv
     */
    public static typLenVal getCapabilityTlv() {
        return new typLenVal(0, 8, 8, 8, 1, 0, 2, 1, 0, 512, true);
    }

    /**
     * get prefix sid tlv
     *
     * @return tlv
     */
    public static typLenVal getPrefSidTlv() {
        return new typLenVal(0, 8, 8, 16, 1, 0, 3, 1, 0, 512, true);
    }

    /**
     * get bier tlv
     *
     * @return tlv
     */
    public static typLenVal getBierTlv() {
        return new typLenVal(0, 8, 8, 16, 1, 0, 3, 1, 0, 512, true);
    }

    /**
     * parse attribute
     *
     * @param src source where from read
     * @param attr attribute read to
     */
    public static void parseAttrib(packHolder src, packHolder attr) {
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
        byte[] buf = new byte[len];
        src.getCopy(buf, 0, 0, len);
        src.getSkip(len);
        attr.clear();
        attr.ETHcos = flg;
        attr.ETHtype = typ;
        attr.putCopy(buf, 0, 0, buf.length);
        attr.putSkip(buf.length);
        attr.merge2beg();
    }

    private static void placeAttrib(int flg, int typ, packHolder trg, packHolder attr) {
        attr.merge2beg();
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
     * @param ntry table entry
     */
    public static void decodeAttribSet(tabRouteEntry<addrIP> ntry) {
        if (ntry.attribVal == null) {
            return;
        }
        packHolder pck = new packHolder(true, true);
        packHolder cur = new packHolder(true, true);
        pck.putCopy(ntry.attribVal, 0, 0, ntry.attribVal.length);
        pck.putSkip(ntry.attribVal.length);
        pck.merge2beg();
        ntry.attribVal = null;
        ntry.attribAs = 0;
        for (;;) {
            if (pck.dataSize() < 1) {
                break;
            }
            parseAttrib(pck, cur);
            interpretAttribute(null, ntry, cur);
        }
    }

    /**
     * encode attribute set
     *
     * @param as as number
     * @param ntry table entry
     */
    public static void encodeAttribSet(int as, tabRouteEntry<addrIP> ntry) {
        List<tabRouteEntry<addrIP>> lst = new ArrayList<tabRouteEntry<addrIP>>();
        lst.add(ntry);
        packHolder pck = new packHolder(true, true);
        createReachable(pck, new packHolder(true, true), safiAttrib, false, true, lst, null);
        ntry.attribAs = as;
        ntry.attribVal = pck.getCopy();
    }

    /**
     * interpret attribute
     *
     * @param lower where to signal
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void interpretAttribute(rtrBgpSpeak lower, tabRouteEntry<addrIP> ntry, packHolder pck) {
        switch (pck.ETHtype) {
            case attrOrigin:
                parseOrigin(ntry, pck);
                break;
            case attrAsPath:
                if (lower == null) {
                    parseAsPath(true, ntry, pck);
                } else {
                    parseAsPath(lower.peer32bitAS, ntry, pck);
                }
                break;
            case attrNextHop:
                parseNextHop(ntry, pck);
                break;
            case attrMetric:
                parseMetric(ntry, pck);
                break;
            case attrLocPref:
                parseLocPref(ntry, pck);
                break;
            case attrAtomicAggr:
                parseAtomicAggr(ntry);
                break;
            case attrAggregator:
                if (lower == null) {
                    parseAggregator(true, ntry, pck);
                } else {
                    parseAggregator(lower.peer32bitAS, ntry, pck);
                }
                break;
            case attrStdComm:
                parseStdComm(ntry, pck);
                break;
            case attrExtComm:
                parseExtComm(ntry, pck);
                break;
            case attrLrgComm:
                parseLrgComm(ntry, pck);
                break;
            case attrOriginator:
                parseOriginator(ntry, pck);
                break;
            case attrTraffEng:
                parseTraffEng(ntry, pck);
                break;
            case attrAccIgp:
                parseAccIgp(ntry, pck);
                break;
            case attrPmsiTun:
                parsePmsiTun(ntry, pck);
                break;
            case attrTunEnc:
                parseTunEnc(ntry, pck);
                break;
            case attrAttribSet:
                parseAttribSet(ntry, pck);
                break;
            case attrPrefSid:
                parsePrefSid(ntry, pck);
                break;
            case attrBier:
                parseBier(ntry, pck);
                break;
            case attrClustList:
                parseClustList(ntry, pck);
                break;
            case attrReachable:
                if (lower == null) {
                    break;
                }
                parseReachable(lower, ntry, pck);
                break;
            case attrUnReach:
                if (lower == null) {
                    break;
                }
                parseUnReach(lower, ntry, pck);
                break;
            default:
                if (debugger.rtrBgpTraf) {
                    logger.debug("unknown (" + pck.ETHtype + ") attrib " + pck.dump());
                }
        }
    }

    /**
     * create withdraw message
     *
     * @param pck packet to update
     * @param hlp helper packet
     * @param safi address family
     * @param addpath additional path
     * @param lst list of prefixes to withdraw
     */
    public static void createWithdraw(packHolder pck, packHolder hlp, int safi, boolean addpath, List<tabRouteEntry<addrIP>> lst) {
        if (safi == safiIp4uni) {
            for (int i = 0; i < lst.size(); i++) {
                tabRouteEntry<addrIP> ntry = lst.get(i);
                if (addpath) {
                    pck.msbPutD(0, 1);
                    pck.putSkip(4);
                }
                writePrefix(safiIp4uni, pck, ntry);
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
        placeUnreach(safi, addpath, pck, hlp, lst);
        pck.merge2beg();
        pck.msbPutW(0, pck.dataSize());
        pck.putSkip(2);
        pck.merge2beg();
        pck.msbPutW(0, 0);
        pck.putSkip(2);
        pck.merge2beg();
    }

    /**
     * create reachable message
     *
     * @param pck packet to update
     * @param hlp helper packet
     * @param safi address family
     * @param addpath additional path
     * @param longAS long as number supported
     * @param lst list of prefixes to advertise
     * @param user user defined attribute
     */
    public static void createReachable(packHolder pck, packHolder hlp, int safi, boolean addpath, boolean longAS, List<tabRouteEntry<addrIP>> lst, byte[] user) {
        tabRouteEntry<addrIP> ntry = lst.get(0);
        placeUser(pck, hlp, user);
        placeOrigin(pck, hlp, ntry);
        placeAsPath(longAS, pck, hlp, ntry);
        placeMetric(pck, hlp, ntry);
        placeLocPref(pck, hlp, ntry);
        placeAtomicAggr(pck, hlp, ntry);
        placeAggregator(longAS, pck, hlp, ntry);
        placeStdComm(pck, hlp, ntry);
        placeExtComm(pck, hlp, ntry);
        placeLrgComm(pck, hlp, ntry);
        placeOriginator(pck, hlp, ntry);
        placeClustList(pck, hlp, ntry);
        placeTraffEng(pck, hlp, ntry);
        placeAccIgp(pck, hlp, ntry);
        placePmsiTun(pck, hlp, ntry);
        placeTunEnc(pck, hlp, ntry);
        placePrefSid(pck, hlp, ntry);
        placeBier(pck, hlp, ntry);
        placeAttribSet(pck, hlp, ntry);
        if (safi == safiAttrib) {
            pck.merge2beg();
            return;
        }
        if (safi == safiIp4uni) {
            placeNextHop(pck, hlp, ntry);
            pck.merge2beg();
            pck.msbPutW(0, 0);
            pck.msbPutW(2, pck.dataSize());
            pck.putSkip(4);
            pck.merge2beg();
            for (int i = 0; i < lst.size(); i++) {
                ntry = lst.get(i);
                if (addpath) {
                    pck.msbPutD(0, 1);
                    pck.putSkip(4);
                }
                writePrefix(safiIp4uni, pck, ntry);
            }
            pck.merge2end();
            return;
        }
        placeReachable(safi, addpath, pck, hlp, lst);
        pck.merge2beg();
        pck.msbPutW(0, 0);
        pck.msbPutW(2, pck.dataSize());
        pck.putSkip(4);
        pck.merge2beg();
    }

    /**
     * place user defined attribute
     *
     * @param trg target packet
     * @param hlp helper packet
     * @param user attribute value
     */
    public static void placeUser(packHolder trg, packHolder hlp, byte[] user) {
        if (user == null) {
            return;
        }
        if (user.length < 2) {
            return;
        }
        hlp.clear();
        hlp.putCopy(user, 1, 0, user.length - 1);
        hlp.putSkip(user.length - 1);
        placeAttrib(flagOptional | flagTransitive, user[0], trg, hlp);
    }

    /**
     * place origin type attribute
     *
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeOrigin(packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        hlp.clear();
        hlp.putByte(0, ntry.origin % 3);
        hlp.putSkip(1);
        placeAttrib(flagTransitive, attrOrigin, trg, hlp);
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
                    pck.msbPutW(0, asNum16bit(i));
                    pck.putSkip(2);
                }
            }
        }
    }

    /**
     * place as path attribute
     *
     * @param longAs long as support
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeAsPath(boolean longAs, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        hlp.clear();
        placeAsList(longAs, hlp, 3, ntry.confSeq); // confed seq
        placeAsList(longAs, hlp, 4, ntry.confSet); // confed set
        placeAsList(longAs, hlp, 2, ntry.pathSeq); // as seq
        placeAsList(longAs, hlp, 1, ntry.pathSet); // as set
        placeAttrib(flagTransitive, attrAsPath, trg, hlp);
    }

    /**
     * place next hop attribute
     *
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeNextHop(packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        hlp.clear();
        hlp.putAddr(0, ntry.nextHop.toIPv4());
        hlp.putSkip(addrIPv4.size);
        placeAttrib(flagTransitive, attrNextHop, trg, hlp);
    }

    /**
     * place metric attribute
     *
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeMetric(packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.metric < 1) {
            return;
        }
        hlp.clear();
        hlp.msbPutD(0, ntry.metric);
        hlp.putSkip(4);
        placeAttrib(flagOptional, attrMetric, trg, hlp);
    }

    /**
     * place local preference attribute
     *
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeLocPref(packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.locPref < 1) {
            return;
        }
        hlp.clear();
        hlp.msbPutD(0, ntry.locPref);
        hlp.putSkip(4);
        placeAttrib(flagTransitive, attrLocPref, trg, hlp);
    }

    /**
     * place atomic aggregator attribute
     *
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeAtomicAggr(packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (!ntry.atomicAggr) {
            return;
        }
        hlp.clear();
        placeAttrib(flagTransitive, attrAtomicAggr, trg, hlp);
    }

    /**
     * place aggregator attribute
     *
     * @param longAs long as support
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeAggregator(boolean longAs, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.aggrRtr == null) {
            return;
        }
        hlp.clear();
        if (longAs) {
            hlp.msbPutD(0, ntry.aggrAs);
            hlp.putSkip(4);
        } else {
            hlp.msbPutW(0, asNum16bit(ntry.aggrAs));
            hlp.putSkip(2);
        }
        hlp.putAddr(0, ntry.aggrRtr.toIPv4());
        hlp.putSkip(addrIPv4.size);
        placeAttrib(flagOptional | flagTransitive, attrAggregator, trg, hlp);
    }

    /**
     * place standard community attribute
     *
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeStdComm(packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.stdComm == null) {
            return;
        }
        hlp.clear();
        for (int i = 0; i < ntry.stdComm.size(); i++) {
            hlp.msbPutD(0, ntry.stdComm.get(i));
            hlp.putSkip(4);
        }
        placeAttrib(flagOptional | flagTransitive, attrStdComm, trg, hlp);
    }

    /**
     * place extended community attribute
     *
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeExtComm(packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.extComm == null) {
            return;
        }
        hlp.clear();
        for (int i = 0; i < ntry.extComm.size(); i++) {
            hlp.msbPutQ(0, ntry.extComm.get(i));
            hlp.putSkip(8);
        }
        placeAttrib(flagOptional | flagTransitive, attrExtComm, trg, hlp);
    }

    /**
     * place large community attribute
     *
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeLrgComm(packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.lrgComm == null) {
            return;
        }
        hlp.clear();
        for (int i = 0; i < ntry.lrgComm.size(); i++) {
            tabLargeComm d = ntry.lrgComm.get(i);
            hlp.msbPutD(0, d.as);
            hlp.msbPutD(4, d.d1);
            hlp.msbPutD(8, d.d2);
            hlp.putSkip(12);
        }
        placeAttrib(flagOptional | flagTransitive, attrLrgComm, trg, hlp);
    }

    /**
     * place originator attribute
     *
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeOriginator(packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.originator == null) {
            return;
        }
        hlp.clear();
        hlp.putAddr(0, ntry.originator.toIPv4());
        hlp.putSkip(addrIPv4.size);
        placeAttrib(flagOptional, attrOriginator, trg, hlp);
    }

    /**
     * place accumulated igp attribute
     *
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeAccIgp(packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.accIgp < 1) {
            return;
        }
        hlp.clear();
        hlp.putByte(0, 1); // type
        hlp.msbPutW(1, 11); // length
        hlp.msbPutQ(3, ntry.accIgp); // value
        hlp.putSkip(11);
        placeAttrib(flagOptional, attrAccIgp, trg, hlp);
    }

    /**
     * place traffic engineering attribute
     *
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeTraffEng(packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.bandwidth < 1) {
            return;
        }
        hlp.clear();
        hlp.putByte(0, 1); // psc1
        hlp.putByte(1, 1); // packet
        hlp.msbPutW(2, 0); // reserved
        int i = Float.floatToIntBits(ntry.bandwidth / 8);
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
        placeAttrib(flagOptional, attrTraffEng, trg, hlp);
    }

    /**
     * place pmsi tunnel attribute
     *
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placePmsiTun(packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.pmsiTun == null) {
            return;
        }
        hlp.clear();
        hlp.msbPutW(0, ntry.pmsiTyp);
        hlp.msbPutD(2, ntry.pmsiLab << 8);
        hlp.putSkip(5);
        hlp.putCopy(ntry.pmsiTun, 0, 0, ntry.pmsiTun.length);
        hlp.putSkip(ntry.pmsiTun.length);
        placeAttrib(flagOptional | flagTransitive, attrPmsiTun, trg, hlp);
    }

    /**
     * place tunnel encapsulation attribute
     *
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeTunEnc(packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.tunelVal == null) {
            return;
        }
        hlp.clear();
        hlp.msbPutW(0, ntry.tunelTyp);
        hlp.msbPutW(2, ntry.tunelVal.length);
        hlp.putSkip(4);
        hlp.putCopy(ntry.tunelVal, 0, 0, ntry.tunelVal.length);
        hlp.putSkip(ntry.tunelVal.length);
        placeAttrib(flagOptional | flagTransitive, attrTunEnc, trg, hlp);
    }

    /**
     * place attribute set attribute
     *
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeAttribSet(packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.attribVal == null) {
            return;
        }
        hlp.clear();
        hlp.msbPutD(0, ntry.attribAs);
        hlp.putSkip(4);
        hlp.putCopy(ntry.attribVal, 0, 0, ntry.attribVal.length);
        hlp.putSkip(ntry.attribVal.length);
        placeAttrib(flagOptional | flagTransitive, attrAttribSet, trg, hlp);
    }

    /**
     * place prefix sid attribute
     *
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placePrefSid(packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.segrouIdx == 0) {
            return;
        }
        hlp.clear();
        typLenVal tlv = getPrefSidTlv();
        tlv.valDat[0] = 0; // reserved
        bits.msbPutW(tlv.valDat, 1, 0); // flags
        bits.msbPutD(tlv.valDat, 3, ntry.segrouIdx); // index
        tlv.putBytes(hlp, 1, 7, tlv.valDat);
        if (ntry.segrouSiz != 0) {
            tlv = getPrefSidTlv();
            bits.msbPutW(tlv.valDat, 0, 0); // flags
            bits.msbPutD(tlv.valDat, 2, ntry.segrouBeg << 8); // base
            bits.msbPutD(tlv.valDat, 5, ntry.segrouSiz << 8); // range
            tlv.putBytes(hlp, 3, 8, tlv.valDat);
        }
        placeAttrib(flagOptional | flagTransitive, attrPrefSid, trg, hlp);
    }

    /**
     * place bier attribute
     *
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeBier(packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.bierIdx == 0) {
            return;
        }
        hlp.clear();
        typLenVal tlv = getBierTlv();
        tlv.valDat[0] = 0; // subdomain
        bits.msbPutW(tlv.valDat, 1, ntry.bierIdx); // bfr id
        tlv.putBytes(hlp, 1, 4, tlv.valDat);
        if (ntry.bierSiz != 0) {
            tlv = getBierTlv();
            bits.msbPutD(tlv.valDat, 0, ntry.bierBeg << 8); // base
            tlv.valDat[3] = (byte) ntry.bierSiz; // range
            tlv.valDat[4] = (byte) (ntry.bierHdr << 4); // bsl
            tlv.putBytes(hlp, 2, 8, tlv.valDat);
        }
        placeAttrib(flagOptional | flagTransitive, attrBier, trg, hlp);
    }

    /**
     * place cluster list attribute
     *
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placeClustList(packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.clustList == null) {
            return;
        }
        hlp.clear();
        for (int i = 0; i < ntry.clustList.size(); i++) {
            hlp.putAddr(0, ntry.clustList.get(i).toIPv4());
            hlp.putSkip(addrIPv4.size);
        }
        placeAttrib(flagOptional, attrClustList, trg, hlp);
    }

    /**
     * place reachable attribute
     *
     * @param safi sub address family
     * @param addpath additional path
     * @param trg target packet
     * @param hlp helper packet
     * @param lst list of table entries
     */
    public static void placeReachable(int safi, boolean addpath, packHolder trg, packHolder hlp, List<tabRouteEntry<addrIP>> lst) {
        final int sfi = safi & safiMask;
        int i = 0;
        if ((sfi == safiMplsVpnU) || (sfi == safiMplsVpnM)) {
            i = 8;
        }
        hlp.clear();
        hlp.msbPutD(0, safi2triplet(safi));
        hlp.putByte(3, addressSize(safi) + i);
        hlp.putSkip(4);
        if ((sfi == safiMplsVpnU) || (sfi == safiMplsVpnM)) {
            hlp.msbPutQ(0, 0); // rd
            hlp.putSkip(8);
        }
        writeAddress(safi, hlp, lst.get(0).nextHop);
        hlp.putByte(0, 0);
        hlp.putSkip(1);
        for (i = 0; i < lst.size(); i++) {
            tabRouteEntry<addrIP> ntry = lst.get(i);
            if (addpath) {
                hlp.msbPutD(0, 1);
                hlp.putSkip(4);
            }
            writePrefix(safi, hlp, ntry);
        }
        placeAttrib(flagOptional, attrReachable, trg, hlp);
    }

    /**
     * place unreachable attribute
     *
     * @param safi sub address family
     * @param addpath additional path
     * @param trg target packet
     * @param hlp helper packet
     * @param lst list of table entries
     */
    public static void placeUnreach(int safi, boolean addpath, packHolder trg, packHolder hlp, List<tabRouteEntry<addrIP>> lst) {
        hlp.clear();
        hlp.msbPutD(0, safi2triplet(safi));
        hlp.putSkip(3);
        for (int i = 0; i < lst.size(); i++) {
            tabRouteEntry<addrIP> ntry = lst.get(i);
            if (addpath) {
                hlp.msbPutD(0, 1);
                hlp.putSkip(4);
            }
            writePrefix(safi, hlp, ntry);
        }
        placeAttrib(flagOptional, attrUnReach, trg, hlp);
    }

    /**
     * find in address list
     *
     * @param <T> address type
     * @param lst list of addresses
     * @param adr address to find
     * @return index of entry, -1 if not found
     */
    public static <T extends addrType> int findAddrList(List<T> lst, T adr) {
        for (int i = 0; i < lst.size(); i++) {
            if (adr.compare(lst.get(i), adr) == 0) {
                return i;
            }
        }
        return -1;
    }

    /**
     * find on integer list
     *
     * @param lst list to use
     * @param val value to find
     * @return position, -1 if not found
     */
    public static int findIntList(List<Integer> lst, int val) {
        if (lst == null) {
            return -1;
        }
        for (int i = 0; i < lst.size(); i++) {
            if (lst.get(i) == val) {
                return i;
            }
        }
        return -1;
    }

    /**
     * first on integer list
     *
     * @param lst list to use
     * @param val value to check
     * @return false if yes, true if not
     */
    public static boolean firstIntList(List<Integer> lst, int val) {
        if (lst == null) {
            return true;
        }
        if (lst.size() < 1) {
            return true;
        }
        if (lst.get(0) != val) {
            return true;
        }
        return false;
    }

    /**
     * find on long list
     *
     * @param lst list to use
     * @param val value to find
     * @return position, -1 if not found
     */
    public static int findLongList(List<Long> lst, long val) {
        if (lst == null) {
            return -1;
        }
        for (int i = 0; i < lst.size(); i++) {
            if (lst.get(i) == val) {
                return i;
            }
        }
        return -1;
    }

    /**
     * find on large list
     *
     * @param lst list to use
     * @param val value to find
     * @return position, -1 if not found
     */
    public static int findLrgList(List<tabLargeComm> lst, tabLargeComm val) {
        if (lst == null) {
            return -1;
        }
        for (int i = 0; i < lst.size(); i++) {
            if (val.compare(val, lst.get(i)) == 0) {
                return i;
            }
        }
        return -1;
    }

    /**
     * replace on integer list
     *
     * @param lst list to use
     * @param src source to replace
     * @param trg target to replace
     */
    public static void replaceIntList(List<Integer> lst, int src, int trg) {
        if (lst == null) {
            return;
        }
        for (int i = 0; i < lst.size(); i++) {
            if (lst.get(i) == src) {
                lst.set(i, trg);
            }
        }
    }

    /**
     * remove private as numbers
     *
     * @param lst list to use
     */
    public static void removePrivateAs(List<Integer> lst) {
        if (lst == null) {
            return;
        }
        for (int i = lst.size() - 1; i >= 0; i--) {
            if (asNumPrivate(lst.get(i))) {
                lst.remove(i);
            }
        }
    }

}
