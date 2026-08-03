package org.freertr.spf;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrPrefix;
import org.freertr.addr.addrType;
import org.freertr.cry.cryHashMd5;
import org.freertr.enc.encTlv;
import org.freertr.pack.packHolder;
import org.freertr.rtr.rtrBgpUtil;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteEntry;
import org.freertr.util.bits;
import org.freertr.rtr.rtrBgpAfi;

/**
 * link state encoder
 *
 * @author matecsaba
 */
public class spfLnkst {

    private spfLnkst() {
    }

    /**
     * nlri header size
     */
    public final static int nlriHdrSize = 11;

    /**
     * node nlri type
     */
    public final static int nlriTypNode = 1;

    /**
     * link nlri type
     */
    public final static int nlriTypLink = 2;

    /**
     * ipv4 nlri type
     */
    public final static int nlriTypIpv4 = 3;

    /**
     * ipv6 nlri type
     */
    public final static int nlriTypIpv6 = 4;

    /**
     * isis level1
     */
    public final static int protoIsisL1 = 1;

    /**
     * isis level2
     */
    public final static int protoIsisL2 = 2;

    /**
     * ospf v2
     */
    public final static int protoOspfV2 = 3;

    /**
     * direct
     */
    public final static int protoDirect = 4;

    /**
     * static
     */
    public final static int protoStatic = 5;

    /**
     * ospf v3
     */
    public final static int protoOspfV3 = 6;

    /**
     * bgp
     */
    public final static int protoBgp = 7;

    /**
     * rift
     */
    public final static int protoRift = 100;

    /**
     * lsrp
     */
    public final static int protoLsrp = 227;

    /**
     * local node descriptor
     */
    public final static int typNodeLocal = 256;

    /**
     * remote node descriptor
     */
    public final static int typNodeRemote = 257;

    /**
     * link local/remote identifier
     */
    public final static int typLinkIdentifier = 258;

    /**
     * ipv4 interface address
     */
    public final static int typIpv4interface = 259;

    /**
     * ipv4 neighbor address
     */
    public final static int typIpv4neighbor = 260;

    /**
     * ipv6 interface address
     */
    public final static int typIpv6interface = 261;

    /**
     * ipv6 neighbor address
     */
    public final static int typIpv6neighbor = 262;

    /**
     * multi topology identifier
     */
    public final static int typMultiTopoId = 263;

    /**
     * ospf route type
     */
    public final static int typOspfRouteType = 264;

    /**
     * ip reachability information
     */
    public final static int typIpReachInfo = 265;

    /**
     * node msd
     */
    public final static int typNodeMsd = 266;

    /**
     * link msd
     */
    public final static int typLinkMsd = 267;

    /**
     * autonomous system
     */
    public final static int typAutonSys = 512;

    /**
     * bgp ls identifier
     */
    public final static int typBgpLsId = 513;

    /**
     * ospf area id
     */
    public final static int typOspfAreaId = 514;

    /**
     * igp router id
     */
    public final static int typIgpRouterId = 515;

    /**
     * bgp router id
     */
    public final static int typBgpRouterId = 516;

    /**
     * bgp confederation member
     */
    public final static int typBgpConfedMem = 517;

    /**
     * srv6 sid information
     */
    public final static int typSrv6sidInfo = 518;

    /**
     * node flag bits
     */
    public final static int typNodeFlags = 1024;

    /**
     * node attribute
     */
    public final static int typNodeAttrs = 1025;

    /**
     * node name
     */
    public final static int typNodeName = 1026;

    /**
     * isis area identifier
     */
    public final static int typIsisAreaId = 1027;

    /**
     * local node ipv4 router id
     */
    public final static int typIpv4locRid = 1028;

    /**
     * local node ipv6 router id
     */
    public final static int typIpv6locRid = 1029;

    /**
     * remote node ipv4 router id
     */
    public final static int typIpv4remRid = 1030;

    /**
     * remote node ipv6 router id
     */
    public final static int typIpv6remRid = 1031;

    /**
     * s-bfd discriminator
     */
    public final static int typSbfdDisc = 1032;

    /**
     * sr capabilities
     */
    public final static int typSrCapa = 1034;

    /**
     * sr algorithm
     */
    public final static int typSrAlgo = 1035;

    /**
     * sr local block
     */
    public final static int typSrlb = 1036;

    /**
     * sr ms preferences
     */
    public final static int typSrmsPref = 1037;

    /**
     * srv6 capabilities
     */
    public final static int typSrv6capa = 1038;

    /**
     * flexible algorithm definition
     */
    public final static int typFlexAlgoDef = 1039;

    /**
     * flexible algorithm exclude any affinity
     */
    public final static int typFlexAlgoExcAny = 1040;

    /**
     * flexible algorithm include any affinity
     */
    public final static int typFlexAlgoIncAny = 1041;

    /**
     * flexible algorithm include all affinity
     */
    public final static int typFlexAlgoIncAll = 1042;

    /**
     * flexible algorithm definition flags
     */
    public final static int typFlexAlgoFlg = 1043;

    /**
     * flexible algorithm prefix metric
     */
    public final static int typFlexAlgoMet = 1044;

    /**
     * flexible algorithm exclude srlg affinity
     */
    public final static int typFlexAlgoExcSrlg = 1045;

    /**
     * flexible algorithm unsupported
     */
    public final static int typFlexAlgoUnsupp = 1046;

    /**
     * administrative group
     */
    public final static int typAdmGrp = 1088;

    /**
     * maximum link bandwidth
     */
    public final static int typMaxBwdt = 1089;

    /**
     * maximum reservable bandwidth
     */
    public final static int typMaxRsvbl = 1090;

    /**
     * unreserved bandwidth
     */
    public final static int typUnrsvBwdt = 1091;

    /**
     * te default metric
     */
    public final static int typTeDefMet = 1092;

    /**
     * link protection type
     */
    public final static int typLnkProt = 1093;

    /**
     * mpls protocol mask
     */
    public final static int typMplsMask = 1094;

    /**
     * igp metric
     */
    public final static int typIgpMetric = 1095;

    /**
     * shared risk link group
     */
    public final static int typSrlg = 1096;

    /**
     * opaque link attribute
     */
    public final static int typLinkOpaque = 1097;

    /**
     * link name
     */
    public final static int typLinkName = 1098;

    /**
     * adjacency sid
     */
    public final static int typAdjSid = 1099;

    /**
     * lan adjacency sid
     */
    public final static int typLanAdjSid = 1100;

    /**
     * peer node sid
     */
    public final static int typPeerNodeSid = 1101;

    /**
     * peer adj sid
     */
    public final static int typPeerAdjSid = 1102;

    /**
     * peer set sid
     */
    public final static int typPeerSetSid = 1103;

    /**
     * rtm capability
     */
    public final static int typRtmCapa = 1105;

    /**
     * srv6 end.x sid
     */
    public final static int typSrv6EndX = 1106;

    /**
     * isis srv6 end.x sid
     */
    public final static int typIsisEndX = 1107;

    /**
     * ospf srv6 end.x sid
     */
    public final static int typOspfEndX = 1108;

    /**
     * unidirectional link delay
     */
    public final static int typUniLnkDel = 1114;

    /**
     * min/max unidirectional link delay
     */
    public final static int typUniMinMax = 1115;

    /**
     * unidirectional delay variation
     */
    public final static int typUniDelVar = 1116;

    /**
     * unidirectional link loss
     */
    public final static int typUniLnkLos = 1117;

    /**
     * unidirectional residual bandwidth
     */
    public final static int typUniResBnd = 1118;

    /**
     * unidirectional available bandwidth
     */
    public final static int typUniAvaBnd = 1119;

    /**
     * unidirectional utilized bandwidth
     */
    public final static int typUniUsdBnd = 1120;

    /**
     * graceful link shutdown
     */
    public final static int typGrcLnkSht = 1121;

    /**
     * application specific link attributes
     */
    public final static int typAppLnkAtr = 1122;

    /**
     * igp flags
     */
    public final static int typIgpFlags = 1152;

    /**
     * igp route tag
     */
    public final static int typIgpTag = 1153;

    /**
     * igp extended route tag
     */
    public final static int typIgpExtTag = 1154;

    /**
     * prefix metric
     */
    public final static int typPrfxMetric = 1155;

    /**
     * ospf forwarding address
     */
    public final static int typOspfFwdAdr = 1156;

    /**
     * opaque prefix attribute
     */
    public final static int typPrfxOpaque = 1157;

    /**
     * prefix sid
     */
    public final static int typPrfxSid = 1158;

    /**
     * range
     */
    public final static int typRange = 1159;

    /**
     * sid label
     */
    public final static int typSidLabel = 1161;

    /**
     * srv6 locator
     */
    public final static int typSv6Locator = 1162;

    /**
     * prefix attribute flags
     */
    public final static int typPrfxFlags = 1170;

    /**
     * source router identifier
     */
    public final static int typSrcRtrId = 1171;

    /**
     * l2 bundle member attributes
     */
    public final static int typL2bunMem = 1172;

    /**
     * extended administrative group
     */
    public final static int typExtAdmGrp = 1173;

    /**
     * source ospf router id
     */
    public final static int typSrcOspfId = 1174;

    /**
     * spf capability
     */
    public final static int typSpfCapa = 1180;

    /**
     * sequence number
     */
    public final static int typSeqNum = 1181;

    /**
     * ipv4 link prefix length
     */
    public final static int typIpv4pfxLen = 1182;

    /**
     * ipv6 link prefix length
     */
    public final static int typIpv6pfxLen = 1183;

    /**
     * spf status
     */
    public final static int typSpfStat = 1184;

    /**
     * srv6 endpoint behavior
     */
    public final static int typSrv6EndBhv = 1250;

    /**
     * srv6 bgp peer node sid
     */
    public final static int typSrv6bgpPeer = 1251;

    /**
     * srv6 sid structure
     */
    public final static int typSrv6sidStr = 1252;

    /**
     * bier tlv structure
     */
    public final static int typBier = 12345;

    /**
     * get tlv encoder
     *
     * @return tlv
     */
    public static encTlv getTlv() {
        return new encTlv(0, 16, 16, 16, 1, 0, 4, 1, 0, 1024, true);
    }

    /**
     * put header
     *
     * @param tlv tlv to use
     * @param pck packet to use
     * @param prt protocol to use
     * @param typ identifier
     */
    public static void createHeader(encTlv tlv, packHolder pck, int prt, int typ) {
        pck.clear();
        pck.msbPutW(0, typ); // type
        pck.putByte(2, prt); // protocol
        pck.msbPutQ(3, 0); // identifier
        pck.putSkip(nlriHdrSize);
    }

    /**
     * add a node
     *
     * @param <Ta> type to use
     * @param tlv tlv to use
     * @param pck packet to use
     * @param hlp helper to use
     * @param siz size of router id
     * @param asn asn to use
     * @param adv ls id
     * @param par area id
     * @param nod advertising router
     * @param typ node type
     */
    public static <Ta extends addrType> void createNode(encTlv tlv, packHolder pck, packHolder hlp, int siz, int asn, addrIPv4 adv, int par, spfNode<Ta> nod, int typ) {
        hlp.clear();
        tlv.valSiz = 4;
        tlv.valTyp = typAutonSys;
        bits.msbPutD(tlv.valDat, 0, asn);
        tlv.putThis(hlp);
        tlv.putAddr(hlp, typBgpLsId, adv);
        hlp.merge2end();
        if (par != -1) {
            byte[] buf = new byte[4];
            bits.msbPutD(buf, 0, par);
            tlv.putBytes(hlp, typOspfAreaId, buf);
        }
        nod.name.toBuffer(tlv.valDat, 0);
        tlv.putBytes(hlp, typIgpRouterId, siz, tlv.valDat);
        hlp.merge2end();
        byte[] buf = hlp.getCopy();
        bits.byteCopy(buf, 0, tlv.valDat, 0, buf.length);
        tlv.valSiz = buf.length;
        tlv.putBytes(pck, typ); // node type
        pck.merge2end();
    }

    /**
     * add a node
     *
     * @param tlv tlv to use
     * @param pck packet to use
     * @param hlp helper to use
     * @param asn asn to use
     * @param adv router id
     * @param typ node type
     */
    public static void createSpfNode(encTlv tlv, packHolder pck, packHolder hlp, int asn, addrIPv4 adv, int typ) {
        hlp.clear();
        tlv.valSiz = 4;
        tlv.valTyp = typAutonSys;
        bits.msbPutD(tlv.valDat, 0, asn);
        tlv.putThis(hlp);
        tlv.putAddr(hlp, typBgpRouterId, adv);
        hlp.merge2end();
        byte[] buf = hlp.getCopy();
        bits.byteCopy(buf, 0, tlv.valDat, 0, buf.length);
        tlv.valSiz = buf.length;
        tlv.putBytes(pck, typ);
        pck.merge2end();
    }

    /**
     * add a link
     *
     * @param tlv tlv to use
     * @param pck packet to use
     * @param loc local ip
     * @param rem remote ip
     */
    public static void createSpfLink(encTlv tlv, packHolder pck, addrIP loc, addrIP rem) {
        if (loc == null) {
            return;
        }
        if (rem == null) {
            return;
        }
        if (loc.isIPv4()) {
            tlv.putAddr(pck, typIpv4interface, loc.toIPv4());
            tlv.putAddr(pck, typIpv4neighbor, rem.toIPv4());
        } else {
            tlv.putAddr(pck, typIpv6interface, loc.toIPv6());
            tlv.putAddr(pck, typIpv6neighbor, rem.toIPv6());
        }
        pck.merge2end();
    }

    /**
     * get prefix type
     *
     * @param ntry route entry
     * @return type to use
     */
    public static int getPrefixType(tabRouteEntry<addrIP> ntry) {
        if (ntry.prefix.network.isIPv4()) {
            return nlriTypIpv4;
        } else {
            return nlriTypIpv6;
        }
    }

    /**
     * add prefix
     *
     * @param tab table to update
     * @param old table to check
     * @param tlv tlv to use
     * @param pck packet to use
     * @param hlp helper to use
     * @param ntry route entry
     */
    public static void createPrefix(tabRoute<addrIP> tab, tabRoute<addrIP> old, encTlv tlv, packHolder pck, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        hlp.clear();
        if (ntry.prefix.network.isIPv4()) {
            rtrBgpAfi.ipv4uni.writePrefix(true, hlp, ntry);
        } else {
            rtrBgpAfi.ipv6uni.writePrefix(true, hlp, ntry);
        }
        hlp.merge2end();
        tlv.putBytes(pck, typIpReachInfo, hlp.getCopy());
        pck.merge2end();
        tabRouteEntry<addrIP> rou = new tabRouteEntry<addrIP>();
        rou.nlri = pck.getCopy();
        pck.clear();
        if (ntry.best.metric >= 0) {
            bits.msbPutD(tlv.valDat, 0, ntry.best.metric);
            tlv.putBytes(pck, typPrfxMetric, 4, tlv.valDat);
        }
        if (ntry.best.tag > 0) {
            bits.msbPutD(tlv.valDat, 0, ntry.best.tag);
            tlv.putBytes(pck, typIgpTag, 4, tlv.valDat);
        }
        if (ntry.best.segrouIdx > 0) {
            bits.msbPutW(tlv.valDat, 0, 0); // flags
            bits.msbPutW(tlv.valDat, 2, 0); // reserved
            bits.msbPutD(tlv.valDat, 4, ntry.best.segrouIdx);
            tlv.putBytes(pck, typPrfxSid, 8, tlv.valDat);
        }
        if (ntry.best.bierIdx > 0) {
            tlv.valDat[0] = (byte) ntry.best.bierSub;
            bits.msbPutW(tlv.valDat, 1, ntry.best.bierIdx);
            tlv.valDat[3] = 0; // reserved
            bits.msbPutW(tlv.valDat, 4, 2); // type
            bits.msbPutW(tlv.valDat, 6, 4); //length
            bits.msbPutD(tlv.valDat, 8, ntry.best.bierBeg | (ntry.best.bierHdr << 20));
            tlv.valDat[8] = (byte) ntry.best.bierSiz;
            tlv.putBytes(pck, typBier, 12, tlv.valDat);
        }
        doCreation(tab, old, tlv, pck, rou);
    }

    private static void doCreation(tabRoute<addrIP> tab, tabRoute<addrIP> old, encTlv tlv, packHolder pck, tabRouteEntry<addrIP> rou) {
        rou.best.rouSrc = rtrBgpUtil.peerOriginate;
        addrIP adr = new addrIP();
        adr.fromBuf(cryHashMd5.compute(new cryHashMd5(), rou.nlri), 0);
        rou.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
        pck.merge2end();
        if (pck.dataSize() > 0) {
            rou.best.linkStat = pck.getCopy();
        }
        if (old == null) {
            tab.add(tabRoute.addType.always, rou, false, true);
            return;
        }
        tabRouteEntry<addrIP> cur = old.find(rou);
        if (cur == null) {
            tab.add(tabRoute.addType.always, rou, false, true);
            return;
        }
        rou.best.time = cur.best.time;
        int i = pck.dataSize();
        bits.msbPutQ(tlv.valDat, 0, rou.best.time);
        tlv.putBytes(pck, typSeqNum, 8, tlv.valDat);
        pck.merge2end();
        rou.best.linkStat = pck.getCopy();
        if (rou.differs(tabRoute.addType.notyet, cur) == 0) {
            tab.add(tabRoute.addType.always, rou, false, false);
            return;
        }
        pck.setDataSize(i);
        rou.best.time = bits.getTime();
        bits.msbPutQ(tlv.valDat, 0, rou.best.time);
        tlv.putBytes(pck, typSeqNum, 8, tlv.valDat);
        pck.merge2end();
        rou.best.linkStat = pck.getCopy();
        tab.add(tabRoute.addType.always, rou, false, false);
    }

    /**
     * add a link state entry
     *
     * @param tab table to update
     * @param old table to check
     * @param tlv tlv to use
     * @param pck packet to use
     * @param hlp helper to use
     * @param siz size of metric
     * @param met metric
     */
    public static void createEntry(tabRoute<addrIP> tab, tabRoute<addrIP> old, encTlv tlv, packHolder pck, packHolder hlp, int siz, int met) {
        tabRouteEntry<addrIP> rou = new tabRouteEntry<addrIP>();
        rou.nlri = pck.getCopy();
        switch (siz) {
            case 1:
                tlv.valDat[0] = (byte) met;
                break;
            case 2:
                bits.msbPutW(tlv.valDat, 0, met);
                break;
            case 3:
                bits.msbPutD(tlv.valDat, 0, met << 8);
                break;
            case 4:
                bits.msbPutD(tlv.valDat, 0, met);
                break;
        }
        if (siz > 0) {
            tlv.putBytes(hlp, typIgpMetric, siz, tlv.valDat);
        }
        doCreation(tab, old, tlv, hlp, rou);
    }

    private static boolean findTlv(encTlv tlv, packHolder pck, int num) {
        int i = pck.dataSize();
        for (;;) {
            if (tlv.getBytes(pck)) {
                pck.setBytesLeft(i);
                return true;
            }
            if (tlv.valTyp == num) {
                pck.setBytesLeft(i);
                return false;
            }
        }
    }

    private static addrIPv4 findNode(encTlv tlv, packHolder pck, packHolder hlp, int num) {
        if (findTlv(tlv, pck, num)) {
            return null;
        }
        hlp.clear();
        hlp.putCopy(tlv.valDat, 0, 0, tlv.valSiz);
        hlp.putSkip(tlv.valSiz);
        hlp.merge2beg();
        if (findTlv(tlv, hlp, typBgpRouterId)) {
            return null;
        }
        addrIPv4 adr = new addrIPv4();
        adr.fromBuf(tlv.valDat, 0);
        return adr;
    }

    private static int findInt(encTlv tlv, packHolder pck, int num) {
        if (findTlv(tlv, pck, num)) {
            return 0;
        }
        return bits.msbGetD(tlv.valDat, 0);
    }

    /**
     * read spf node
     *
     * @param spf spf to use
     * @param tlv tlv to use
     * @param pck packet to use
     * @param hlp helper to use
     */
    public static void readSpfNode(spfCalc<addrIPv4> spf, encTlv tlv, packHolder pck, packHolder hlp) {
        addrIPv4 loc = findNode(tlv, pck, hlp, typNodeLocal);
        if (loc == null) {
            return;
        }
        if (!findTlv(tlv, pck, typNodeName)) {
            spf.addIdent(loc, tlv.getStr());
        }
        spf.addStub(loc, !findTlv(tlv, pck, typSpfStat));
        if (!findTlv(tlv, pck, typSrCapa)) {
            spf.addSegRouB(loc, bits.msbGetD(tlv.valDat, 5) >>> 8);
        }
    }

    /**
     * read spf link
     *
     * @param spf spf to use
     * @param tlv tlv to use
     * @param pck packet to use
     * @param hlp helper to use
     */
    public static void readSpfLink(spfCalc<addrIPv4> spf, encTlv tlv, packHolder pck, packHolder hlp) {
        addrIPv4 loc = findNode(tlv, pck, hlp, typNodeLocal);
        if (loc == null) {
            return;
        }
        addrIPv4 rem = findNode(tlv, pck, hlp, typNodeRemote);
        if (rem == null) {
            return;
        }
        int met = findInt(tlv, pck, typIgpMetric);
        if (met < 1) {
            return;
        }
        spf.addConn(loc, rem, met, true, !findTlv(tlv, pck, typSpfStat), null);
    }

    /**
     * read spf prefix
     *
     * @param spf spf to use
     * @param tlv tlv to use
     * @param pck packet to use
     * @param hlp helper to use
     * @param safi safi to use
     * @param dist distance to use
     */
    public static void readSpfPref(spfCalc<addrIPv4> spf, encTlv tlv, packHolder pck, packHolder hlp, rtrBgpAfi safi, int dist) {
        addrIPv4 loc = findNode(tlv, pck, hlp, typNodeLocal);
        if (loc == null) {
            return;
        }
        if (findTlv(tlv, pck, typIpReachInfo)) {
            return;
        }
        hlp.clear();
        hlp.putCopy(tlv.valDat, 0, 0, tlv.valSiz);
        hlp.putSkip(tlv.valSiz);
        hlp.merge2beg();
        tabRouteEntry<addrIP> ntry = safi.readPrefix(true, hlp);
        if (ntry == null) {
            return;
        }
        ntry.best.metric = findInt(tlv, pck, typPrfxMetric);
        ntry.best.tag = findInt(tlv, pck, typIgpTag);
        ntry.best.distance = dist;
        spf.addPref(loc, ntry, false);
        if (!findTlv(tlv, pck, typPrfxSid)) {
            ntry.best.segrouIdx = bits.msbGetD(tlv.valDat, 4);
            spf.addSegRouI(loc, ntry.prefix, ntry.best.segrouIdx, 0);
        }
        if (!findTlv(tlv, pck, typBier)) {
            ntry.best.bierIdx = bits.msbGetW(tlv.valDat, 1);
            ntry.best.bierSub = tlv.valDat[0] & 0xff;
            spf.addBierI(loc, ntry.best.bierIdx);
            spf.addBierS(loc, ntry.best.bierSub);
            if (bits.msbGetW(tlv.valDat, 4) == 2) {
                ntry.best.bierBeg = bits.msbGetD(tlv.valDat, 8) & 0xfffff;
                ntry.best.bierSiz = tlv.valDat[8] & 0xff;
                ntry.best.bierHdr = (tlv.valDat[9] & 0xff) >>> 4;
                spf.addBierB(loc, ntry.best.bierBeg);
            }
        }
    }

}
