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
    public static final int nlriHdrSize = 11;

    /**
     * node nlri type
     */
    public static final int nlriTypNode = 1;

    /**
     * link nlri type
     */
    public static final int nlriTypLink = 2;

    /**
     * ipv4 nlri type
     */
    public static final int nlriTypIpv4 = 3;

    /**
     * ipv6 nlri type
     */
    public static final int nlriTypIpv6 = 4;

    /**
     * isis level1
     */
    public static final int protoIsisL1 = 1;

    /**
     * isis level2
     */
    public static final int protoIsisL2 = 2;

    /**
     * ospf v2
     */
    public static final int protoOspfV2 = 3;

    /**
     * direct
     */
    public static final int protoDirect = 4;

    /**
     * static
     */
    public static final int protoStatic = 5;

    /**
     * ospf v3
     */
    public static final int protoOspfV3 = 6;

    /**
     * bgp
     */
    public static final int protoBgp = 7;

    /**
     * rift
     */
    public static final int protoRift = 100;

    /**
     * lsrp
     */
    public static final int protoLsrp = 227;

    /**
     * local node descriptor
     */
    public static final int typNodeLocal = 256;

    /**
     * remote node descriptor
     */
    public static final int typNodeRemote = 257;

    /**
     * link local/remote identifier
     */
    public static final int typLinkIdentifier = 258;

    /**
     * ipv4 interface address
     */
    public static final int typIpv4interface = 259;

    /**
     * ipv4 neighbor address
     */
    public static final int typIpv4neighbor = 260;

    /**
     * ipv6 interface address
     */
    public static final int typIpv6interface = 261;

    /**
     * ipv6 neighbor address
     */
    public static final int typIpv6neighbor = 262;

    /**
     * multi topology identifier
     */
    public static final int typMultiTopoId = 263;

    /**
     * ospf route type
     */
    public static final int typOspfRouteType = 264;

    /**
     * ip reachability information
     */
    public static final int typIpReachInfo = 265;

    /**
     * node msd
     */
    public static final int typNodeMsd = 266;

    /**
     * link msd
     */
    public static final int typLinkMsd = 267;

    /**
     * autonomous system
     */
    public static final int typAutonSys = 512;

    /**
     * bgp ls identifier
     */
    public static final int typBgpLsId = 513;

    /**
     * ospf area id
     */
    public static final int typOspfAreaId = 514;

    /**
     * igp router id
     */
    public static final int typIgpRouterId = 515;

    /**
     * bgp router id
     */
    public static final int typBgpRouterId = 516;

    /**
     * bgp confederation member
     */
    public static final int typBgpConfedMem = 517;

    /**
     * srv6 sid information
     */
    public static final int typSrv6sidInfo = 518;

    /**
     * node flag bits
     */
    public static final int typNodeFlags = 1024;

    /**
     * node attribute
     */
    public static final int typNodeAttrs = 1025;

    /**
     * node name
     */
    public static final int typNodeName = 1026;

    /**
     * isis area identifier
     */
    public static final int typIsisAreaId = 1027;

    /**
     * local node ipv4 router id
     */
    public static final int typIpv4locRid = 1028;

    /**
     * local node ipv6 router id
     */
    public static final int typIpv6locRid = 1029;

    /**
     * remote node ipv4 router id
     */
    public static final int typIpv4remRid = 1030;

    /**
     * remote node ipv6 router id
     */
    public static final int typIpv6remRid = 1031;

    /**
     * s-bfd discriminator
     */
    public static final int typSbfdDisc = 1032;

    /**
     * sr capabilities
     */
    public static final int typSrCapa = 1034;

    /**
     * sr algorithm
     */
    public static final int typSrAlgo = 1035;

    /**
     * sr local block
     */
    public static final int typSrlb = 1036;

    /**
     * sr ms preferences
     */
    public static final int typSrmsPref = 1037;

    /**
     * srv6 capabilities
     */
    public static final int typSrv6capa = 1038;

    /**
     * flexible algorithm definition
     */
    public static final int typFlexAlgoDef = 1039;

    /**
     * flexible algorithm exclude any affinity
     */
    public static final int typFlexAlgoExcAny = 1040;

    /**
     * flexible algorithm include any affinity
     */
    public static final int typFlexAlgoIncAny = 1041;

    /**
     * flexible algorithm include all affinity
     */
    public static final int typFlexAlgoIncAll = 1042;

    /**
     * flexible algorithm definition flags
     */
    public static final int typFlexAlgoFlg = 1043;

    /**
     * flexible algorithm prefix metric
     */
    public static final int typFlexAlgoMet = 1044;

    /**
     * flexible algorithm exclude srlg affinity
     */
    public static final int typFlexAlgoExcSrlg = 1045;

    /**
     * flexible algorithm unsupported
     */
    public static final int typFlexAlgoUnsupp = 1046;

    /**
     * administrative group
     */
    public static final int typAdmGrp = 1088;

    /**
     * maximum link bandwidth
     */
    public static final int typMaxBwdt = 1089;

    /**
     * maximum reservable bandwidth
     */
    public static final int typMaxRsvbl = 1090;

    /**
     * unreserved bandwidth
     */
    public static final int typUnrsvBwdt = 1091;

    /**
     * te default metric
     */
    public static final int typTeDefMet = 1092;

    /**
     * link protection type
     */
    public static final int typLnkProt = 1093;

    /**
     * mpls protocol mask
     */
    public static final int typMplsMask = 1094;

    /**
     * igp metric
     */
    public static final int typIgpMetric = 1095;

    /**
     * shared risk link group
     */
    public static final int typSrlg = 1096;

    /**
     * opaque link attribute
     */
    public static final int typLinkOpaque = 1097;

    /**
     * link name
     */
    public static final int typLinkName = 1098;

    /**
     * adjacency sid
     */
    public static final int typAdjSid = 1099;

    /**
     * lan adjacency sid
     */
    public static final int typLanAdjSid = 1100;

    /**
     * peer node sid
     */
    public static final int typPeerNodeSid = 1101;

    /**
     * peer adj sid
     */
    public static final int typPeerAdjSid = 1102;

    /**
     * peer set sid
     */
    public static final int typPeerSetSid = 1103;

    /**
     * rtm capability
     */
    public static final int typRtmCapa = 1105;

    /**
     * srv6 end.x sid
     */
    public static final int typSrv6EndX = 1106;

    /**
     * isis srv6 end.x sid
     */
    public static final int typIsisEndX = 1107;

    /**
     * ospf srv6 end.x sid
     */
    public static final int typOspfEndX = 1108;

    /**
     * unidirectional link delay
     */
    public static final int typUniLnkDel = 1114;

    /**
     * min/max unidirectional link delay
     */
    public static final int typUniMinMax = 1115;

    /**
     * unidirectional delay variation
     */
    public static final int typUniDelVar = 1116;

    /**
     * unidirectional link loss
     */
    public static final int typUniLnkLos = 1117;

    /**
     * unidirectional residual bandwidth
     */
    public static final int typUniResBnd = 1118;

    /**
     * unidirectional available bandwidth
     */
    public static final int typUniAvaBnd = 1119;

    /**
     * unidirectional utilized bandwidth
     */
    public static final int typUniUsdBnd = 1120;

    /**
     * graceful link shutdown
     */
    public static final int typGrcLnkSht = 1121;

    /**
     * application specific link attributes
     */
    public static final int typAppLnkAtr = 1122;

    /**
     * igp flags
     */
    public static final int typIgpFlags = 1152;

    /**
     * igp route tag
     */
    public static final int typIgpTag = 1153;

    /**
     * igp extended route tag
     */
    public static final int typIgpExtTag = 1154;

    /**
     * prefix metric
     */
    public static final int typPrfxMetric = 1155;

    /**
     * ospf forwarding address
     */
    public static final int typOspfFwdAdr = 1156;

    /**
     * opaque prefix attribute
     */
    public static final int typPrfxOpaque = 1157;

    /**
     * prefix sid
     */
    public static final int typPrfxSid = 1158;

    /**
     * range
     */
    public static final int typRange = 1159;

    /**
     * sid label
     */
    public static final int typSidLabel = 1161;

    /**
     * srv6 locator
     */
    public static final int typSv6Locator = 1162;

    /**
     * prefix attribute flags
     */
    public static final int typPrfxFlags = 1170;

    /**
     * source router identifier
     */
    public static final int typSrcRtrId = 1171;

    /**
     * l2 bundle member attributes
     */
    public static final int typL2bunMem = 1172;

    /**
     * extended administrative group
     */
    public static final int typExtAdmGrp = 1173;

    /**
     * source ospf router id
     */
    public static final int typSrcOspfId = 1174;

    /**
     * spf capability
     */
    public static final int typSpfCapa = 1180;

    /**
     * sequence number
     */
    public static final int typSeqNum = 1181;

    /**
     * ipv4 link prefix length
     */
    public static final int typIpv4pfxLen = 1182;

    /**
     * ipv6 link prefix length
     */
    public static final int typIpv6pfxLen = 1183;

    /**
     * spf status
     */
    public static final int typSpfStat = 1184;

    /**
     * srv6 endpoint behavior
     */
    public static final int typSrv6EndBhv = 1250;

    /**
     * srv6 bgp peer node sid
     */
    public static final int typSrv6bgpPeer = 1251;

    /**
     * srv6 sid structure
     */
    public static final int typSrv6sidStr = 1252;

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
     * @param tab table to use
     * @param tlv tlv to use
     * @param pck packet to use
     * @param hlp helper to use
     * @param ntry route entry
     * @param seq sequence
     */
    public static void createPrefix(tabRoute<addrIP> tab, encTlv tlv, packHolder pck, packHolder hlp, tabRouteEntry<addrIP> ntry, long seq) {
        hlp.clear();
        if (ntry.prefix.network.isIPv4()) {
            rtrBgpUtil.writePrefix(rtrBgpUtil.safiIp4uni, true, hlp, ntry);
        } else {
            rtrBgpUtil.writePrefix(rtrBgpUtil.safiIp6uni, true, hlp, ntry);
        }
        hlp.merge2end();
        tlv.putBytes(pck, typIpReachInfo, hlp.getCopy());
        pck.merge2end();
        tabRouteEntry<addrIP> rou = new tabRouteEntry<addrIP>();
        rou.best.rouSrc = rtrBgpUtil.peerOriginate;
        addrIP adr = new addrIP();
        rou.nlri = pck.getCopy();
        adr.fromBuf(cryHashMd5.compute(new cryHashMd5(), rou.nlri), 0);
        rou.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
        pck.clear();
        if (seq >= 0) {
            bits.msbPutQ(tlv.valDat, 0, seq);
            tlv.putBytes(pck, typSeqNum, 8, tlv.valDat);
            pck.merge2end();
        }
        if (ntry.best.metric >= 0) {
            bits.msbPutD(tlv.valDat, 0, ntry.best.metric);
            tlv.putBytes(pck, typPrfxMetric, 4, tlv.valDat);
            pck.merge2end();
        }
        if (ntry.best.tag > 0) {
            bits.msbPutD(tlv.valDat, 0, ntry.best.tag);
            tlv.putBytes(pck, typIgpTag, 4, tlv.valDat);
            pck.merge2end();
        }
        if (pck.dataSize() > 0) {
            rou.best.linkStat = pck.getCopy();
        }
        tab.add(tabRoute.addType.better, rou, true, true);
    }

    /**
     * add a link state entry
     *
     * @param tab table to update
     * @param tlv tlv to use
     * @param pck packet to use
     * @param hlp helper to use
     * @param siz size of metric
     * @param met metric
     * @param seq sequence
     */
    public static void createEntry(tabRoute<addrIP> tab, encTlv tlv, packHolder pck, packHolder hlp, int siz, int met, long seq) {
        tabRouteEntry<addrIP> rou = new tabRouteEntry<addrIP>();
        rou.best.rouSrc = rtrBgpUtil.peerOriginate;
        addrIP adr = new addrIP();
        rou.nlri = pck.getCopy();
        adr.fromBuf(cryHashMd5.compute(new cryHashMd5(), rou.nlri), 0);
        rou.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
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
        if (seq >= 0) {
            bits.msbPutQ(tlv.valDat, 0, seq);
            tlv.putBytes(hlp, typSeqNum, 8, tlv.valDat);
        }
        hlp.merge2end();
        if (hlp.dataSize() > 0) {
            rou.best.linkStat = hlp.getCopy();
        }
        tab.add(tabRoute.addType.better, rou, true, true);
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
        String nam = null;
        if (!findTlv(tlv, pck, typNodeName)) {
            nam = tlv.getStr();
        }
        spf.addIdent(loc, nam);
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
        spf.addConn(loc, rem, met, true, false, null);
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
    public static void readSpfPref(spfCalc<addrIPv4> spf, encTlv tlv, packHolder pck, packHolder hlp, int safi, int dist) {
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
        tabRouteEntry<addrIP> ntry = rtrBgpUtil.readPrefix(safi, true, hlp);
        if (ntry == null) {
            return;
        }
        ntry.best.metric = findInt(tlv, pck, typPrfxMetric);
        ntry.best.tag = findInt(tlv, pck, typIgpTag);
        ntry.best.distance = dist;
        spf.addPref(loc, ntry, false);
    }

}
