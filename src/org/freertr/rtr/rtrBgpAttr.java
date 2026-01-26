package org.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.enc.encTlv;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabLargeComm;
import org.freertr.tab.tabRouteBlob;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRouteUtil;
import org.freertr.util.bits;
import org.freertr.util.logger;

/**
 * one bgp4 attribute
 *
 * @author matecsaba
 */
public class rtrBgpAttr {

    /**
     * create instance
     */
    private void rtrBgpAttr() {
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
        rtrBgpUtil.updtStatsArr(false, spkr.parent.attrStats, pck.ETHtype, pck);
        rtrBgpUtil.updtStatsArr(false, spkr.neigh.attrStats, pck.ETHtype, pck);
        if (spkr.neigh.attribFilter != null) {
            if (spkr.neigh.attribFilter.matches(pck.ETHtype)) {
                logger.info("filtered attribute " + pck.ETHtype + " from " + spkr.neigh.peerAddr + " (" + pck.dump() + ")");
                return;
            }
        }
        switch (pck.ETHtype) {
            case rtrBgpUtil.attrReachable:
                parseReachable(spkr, add, pck);
                return;
            case rtrBgpUtil.attrUnReach:
                parseUnReach(spkr, del, pck);
                return;
            case rtrBgpUtil.attrOrigin:
                parseOrigin(ntry, pck);
                return;
            case rtrBgpUtil.attrAsPath:
                parseAsPath(spkr, ntry, pck);
                return;
            case rtrBgpUtil.attrNextHop:
                parseNextHop(ntry, pck);
                return;
            case rtrBgpUtil.attrMetric:
                parseMetric(ntry, pck);
                return;
            case rtrBgpUtil.attrLocPref:
                parseLocPref(ntry, pck);
                return;
            case rtrBgpUtil.attrAtomicAggr:
                parseAtomicAggr(ntry);
                return;
            case rtrBgpUtil.attrEntropyLab:
                parseEntropyLab(ntry, pck);
                return;
            case rtrBgpUtil.attrAggregator:
                parseAggregator(spkr, ntry, pck);
                return;
            case rtrBgpUtil.attrConnector:
                parseConnector(ntry, pck);
                return;
            case rtrBgpUtil.attrPathLimit:
                parsePathLimit(ntry, pck);
                return;
            case rtrBgpUtil.attrPeDistLab:
                parsePeDistLab(ntry, pck);
                return;
            case rtrBgpUtil.attrStdComm:
                parseStdComm(ntry, pck);
                return;
            case rtrBgpUtil.attrExtComm:
                parseExtComm(ntry, pck);
                return;
            case rtrBgpUtil.attrLrgComm:
                parseLrgComm(ntry, pck);
                return;
            case rtrBgpUtil.attrOriginator:
                parseOriginator(ntry, pck);
                return;
            case rtrBgpUtil.attrTraffEng:
                parseTraffEng(ntry, pck);
                return;
            case rtrBgpUtil.attrAccIgp:
                parseAccIgp(ntry, pck);
                return;
            case rtrBgpUtil.attrPmsiTun:
                parsePmsiTun(ntry, pck);
                return;
            case rtrBgpUtil.attrLinkState:
                parseLnkSta(ntry, pck);
                return;
            case rtrBgpUtil.attrTunEnc:
                parseTunEnc(ntry, pck);
                return;
            case rtrBgpUtil.attrAttribSet:
                parseAttribSet(ntry, pck);
                return;
            case rtrBgpUtil.attrNshChain:
                parseNshChain(ntry, pck);
                return;
            case rtrBgpUtil.attrDomPath:
                parseDomainPath(ntry, pck);
                return;
            case rtrBgpUtil.attrBfdDisc:
                parseBfdDiscr(ntry, pck);
                return;
            case rtrBgpUtil.attrHopCapa:
                parseHopCapa(ntry, pck);
                return;
            case rtrBgpUtil.attrPrefSid:
                parsePrefSid(ntry, pck);
                return;
            case rtrBgpUtil.attrBier:
                parseBier(ntry, pck);
                return;
            case rtrBgpUtil.attrClustList:
                parseClustList(ntry, pck);
                return;
            case rtrBgpUtil.attrOnlyCust:
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
        encTlv tlv = rtrBgpUtil.getPrefSidTlv();
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
        encTlv tlv = rtrBgpUtil.getBierTlv();
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
        int safi = rtrBgpUtil.triplet2safi(pck.msbGetD(0));
        int idx = spkr.parent.safi2idx(safi);
        if (idx < 0) {
            return;
        }
        int sfi = safi & rtrBgpUtil.sfiMask;
        int len = pck.getByte(3);
        rtrBgpAfi rdr = spkr.parent.safi2rdr[idx];
        boolean addpath = spkr.addpathRx[idx];
        boolean oneLab = !spkr.peerMltLab[idx];
        boolean v6nh = len >= addrIPv6.size;
        pck.getSkip(4);
        len = pck.dataSize() - len;
        addrIP nextHop = null;
        for (; pck.dataSize() > len;) {
            if ((sfi == rtrBgpUtil.sfiMplsVpnU) || (sfi == rtrBgpUtil.sfiMplsVpnM) || (sfi == rtrBgpUtil.sfiClsTrnPl)) {
                pck.getSkip(8); // rd
            }
            addrIP adr;
            if (v6nh) {
                adr = rtrBgpAfi.readAddress(rtrBgpUtil.afiIpv6, pck);
            } else {
                adr = rtrBgpAfi.readAddress(rtrBgpUtil.afiIpv4, pck);
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
            tabRouteEntry<addrIP> res = rdr.readPrefix(oneLab, pck);
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
        int safi = rtrBgpUtil.triplet2safi(pck.msbGetD(0));
        pck.getSkip(3);
        int idx = spkr.parent.safi2idx(safi);
        if (idx < 0) {
            return;
        }
        boolean addpath = spkr.addpathRx[idx];
        rtrBgpAfi rdr = spkr.parent.safi2rdr[idx];
        int ident = 0;
        for (; pck.dataSize() > 0;) {
            if (addpath) {
                ident = pck.msbGetD(0);
                pck.getSkip(4);
            }
            tabRouteEntry<addrIP> res = rdr.readPrefix(true, pck);
            if (res == null) {
                continue;
            }
            res.oldDst = idx;
            res.best.ident = ident;
            pfxs.add(res);
        }
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
        if ((flg & rtrBgpUtil.flagLength) == 0) {
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
        rtrBgpUtil.updtStatsArr(true, spkr.parent.attrStats, typ, attr);
        rtrBgpUtil.updtStatsArr(true, spkr.neigh.attrStats, typ, attr);
        byte[] buf = attr.getCopy();
        if (buf.length > 0xff) {
            flg |= rtrBgpUtil.flagLength;
        }
        trg.putByte(0, flg); // flags
        trg.putByte(1, typ); // type
        if ((flg & rtrBgpUtil.flagLength) == 0) {
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
        placeAttrib(spkr, rtrBgpUtil.flagTransitive, rtrBgpUtil.attrOrigin, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagTransitive, rtrBgpUtil.attrAsPath, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagTransitive, rtrBgpUtil.attrNextHop, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagOptional, rtrBgpUtil.attrMetric, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagTransitive, rtrBgpUtil.attrLocPref, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagTransitive, rtrBgpUtil.attrEntropyLab, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagTransitive, rtrBgpUtil.attrAtomicAggr, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrAggregator, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrConnector, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrPathLimit, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrPeDistLab, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrStdComm, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrExtComm, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrLrgComm, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagOptional, rtrBgpUtil.attrOriginator, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagOptional, rtrBgpUtil.attrAccIgp, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagOptional, rtrBgpUtil.attrTraffEng, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrPmsiTun, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrTunEnc, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagOptional, rtrBgpUtil.attrLinkState, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrAttribSet, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrNshChain, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrDomPath, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrHopCapa, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrBfdDisc, trg, hlp);
    }

    /**
     * place prefix sid attribute
     *
     * @param spkr where to signal
     * @param idx sub address family
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public static void placePrefSid(rtrBgpSpeak spkr, int idx, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        int afi = spkr.parent.idx2safi[idx] & rtrBgpUtil.afiMask;
        hlp.clear();
        encTlv tlv = rtrBgpUtil.getPrefSidTlv();
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
                case rtrBgpUtil.afiIpv4:
                    i = 0x13;
                    o = 5;
                    break;
                case rtrBgpUtil.afiIpv6:
                    i = 0x12;
                    o = 5;
                    break;
                case rtrBgpUtil.afiL2vpn:
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
        placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrPrefSid, trg, hlp);
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
        encTlv tlv = rtrBgpUtil.getBierTlv();
        tlv.valDat[0] = (byte) ntry.best.bierSub; // subdomain
        bits.msbPutW(tlv.valDat, 1, ntry.best.bierIdx); // bfr id
        tlv.putBytes(hlp, 1, 4, tlv.valDat);
        if (ntry.best.bierSiz != 0) {
            bits.msbPutD(tlv.valDat, 0, ntry.best.bierBeg); // base + bsl
            tlv.valDat[1] |= (byte) (ntry.best.bierHdr << 4); // bsl
            tlv.valDat[0] = (byte) ntry.best.bierSiz; // range
            tlv.putBytes(hlp, 2, 4, tlv.valDat);
        }
        placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrBier, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagOptional, rtrBgpUtil.attrClustList, trg, hlp);
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
        placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrOnlyCust, trg, hlp);
    }

    /**
     * place reachable attribute
     *
     * @param spkr where to signal
     * @param idx sub address family
     * @param addpath additional path
     * @param trg target packet
     * @param hlp helper packet
     * @param lst list of table entries
     */
    public static void placeReachable(rtrBgpSpeak spkr, int idx, boolean addpath, packHolder trg, packHolder hlp, List<tabRouteEntry<addrIP>> lst) {
        int safi = spkr.parent.idx2safi[idx];
        rtrBgpAfi rdr = spkr.parent.safi2rdr[idx];
        boolean oneLab = !spkr.peerMltLab[idx];
        int afi = safi & rtrBgpUtil.afiMask;
        int sfi = safi & rtrBgpUtil.sfiMask;
        addrIP nextHop = lst.get(0).best.nextHop;
        boolean v6nh = afi == rtrBgpUtil.afiIpv6;
        if (!v6nh) {
            v6nh = !nextHop.isIPv4();
        }
        int i = v6nh ? addrIPv6.size : addrIPv4.size;
        if ((sfi == rtrBgpUtil.sfiMplsVpnU) || (sfi == rtrBgpUtil.sfiMplsVpnM) || (sfi == rtrBgpUtil.sfiClsTrnPl)) {
            i += 8;
        }
        hlp.clear();
        hlp.msbPutD(0, rtrBgpUtil.safi2triplet(safi));
        hlp.putByte(3, i);
        hlp.putSkip(4);
        if ((sfi == rtrBgpUtil.sfiMplsVpnU) || (sfi == rtrBgpUtil.sfiMplsVpnM) || (sfi == rtrBgpUtil.sfiClsTrnPl)) {
            hlp.msbPutQ(0, 0); // rd
            hlp.putSkip(8);
        }
        if (v6nh) {
            rtrBgpAfi.writeAddress(rtrBgpUtil.afiIpv6, hlp, nextHop);
        } else {
            rtrBgpAfi.writeAddress(rtrBgpUtil.afiIpv4, hlp, nextHop);
        }
        hlp.putByte(0, 0);
        hlp.putSkip(1);
        for (i = 0; i < lst.size(); i++) {
            tabRouteEntry<addrIP> ntry = lst.get(i);
            if (addpath) {
                hlp.msbPutD(0, ntry.best.ident);
                hlp.putSkip(4);
            }
            rdr.writePrefix(oneLab, hlp, ntry);
        }
        placeAttrib(spkr, rtrBgpUtil.flagOptional, rtrBgpUtil.attrReachable, trg, hlp);
    }

    /**
     * place unreachable attribute
     *
     * @param spkr where to signal
     * @param idx sub address family
     * @param addpath additional path
     * @param trg target packet
     * @param hlp helper packet
     * @param lst list of table entries
     */
    public static void placeUnreach(rtrBgpSpeak spkr, int idx, boolean addpath, packHolder trg, packHolder hlp, List<tabRouteEntry<addrIP>> lst) {
        int safi = spkr.parent.idx2safi[idx];
        rtrBgpAfi rdr = spkr.parent.safi2rdr[idx];
        hlp.clear();
        hlp.msbPutD(0, rtrBgpUtil.safi2triplet(safi));
        hlp.putSkip(3);
        for (int i = 0; i < lst.size(); i++) {
            tabRouteEntry<addrIP> ntry = lst.get(i);
            if (addpath) {
                hlp.msbPutD(0, ntry.best.ident);
                hlp.putSkip(4);
            }
            rdr.writePrefix(true, hlp, ntry);
        }
        placeAttrib(spkr, rtrBgpUtil.flagOptional, rtrBgpUtil.attrUnReach, trg, hlp);
    }

}
