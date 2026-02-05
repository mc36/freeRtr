package org.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.enc.encTlv;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabIpv6comm;
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
public interface rtrBgpAttr {

    /**
     * origin type attribute
     */
    public rtrBgpAttr attrOriginType = new rtrBgpAttrOriginType();

    /**
     * as path attribute
     */
    public rtrBgpAttr attrAsPath = new rtrBgpAttrAsPath();

    /**
     * metric attribute
     */
    public rtrBgpAttr attrMetric = new rtrBgpAttrMetric();

    /**
     * next hop attribute
     */
    public rtrBgpAttr attrNextHop = new rtrBgpAttrNextHop();

    /**
     * local preference attribute
     */
    public rtrBgpAttr attrLocPref = new rtrBgpAttrLocPref();

    /**
     * atomic aggregator attribute
     */
    public rtrBgpAttr attrAtomicAggr = new rtrBgpAttrAtomicAggr();

    /**
     * entropy label attribute
     */
    public rtrBgpAttr attrEntropyLab = new rtrBgpAttrEntropyLab();

    /**
     * aggregator attribute
     */
    public rtrBgpAttr attrAggregator = new rtrBgpAttrAggregator();

    /**
     * connector attribute
     */
    public rtrBgpAttr attrConnector = new rtrBgpAttrConnector();

    /**
     * path limit attribute
     */
    public rtrBgpAttr attrPathLimit = new rtrBgpAttrPathLimit();

    /**
     * pe distinguisher attribute
     */
    public rtrBgpAttr attrPeDistLab = new rtrBgpAttrPeDistLab();

    /**
     * standard community
     */
    public rtrBgpAttr attrStdComm = new rtrBgpAttrStdComm();

    /**
     * extended community
     */
    public rtrBgpAttr attrExtComm = new rtrBgpAttrExtComm();

    /**
     * large community
     */
    public rtrBgpAttr attrLrgComm = new rtrBgpAttrLrgComm();

    /**
     * ipv6 community
     */
    public rtrBgpAttr attrIpv6comm = new rtrBgpAttrIp6comm();

    /**
     * originator attribute
     */
    public rtrBgpAttr attrOriginator = new rtrBgpAttrOriginator();

    /**
     * traffic engineering attribute
     */
    public rtrBgpAttr attrTraffEng = new rtrBgpAttrTraffEng();

    /**
     * accumulated igp attribute
     */
    public rtrBgpAttr attrAccIgp = new rtrBgpAttrAccIgp();

    /**
     * pmsi tunnel attribute
     */
    public rtrBgpAttr attrPmsiTun = new rtrBgpAttrPmsiTun();

    /**
     * link state attribute
     */
    public rtrBgpAttr attrLinkState = new rtrBgpAttrLinkState();

    /**
     * tunnel encap attribute
     */
    public rtrBgpAttr attrTunEnc = new rtrBgpAttrTunEnc();

    /**
     * attribute set attribute
     */
    public rtrBgpAttr attrAttribSet = new rtrBgpAttrAttribSet();

    /**
     * nsh chain attribute
     */
    public rtrBgpAttr attrNshChain = new rtrBgpAttrNshChain();

    /**
     * domain path attribute
     */
    public rtrBgpAttr attrDomainPath = new rtrBgpAttrDomainPath();

    /**
     * safi specific attribute
     */
    public rtrBgpAttr attrSafiSpec = new rtrBgpAttrSafiSpec();

    /**
     * bfd discriminator attribute
     */
    public rtrBgpAttr attrBfdDisc = new rtrBgpAttrBfdDisc();

    /**
     * hop capabilities attribute
     */
    public rtrBgpAttr attrHopCapa = new rtrBgpAttrHopCapa();

    /**
     * prefix sid attribute
     */
    public rtrBgpAttr attrPrefSid = new rtrBgpAttrPrefSid();

    /**
     * bier attribute
     */
    public rtrBgpAttr attrBier = new rtrBgpAttrBier();

    /**
     * cluster list attribute
     */
    public rtrBgpAttr attrClustList = new rtrBgpAttrClustList();

    /**
     * only to customer attribute
     */
    public rtrBgpAttr attrOnlyCust = new rtrBgpAttrOnlyCust();

    /**
     * unknown attribute
     */
    public rtrBgpAttr attrUnknown = new rtrBgpAttrUnknown();

    /**
     * reachable attribute
     */
    public rtrBgpAttr attrReachable = new rtrBgpAttrReachable();

    /**
     * unreachable attribute
     */
    public rtrBgpAttr attrUnReach = new rtrBgpAttrUnReach();

    /**
     * layer2 behavior
     */
    public final static int behavDx2 = 0x15;

    /**
     * ipv4 behavior
     */
    public final static int behavDt4 = 0x13;

    /**
     * ipv6 behavior
     */
    public final static int behavDt6 = 0x12;

    /**
     * read attribute
     *
     * @param spkr where to signal
     * @param ntry table entry
     * @param pck packet to parse
     */
    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck);

    /**
     * write attribute
     *
     * @param spkr where to signal
     * @param trg target packet
     * @param hlp helper packet
     * @param ntry table entry
     */
    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry);

    /**
     * interpret attribute
     *
     * @param spkr where to signal
     * @param ntry table entry
     * @param pck packet to parse
     */
    public static void interpretAttribute(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        rtrBgpUtil.updtStatsArr(false, spkr.parent.attrStats, pck.ETHtype, pck);
        rtrBgpUtil.updtStatsArr(false, spkr.neigh.attrStats, pck.ETHtype, pck);
        if (spkr.neigh.attribFilter != null) {
            if (spkr.neigh.attribFilter.matches(pck.ETHtype)) {
                logger.info("filtered attribute " + pck.ETHtype + " from " + spkr.neigh.peerAddr + " (" + pck.dump() + ")");
                return;
            }
        }
        rtrBgp.bgpAttrsRx[pck.ETHtype & 0xff].readAttrib(spkr, ntry, pck);
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

class rtrBgpAttrOriginType implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.originType = pck.getByte(0);
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        hlp.clear();
        hlp.putByte(0, ntry.best.originType % 3);
        hlp.putSkip(1);
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagTransitive, rtrBgpUtil.attrOriginType, trg, hlp);
    }

}

class rtrBgpAttrAsPath implements rtrBgpAttr {

    private static void rdLst(boolean longAs, List<Integer> lst, packHolder pck) {
        int max = pck.getByte(0);
        pck.getSkip(1);
        if (longAs) {
            for (int pos = 0; pos < max; pos++) {
                int i = pck.msbGetD(0);
                pck.getSkip(4);
                lst.add(i);
            }
            return;
        }
        for (int pos = 0; pos < max; pos++) {
            int i = pck.msbGetW(0);
            pck.getSkip(2);
            lst.add(i);
        }
    }

    private static void wrLst(boolean longAs, packHolder pck, int typ, List<Integer> lst) {
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
            if (longAs) {
                for (; pos < end; pos++) {
                    int i = lst.get(pos);
                    pck.msbPutD(0, i);
                    pck.putSkip(4);
                }
                continue;
            }
            for (; pos < end; pos++) {
                int i = lst.get(pos);
                i = tabRouteUtil.asNum16bit(i);
                pck.msbPutW(0, i);
                pck.putSkip(2);
            }
        }
    }

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.pathSeq = new ArrayList<Integer>();
        ntry.best.pathSet = new ArrayList<Integer>();
        ntry.best.confSeq = new ArrayList<Integer>();
        ntry.best.confSet = new ArrayList<Integer>();
        for (; pck.dataSize() > 0;) {
            int i = pck.getByte(0);
            pck.getSkip(1);
            switch (i) {
                case 1: // as set
                    rdLst(spkr.peer32bitAS, ntry.best.pathSet, pck);
                    break;
                case 2: // as seq
                    rdLst(spkr.peer32bitAS, ntry.best.pathSeq, pck);
                    break;
                case 3: // confed seq
                    rdLst(spkr.peer32bitAS, ntry.best.confSeq, pck);
                    break;
                case 4: // confed set
                    rdLst(spkr.peer32bitAS, ntry.best.pathSet, pck);
                    break;
            }
        }
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        hlp.clear();
        wrLst(spkr.peer32bitAS, hlp, 3, ntry.best.confSeq); // confed seq
        wrLst(spkr.peer32bitAS, hlp, 4, ntry.best.confSet); // confed set
        wrLst(spkr.peer32bitAS, hlp, 2, ntry.best.pathSeq); // as seq
        wrLst(spkr.peer32bitAS, hlp, 1, ntry.best.pathSet); // as set
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagTransitive, rtrBgpUtil.attrAsPath, trg, hlp);
    }

}

class rtrBgpAttrMetric implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.metric = pck.msbGetD(0);
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.metric < 1) {
            return;
        }
        hlp.clear();
        hlp.msbPutD(0, ntry.best.metric);
        hlp.putSkip(4);
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional, rtrBgpUtil.attrMetric, trg, hlp);
    }

}

class rtrBgpAttrNextHop implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        addrIPv4 adr = new addrIPv4();
        pck.getAddr(adr, 0);
        ntry.best.nextHop = new addrIP();
        ntry.best.nextHop.fromIPv4addr(adr);
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        hlp.clear();
        hlp.putAddr(0, ntry.best.nextHop.toIPv4());
        hlp.putSkip(addrIPv4.size);
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagTransitive, rtrBgpUtil.attrNextHop, trg, hlp);
    }

}

class rtrBgpAttrLocPref implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.locPref = pck.msbGetD(0);
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.locPref < 1) {
            return;
        }
        hlp.clear();
        hlp.msbPutD(0, ntry.best.locPref);
        hlp.putSkip(4);
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagTransitive, rtrBgpUtil.attrLocPref, trg, hlp);
    }

}

class rtrBgpAttrAtomicAggr implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.atomicAggr = true;
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (!ntry.best.atomicAggr) {
            return;
        }
        hlp.clear();
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagTransitive, rtrBgpUtil.attrAtomicAggr, trg, hlp);
    }

}

class rtrBgpAttrEntropyLab implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.entropyLabel = pck.getCopy();
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.entropyLabel == null) {
            return;
        }
        if (ntry.best.entropyLabel.length < 1) {
            return;
        }
        hlp.clear();
        hlp.putCopy(ntry.best.entropyLabel, 0, 0, ntry.best.entropyLabel.length);
        hlp.putSkip(ntry.best.entropyLabel.length);
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagTransitive, rtrBgpUtil.attrEntropyLab, trg, hlp);
    }

}

class rtrBgpAttrAggregator implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        if (spkr.peer32bitAS) {
            ntry.best.aggrAs = pck.msbGetD(0);
            pck.getSkip(4);
        } else {
            ntry.best.aggrAs = pck.msbGetW(0);
            pck.getSkip(2);
        }
        addrIPv4 adr = new addrIPv4();
        pck.getAddr(adr, 0);
        ntry.best.aggrRtr = new addrIP();
        ntry.best.aggrRtr.fromIPv4addr(adr);
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
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
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrAggregator, trg, hlp);
    }

}

class rtrBgpAttrConnector implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        addrIPv4 adr = new addrIPv4();
        pck.getAddr(adr, 4);
        ntry.best.connRtr = new addrIP();
        ntry.best.connRtr.fromIPv4addr(adr);
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.connRtr == null) {
            return;
        }
        hlp.clear();
        hlp.msbPutD(0, 1);
        hlp.putAddr(4, ntry.best.connRtr.toIPv4());
        hlp.putSkip(4 + addrIPv4.size);
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrConnector, trg, hlp);
    }

}

class rtrBgpAttrPathLimit implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.pathLim = pck.getByte(0);
        ntry.best.pathAsn = pck.msbGetD(1);
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.pathLim < 1) {
            return;
        }
        hlp.clear();
        hlp.putByte(0, ntry.best.pathLim);
        hlp.msbPutD(1, ntry.best.pathAsn);
        hlp.putSkip(5);
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrPathLimit, trg, hlp);
    }

}

class rtrBgpAttrPeDistLab implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        int i;
        ntry.best.pediRtr = new addrIP();
        if (pck.dataSize() > 8) {
            addrIPv6 adr = new addrIPv6();
            pck.getAddr(adr, 0);
            ntry.best.pediRtr.fromIPv6addr(adr);
            pck.getSkip(addrIPv6.size);
        } else {
            addrIPv4 adr = new addrIPv4();
            pck.getAddr(adr, 0);
            ntry.best.pediRtr.fromIPv4addr(adr);
            pck.getSkip(addrIPv4.size);
        }
        ntry.best.pediLab = pck.msbGetD(0) >>> 12;
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
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
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrPeDistLab, trg, hlp);
    }

}

class rtrBgpAttrStdComm implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.stdComm = new ArrayList<Integer>();
        for (; pck.dataSize() >= 4;) {
            ntry.best.stdComm.add(pck.msbGetD(0));
            pck.getSkip(4);
        }
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
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
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrStdComm, trg, hlp);
    }

}

class rtrBgpAttrExtComm implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.extComm = new ArrayList<Long>();
        for (; pck.dataSize() >= 8;) {
            ntry.best.extComm.add(pck.msbGetQ(0));
            pck.getSkip(8);
        }
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
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
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrExtComm, trg, hlp);
    }

}

class rtrBgpAttrLrgComm implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
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

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
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
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrLrgComm, trg, hlp);
    }

}

class rtrBgpAttrIp6comm implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.ip6comm = new ArrayList<tabIpv6comm>();
        for (; pck.dataSize() >= 20;) {
            tabIpv6comm d = new tabIpv6comm();
            d.typ = pck.msbGetW(0);
            pck.getAddr(d.adr, 2);
            d.loc = pck.msbGetD(18);
            ntry.best.ip6comm.add(d);
            pck.getSkip(20);
        }
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.ip6comm == null) {
            return;
        }
        if (ntry.best.ip6comm.size() < 1) {
            return;
        }
        hlp.clear();
        for (int i = 0; i < ntry.best.ip6comm.size(); i++) {
            tabIpv6comm d = ntry.best.ip6comm.get(i);
            hlp.msbPutW(0, d.typ);
            hlp.putAddr(2, d.adr);
            hlp.msbPutW(18, d.loc);
            hlp.putSkip(20);
        }
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrIpv6comm, trg, hlp);
    }

}

class rtrBgpAttrOriginator implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        addrIPv4 adr = new addrIPv4();
        pck.getAddr(adr, 0);
        ntry.best.originator = new addrIP();
        ntry.best.originator.fromIPv4addr(adr);
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.originator == null) {
            return;
        }
        hlp.clear();
        hlp.putAddr(0, ntry.best.originator.toIPv4());
        hlp.putSkip(addrIPv4.size);
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional, rtrBgpUtil.attrOriginator, trg, hlp);
    }

}

class rtrBgpAttrTraffEng implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.bandwidth = ((Float) Float.intBitsToFloat(pck.msbGetD(4))).intValue() * 8;
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
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
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional, rtrBgpUtil.attrTraffEng, trg, hlp);
    }

}

class rtrBgpAttrAccIgp implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        if (pck.getByte(0) != 1) {
            return;
        }
        if (pck.msbGetW(1) < 11) {
            return;
        }
        ntry.best.accIgp = (int) pck.msbGetQ(3);
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.accIgp < 1) {
            return;
        }
        hlp.clear();
        hlp.putByte(0, 1); // type
        hlp.msbPutW(1, 11); // length
        hlp.msbPutQ(3, ntry.best.accIgp); // value
        hlp.putSkip(11);
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional, rtrBgpUtil.attrAccIgp, trg, hlp);
    }

}

class rtrBgpAttrPmsiTun implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.pmsiTyp = pck.msbGetW(0);
        ntry.best.pmsiLab = pck.msbGetD(2) >>> 8;
        pck.getSkip(5);
        ntry.best.pmsiTun = pck.getCopy();
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.pmsiTun == null) {
            return;
        }
        hlp.clear();
        hlp.msbPutW(0, ntry.best.pmsiTyp);
        hlp.msbPutD(2, ntry.best.pmsiLab << 8);
        hlp.putSkip(5);
        hlp.putCopy(ntry.best.pmsiTun, 0, 0, ntry.best.pmsiTun.length);
        hlp.putSkip(ntry.best.pmsiTun.length);
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrPmsiTun, trg, hlp);
    }

}

class rtrBgpAttrLinkState implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.linkStat = pck.getCopy();
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.linkStat == null) {
            return;
        }
        hlp.clear();
        hlp.putCopy(ntry.best.linkStat, 0, 0, ntry.best.linkStat.length);
        hlp.putSkip(ntry.best.linkStat.length);
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional, rtrBgpUtil.attrLinkState, trg, hlp);
    }

}

class rtrBgpAttrTunEnc implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.tunelTyp = pck.msbGetW(0);
        int len = pck.msbGetW(2);
        pck.getSkip(4);
        if (pck.dataSize() < len) {
            return;
        }
        pck.setDataSize(len);
        ntry.best.tunelVal = pck.getCopy();
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.tunelVal == null) {
            return;
        }
        hlp.clear();
        hlp.msbPutW(0, ntry.best.tunelTyp);
        hlp.msbPutW(2, ntry.best.tunelVal.length);
        hlp.putSkip(4);
        hlp.putCopy(ntry.best.tunelVal, 0, 0, ntry.best.tunelVal.length);
        hlp.putSkip(ntry.best.tunelVal.length);
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrTunEnc, trg, hlp);
    }

}

class rtrBgpAttrAttribSet implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.attribAs = pck.msbGetD(0);
        pck.getSkip(4);
        ntry.best.attribVal = pck.getCopy();
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.attribVal == null) {
            return;
        }
        hlp.clear();
        hlp.msbPutD(0, ntry.best.attribAs);
        hlp.putSkip(4);
        hlp.putCopy(ntry.best.attribVal, 0, 0, ntry.best.attribVal.length);
        hlp.putSkip(ntry.best.attribVal.length);
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrAttribSet, trg, hlp);
    }

}

class rtrBgpAttrNshChain implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.nshChain = pck.getCopy();
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.nshChain == null) {
            return;
        }
        hlp.clear();
        hlp.putCopy(ntry.best.nshChain, 0, 0, ntry.best.nshChain.length);
        hlp.putSkip(ntry.best.nshChain.length);
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrNshChain, trg, hlp);
    }

}

class rtrBgpAttrDomainPath implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.domainPath = pck.getCopy();
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.domainPath == null) {
            return;
        }
        hlp.clear();
        hlp.putCopy(ntry.best.domainPath, 0, 0, ntry.best.domainPath.length);
        hlp.putSkip(ntry.best.domainPath.length);
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrDomainPath, trg, hlp);
    }

}

class rtrBgpAttrSafiSpec implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.safiSpec = pck.getCopy();
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.safiSpec == null) {
            return;
        }
        hlp.clear();
        hlp.putCopy(ntry.best.safiSpec, 0, 0, ntry.best.safiSpec.length);
        hlp.putSkip(ntry.best.safiSpec.length);
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrSafiSpec, trg, hlp);
    }

}

class rtrBgpAttrBfdDisc implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.bfdDiscr = pck.getCopy();
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.bfdDiscr == null) {
            return;
        }
        hlp.clear();
        hlp.putCopy(ntry.best.bfdDiscr, 0, 0, ntry.best.bfdDiscr.length);
        hlp.putSkip(ntry.best.bfdDiscr.length);
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrHopCapa, trg, hlp);
    }

}

class rtrBgpAttrHopCapa implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.hopCapa = pck.getCopy();
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.hopCapa == null) {
            return;
        }
        hlp.clear();
        hlp.putCopy(ntry.best.hopCapa, 0, 0, ntry.best.hopCapa.length);
        hlp.putSkip(ntry.best.hopCapa.length);
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrBfdDisc, trg, hlp);
    }

}

class rtrBgpAttrPrefSid implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
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
                    ntry.best.segrouEth = tlv.valTyp == 6;
                    ntry.best.segrouBeh = bits.msbGetW(tlv.valDat, 22);
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

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
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
            tlv.valDat[0] = 0; // reserved
            tlv.valDat[1] = 1; // subtlv type
            bits.msbPutW(tlv.valDat, 2, 30); // size
            tlv.valDat[4] = 0; // reserved
            ntry.best.segrouPrf.toIPv6().toBuffer(tlv.valDat, 5);
            tlv.valDat[21] = 0; // sid flags
            bits.msbPutW(tlv.valDat, 22, ntry.best.segrouBeh); // behavior
            tlv.valDat[24] = 0; // reserved
            tlv.valDat[25] = 1; // sid structure
            bits.msbPutW(tlv.valDat, 26, 6); // subsubtlv length
            tlv.valDat[28] = 32; // locator block length
            tlv.valDat[29] = 16; // locator node length
            tlv.valDat[30] = 16; // locator function length
            tlv.valDat[31] = 0; // locator argument length
            tlv.valDat[32] = (byte) ntry.best.segrouSiz; // transposition length
            tlv.valDat[33] = (byte) ntry.best.segrouOfs; // transposition offset
            tlv.putBytes(hlp, ntry.best.segrouEth ? 6 : 5, 34, tlv.valDat);
        }
        if (hlp.headSize() < 1) {
            return;
        }
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrPrefSid, trg, hlp);
    }

}

class rtrBgpAttrBier implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
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

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
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
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrBier, trg, hlp);
    }

}

class rtrBgpAttrClustList implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.clustList = new ArrayList<addrIP>();
        for (; pck.dataSize() >= 4;) {
            addrIPv4 adr4 = new addrIPv4();
            pck.getAddr(adr4, 0);
            pck.getSkip(addrIPv4.size);
            addrIP addr = new addrIP();
            addr.fromIPv4addr(adr4);
            ntry.best.clustList.add(addr);
        }
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.clustList == null) {
            return;
        }
        hlp.clear();
        for (int i = 0; i < ntry.best.clustList.size(); i++) {
            hlp.putAddr(0, ntry.best.clustList.get(i).toIPv4());
            hlp.putSkip(addrIPv4.size);
        }
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional, rtrBgpUtil.attrClustList, trg, hlp);
    }

}

class rtrBgpAttrOnlyCust implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        ntry.best.onlyCust = pck.msbGetD(0);
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        if (ntry.best.onlyCust == 0) {
            return;
        }
        hlp.clear();
        hlp.msbPutD(0, ntry.best.onlyCust);
        hlp.putSkip(4);
        rtrBgpAttr.placeAttrib(spkr, rtrBgpUtil.flagOptional | rtrBgpUtil.flagTransitive, rtrBgpUtil.attrOnlyCust, trg, hlp);
    }

}

class rtrBgpAttrUnknown implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        if (spkr.neigh.unknownsLog) {
            logger.info("got unknown (" + pck.ETHtype + ") attribute " + spkr.neigh.peerAddr + " -> " + spkr.neigh.localAddr + " " + pck.dump());
        }
        if (ntry.best.unknown == null) {
            ntry.best.unknown = new ArrayList<tabRouteBlob>();
        }
        tabRouteBlob blb = new tabRouteBlob();
        blb.type = pck.ETHtype;
        blb.flag = pck.ETHcos;
        blb.data = pck.getCopy();
        ntry.best.unknown.add(blb);
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
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
            rtrBgpAttr.placeAttrib(spkr, blb.flag, blb.type, trg, hlp);
        }
    }

}

class rtrBgpAttrReachable implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
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
            spkr.currAdd.add(res);
        }
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
    }

}

class rtrBgpAttrUnReach implements rtrBgpAttr {

    public void readAttrib(rtrBgpSpeak spkr, tabRouteEntry<addrIP> ntry, packHolder pck) {
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
            spkr.currDel.add(res);
        }
    }

    public void writeAttrib(rtrBgpSpeak spkr, packHolder trg, packHolder hlp, tabRouteEntry<addrIP> ntry) {
    }

}
