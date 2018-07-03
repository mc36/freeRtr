package rtr;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrPrefix;
import ip.ipFwdIface;
import ip.ipMpls;
import java.util.Comparator;
import java.util.List;
import pack.packHolder;
import tab.tabGen;
import tab.tabLabel;
import tab.tabLabelBier;
import tab.tabListing;
import tab.tabPrfxlstN;
import tab.tabRoute;
import tab.tabRouteEntry;
import tab.tabRtrmapN;
import tab.tabRtrplcN;
import util.bits;
import util.debugger;
import util.logger;
import util.notifier;
import util.shrtPthFrst;
import util.state;
import util.syncInt;
import util.typLenVal;

/**
 * ospfv3 area
 *
 * @author matecsaba
 */
public class rtrOspf6area implements Comparator<rtrOspf6area>, Runnable {

    /**
     * area number
     */
    public final int area;

    /**
     * lsas in this area
     */
    protected final tabGen<rtrOspf6lsa> lsas;

    /**
     * segment routing usage
     */
    protected boolean[] segrouUsd;

    /**
     * bier results
     */
    protected tabLabelBier bierRes;

    /**
     * computed routes
     */
    protected final tabRoute<addrIP> routes;

    /**
     * stub area
     */
    public boolean stub;

    /**
     * nssa area
     */
    public boolean nssa;

    /**
     * hostname
     */
    public boolean hostname;

    /**
     * traffic engineering
     */
    public boolean traffEng;

    /**
     * segment routing enabled
     */
    public boolean segrouEna;

    /**
     * bier enabled
     */
    public boolean bierEna;

    /**
     * advertise default route
     */
    public boolean defOrigin;

    /**
     * learn prefix list
     */
    public tabListing<tabPrfxlstN, addrIP> prflstFrom;

    /**
     * learn prefix list
     */
    public tabListing<tabPrfxlstN, addrIP> prflstInto;

    /**
     * advertise route map
     */
    public tabListing<tabRtrmapN, addrIP> roumapFrom;

    /**
     * advertise route map
     */
    public tabListing<tabRtrmapN, addrIP> roumapInto;

    /**
     * advertise route policy
     */
    public tabListing<tabRtrplcN, addrIP> roupolFrom;

    /**
     * advertise route policy
     */
    public tabListing<tabRtrplcN, addrIP> roupolInto;

    /**
     * last spf
     */
    protected shrtPthFrst<rtrOspf6areaSpf> lastSpf;

    private final rtrOspf6 lower;

    private syncInt todo = new syncInt(0); // 1=need2run, 2=running, 0xffff0=works

    private tabGen<rtrOspf6lsa> need2adv;

    private notifier notif;

    /**
     * create one area
     *
     * @param parent the ospf protocol
     * @param num area number
     */
    public rtrOspf6area(rtrOspf6 parent, int num) {
        lower = parent;
        area = num;
        lsas = new tabGen<rtrOspf6lsa>();
        need2adv = new tabGen<rtrOspf6lsa>();
        routes = new tabRoute<addrIP>("computed");
        notif = new notifier();
    }

    public String toString() {
        return "ospf area " + area;
    }

    public int compare(rtrOspf6area o1, rtrOspf6area o2) {
        if (o1.area < o2.area) {
            return -1;
        }
        if (o1.area > o2.area) {
            return +1;
        }
        return 0;
    }

    /**
     * get capability
     *
     * @return capability code
     */
    protected int getCapabilities() {
        int i = 0x11;
        if (!(stub || nssa)) {
            i |= 0x2; // external
        }
        if (nssa) {
            i |= 0x8; // nssa
        }
        if (traffEng || hostname || segrouEna) {
            i |= 0x40; // opaque
        }
        return i;
    }

    /**
     * get capability
     *
     * @return capability code
     */
    protected int getCapaFlags() {
        int i = 0;
        if (lower.routerRedistedU.size() > 0) {
            i |= 0x02; // external bit
        }
        if (lower.amIabr()) {
            i |= 0x01; // area border bit
            if (nssa) {
                i |= 0x10; // type7 translator
            }
        }
        return i;
    }

    /**
     * advertise one lsa
     *
     * @param lsa lsa to advertise
     * @param purge set true to purge it out
     */
    protected synchronized void generateLsa(rtrOspf6lsa lsa, boolean purge) {
        long tim = bits.getTime();
        if (purge) {
            tim -= rtrOspf6lsa.lsaMaxAge;
            if ((lsa.created <= tim) && (lsa.doNotAge == false)) {
                return;
            }
        }
        lsa.created = tim;
        lsa.doNotAge = false;
        rtrOspf6lsa old = lsas.find(lsa);
        if (old == null) {
            lsa.sequence = 0x80000001;
        } else {
            lsa.sequence = old.sequence + 1;
        }
        if (debugger.rtrOspf6evnt) {
            logger.debug("generate lsa " + lsa);
        }
        lsa.generateCheckSum();
        lsas.put(lsa);
    }

    /**
     * advertise needed lsas
     */
    protected void advertiseLsas() {
        if (debugger.rtrOspf6evnt) {
            logger.debug("advertise lsas in area " + area);
        }
        for (int i = 0; i < lsas.size(); i++) {
            rtrOspf6lsa ntry = lsas.get(i);
            if (ntry == null) {
                continue;
            }
            if (lower.routerID.compare(ntry.rtrID, lower.routerID) != 0) {
                continue;
            }
            if (need2adv.find(ntry) != null) {
                continue;
            }
            generateLsa(ntry, true);
        }
        for (int i = 0; i < need2adv.size(); i++) {
            rtrOspf6lsa ntry = need2adv.get(i);
            if (ntry == null) {
                continue;
            }
            if (!ntry.contentDiffers(lsas.find(ntry))) {
                continue;
            }
            generateLsa(ntry, false);
        }
    }

    /**
     * purge out aged lsas
     */
    protected void purgeLsas() {
        if (debugger.rtrOspf6evnt) {
            logger.debug("purge lsas in area " + area);
        }
        long tim1 = bits.getTime() - rtrOspf6lsa.lsaMaxAge - (rtrOspf6lsa.lsaMaxAge / 100);
        long tim2 = tim1 + (rtrOspf6lsa.lsaMaxAge / 2);
        for (int i = lsas.size(); i >= 0; i--) {
            rtrOspf6lsa ntry = lsas.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.doNotAge) {
                continue;
            }
            if (ntry.created < tim1) {
                if (debugger.rtrOspf6evnt) {
                    logger.debug("purge " + ntry);
                }
                lsas.del(ntry);
                continue;
            }
            if (lower.routerID.compare(ntry.rtrID, lower.routerID) != 0) {
                continue;
            }
            if (ntry.created > tim2) {
                continue;
            }
            if (need2adv.find(ntry) == null) {
                continue;
            }
            generateLsa(ntry, false);
        }
    }

    /**
     * advertise one lsa
     *
     * @param typ type
     * @param id id
     * @param pck payload
     */
    protected void advertiseLsa(int typ, int id, packHolder pck) {
        pck.merge2beg();
        rtrOspf6lsa lsa = new rtrOspf6lsa();
        lsa.lsaType = typ;
        lsa.lsaID = id;
        lsa.rtrID = lower.routerID.copyBytes();
        lsa.bufDat = pck.getCopy();
        need2adv.put(lsa);
    }

    /**
     * advertise one te
     *
     * @param typ type
     * @param id id
     * @param pck payload
     */
    protected void advertiseTe(int typ, int id, packHolder pck) {
        pck.merge2beg();
        pck.msbPutW(0, typ); // type
        pck.msbPutW(2, pck.dataSize()); // length
        pck.putSkip(4);
        advertiseLsa(rtrOspf6lsa.lsaTraffEng, id, pck);
    }

    private void putLink2rtrLsa(packHolder pck, int typ, int locLnk, int remLnk, addrIPv4 peer, int met) {
        pck.putByte(0, typ); // type
        pck.putByte(1, 0); // reserved
        pck.msbPutW(2, met); // metric
        pck.msbPutD(4, locLnk); // interface id
        pck.msbPutD(8, remLnk); // neighbor interface id
        pck.putAddr(12, peer); // neighbor router id
        pck.putSkip(16);
    }

    private void putLink2ertrLsa(packHolder pck, int typ, int locLnk, int remLnk, addrIPv4 peer, int met, byte[] subs) {
        pck.msbPutW(0, 1); // tlv type
        pck.msbPutW(2, 16 + subs.length); // tlv size
        pck.putByte(4, typ); // type
        pck.putByte(5, 0); // reserved
        pck.msbPutW(6, met); // metric
        pck.msbPutD(8, locLnk); // interface id
        pck.msbPutD(12, remLnk); // neighbor interface id
        pck.putAddr(16, peer); // neighbor router id
        pck.putSkip(20);
        pck.putCopy(subs, 0, 0, subs.length); // subtlvs
        pck.putSkip(subs.length);
    }

    private static int prefixSize(addrPrefix<addrIPv6> prf) {
        return ((prf.maskLen + 31) / 32) * 4;
    }

    private static int prefixWrite(packHolder pck, int ofs, rtrOspf6areaPref pref) {
        addrPrefix<addrIPv6> prf = addrPrefix.ip2ip6(pref.prefix);
        pck.putByte(ofs + 0, prf.maskLen); // prefix length
        pck.putByte(ofs + 1, pref.option); // prefix options
        pck.msbPutW(ofs + 2, pref.metric); // metric
        pck.putAddr(ofs + 4, prf.network); // address
        return 4 + prefixSize(prf);
    }

    private static int prefixRead(packHolder pck, int ofs, rtrOspf6areaPref pref) {
        int len = pck.getByte(ofs + 0); // prefix length
        pref.option = pck.getByte(ofs + 1); // prefix options
        pref.metric = pck.msbGetW(ofs + 2); // metric
        addrIPv6 adr = new addrIPv6();
        pck.getAddr(adr, ofs + 4); // address
        addrPrefix<addrIPv6> prf = new addrPrefix<addrIPv6>(adr, len);
        pref.prefix = addrPrefix.ip6toIP(prf);
        return 4 + prefixSize(prf);
    }

    private void createRtrLsa() {
        packHolder pck = new packHolder(true, true);
        pck.msbPutD(0, getCapabilities()); // optional capabilities
        pck.putByte(0, getCapaFlags()); // flags
        pck.putSkip(4);
        for (int i = 0; i < lower.ifaces.size(); i++) {
            rtrOspf6iface ifc = lower.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.area.area != area) {
                continue;
            }
            if (ifc.needDR()) {
                if (ifc.drAddr.isEmpty()) {
                    continue;
                }
                putLink2rtrLsa(pck, rtrOspf6lsa.lnkTrns, ifc.iface.ifwNum, ifc.DRintId(), ifc.drAddr, ifc.metric);
                continue;
            }
            for (int o = 0; o < ifc.neighs.size(); o++) {
                rtrOspf6neigh nei = ifc.neighs.get(o);
                if (nei == null) {
                    continue;
                }
                if (!nei.isFull()) {
                    continue;
                }
                putLink2rtrLsa(pck, rtrOspf6lsa.lnkP2p, ifc.iface.ifwNum, nei.rtrInt, nei.rtrID, ifc.metric);
            }
        }
        advertiseLsa(rtrOspf6lsa.lsaRouter, 0, pck);
    }

    private void createLnkLsa(rtrOspf6iface ifc) {
        packHolder pck = new packHolder(true, true);
        pck.msbPutD(0, getCapabilities()); // optional capabilities
        pck.putByte(0, ifc.drPriority); // priority
        pck.putAddr(4, ifc.iface.addr.toIPv6()); // local address
        pck.msbPutD(20, 0); // number of prefixes
        pck.putSkip(24);
        advertiseLsa(rtrOspf6lsa.lsaLink, ifc.iface.ifwNum, pck);
    }

    private void createNetLsa(rtrOspf6iface ifc) {
        packHolder pck = new packHolder(true, true);
        pck.msbPutD(0, getCapabilities()); // optional capabilities
        pck.putAddr(4, lower.routerID); // myself also full
        pck.putSkip(8);
        for (int i = 0; i < ifc.neighs.size(); i++) {
            rtrOspf6neigh nei = ifc.neighs.get(i);
            if (nei == null) {
                continue;
            }
            if (!nei.isFull()) {
                continue;
            }
            pck.putAddr(0, nei.rtrID);
            pck.putSkip(addrIPv4.size);
        }
        advertiseLsa(rtrOspf6lsa.lsaNetwork, ifc.iface.ifwNum, pck);
    }

    private void createPrefLsa(rtrOspf6iface ifc) {
        rtrOspf6areaPref prf = new rtrOspf6areaPref();
        prf.metric = ifc.metric;
        prf.option = rtrOspf6lsa.prefProp;
        prf.prefix = ifc.iface.network;
        packHolder pck = new packHolder(true, true);
        pck.msbPutW(0, 1); // number of prefixes
        pck.msbPutW(2, rtrOspf6lsa.lsaRouter); // referenced lsa type
        pck.msbPutD(4, 0); // referenced lsa link id
        pck.putAddr(8, lower.routerID); // referenced lsa router id
        int i = prefixWrite(pck, 12, prf);
        pck.putSkip(12 + i);
        advertiseLsa(rtrOspf6lsa.lsaPrefix, ifc.iface.ifwNum, pck);
    }

    private void createExtLsa(int seq, addrPrefix<addrIP> pref, int org, int met, int tag) {
        rtrOspf6areaPref prf = new rtrOspf6areaPref();
        prf.metric = 0;
        prf.option = rtrOspf6lsa.prefProp;
        prf.prefix = pref;
        if (met < 0) {
            met = 0;
        }
        if (met > 0xffffff) {
            met = 0xffffff;
        }
        int i = 0x01;
        if (org != 111) {
            i |= 0x04;
        }
        packHolder pck = new packHolder(true, true);
        pck.msbPutD(0, met); // metric
        pck.putByte(0, i); // flags
        i = prefixWrite(pck, 4, prf);
        pck.putSkip(i + 4);
        pck.msbPutD(0, tag); // route tag
        pck.putSkip(4);
        if (nssa) {
            i = rtrOspf6lsa.lsaNssaExt;
        } else {
            i = rtrOspf6lsa.lsaAsExt;
        }
        advertiseLsa(i, seq, pck);
    }

    private void createSumLsa(int seq, addrPrefix<addrIP> pref, int met) {
        if (met < 0) {
            met = 0;
        }
        if (met > 0xffffff) {
            met = 0xffffff;
        }
        rtrOspf6areaPref prf = new rtrOspf6areaPref();
        prf.metric = 0;
        prf.option = 0;
        prf.prefix = pref;
        packHolder pck = new packHolder(true, true);
        pck.msbPutD(0, met); // metric
        int i = prefixWrite(pck, 4, prf);
        pck.putSkip(i + 4);
        advertiseLsa(rtrOspf6lsa.lsaInterPrf, seq, pck);
    }

    private void createTeLsa(int seq, rtrOspf6iface ifc, rtrOspf6neigh nei) {
        packHolder pck = new packHolder(true, true);
        if (ifc == null) {
            pck.putAddr(0, lower.traffEngID);
            pck.putSkip(addrIPv6.size);
            advertiseTe(rtrOspfTe.typRouter6, seq, pck);
            return;
        }
        rtrOspfTe.putGenTlv1(pck, nei == null);
        typLenVal tlv = rtrOspfTe.getTlvHandler();
        tlv.valTyp = rtrOspfTe.typNeighId;
        tlv.valSiz = 8;
        if (nei == null) {
            bits.msbPutD(tlv.valDat, 0, ifc.DRintId());
            ifc.drAddr.toBuffer(tlv.valDat, 4);
        } else {
            bits.msbPutD(tlv.valDat, 0, nei.rtrInt);
            nei.rtrID.toBuffer(tlv.valDat, 4);
        }
        tlv.putThis(pck);
        tlv.putAddr(pck, rtrOspfTe.typLoc6adr, ifc.iface.addr.toIPv6());
        if (nei != null) {
            tlv.putAddr(pck, rtrOspfTe.typRem6adr, nei.peer);
        }
        rtrOspfTe.putGenTlv2(pck, ifc.teMetric, ifc.teBandwidth, ifc.teAffinity, ifc.teSrlg);
        advertiseTe(rtrOspfTe.typLink, seq, pck);
    }

    private void createErtrLsa() {
        if (!segrouEna) {
            return;
        }
        packHolder pck = new packHolder(true, true);
        pck.msbPutD(0, getCapabilities()); // optional capabilities
        pck.putByte(0, getCapaFlags()); // flags
        pck.putSkip(4);
        for (int i = 0; i < lower.ifaces.size(); i++) {
            rtrOspf6iface ifc = lower.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.area.area != area) {
                continue;
            }
            if (ifc.needDR()) {
                if (ifc.drAddr.isEmpty()) {
                    continue;
                }
                putLink2ertrLsa(pck, rtrOspf6lsa.lnkTrns, ifc.iface.ifwNum, ifc.DRintId(), ifc.drAddr, ifc.metric, new byte[0]);
                continue;
            }
            for (int o = 0; o < ifc.neighs.size(); o++) {
                rtrOspf6neigh nei = ifc.neighs.get(o);
                if (nei == null) {
                    continue;
                }
                if (!nei.isFull()) {
                    continue;
                }
                byte[] buf = new byte[0];
                if (nei.segrouLab != null) {
                    buf = rtrOspfSr.putAdj(nei.segrouLab.getValue());
                }
                putLink2ertrLsa(pck, rtrOspf6lsa.lnkP2p, ifc.iface.ifwNum, nei.rtrInt, nei.rtrID, ifc.metric, buf);
            }
        }
        advertiseLsa(rtrOspf6lsa.lsaErouter, 0, pck);
    }

    private void createEprfLsa(int seq, addrPrefix<addrIP> pref, int lsa, int tlv, int met, int opt, byte[] subs) {
        if (met < 0) {
            met = 0;
        }
        if (met > 0xffffff) {
            met = 0xffffff;
        }
        addrPrefix<addrIPv6> prf = addrPrefix.ip2ip6(pref);
        packHolder pck = new packHolder(true, true);
        pck.msbPutD(0, met); // metric
        pck.putByte(4, prf.maskLen); // prefix length
        pck.putByte(5, opt); // prefix options
        pck.msbPutW(6, 0); // reserved
        pck.putAddr(8, prf.network); // address
        pck.putSkip(8 + prefixSize(prf));
        pck.putCopy(subs, 0, 0, subs.length); // subtlvs
        pck.putSkip(subs.length);
        pck.merge2beg();
        pck.msbPutW(0, tlv); // type
        pck.msbPutW(2, pck.dataSize()); // length
        pck.putSkip(4);
        advertiseLsa(lsa, seq, pck);
    }

    private tabRouteEntry<addrIP> parseEprfTlv(typLenVal tlv) {
        if ((tlv.valTyp != rtrOspf6lsa.tlvPrefix) && (tlv.valTyp != rtrOspf6lsa.tlvInterPrf)) {
            return null;
        }
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.metric = bits.msbGetD(tlv.valDat, 0) & 0xffffff; // metric
        addrIPv6 a = new addrIPv6();
        a.fromBuf(tlv.valDat, 8); // address
        addrPrefix<addrIPv6> prf = new addrPrefix<addrIPv6>(a, tlv.valDat[4] & 0xff);
        ntry.prefix = addrPrefix.ip6toIP(prf);
        int i = 8 + prefixSize(prf);
        if (i > tlv.valSiz) {
            return null;
        }
        packHolder pck = new packHolder(true, true);
        pck.putCopy(tlv.valDat, i, 0, tlv.valSiz - i);
        pck.putSkip(tlv.valSiz - i);
        pck.merge2beg();
        tlv = rtrOspfTe.getTlvHandler();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            rtrOspfSr.getPref(tlv, ntry);
            rtrOspfBr.getPref(tlv, ntry);
        }
        return ntry;
    }

    private void createLnkLsas() {
        for (int i = 0; i < lower.ifaces.size(); i++) {
            rtrOspf6iface ifc = lower.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.area.area != area) {
                continue;
            }
            createLnkLsa(ifc);
        }
    }

    private void createNetLsas() {
        for (int i = 0; i < lower.ifaces.size(); i++) {
            rtrOspf6iface ifc = lower.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.area.area != area) {
                continue;
            }
            if (!ifc.amIdr()) {
                continue;
            }
            createNetLsa(ifc);
        }
    }

    private void createPrefLsas() {
        for (int i = 0; i < lower.ifaces.size(); i++) {
            rtrOspf6iface ifc = lower.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.area.area != area) {
                continue;
            }
            if (ifc.suppressAddr) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
                continue;
            }
            createPrefLsa(ifc);
        }
    }

    private void createExtLsas() {
        if (stub) {
            return;
        }
        tabRoute<addrIP> rs = new tabRoute<addrIP>("fl");
        if (defOrigin) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = addrPrefix.ip6toIP(addrPrefix.defaultRoute6());
            ntry.origin = 111;
            tabRoute.addUpdatedEntry(tabRoute.addType.better, rs, rtrBgpUtil.safiUnicast, ntry, roumapInto, roupolInto, prflstInto);
        }
        tabRoute.addUpdatedTable(tabRoute.addType.better, rtrBgpUtil.safiUnicast, rs, lower.routerRedistedU, roumapInto, roupolInto, prflstInto);
        for (int i = 0; i < rs.size(); i++) {
            tabRouteEntry<addrIP> ntry = rs.get(i);
            if (ntry == null) {
                continue;
            }
            createExtLsa(i, ntry.prefix, ntry.origin, ntry.metric, ntry.tag);
        }
    }

    private void createSumLsas() {
        if (!lower.amIabr()) {
            return;
        }
        if (stub || nssa) {
            createSumLsa(0, addrPrefix.ip6toIP(addrPrefix.defaultRoute6()), 0);
            return;
        }
        tabRoute<addrIP> rs = new tabRoute<addrIP>("rs");
        for (int i = 0; i < lower.ifaces.size(); i++) {
            rtrOspf6iface ifc = lower.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.area.area == area) {
                continue;
            }
            if (ifc.suppressAddr) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
                continue;
            }
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = ifc.iface.network.copyBytes();
            ntry.metric = ifc.metric;
            tabRoute.addUpdatedEntry(tabRoute.addType.better, rs, rtrBgpUtil.safiUnicast, ntry, roumapInto, roupolInto, prflstInto);
        }
        for (int i = 0; i < lower.routerComputedU.size(); i++) {
            tabRouteEntry<addrIP> ntry = lower.routerComputedU.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.rouSrc == area) {
                continue;
            }
            tabRoute.addUpdatedEntry(tabRoute.addType.better, rs, rtrBgpUtil.safiUnicast, ntry, roumapInto, roupolInto, prflstInto);
        }
        for (int i = 0; i < rs.size(); i++) {
            tabRouteEntry<addrIP> ntry = rs.get(i);
            if (ntry == null) {
                continue;
            }
            createSumLsa(i, ntry.prefix, ntry.metric);
        }
    }

    private void createTeLsas() {
        if (!traffEng) {
            return;
        }
        createTeLsa(0, null, null);
        int seq = 1;
        for (int o = 0; o < lower.ifaces.size(); o++) {
            rtrOspf6iface ifc = lower.ifaces.get(o);
            if (ifc == null) {
                continue;
            }
            if (ifc.teSuppress) {
                continue;
            }
            if (ifc.area.area != area) {
                continue;
            }
            if (ifc.needDR()) {
                createTeLsa(seq++, ifc, null);
                continue;
            }
            for (int i = 0; i < ifc.neighs.size(); i++) {
                rtrOspf6neigh nei = ifc.neighs.get(i);
                if (nei == null) {
                    continue;
                }
                if (!nei.isFull()) {
                    continue;
                }
                createTeLsa(seq++, ifc, nei);
            }
        }
    }

    private void createRiLsa() {
        if (!(traffEng || hostname || segrouEna)) {
            return;
        }
        packHolder pck = new packHolder(true, true);
        rtrOspfRi.putCapa(pck, traffEng);
        if (hostname) {
            rtrOspfRi.putHstnam(pck);
        }
        if (segrouEna && (lower.segrouLab != null)) {
            rtrOspfSr.putBase(pck, lower.segrouLab);
        }
        advertiseLsa(rtrOspf6lsa.lsaRtrInfo, 0, pck);
    }

    private void createEprfLsas() {
        if (!segrouEna && !bierEna) {
            return;
        }
        int seq = 1;
        for (int i = 0; i < lower.ifaces.size(); i++) {
            rtrOspf6iface ifc = lower.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            byte[] buf = new byte[0];
            if (ifc.srIndex > 0) {
                buf = rtrOspfSr.putPref(ifc.srIndex);
            }
            if (ifc.brIndex > 0) {
                buf = bits.byteConcat(buf, rtrOspfBr.putPref(lower.bierLab, lower.bierLen, ifc.brIndex));
            }
            if (buf.length < 1) {
                continue;
            }
            int o;
            int p;
            if (ifc.area.area == area) {
                o = rtrOspf6lsa.lsaEprefix;
                p = rtrOspf6lsa.tlvPrefix;
            } else {
                o = rtrOspf6lsa.lsaEinterPrf;
                p = rtrOspf6lsa.tlvInterPrf;
            }
            int q = rtrOspf6lsa.prefProp;
            if (ifc.srNode) {
                q |= rtrOspf6lsa.prefNode;
            }
            createEprfLsa(seq++, ifc.iface.network, o, p, ifc.metric, q, buf);
        }
        if (stub || nssa) {
            return;
        }
        for (int i = 0; i < lower.routerComputedU.size(); i++) {
            tabRouteEntry<addrIP> ntry = lower.routerComputedU.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.rouSrc == area) {
                continue;
            }
            byte[] buf = new byte[0];
            if (ntry.segrouIdx > 0) {
                buf = rtrOspfSr.putPref(ntry.segrouIdx);
            }
            if (ntry.bierIdx > 0) {
                buf = bits.byteConcat(buf, rtrOspfBr.putPref(lower.bierLab, lower.bierLen, ntry.bierIdx));
            }
            if (buf.length < 1) {
                continue;
            }
            createEprfLsa(seq++, ntry.prefix, rtrOspf6lsa.lsaEinterPrf, rtrOspf6lsa.tlvInterPrf, ntry.metric, rtrOspf6lsa.prefProp, buf);
        }
    }

    private void generateLsas() {
        if (debugger.rtrOspf6evnt) {
            logger.debug("generate lsas in area " + area);
        }
        need2adv.clear();
        createRtrLsa();
        createLnkLsas();
        createNetLsas();
        createPrefLsas();
        createExtLsas();
        createSumLsas();
        createTeLsas();
        createRiLsa();
        createErtrLsa();
        createEprfLsas();
        advertiseLsas();
    }

    private void calculateSpf() {
        if (debugger.rtrOspf6evnt) {
            logger.debug("calculate spf on area " + area);
        }
        long tim = bits.getTime() - rtrOspf6lsa.lsaMaxAge + 1;
        shrtPthFrst<rtrOspf6areaSpf> spf = new shrtPthFrst<rtrOspf6areaSpf>(lastSpf);
        for (int i = 0; i < lsas.size(); i++) {
            rtrOspf6lsa ntry = lsas.get(i);
            if (ntry == null) {
                continue;
            }
            if (!ntry.doNotAge) {
                if (ntry.created < tim) {
                    continue;
                }
            }
            rtrOspf6areaSpf src = new rtrOspf6areaSpf(ntry.rtrID, ntry.lsaID);
            packHolder pck = ntry.getPayload();
            switch (ntry.lsaType) {
                case rtrOspf6lsa.lsaRouter:
                    pck.getSkip(4);
                    for (;;) {
                        if (pck.dataSize() < 16) {
                            break;
                        }
                        int typ = pck.getByte(0); // type
                        int met = pck.msbGetW(2); // metric
                        int lnk = pck.msbGetD(8); // neighbor link id
                        addrIPv4 adr = new addrIPv4();
                        pck.getAddr(adr, 12); // neighbor router id
                        pck.getSkip(16);
                        switch (typ) {
                            case rtrOspf6lsa.lnkP2p:
                                spf.addConn(src, new rtrOspf6areaSpf(adr, 0), met, true);
                                break;
                            case rtrOspf6lsa.lnkTrns:
                                spf.addConn(src, new rtrOspf6areaSpf(adr, lnk), met, false);
                                break;
                            default:
                                break;
                        }
                    }
                    break;
                case rtrOspf6lsa.lsaNetwork:
                    pck.getSkip(4);
                    for (;;) {
                        if (pck.dataSize() < addrIPv4.size) {
                            break;
                        }
                        addrIPv4 adr = new addrIPv4();
                        pck.getAddr(adr, 0); // router id
                        pck.getSkip(addrIPv4.size);
                        spf.addConn(src, new rtrOspf6areaSpf(adr, 0), 0, true);
                    }
                    break;
                case rtrOspf6lsa.lsaInterRtr:
                    src = new rtrOspf6areaSpf(ntry.rtrID, 0);
                    addrIPv4 adr = new addrIPv4();
                    pck.getAddr(adr, 8); // router id
                    int met = pck.msbGetD(4) & 0xffffff; // metric
                    spf.addConn(src, new rtrOspf6areaSpf(adr, 0), met, true);
                    break;
                case rtrOspf6lsa.lsaRtrInfo:
                    typLenVal tlv = rtrOspfTe.getTlvHandler();
                    for (;;) {
                        if (tlv.getBytes(pck)) {
                            break;
                        }
                        int o = rtrOspfSr.getBase(tlv);
                        if (o < 1) {
                            continue;
                        }
                        spf.addSegRouB(src, o);
                    }
                    break;
                case rtrOspf6lsa.lsaEinterPrf:
                case rtrOspf6lsa.lsaEprefix:
                    if (!bierEna) {
                        continue;
                    }
                    tlv = rtrOspfTe.getTlvHandler();
                    for (;;) {
                        if (tlv.getBytes(pck)) {
                            break;
                        }
                        tabRouteEntry<addrIP> pref = parseEprfTlv(tlv);
                        if (pref == null) {
                            continue;
                        }
                        spf.addBierB(new rtrOspf6areaSpf(ntry.rtrID, 0), pref.bierBeg);
                    }
                    break;
                default:
                    break;
            }
        }
        spf.doCalc(new rtrOspf6areaSpf(lower.routerID, 0), null);
        if (segrouEna && (lower.segrouLab != null)) {
            segrouUsd = new boolean[lower.segrouMax];
        } else {
            segrouUsd = null;
        }
        for (int i = 0; i < lower.ifaces.size(); i++) {
            rtrOspf6iface ifc = lower.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.area.area != area) {
                continue;
            }
            if ((segrouUsd != null) && (ifc.srIndex > 0)) {
                segrouUsd[ifc.srIndex] = true;
                lower.segrouLab[ifc.srIndex].setFwdCommon(9, lower.fwdCore);
            }
            if (ifc.needDR()) {
                if (ifc.drAddr.isEmpty()) {
                    continue;
                }
                spf.addNextHop(ifc.metric, new rtrOspf6areaSpf(ifc.drAddr, ifc.DRintId()), ifc.DRintAdr(), ifc.iface);
            }
            for (int o = 0; o < ifc.neighs.size(); o++) {
                rtrOspf6neigh nei = ifc.neighs.get(o);
                if (nei == null) {
                    continue;
                }
                if (!nei.seenMyself) {
                    continue;
                }
                addrIP adr = new addrIP();
                adr.fromIPv6addr(nei.peer);
                spf.addNextHop(ifc.metric, new rtrOspf6areaSpf(nei.rtrID, 0), adr, ifc.iface);
            }
        }
        tabRoute<addrIP> rs = new tabRoute<addrIP>("rs");
        for (int i = 0; i < lsas.size(); i++) {
            rtrOspf6lsa ntry = lsas.get(i);
            if (ntry == null) {
                continue;
            }
            if (!ntry.doNotAge) {
                if (ntry.created < tim) {
                    continue;
                }
            }
            rtrOspf6areaSpf src = new rtrOspf6areaSpf(ntry.rtrID, 0);
            addrIP hop = spf.getNextHop(src);
            if (hop == null) {
                continue;
            }
            int hops = spf.getHops(src);
            ipFwdIface iface = (ipFwdIface) spf.getNextIfc(src);
            int met = spf.getMetric(src);
            int srb = spf.getSegRouB(src, false);
            int sro = spf.getSegRouB(src, true);
            int brb = spf.getBierB(src, false);
            int bro = spf.getBierB(src, true);
            tabRouteEntry<addrIP> pref;
            rtrOspf6areaPref prf6;
            packHolder pck = ntry.getPayload();
            switch (ntry.lsaType) {
                case rtrOspf6lsa.lsaInterPrf:
                    met += pck.msbGetD(0) & 0xffffff;
                    prf6 = new rtrOspf6areaPref();
                    prefixRead(pck, 4, prf6);
                    pref = new tabRouteEntry<addrIP>();
                    pref.prefix = prf6.prefix;
                    pref.nextHop = hop.copyBytes();
                    pref.metric = met;
                    pref.origin = 110;
                    pref.distance = lower.distantSum;
                    pref.srcRtr = ntry.rtrID.copyBytes();
                    pref.iface = iface;
                    pref.rouSrc = area;
                    rs.add(tabRoute.addType.better, pref, false, true);
                    break;
                case rtrOspf6lsa.lsaLink:
                    int o = pck.msbGetD(20); // number of prefixes
                    pck.getSkip(24);
                    for (int p = 0; p < o; p++) {
                        prf6 = new rtrOspf6areaPref();
                        pck.getSkip(prefixRead(pck, 0, prf6));
                        pref = new tabRouteEntry<addrIP>();
                        pref.prefix = prf6.prefix;
                        pref.nextHop = hop.copyBytes();
                        pref.metric = met;
                        pref.origin = 109;
                        pref.distance = lower.distantSum;
                        pref.srcRtr = ntry.rtrID.copyBytes();
                        pref.iface = iface;
                        pref.rouSrc = area;
                        rs.add(tabRoute.addType.better, pref, false, true);
                    }
                    break;
                case rtrOspf6lsa.lsaPrefix:
                    o = pck.msbGetW(0); // number of prefixes
                    pck.getSkip(12);
                    for (int p = 0; p < o; p++) {
                        prf6 = new rtrOspf6areaPref();
                        pck.getSkip(prefixRead(pck, 0, prf6));
                        pref = new tabRouteEntry<addrIP>();
                        pref.prefix = prf6.prefix;
                        pref.nextHop = hop.copyBytes();
                        pref.metric = met;
                        pref.origin = 109;
                        pref.distance = lower.distantSum;
                        pref.srcRtr = ntry.rtrID.copyBytes();
                        pref.iface = iface;
                        pref.rouSrc = area;
                        rs.add(tabRoute.addType.better, pref, false, true);
                    }
                    break;
                case rtrOspf6lsa.lsaAsExt:
                case rtrOspf6lsa.lsaNssaExt:
                    o = pck.msbGetD(0);
                    pck.getSkip(4);
                    prf6 = new rtrOspf6areaPref();
                    pck.getSkip(prefixRead(pck, 0, prf6));
                    if ((o & 0x02000000) != 0) {
                        pck.getSkip(addrIPv6.size);
                    }
                    pref = new tabRouteEntry<addrIP>();
                    pref.prefix = prf6.prefix;
                    pref.nextHop = hop.copyBytes();
                    pref.metric = o & 0xffffff;
                    if ((o & 0x04000000) != 0) {
                        pref.origin = 112;
                    } else {
                        pref.metric += met;
                        pref.origin = 111;
                    }
                    pref.distance = lower.distantExt;
                    pref.srcRtr = ntry.rtrID.copyBytes();
                    pref.iface = iface;
                    pref.rouSrc = area;
                    if ((o & 0x01000000) != 0) {
                        pref.tag = pck.msbGetD(0); // route tag
                    }
                    rs.add(tabRoute.addType.better, pref, false, true);
                    break;
                case rtrOspf6lsa.lsaEinterPrf:
                case rtrOspf6lsa.lsaEprefix:
                    if ((segrouUsd == null) && !bierEna) {
                        continue;
                    }
                    typLenVal tlv = rtrOspfTe.getTlvHandler();
                    for (;;) {
                        if (tlv.getBytes(pck)) {
                            break;
                        }
                        pref = parseEprfTlv(tlv);
                        if (pref == null) {
                            continue;
                        }
                        tabRouteEntry<addrIP> old = rs.find(pref);
                        if (old == null) {
                            continue;
                        }
                        if (hop.compare(hop, old.nextHop) != 0) {
                            continue;
                        }
                        spf.addSegRouI(src, pref.segrouIdx);
                        spf.addBierI(src, pref.bierIdx, old.origin == 109);
                        old.segrouIdx = pref.segrouIdx;
                        old.segrouBeg = srb;
                        old.segrouOld = sro;
                        old.bierIdx = pref.bierIdx;
                        old.bierHdr = pref.bierHdr;
                        old.bierBeg = brb;
                        old.bierOld = bro;
                        if ((pref.segrouIdx < 1) || (pref.segrouIdx >= lower.segrouMax) || (srb < 1)) {
                            continue;
                        }
                        List<Integer> lab = tabLabel.int2labels(srb + pref.segrouIdx);
                        if (((pref.rouSrc & 16) != 0) && (hops <= 1)) {
                            lab = tabLabel.int2labels(ipMpls.labelImp);
                        }
                        lower.segrouLab[pref.segrouIdx].setFwdMpls(9, lower.fwdCore, iface, hop, lab);
                        segrouUsd[pref.segrouIdx] = true;
                        old.labelRem = lab;
                    }
                    break;
                default:
                    break;
            }
        }
        routes.clear();
        tabRoute.addUpdatedTable(tabRoute.addType.better, rtrBgpUtil.safiUnicast, routes, rs, roumapFrom, roupolFrom, prflstFrom);
        lower.routerDoAggregates(rtrBgpUtil.safiUnicast, routes, null, lower.fwdCore.commonLabel, 0, null, 0);
        if (bierEna) {
            bierRes = spf.getBierI();
        } else {
            bierRes = null;
        }
        if (debugger.rtrOspf6evnt) {
            logger.debug("unreachable:" + spf.listUnreachables());
            logger.debug("reachable:" + spf.listReachables());
        }
        lastSpf = spf;
        lower.routerCreateComputed();
    }

    /**
     * schedule work
     *
     * @param i what to do: 1=genLsa, 2=calcSpf, 4=genAll, 7=all
     */
    public void schedWork(int i) {
        todo.or(i << 4);
        notif.wakeup();
    }

    public void run() {
        if (debugger.rtrOspf6evnt) {
            logger.debug("started area " + area);
        }
        todo.or(2);
        for (;;) {
            try {
                notif.misleep(10000);
                int ver = todo.ver();
                int val = todo.get();
                todo.andIf(0xf, ver);
                if ((val & 1) == 0) {
                    break;
                }
                if ((val & 0x10) != 0) {
                    generateLsas();
                }
                if ((val & 0x20) != 0) {
                    calculateSpf();
                }
                if ((val & 0x40) != 0) {
                    lower.genLsas(1);
                }
                purgeLsas();
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
        todo.and(1);
        if (debugger.rtrOspf6evnt) {
            logger.debug("stopped area " + area);
        }
    }

    /**
     * start this area
     */
    public void startNow() {
        if ((todo.get() & 2) != 0) {
            return;
        }
        todo.or(1);
        new Thread(this).start();
    }

    /**
     * stop this area
     */
    public void stopNow() {
        todo.and(2);
    }

}

class rtrOspf6areaPref {

    protected int option;

    protected int metric;

    protected addrPrefix<addrIP> prefix;

}
