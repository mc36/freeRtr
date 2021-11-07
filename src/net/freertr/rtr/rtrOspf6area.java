package net.freertr.rtr;

import java.util.Comparator;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrIPv4;
import net.freertr.addr.addrIPv6;
import net.freertr.addr.addrPrefix;
import net.freertr.pack.packHolder;
import net.freertr.tab.tabGen;
import net.freertr.tab.tabIndex;
import net.freertr.tab.tabLabelBier;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabPrfxlstN;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRtrmapN;
import net.freertr.tab.tabRtrplcN;
import net.freertr.util.bits;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.notifier;
import net.freertr.util.shrtPthFrst;
import net.freertr.util.state;
import net.freertr.util.syncInt;
import net.freertr.util.typLenVal;

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
    protected tabGen<tabIndex<addrIP>> segrouUsd;

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
        lastSpf = new shrtPthFrst<rtrOspf6areaSpf>(null);
        lower = parent;
        area = num;
        lsas = new tabGen<rtrOspf6lsa>();
        need2adv = new tabGen<rtrOspf6lsa>();
        routes = new tabRoute<addrIP>("computed");
        notif = new notifier();
        hostname = true;
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
        int done = 0;
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
            done++;
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
            done++;
        }
        if (done > 0) {
            wakeNeighs();
        }
    }

    /**
     * wake up neighbors
     */
    protected void wakeNeighs() {
        for (int o = 0; o < lower.ifaces.size(); o++) {
            rtrOspf6iface iface = lower.ifaces.get(o);
            if (iface == null) {
                continue;
            }
            for (int i = 0; i < iface.neighs.size(); i++) {
                rtrOspf6neigh neigh = iface.neighs.get(i);
                if (neigh == null) {
                    continue;
                }
                if (neigh.area.area != area) {
                    continue;
                }
                neigh.notif.wakeup();
            }
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
        int done = 0;
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
            done++;
        }
        if (done > 0) {
            wakeNeighs();
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

    /**
     * write prefix
     *
     * @param pck packet
     * @param ofs offset
     * @param pref prefix
     * @return size
     */
    protected static int prefixWrite(packHolder pck, int ofs, rtrOspf6pref pref) {
        addrPrefix<addrIPv6> prf = addrPrefix.ip2ip6(pref.prefix);
        pck.putByte(ofs + 0, prf.maskLen); // prefix length
        pck.putByte(ofs + 1, pref.option); // prefix options
        pck.msbPutW(ofs + 2, pref.metric); // metric
        pck.putAddr(ofs + 4, prf.network); // address
        return 4 + prefixSize(prf);
    }

    /**
     * read prefix
     *
     * @param pck packet
     * @param ofs offset
     * @param pref prefix
     * @return size
     */
    protected static int prefixRead(packHolder pck, int ofs, rtrOspf6pref pref) {
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
            if (ifc.areas.find(this) == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
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
                if (nei.area.area != area) {
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
            if (nei.area.area != area) {
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
        rtrOspf6pref prf = new rtrOspf6pref();
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
        rtrOspf6pref prf = new rtrOspf6pref();
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
        rtrOspf6pref prf = new rtrOspf6pref();
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
            if (ifc.areas.find(this) == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
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
                if (nei.area.area != area) {
                    continue;
                }
                if (!nei.isFull()) {
                    continue;
                }
                byte[] buf = new byte[0];
                if (nei.segrouLab != null) {
                    buf = rtrOspfSr.putAdj(nei.segrouLab.label);
                }
                putLink2ertrLsa(pck, rtrOspf6lsa.lnkP2p, ifc.iface.ifwNum, nei.rtrInt, nei.rtrID, ifc.metric, buf);
            }
        }
        advertiseLsa(rtrOspf6lsa.lsaErouter, 0, pck);
    }

    private void createEprfLsa(int seq, addrPrefix<addrIP> pref, int lsa, int tlv, int met, int opt, byte[] beg, byte[] subs) {
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
        pck.putCopy(beg, 0, 0, beg.length);
        pck.putSkip(beg.length);
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
        ntry.best.metric = bits.msbGetD(tlv.valDat, 0) & 0xffffff; // metric
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
            if (ifc.areas.find(this) == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
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
            if (ifc.areas.find(this) == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
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
            if (ifc.suppressAddr) {
                continue;
            }
            if (ifc.areas.get(0).area != area) {
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
            ntry.best.origin = 111;
            tabRoute.addUpdatedEntry(tabRoute.addType.better, rs, rtrBgpUtil.sfiUnicast, 0, ntry, true, roumapInto, roupolInto, prflstInto);
        }
        tabRoute.addUpdatedTable(tabRoute.addType.better, rtrBgpUtil.sfiUnicast, 0, rs, lower.routerRedistedU, true, roumapInto, roupolInto, prflstInto);
        for (int i = 0; i < rs.size(); i++) {
            tabRouteEntry<addrIP> ntry = rs.get(i);
            if (ntry == null) {
                continue;
            }
            createExtLsa(i, ntry.prefix, ntry.best.origin, ntry.best.metric, ntry.best.tag);
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
        tabRoute<addrIP> oa = new tabRoute<addrIP>("rs");
        for (int i = 0; i < lower.ifaces.size(); i++) {
            rtrOspf6iface ifc = lower.ifaces.get(i);
            if (ifc == null) {
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
            ntry.best.metric = ifc.metric;
            oa.add(tabRoute.addType.better, ntry, true, true);
            if (ifc.areas.get(0).area == area) {
                continue;
            }
            tabRoute.addUpdatedEntry(tabRoute.addType.better, rs, rtrBgpUtil.sfiUnicast, 0, ntry, true, roumapInto, roupolInto, prflstInto);
        }
        for (int i = 0; i < lower.routerComputedU.size(); i++) {
            tabRouteEntry<addrIP> ntry = lower.routerComputedU.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.best.aggrAs == area) {
                continue;
            }
            if (oa.find(ntry.prefix) != null) {
                continue;
            }
            tabRoute.addUpdatedEntry(tabRoute.addType.better, rs, rtrBgpUtil.sfiUnicast, 0, ntry, true, roumapInto, roupolInto, prflstInto);
        }
        for (int i = 0; i < rs.size(); i++) {
            tabRouteEntry<addrIP> ntry = rs.get(i);
            if (ntry == null) {
                continue;
            }
            createSumLsa(i, ntry.prefix, ntry.best.metric);
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
            if (ifc.areas.find(this) == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
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
                if (nei.area.area != area) {
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
            if (ifc.iface.lower.getState() != state.states.up) {
                continue;
            }
            byte[] buf = new byte[0];
            if (ifc.srIndex > 0) {
                buf = rtrOspfSr.putPref(ifc.srIndex, ifc.srPop);
            }
            if (ifc.brIndex > 0) {
                buf = bits.byteConcat(buf, rtrOspfBr.putPref(lower.bierLab, lower.bierLen, ifc.brIndex));
            }
            if (buf.length < 1) {
                continue;
            }
            int o;
            int p;
            byte[] buf2;
            if (ifc.areas.find(this) != null) {
                o = rtrOspf6lsa.lsaEprefix;
                p = rtrOspf6lsa.tlvPrefix;
                buf2 = new byte[12];
            } else {
                o = rtrOspf6lsa.lsaEinterPrf;
                p = rtrOspf6lsa.tlvInterPrf;
                buf2 = new byte[0];
            }
            int q = rtrOspf6lsa.prefProp;
            if (ifc.srNode) {
                q |= rtrOspf6lsa.prefNode;
            }
            createEprfLsa(seq++, ifc.iface.network, o, p, ifc.metric, q, buf2, buf);
        }
        if (stub || nssa) {
            return;
        }
        for (int i = 0; i < lower.routerComputedU.size(); i++) {
            tabRouteEntry<addrIP> ntry = lower.routerComputedU.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.best.aggrAs == area) {
                continue;
            }
            byte[] buf = new byte[0];
            if (ntry.best.segrouIdx > 0) {
                buf = rtrOspfSr.putPref(ntry.best.segrouIdx, false);
            }
            if (ntry.best.bierIdx > 0) {
                buf = bits.byteConcat(buf, rtrOspfBr.putPref(lower.bierLab, lower.bierLen, ntry.best.bierIdx));
            }
            if (buf.length < 1) {
                continue;
            }
            createEprfLsa(seq++, ntry.prefix, rtrOspf6lsa.lsaEinterPrf, rtrOspf6lsa.tlvInterPrf, ntry.best.metric, rtrOspf6lsa.prefProp, new byte[0], buf);
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
            tabRouteEntry<addrIP> pref;
            rtrOspf6pref prf6;
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
                                spf.addConn(src, new rtrOspf6areaSpf(adr, 0), met, true, false, "" + lnk);
                                break;
                            case rtrOspf6lsa.lnkTrns:
                                spf.addConn(src, new rtrOspf6areaSpf(adr, lnk), met, false, false, null);
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
                        spf.addConn(src, new rtrOspf6areaSpf(adr, 0), 0, true, false, null);
                    }
                    break;
                case rtrOspf6lsa.lsaInterRtr:
                    src = new rtrOspf6areaSpf(ntry.rtrID, 0);
                    addrIPv4 adr = new addrIPv4();
                    pck.getAddr(adr, 8); // router id
                    int met = pck.msbGetD(4) & 0xffffff; // metric
                    spf.addConn(src, new rtrOspf6areaSpf(adr, 0), met, true, false, null);
                    break;
                case rtrOspf6lsa.lsaInterPrf:
                    src = new rtrOspf6areaSpf(ntry.rtrID, 0);
                    met = pck.msbGetD(0) & 0xffffff;
                    prf6 = new rtrOspf6pref();
                    prefixRead(pck, 4, prf6);
                    pref = new tabRouteEntry<addrIP>();
                    pref.prefix = prf6.prefix;
                    pref.best.metric = met;
                    pref.best.origin = 110;
                    pref.best.distance = lower.distantSum;
                    pref.best.aggrAs = area;
                    spf.addPref(src, pref, false);
                    break;
                case rtrOspf6lsa.lsaLink:
                    src = new rtrOspf6areaSpf(ntry.rtrID, 0);
                    int o = pck.msbGetD(20); // number of prefixes
                    pck.getSkip(24);
                    for (int p = 0; p < o; p++) {
                        prf6 = new rtrOspf6pref();
                        pck.getSkip(prefixRead(pck, 0, prf6));
                        pref = new tabRouteEntry<addrIP>();
                        pref.prefix = prf6.prefix;
                        pref.best.metric = prf6.metric;
                        pref.best.origin = 109;
                        pref.best.distance = lower.distantSum;
                        pref.best.aggrAs = area;
                        spf.addPref(src, pref, false);
                    }
                    break;
                case rtrOspf6lsa.lsaPrefix:
                    src = new rtrOspf6areaSpf(ntry.rtrID, 0);
                    o = pck.msbGetW(0); // number of prefixes
                    pck.getSkip(12);
                    for (int p = 0; p < o; p++) {
                        prf6 = new rtrOspf6pref();
                        pck.getSkip(prefixRead(pck, 0, prf6));
                        pref = new tabRouteEntry<addrIP>();
                        pref.prefix = prf6.prefix;
                        pref.best.metric = prf6.metric;
                        pref.best.origin = 109;
                        pref.best.distance = lower.distantSum;
                        pref.best.aggrAs = area;
                        spf.addPref(src, pref, false);
                    }
                    break;
                case rtrOspf6lsa.lsaAsExt:
                case rtrOspf6lsa.lsaNssaExt:
                    src = new rtrOspf6areaSpf(ntry.rtrID, 0);
                    o = pck.msbGetD(0);
                    pck.getSkip(4);
                    prf6 = new rtrOspf6pref();
                    pck.getSkip(prefixRead(pck, 0, prf6));
                    if ((o & 0x02000000) != 0) {
                        pck.getSkip(addrIPv6.size);
                    }
                    pref = new tabRouteEntry<addrIP>();
                    pref.prefix = prf6.prefix;
                    pref.best.metric = o & 0xffffff;
                    pref.best.distance = lower.distantExt;
                    pref.best.aggrAs = area;
                    if ((o & 0x01000000) != 0) {
                        pref.best.tag = pck.msbGetD(0); // route tag
                    }
                    if ((o & 0x04000000) != 0) {
                        pref.best.origin = 112;
                        spf.addPref(src, pref, true);
                    } else {
                        pref.best.origin = 111;
                        spf.addPref(src, pref, false);
                    }
                    break;
                case rtrOspf6lsa.lsaRtrInfo:
                    typLenVal tlv = rtrOspfTe.getTlvHandler();
                    for (;;) {
                        if (tlv.getBytes(pck)) {
                            break;
                        }
                        o = rtrOspfSr.getBase(tlv);
                        spf.addSegRouB(src, o);
                        spf.addIdent(src, rtrOspfRi.getHstnam(tlv));
                    }
                    break;
                case rtrOspf6lsa.lsaEinterPrf:
                case rtrOspf6lsa.lsaEprefix:
                    src = new rtrOspf6areaSpf(ntry.rtrID, 0);
                    tlv = rtrOspfTe.getTlvHandler();
                    if (ntry.lsaType == rtrOspf6lsa.lsaEprefix) {
                        pck.getSkip(12);
                    }
                    for (;;) {
                        if (tlv.getBytes(pck)) {
                            break;
                        }
                        pref = parseEprfTlv(tlv);
                        if (pref == null) {
                            continue;
                        }
                        spf.addBierB(src, pref.best.bierBeg);
                        spf.addSegRouI(src, pref.prefix, pref.best.segrouIdx, pref.best.rouSrc & 16);
                        spf.addBierI(src, pref.prefix, pref.best.bierIdx, pref.best.bierHdr);
                    }
                    break;
                default:
                    break;
            }
        }
        spf.doCalc(new rtrOspf6areaSpf(lower.routerID, 0), null);
        if (segrouEna && (lower.segrouLab != null)) {
            segrouUsd = new tabGen<tabIndex<addrIP>>();
        } else {
            segrouUsd = null;
        }
        for (int i = 0; i < lower.ifaces.size(); i++) {
            rtrOspf6iface ifc = lower.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.areas.find(this) == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
                continue;
            }
            if ((segrouUsd != null) && (ifc.srIndex > 0)) {
                tabIndex.add2table(segrouUsd, new tabIndex<addrIP>(ifc.srIndex, new addrPrefix<addrIP>(new addrIP(), 0)));
                lower.segrouLab[ifc.srIndex].setFwdCommon(9, lower.fwdCore);
            }
            if (ifc.needDR()) {
                if (ifc.drAddr.isEmpty()) {
                    continue;
                }
                spf.addNextHop(ifc.metric, new rtrOspf6areaSpf(ifc.drAddr, ifc.DRintId()), ifc.DRintAdr(), ifc.iface, null, null);
            }
            for (int o = 0; o < ifc.neighs.size(); o++) {
                rtrOspf6neigh nei = ifc.neighs.get(o);
                if (nei == null) {
                    continue;
                }
                if (nei.area.area != area) {
                    continue;
                }
                if (!nei.seenMyself) {
                    continue;
                }
                addrIP adr = new addrIP();
                adr.fromIPv6addr(nei.peer);
                spf.addNextHop(ifc.metric, new rtrOspf6areaSpf(nei.rtrID, 0), adr, ifc.iface, null, null);
            }
        }
        tabRoute<addrIP> rs = spf.getRoutes(lower.fwdCore, 9, lower.segrouLab, segrouUsd);
        routes.clear();
        tabRoute.addUpdatedTable(tabRoute.addType.ecmp, rtrBgpUtil.sfiUnicast, 0, routes, rs, true, roumapFrom, roupolFrom, prflstFrom);
        lower.routerDoAggregates(rtrBgpUtil.sfiUnicast, routes, routes, lower.fwdCore.commonLabel, null, 0);
        if (bierEna) {
            bierRes = spf.getBierI(0, 0);
        } else {
            bierRes = null;
        }
        if (debugger.rtrOspf6evnt) {
            logger.debug("unreachable:" + spf.listReachablility(false));
            logger.debug("reachable:" + spf.listReachablility(true));
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
                notif.misleep(30000);
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
