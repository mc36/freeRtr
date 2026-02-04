package org.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrPrefix;
import org.freertr.enc.encBase64;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.tab.tabLabelBier;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRtrmapN;
import org.freertr.tab.tabRtrplcN;
import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.util.logger;
import org.freertr.util.notifier;
import org.freertr.spf.spfCalc;
import org.freertr.util.state;
import org.freertr.util.syncInt;
import org.freertr.enc.encTlv;
import org.freertr.tab.tabLabelEntry;
import org.freertr.util.cmds;

/**
 * ospfv2 area
 *
 * @author matecsaba
 */
public class rtrOspf4area implements Comparable<rtrOspf4area>, Runnable {

    /**
     * area number
     */
    public final int area;

    /**
     * ha mode
     */
    public boolean haMode;

    /**
     * lsas in this area
     */
    protected final tabGen<rtrOspf4lsa> lsas;

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
     * computed routes
     */
    protected List<tabRoute<addrIP>> algos;

    /**
     * maximum metric area
     */
    public boolean maxMetric;

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
     * suppress interface addresses
     */
    public boolean suppressAddr;

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
     * advertise route map
     */
    public tabListing<tabRtrplcN, addrIP> roupolFrom;

    /**
     * advertise route map
     */
    public tabListing<tabRtrplcN, addrIP> roupolInto;

    /**
     * last spf
     */
    protected spfCalc<addrIPv4> lastSpf;

    private final rtrOspf4 lower;

    private syncInt todo = new syncInt(0); // 1=need2run, 2=running, 0xffff0=works

    private tabGen<rtrOspf4lsa> need2adv;

    private notifier notif;

    /**
     * create one area
     *
     * @param parent the ospf protocol
     * @param num area number
     */
    public rtrOspf4area(rtrOspf4 parent, int num) {
        lastSpf = new spfCalc<addrIPv4>(null);
        lower = parent;
        area = num;
        lsas = new tabGen<rtrOspf4lsa>();
        need2adv = new tabGen<rtrOspf4lsa>();
        routes = new tabRoute<addrIP>("computed");
        notif = new notifier();
        hostname = true;
    }

    public String toString() {
        return "ospf area " + area;
    }

    public int compareTo(rtrOspf4area o) {
        if (area < o.area) {
            return -1;
        }
        if (area > o.area) {
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
        int i = 0;
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
     * advertise one lsa
     *
     * @param lsa lsa to advertise
     * @param purge set true to purge it out
     * @param cntnt check content
     */
    protected synchronized void generateLsa(rtrOspf4lsa lsa, boolean purge, boolean cntnt) {
        long tim = bits.getTime();
        if (purge) {
            tim -= rtrOspf4lsa.lsaMaxAge;
            if ((lsa.created <= tim) && (lsa.doNotAge == false)) {
                return;
            }
        }
        lsa.created = tim;
        lsa.doNotAge = false;
        rtrOspf4lsa old = lsas.find(lsa);
        if (old == null) {
            lsa.sequence = 0x80000001;
        } else {
            lsa.sequence = old.sequence + 1;
        }
        if (cntnt) {
            if (!lsa.contentDiffers(old)) {
                return;
            }
        }
        if (debugger.rtrOspf4evnt) {
            logger.debug("generate lsa " + lsa);
        }
        lsa.generateCheckSum();
        lsas.put(lsa);
    }

    /**
     * advertise needed lsas
     */
    protected void advertiseLsas() {
        if (debugger.rtrOspf4evnt) {
            logger.debug("advertise lsas in area " + area);
        }
        int done = 0;
        for (int i = 0; i < lsas.size(); i++) {
            rtrOspf4lsa ntry = lsas.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.rtrID.compareTo(lower.routerID) != 0) {
                continue;
            }
            if (need2adv.find(ntry) != null) {
                continue;
            }
            generateLsa(ntry, true, false);
            done++;
        }
        for (int i = 0; i < need2adv.size(); i++) {
            rtrOspf4lsa ntry = need2adv.get(i);
            if (ntry == null) {
                continue;
            }
            generateLsa(ntry, false, true);
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
            rtrOspf4iface iface = lower.ifaces.get(o);
            if (iface == null) {
                continue;
            }
            for (int i = 0; i < iface.neighs.size(); i++) {
                rtrOspf4neigh neigh = iface.neighs.get(i);
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
        if (debugger.rtrOspf4evnt) {
            logger.debug("purge lsas in area " + area);
        }
        long tim1 = bits.getTime() - rtrOspf4lsa.lsaMaxAge - (rtrOspf4lsa.lsaMaxAge / 100);
        long tim2 = tim1 + (rtrOspf4lsa.lsaMaxAge / 2);
        int done = 0;
        for (int i = lsas.size(); i >= 0; i--) {
            rtrOspf4lsa ntry = lsas.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.doNotAge) {
                continue;
            }
            if (ntry.created < tim1) {
                if (debugger.rtrOspf4evnt) {
                    logger.debug("purge " + ntry);
                }
                lsas.del(ntry);
                continue;
            }
            if (ntry.rtrID.compareTo(lower.routerID) != 0) {
                continue;
            }
            if (ntry.created > tim2) {
                continue;
            }
            if (need2adv.find(ntry) == null) {
                continue;
            }
            generateLsa(ntry, false, false);
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
    protected void advertiseLsa(int typ, addrIPv4 id, packHolder pck) {
        pck.merge2beg();
        rtrOspf4lsa lsa = new rtrOspf4lsa();
        lsa.lsaType = typ;
        lsa.lsaID = id.copyBytes();
        lsa.rtrID = lower.routerID.copyBytes();
        lsa.bufDat = pck.getCopy();
        lsa.capability = getCapabilities();
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
        addrIPv4 i = new addrIPv4();
        byte[] buf = new byte[4];
        bits.msbPutD(buf, 0, id);
        buf[0] = 1;
        i.fromBuf(buf, 0);
        advertiseLsa(rtrOspf4lsa.lsaOpArea, i, pck);
    }

    private void putLink2rtrLsa(packHolder pck, int typ, addrIPv4 lnk, addrIPv4 dat, int met) {
        pck.putAddr(0, lnk); // link id
        pck.putAddr(4, dat); // link data
        pck.putByte(8, typ); // type
        pck.putByte(9, 0); // number of tos
        pck.msbPutW(10, met); // metric
        pck.putSkip(12);
    }

    private static int prefixSize(addrPrefix<addrIPv4> prf) {
        return ((prf.maskLen + 31) / 32) * 4;
    }

    private void createRtrLsa() {
        packHolder pck = new packHolder(true, true);
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
        pck.putByte(0, i); // flags
        pck.putByte(1, 0); // reserved
        pck.msbPutW(2, 0); // number of links
        pck.putSkip(4);
        for (i = 0; i < lower.ifaces.size(); i++) {
            rtrOspf4iface ifc = lower.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.areas.find(this) == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
                continue;
            }
            int met = ifc.metric;
            if (maxMetric) {
                met = 0xffff;
            }
            if (ifc.needDR()) {
                if (ifc.drAddr.isEmpty()) {
                    continue;
                }
                putLink2rtrLsa(pck, rtrOspf4lsa.lnkTrns, ifc.drAddr, ifc.iface.addr.toIPv4(), met);
                continue;
            }
            for (int o = 0; o < ifc.neighs.size(); o++) {
                rtrOspf4neigh nei = ifc.neighs.get(o);
                if (nei == null) {
                    continue;
                }
                if (nei.area.area != area) {
                    continue;
                }
                if (!nei.isFull()) {
                    continue;
                }
                putLink2rtrLsa(pck, rtrOspf4lsa.lnkP2p, nei.rtrID, ifc.iface.addr.toIPv4(), nei.getMetric());
            }
            if ((suppressAddr || ifc.suppressAddr) && (!ifc.unsuppressAddr)) {
                continue;
            }
            if (ifc.areas.get(0).area != area) {
                continue;
            }
            if (ifc.needAdr()) {
                addrIPv4 msk = new addrIPv4();
                msk.fromNetmask(32);
                putLink2rtrLsa(pck, rtrOspf4lsa.lnkStub, ifc.iface.addr.toIPv4(), msk, 0);
            }
            putLink2rtrLsa(pck, rtrOspf4lsa.lnkStub, ifc.iface.addr.toIPv4(), ifc.iface.network.mask.toIPv4(), ifc.metric);
        }
        i = pck.headSize();
        pck.msbPutW(2 - i, i / 12); // number of links
        advertiseLsa(rtrOspf4lsa.lsaRouter, lower.routerID, pck);
    }

    private void createNetLsa(rtrOspf4iface ifc) {
        packHolder pck = new packHolder(true, true);
        pck.putAddr(0, ifc.iface.network.mask.toIPv4()); // netmask
        pck.putAddr(4, lower.routerID); // myself also full
        pck.putSkip(8);
        for (int i = 0; i < ifc.neighs.size(); i++) {
            rtrOspf4neigh nei = ifc.neighs.get(i);
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
        advertiseLsa(rtrOspf4lsa.lsaNetwork, ifc.iface.addr.toIPv4(), pck);
    }

    private void createExtLsa(addrPrefix<addrIPv4> prf, int org, int met, int tag) {
        packHolder pck = new packHolder(true, true);
        pck.putAddr(0, prf.mask); // netmask
        if (met < 0) {
            met = 0;
        }
        if (met > 0xffffff) {
            met = 0xffffff;
        }
        if (org != 111) {
            met |= 0x80000000;
        }
        pck.msbPutD(4, met); // metric
        pck.msbPutD(8, 0); // forwarding address
        pck.msbPutD(12, tag); // route tag
        pck.putSkip(16);
        int i;
        if (nssa) {
            i = rtrOspf4lsa.lsaNssaExt;
        } else {
            i = rtrOspf4lsa.lsaAsExt;
        }
        advertiseLsa(i, prf.network, pck);
    }

    private void createSumLsa(addrPrefix<addrIPv4> prf, int met) {
        if (met < 0) {
            met = 0;
        }
        if (met > 0xffffff) {
            met = 0xffffff;
        }
        packHolder pck = new packHolder(true, true);
        pck.putAddr(0, prf.mask); // netmask
        pck.msbPutD(4, met); // metric
        pck.putSkip(8);
        advertiseLsa(rtrOspf4lsa.lsaSumNet, prf.network, pck);
    }

    private void createTeLsa(int seq, rtrOspf4iface ifc, rtrOspf4neigh nei) {
        packHolder pck = new packHolder(true, true);
        if (ifc == null) {
            pck.putAddr(0, lower.traffEngID);
            pck.putSkip(addrIPv4.size);
            advertiseTe(rtrOspfTe.typRouter4, seq, pck);
            return;
        }
        rtrOspfTe.putGenTlv1(pck, nei == null);
        encTlv tlv = rtrOspfTe.getTlvHandler();
        tlv.valTyp = rtrOspfTe.typLnkId;
        tlv.valSiz = addrIPv4.size;
        if (nei == null) {
            ifc.drAddr.toBuffer(tlv.valDat, 0);
        } else {
            nei.rtrID.toBuffer(tlv.valDat, 0);
        }
        tlv.putThis(pck);
        tlv.putAddr(pck, rtrOspfTe.typLoc4adr, ifc.iface.addr.toIPv4());
        if (nei != null) {
            tlv.putAddr(pck, rtrOspfTe.typRem4adr, nei.peer);
        }
        rtrOspfTe.putGenTlv2(pck, ifc.teMetric, ifc.teBandwidth, ifc.teAffinity, ifc.teSrlg);
        advertiseTe(rtrOspfTe.typLink, seq, pck);
    }

    private void createPrfLsa(int seq, addrPrefix<addrIP> pref, int rou, int flg, byte[] subs) {
        addrPrefix<addrIPv4> prf = addrPrefix.ip2ip4(pref);
        packHolder pck = new packHolder(true, true);
        pck.putByte(0, rou); // route type
        pck.putByte(1, prf.maskLen); // prefix length
        pck.putByte(2, 0); // address family
        pck.putByte(3, flg); // flags
        pck.putAddr(4, prf.network); // address
        pck.putSkip(4 + prefixSize(prf));
        pck.putCopy(subs, 0, 0, subs.length); // subtlvs
        pck.putSkip(subs.length);
        pck.merge2beg();
        pck.msbPutW(0, 1); // type
        pck.msbPutW(2, pck.dataSize()); // length
        pck.putSkip(4);
        addrIPv4 i = new addrIPv4();
        byte[] buf = new byte[4];
        bits.msbPutD(buf, 0, seq);
        buf[0] = 7;
        i.fromBuf(buf, 0);
        advertiseLsa(rtrOspf4lsa.lsaOpArea, i, pck);
    }

    private void createLnkLsa(int seq, int typ, addrIPv4 lnk, addrIPv4 dat, byte[] subs) {
        packHolder pck = new packHolder(true, true);
        pck.msbPutD(0, typ << 24); // link type
        pck.putAddr(4, lnk); // link id
        pck.putAddr(8, dat); // link data
        pck.putSkip(12);
        pck.putCopy(subs, 0, 0, subs.length); // subtlvs
        pck.putSkip(subs.length);
        pck.merge2beg();
        pck.msbPutW(0, 1); // type
        pck.msbPutW(2, pck.dataSize()); // length
        pck.putSkip(4);
        addrIPv4 i = new addrIPv4();
        byte[] buf = new byte[4];
        bits.msbPutD(buf, 0, seq);
        buf[0] = 8;
        i.fromBuf(buf, 0);
        advertiseLsa(rtrOspf4lsa.lsaOpArea, i, pck);
    }

    private boolean checkPrfLsa(rtrOspf4lsa lsa) {
        if (lsa.lsaType != rtrOspf4lsa.lsaOpArea) {
            return true;
        }
        byte[] buf = new byte[4];
        lsa.lsaID.toBuffer(buf, 0);
        if (buf[0] != 7) {
            return true;
        }
        return false;
    }

    private tabRouteEntry<addrIP> parsePrfTlv(encTlv tlv) {
        if (tlv.valTyp != 1) {
            return null;
        }
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        addrIPv4 a = new addrIPv4();
        a.fromBuf(tlv.valDat, 4); // address
        addrPrefix<addrIPv4> prf = new addrPrefix<addrIPv4>(a, tlv.valDat[1]);
        ntry.prefix = addrPrefix.ip4toIP(prf);
        int i = 4 + prefixSize(prf);
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

    private void createNetLsas() {
        for (int i = 0; i < lower.ifaces.size(); i++) {
            rtrOspf4iface ifc = lower.ifaces.get(i);
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

    private void createExtLsas() {
        if (stub) {
            return;
        }
        tabRoute<addrIP> rs = new tabRoute<addrIP>("fl");
        if (defOrigin) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = addrPrefix.ip4toIP(addrPrefix.defaultRoute4());
            ntry.best.originType = 111;
            tabRoute.addUpdatedEntry(tabRoute.addType.better, rs, rtrBgpUtil.sfiUnicast, 0, ntry, true, roumapInto, roupolInto, prflstInto);
        }
        tabRoute.addUpdatedTable(tabRoute.addType.better, rtrBgpUtil.sfiUnicast, 0, rs, lower.routerRedistedU, true, roumapInto, roupolInto, prflstInto);
        for (int i = 0; i < rs.size(); i++) {
            tabRouteEntry<addrIP> ntry = rs.get(i);
            if (ntry == null) {
                continue;
            }
            createExtLsa(addrPrefix.ip2ip4(ntry.prefix), ntry.best.originType, ntry.best.metric, ntry.best.tag);
        }
    }

    private void createSumLsas() {
        if (!lower.amIabr()) {
            return;
        }
        if (stub || nssa) {
            createSumLsa(addrPrefix.defaultRoute4(), 0);
            return;
        }
        tabRoute<addrIP> rs = new tabRoute<addrIP>("rs");
        tabRoute<addrIP> oa = new tabRoute<addrIP>("rs");
        for (int i = 0; i < lower.ifaces.size(); i++) {
            rtrOspf4iface ifc = lower.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if ((suppressAddr || ifc.suppressAddr) && (!ifc.unsuppressAddr)) {
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
            createSumLsa(addrPrefix.ip2ip4(ntry.prefix), ntry.best.metric);
        }
    }

    private void createTeLsas() {
        if (!traffEng) {
            return;
        }
        createTeLsa(0, null, null);
        int seq = 1;
        for (int o = 0; o < lower.ifaces.size(); o++) {
            rtrOspf4iface ifc = lower.ifaces.get(o);
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
                rtrOspf4neigh nei = ifc.neighs.get(i);
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
            rtrOspfSr.putBase(pck, lower.segrouLab, lower.algos);
        }
        advertiseLsa(rtrOspf4lsa.lsaOpArea, rtrOspfRi.getOpaque(), pck);
    }

    private void createLnkLsas() {
        if (!segrouEna) {
            return;
        }
        int seq = 1;
        for (int i = 0; i < lower.ifaces.size(); i++) {
            rtrOspf4iface ifc = lower.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.areas.find(this) == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
                continue;
            }
            for (int o = 0; o < ifc.neighs.size(); o++) {
                rtrOspf4neigh nei = ifc.neighs.get(o);
                if (nei == null) {
                    continue;
                }
                if (nei.area.area != area) {
                    continue;
                }
                if (!nei.isFull()) {
                    continue;
                }
                if (nei.segrouLab == null) {
                    continue;
                }
                byte[] buf = rtrOspfSr.putAdj(nei.segrouLab.label);
                buf = bits.byteConcat(buf, rtrOspfSr.putRem(nei.peer));
                createLnkLsa(seq++, rtrOspf4lsa.lnkP2p, nei.rtrID, ifc.iface.addr.toIPv4(), buf);
            }
        }
    }

    private void createPrfLsas() {
        if (!segrouEna && !bierEna) {
            return;
        }
        int seq = 1;
        for (int i = 0; i < lower.ifaces.size(); i++) {
            rtrOspf4iface ifc = lower.ifaces.get(i);
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
                buf = bits.byteConcat(buf, rtrOspfBr.putPref(lower.bierLab, lower.bierLen, ifc.brIndex, ifc.brSub));
            }
            if (buf.length < 1) {
                continue;
            }
            int o;
            if (ifc.areas.find(this) != null) {
                o = 1; // intra
            } else {
                o = 3; // inter
            }
            int p = 0;
            if (ifc.srNode) {
                p |= 0x40;
            }
            createPrfLsa(seq++, ifc.iface.network, o, p, buf);
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
                buf = bits.byteConcat(buf, rtrOspfBr.putPref(lower.bierLab, lower.bierLen, ntry.best.bierIdx, ntry.best.bierSub));
            }
            if (buf.length < 1) {
                continue;
            }
            createPrfLsa(seq++, ntry.prefix, 3, 0, buf);
        }
    }

    private void generateLsas() {
        if (debugger.rtrOspf4evnt) {
            logger.debug("generate lsas in area " + area);
        }
        need2adv.clear();
        createRtrLsa();
        createNetLsas();
        createExtLsas();
        createSumLsas();
        createTeLsas();
        createRiLsa();
        createLnkLsas();
        createPrfLsas();
        advertiseLsas();
    }

    private void calculateSpf() {
        if (debugger.rtrOspf4evnt) {
            logger.debug("calculate spf on area " + area);
        }
        long tim = bits.getTime() - rtrOspf4lsa.lsaMaxAge + 1;
        spfCalc<addrIPv4> spf = new spfCalc<addrIPv4>(lastSpf);
        for (int i = 0; i < lsas.size(); i++) {
            rtrOspf4lsa ntry = lsas.get(i);
            if (ntry == null) {
                continue;
            }
            if (!ntry.doNotAge) {
                if (ntry.created < tim) {
                    continue;
                }
            }
            addrPrefix<addrIPv4> prf4;
            tabRouteEntry<addrIP> pref;
            addrIPv4 adr4;
            packHolder pck = ntry.getPayload();
            switch (ntry.lsaType) {
                case rtrOspf4lsa.lsaRouter:
                    int o = pck.msbGetW(2); // number of links
                    pck.getSkip(4);
                    for (int p = 0; p < o; p++) {
                        if (pck.dataSize() < 12) {
                            break;
                        }
                        addrIPv4 lnkId = new addrIPv4();
                        addrIPv4 lnkDt = new addrIPv4();
                        pck.getAddr(lnkId, 0); // link id
                        pck.getAddr(lnkDt, 4); // link data
                        int typ = pck.getByte(8); // type
                        int tos = pck.getByte(9); // number of tos
                        int met = pck.msbGetW(10); // metrick
                        pck.getSkip(12);
                        pck.getSkip(tos * 4); // additional tos values
                        switch (typ) {
                            case rtrOspf4lsa.lnkP2p:
                                spf.addConn(ntry.rtrID, lnkId, met, true, met >= 0xffff, "" + lnkDt);
                                break;
                            case rtrOspf4lsa.lnkTrns:
                                spf.addConn(ntry.rtrID, lnkId, met, false, met >= 0xffff, "" + lnkDt);
                                break;
                            case rtrOspf4lsa.lnkStub:
                                prf4 = new addrPrefix<addrIPv4>(lnkId, lnkDt.toNetmask());
                                pref = new tabRouteEntry<addrIP>();
                                pref.prefix = addrPrefix.ip4toIP(prf4);
                                pref.best.metric = met;
                                pref.best.originType = 109;
                                pref.best.distance = lower.distantInt;
                                pref.best.aggrAs = area;
                                spf.addPref(ntry.rtrID, pref, false);
                                break;
                            default:
                                break;
                        }
                    }
                    break;
                case rtrOspf4lsa.lsaNetwork:
                    adr4 = new addrIPv4();
                    pck.getAddr(adr4, 0); // mask
                    prf4 = new addrPrefix<addrIPv4>(ntry.lsaID, adr4.toNetmask());
                    pref = new tabRouteEntry<addrIP>();
                    pref.prefix = addrPrefix.ip4toIP(prf4);
                    pref.best.originType = 109;
                    pref.best.distance = lower.distantInt;
                    pref.best.aggrAs = area;
                    spf.addPref(ntry.rtrID, pref, false);
                    pck.getSkip(addrIPv4.size); // netmask
                    for (;;) {
                        addrIPv4 adr = new addrIPv4();
                        if (pck.dataSize() < addrIPv4.size) {
                            break;
                        }
                        pck.getAddr(adr, 0);
                        pck.getSkip(addrIPv4.size);
                        spf.addConn(ntry.lsaID, adr, 0, true, false, null);
                    }
                    break;
                case rtrOspf4lsa.lsaSumNet:
                    adr4 = new addrIPv4();
                    pck.getAddr(adr4, 0); // mask
                    prf4 = new addrPrefix<addrIPv4>(ntry.lsaID, adr4.toNetmask());
                    pref = new tabRouteEntry<addrIP>();
                    pref.prefix = addrPrefix.ip4toIP(prf4);
                    o = pck.msbGetD(4); // metric
                    pref.best.metric = o & 0xffffff;
                    pref.best.originType = 110;
                    pref.best.distance = lower.distantSum;
                    pref.best.aggrAs = area;
                    spf.addPref(ntry.rtrID, pref, false);
                    break;
                case rtrOspf4lsa.lsaSumAsBr:
                    spf.addConn(ntry.rtrID, ntry.lsaID, pck.msbGetD(4) & 0xffffff, true, false, null);
                    break;
                case rtrOspf4lsa.lsaAsExt:
                case rtrOspf4lsa.lsaNssaExt:
                    adr4 = new addrIPv4();
                    pck.getAddr(adr4, 0); // mask
                    prf4 = new addrPrefix<addrIPv4>(ntry.lsaID, adr4.toNetmask());
                    pref = new tabRouteEntry<addrIP>();
                    pref.prefix = addrPrefix.ip4toIP(prf4);
                    o = pck.msbGetD(4); // metric
                    pref.best.metric = o & 0xffffff;
                    pref.best.distance = lower.distantExt;
                    pref.best.aggrAs = area;
                    pref.best.tag = pck.msbGetD(12); // route tag
                    if ((o & 0x80000000) != 0) {
                        pref.best.originType = 112;
                        spf.addPref(ntry.rtrID, pref, true);
                    } else {
                        pref.best.originType = 111;
                        spf.addPref(ntry.rtrID, pref, false);
                    }
                    break;
                case rtrOspf4lsa.lsaOpArea:
                    if (ntry.lsaID.compareTo(rtrOspfRi.getOpaque()) == 0) {
                        encTlv tlv = rtrOspfTe.getTlvHandler();
                        for (;;) {
                            if (tlv.getBytes(pck)) {
                                break;
                            }
                            o = rtrOspfSr.getBase(tlv);
                            spf.addSegRouB(ntry.rtrID, o);
                            spf.addIdent(ntry.rtrID, rtrOspfRi.getHstnam(tlv));
                            spf.addAlgo(ntry.rtrID, rtrOspfSr.getAlgos(tlv));
                        }
                        continue;
                    }
                    if (checkPrfLsa(ntry)) {
                        continue;
                    }
                    encTlv tlv = rtrOspfTe.getTlvHandler();
                    for (;;) {
                        if (tlv.getBytes(pck)) {
                            break;
                        }
                        pref = parsePrfTlv(tlv);
                        if (pref == null) {
                            continue;
                        }
                        spf.addBierB(ntry.rtrID, pref.best.bierBeg);
                        spf.addSegRouI(ntry.rtrID, pref.prefix, pref.best.segrouIdx, pref.best.rouSrc & 16);
                        spf.addBierI(ntry.rtrID, pref.prefix, pref.best.bierIdx, pref.best.bierHdr, pref.best.bierSub);
                    }
                    break;
                default:
                    break;
            }
        }
        spf.doWork(lower.routerID);
        if (segrouEna && (lower.segrouLab != null)) {
            segrouUsd = new tabGen<tabIndex<addrIP>>();
        } else {
            segrouUsd = null;
        }
        for (int i = 0; i < lower.ifaces.size(); i++) {
            rtrOspf4iface ifc = lower.ifaces.get(i);
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
                lower.segrouLab[ifc.srIndex].setFwdCommon(tabLabelEntry.owner.ospf4srgb, lower.fwdCore);
            }
            if (ifc.needDR()) {
                if (ifc.drAddr.isEmpty()) {
                    continue;
                }
                addrIP adr = new addrIP();
                adr.fromIPv4addr(ifc.drAddr);
                spf.addNextHop(ifc.metric, ifc.drAddr, adr, ifc.iface, null, null);
            }
            for (int o = 0; o < ifc.neighs.size(); o++) {
                rtrOspf4neigh nei = ifc.neighs.get(o);
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
                adr.fromIPv4addr(nei.peer);
                spf.addNextHop(nei.getMetric(), nei.rtrID, adr, ifc.iface, null, null);
            }
        }
        tabRoute<addrIP> rs = spf.getRoutes(lower.fwdCore, tabLabelEntry.owner.ospf4srgb, lower.segrouLab, segrouUsd);
        routes.clear();
        tabRoute.addUpdatedTable(tabRoute.addType.ecmp, rtrBgpUtil.sfiUnicast, 0, routes, rs, true, roumapFrom, roupolFrom, prflstFrom);
        lower.routerDoAggregates(rtrBgpUtil.sfiUnicast, routes, routes, lower.fwdCore.commonLabel, null, 0);
        if (bierEna) {
            bierRes = spf.getBierI(lower.fwdCore, 0, 0);
        } else {
            bierRes = null;
        }
        if (debugger.rtrOspf4evnt) {
            logger.debug("unreachable:" + spf.listReachablility(false));
            logger.debug("reachable:" + spf.listReachablility(true));
        }
        lastSpf = spf;
        algos = new ArrayList<tabRoute<addrIP>>();
        for (int p = 0; p < lower.algos.size(); p++) {
            rtrAlgo alg = lower.algos.get(p);
            if (alg == null) {
                continue;
            }
            spf = lastSpf.copyBytes();
            spf.justFlexAlgo(alg.num);
            spf.doWork(lower.routerID);
            for (int i = 0; i < lower.ifaces.size(); i++) {
                rtrOspf4iface ifc = lower.ifaces.get(i);
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
                    addrIP adr = new addrIP();
                    adr.fromIPv4addr(ifc.drAddr);
                    spf.addNextHop(ifc.metric, ifc.drAddr, adr, ifc.iface, null, null);
                }
                for (int o = 0; o < ifc.neighs.size(); o++) {
                    rtrOspf4neigh nei = ifc.neighs.get(o);
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
                    adr.fromIPv4addr(nei.peer);
                    spf.addNextHop(nei.getMetric(), nei.rtrID, adr, ifc.iface, null, null);
                }
            }
            rs = spf.getRoutes(lower.fwdCore, null, null, null);
            if (debugger.rtrOspf4evnt) {
                logger.debug("algo" + alg.num + " unreachable:" + spf.listReachablility(false));
                logger.debug("algo" + alg.num + " reachable:" + spf.listReachablility(true));
            }
            tabRoute<addrIP> res = new tabRoute<addrIP>("rou");
            tabRoute.addUpdatedTable(tabRoute.addType.ecmp, rtrBgpUtil.sfiUnicast, 0, res, rs, true, roumapFrom, roupolFrom, prflstFrom);
            lower.routerDoAggregates(rtrBgpUtil.sfiUnicast, res, res, lower.fwdCore.commonLabel, null, 0);
            algos.add(res);
        }
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
        if (debugger.rtrOspf4evnt) {
            logger.debug("started area " + area);
        }
        todo.or(2);
        for (int rnd = 0;; rnd++) {
            try {
                notif.misleep(10000);
                int ver = todo.ver();
                int val = todo.get();
                todo.andIf(0xf, ver);
                if ((val & 1) == 0) {
                    break;
                }
                if ((rnd % 6) == 0) {
                    val |= 0x30;
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
        if (debugger.rtrOspf4evnt) {
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

    /**
     * get state information
     *
     * @param lst list to append
     * @param beg beginning to use
     */
    public void stateGet(List<String> lst, String beg) {
        beg += area + " ";
        packHolder pck = new packHolder(true, true);
        for (int i = 0; i < lsas.size(); i++) {
            rtrOspf4lsa ntry = lsas.get(i);
            if (ntry == null) {
                continue;
            }
            pck.clear();
            int o = ntry.writeData(pck, 0, true);
            pck.msbPutW(0, 1800); // remaining
            pck.putSkip(o);
            pck.merge2beg();
            lst.add(beg + encBase64.encodeBytes(pck.getCopy()));
        }
    }

    /**
     * set state information
     *
     * @param cmd string to append
     * @return true on error, false on success
     */
    public boolean stateSet(cmds cmd) {
        byte[] buf = encBase64.decodeBytes(cmd.getRemaining());
        if (buf == null) {
            return true;
        }
        packHolder pck = new packHolder(true, true);
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        pck.merge2beg();
        rtrOspf4lsa ntry = new rtrOspf4lsa();
        if (ntry.readData(pck, 0, true) < 0) {
            return true;
        }
        lsas.put(ntry);
        return false;
    }

}
