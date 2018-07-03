package rtr;

import addr.addrIP;
import addr.addrIPv4;
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
 * ospfv2 area
 *
 * @author matecsaba
 */
public class rtrOspf4area implements Comparator<rtrOspf4area>, Runnable {

    /**
     * area number
     */
    public final int area;

    /**
     * lsas in this area
     */
    protected final tabGen<rtrOspf4lsa> lsas;

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
    protected shrtPthFrst<addrIPv4> lastSpf;

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
        lower = parent;
        area = num;
        lsas = new tabGen<rtrOspf4lsa>();
        need2adv = new tabGen<rtrOspf4lsa>();
        routes = new tabRoute<addrIP>("computed");
        notif = new notifier();
    }

    public String toString() {
        return "ospf area " + area;
    }

    public int compare(rtrOspf4area o1, rtrOspf4area o2) {
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
     */
    protected synchronized void generateLsa(rtrOspf4lsa lsa, boolean purge) {
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
        for (int i = 0; i < lsas.size(); i++) {
            rtrOspf4lsa ntry = lsas.get(i);
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
            rtrOspf4lsa ntry = need2adv.get(i);
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
        if (debugger.rtrOspf4evnt) {
            logger.debug("purge lsas in area " + area);
        }
        long tim1 = bits.getTime() - rtrOspf4lsa.lsaMaxAge - (rtrOspf4lsa.lsaMaxAge / 100);
        long tim2 = tim1 + (rtrOspf4lsa.lsaMaxAge / 2);
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

    private rtrOspf4areaLink getLink2rtrLsa(packHolder pck) {
        if (pck.dataSize() < 12) {
            return null;
        }
        rtrOspf4areaLink l = new rtrOspf4areaLink();
        pck.getAddr(l.linkID, 0); // link id
        pck.getAddr(l.linkDat, 4); // link data
        l.type = pck.getByte(8); // type
        int tos = pck.getByte(9); // number of tos
        l.metric = pck.msbGetW(10); // metrick
        pck.getSkip(12);
        pck.getSkip(tos * 4); // additional tos values
        return l;
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
            if (ifc.area.area != area) {
                continue;
            }
            if (ifc.needDR()) {
                if (ifc.drAddr.isEmpty()) {
                    continue;
                }
                putLink2rtrLsa(pck, rtrOspf4lsa.lnkTrns, ifc.drAddr, ifc.iface.addr.toIPv4(), ifc.metric);
                continue;
            }
            for (int o = 0; o < ifc.neighs.size(); o++) {
                rtrOspf4neigh nei = ifc.neighs.get(o);
                if (nei == null) {
                    continue;
                }
                if (!nei.isFull()) {
                    continue;
                }
                putLink2rtrLsa(pck, rtrOspf4lsa.lnkP2p, nei.rtrID, ifc.iface.addr.toIPv4(), ifc.metric);
            }
            if (ifc.suppressAddr) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
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
        typLenVal tlv = rtrOspfTe.getTlvHandler();
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

    private tabRouteEntry<addrIP> parsePrfTlv(typLenVal tlv) {
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
            if (ifc.area.area != area) {
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
            ntry.origin = 111;
            tabRoute.addUpdatedEntry(tabRoute.addType.better, rs, rtrBgpUtil.safiUnicast, ntry, roumapInto, roupolInto, prflstInto);
        }
        tabRoute.addUpdatedTable(tabRoute.addType.better, rtrBgpUtil.safiUnicast, rs, lower.routerRedistedU, roumapInto, roupolInto, prflstInto);
        for (int i = 0; i < rs.size(); i++) {
            tabRouteEntry<addrIP> ntry = rs.get(i);
            if (ntry == null) {
                continue;
            }
            createExtLsa(addrPrefix.ip2ip4(ntry.prefix), ntry.origin, ntry.metric, ntry.tag);
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
        for (int i = 0; i < lower.ifaces.size(); i++) {
            rtrOspf4iface ifc = lower.ifaces.get(i);
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
            createSumLsa(addrPrefix.ip2ip4(ntry.prefix), ntry.metric);
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
            if (ifc.area.area != area) {
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
            if (ifc.area.area != area) {
                continue;
            }
            for (int o = 0; o < ifc.neighs.size(); o++) {
                rtrOspf4neigh nei = ifc.neighs.get(o);
                if (nei == null) {
                    continue;
                }
                if (!nei.isFull()) {
                    continue;
                }
                if (nei.segrouLab == null) {
                    continue;
                }
                byte[] buf = rtrOspfSr.putAdj(nei.segrouLab.getValue());
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
            if (ifc.area.area == area) {
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
        shrtPthFrst<addrIPv4> spf = new shrtPthFrst<addrIPv4>(lastSpf);
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
            packHolder pck = ntry.getPayload();
            switch (ntry.lsaType) {
                case rtrOspf4lsa.lsaRouter:
                    int o = pck.msbGetW(2); // number of links
                    pck.getSkip(4);
                    for (int p = 0; p < o; p++) {
                        rtrOspf4areaLink l = getLink2rtrLsa(pck);
                        if (l == null) {
                            continue;
                        }
                        switch (l.type) {
                            case rtrOspf4lsa.lnkP2p:
                                spf.addConn(ntry.rtrID, l.linkID, l.metric, true);
                                break;
                            case rtrOspf4lsa.lnkTrns:
                                spf.addConn(ntry.rtrID, l.linkID, l.metric, false);
                                break;
                            default:
                                break;
                        }
                    }
                    break;
                case rtrOspf4lsa.lsaNetwork:
                    pck.getSkip(addrIPv4.size); // netmask
                    for (;;) {
                        addrIPv4 adr = new addrIPv4();
                        if (pck.dataSize() < addrIPv4.size) {
                            break;
                        }
                        pck.getAddr(adr, 0);
                        pck.getSkip(addrIPv4.size);
                        spf.addConn(ntry.lsaID, adr, 0, true);
                    }
                    break;
                case rtrOspf4lsa.lsaSumAsBr:
                    spf.addConn(ntry.rtrID, ntry.lsaID, pck.msbGetD(4) & 0xffffff, true);
                    break;
                case rtrOspf4lsa.lsaOpArea:
                    if (ntry.lsaID.compare(ntry.lsaID, rtrOspfRi.getOpaque()) == 0) {
                        typLenVal tlv = rtrOspfTe.getTlvHandler();
                        for (;;) {
                            if (tlv.getBytes(pck)) {
                                break;
                            }
                            o = rtrOspfSr.getBase(tlv);
                            if (o < 1) {
                                continue;
                            }
                            spf.addSegRouB(ntry.rtrID, o);
                        }
                        continue;
                    }
                    if (!bierEna) {
                        continue;
                    }
                    if (checkPrfLsa(ntry)) {
                        continue;
                    }
                    typLenVal tlv = rtrOspfTe.getTlvHandler();
                    for (;;) {
                        if (tlv.getBytes(pck)) {
                            break;
                        }
                        tabRouteEntry<addrIP> pref = parsePrfTlv(tlv);
                        if (pref == null) {
                            continue;
                        }
                        spf.addBierB(ntry.rtrID, pref.bierBeg);
                    }
                    break;
                default:
                    break;
            }
        }
        spf.doCalc(lower.routerID, null);
        if (segrouEna && (lower.segrouLab != null)) {
            segrouUsd = new boolean[lower.segrouMax];
        } else {
            segrouUsd = null;
        }
        for (int i = 0; i < lower.ifaces.size(); i++) {
            rtrOspf4iface ifc = lower.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.area.area != area) {
                continue;
            }
            if ((segrouUsd != null) && (ifc.srIndex > 0)) {
                segrouUsd[ifc.srIndex] = true;
                lower.segrouLab[ifc.srIndex].setFwdCommon(8, lower.fwdCore);
            }
            if (ifc.needDR()) {
                if (ifc.drAddr.isEmpty()) {
                    continue;
                }
                addrIP adr = new addrIP();
                adr.fromIPv4addr(ifc.drAddr);
                spf.addNextHop(ifc.metric, ifc.drAddr, adr, ifc.iface);
            }
            for (int o = 0; o < ifc.neighs.size(); o++) {
                rtrOspf4neigh nei = ifc.neighs.get(o);
                if (nei == null) {
                    continue;
                }
                if (!nei.seenMyself) {
                    continue;
                }
                addrIP adr = new addrIP();
                adr.fromIPv4addr(nei.peer);
                spf.addNextHop(ifc.metric, nei.rtrID, adr, ifc.iface);
            }
        }
        tabRoute<addrIP> rs = new tabRoute<addrIP>("rs");
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
            addrIP hop = spf.getNextHop(ntry.rtrID);
            if (hop == null) {
                continue;
            }
            int hops = spf.getHops(ntry.rtrID);
            ipFwdIface iface = (ipFwdIface) spf.getNextIfc(ntry.rtrID);
            int met = spf.getMetric(ntry.rtrID);
            int srb = spf.getSegRouB(ntry.rtrID, false);
            int sro = spf.getSegRouB(ntry.rtrID, true);
            int brb = spf.getBierB(ntry.rtrID, false);
            int bro = spf.getBierB(ntry.rtrID, true);
            addrPrefix<addrIPv4> prf4;
            tabRouteEntry<addrIP> pref;
            addrIPv4 adr4;
            packHolder pck = ntry.getPayload();
            switch (ntry.lsaType) {
                case rtrOspf4lsa.lsaRouter:
                    int o = pck.msbGetW(2); // number of links
                    pck.getSkip(4);
                    for (int p = 0; p < o; p++) {
                        rtrOspf4areaLink l = getLink2rtrLsa(pck);
                        if (l == null) {
                            continue;
                        }
                        if (l.type != rtrOspf4lsa.lnkStub) {
                            continue;
                        }
                        prf4 = new addrPrefix<addrIPv4>(l.linkID, l.linkDat.toNetmask());
                        pref = new tabRouteEntry<addrIP>();
                        pref.prefix = addrPrefix.ip4toIP(prf4);
                        pref.nextHop = hop.copyBytes();
                        pref.metric = met;
                        pref.origin = 109;
                        pref.distance = lower.distantInt;
                        pref.srcRtr = ntry.rtrID.copyBytes();
                        pref.iface = iface;
                        pref.rouSrc = area;
                        rs.add(tabRoute.addType.better, pref, false, true);
                    }
                    break;
                case rtrOspf4lsa.lsaNetwork:
                    adr4 = new addrIPv4();
                    pck.getAddr(adr4, 0); // mask
                    prf4 = new addrPrefix<addrIPv4>(ntry.lsaID, adr4.toNetmask());
                    pref = new tabRouteEntry<addrIP>();
                    pref.prefix = addrPrefix.ip4toIP(prf4);
                    pref.nextHop = hop.copyBytes();
                    pref.metric = met;
                    pref.origin = 109;
                    pref.distance = lower.distantInt;
                    pref.srcRtr = ntry.rtrID.copyBytes();
                    pref.iface = iface;
                    pref.rouSrc = area;
                    rs.add(tabRoute.addType.better, pref, false, true);
                    break;
                case rtrOspf4lsa.lsaSumNet:
                    adr4 = new addrIPv4();
                    pck.getAddr(adr4, 0); // mask
                    prf4 = new addrPrefix<addrIPv4>(ntry.lsaID, adr4.toNetmask());
                    pref = new tabRouteEntry<addrIP>();
                    pref.prefix = addrPrefix.ip4toIP(prf4);
                    pref.nextHop = hop.copyBytes();
                    o = pck.msbGetD(4); // metric
                    pref.metric = (o & 0xffffff) + met;
                    pref.origin = 110;
                    pref.distance = lower.distantSum;
                    pref.srcRtr = ntry.rtrID.copyBytes();
                    pref.iface = iface;
                    pref.rouSrc = area;
                    rs.add(tabRoute.addType.better, pref, false, true);
                    break;
                case rtrOspf4lsa.lsaAsExt:
                case rtrOspf4lsa.lsaNssaExt:
                    adr4 = new addrIPv4();
                    pck.getAddr(adr4, 0); // mask
                    prf4 = new addrPrefix<addrIPv4>(ntry.lsaID, adr4.toNetmask());
                    pref = new tabRouteEntry<addrIP>();
                    pref.prefix = addrPrefix.ip4toIP(prf4);
                    pref.nextHop = hop.copyBytes();
                    o = pck.msbGetD(4); // metric
                    pref.metric = o & 0xffffff;
                    if ((o & 0x80000000) != 0) {
                        pref.origin = 112;
                    } else {
                        pref.metric += met;
                        pref.origin = 111;
                    }
                    pref.distance = lower.distantExt;
                    pref.srcRtr = ntry.rtrID.copyBytes();
                    pref.iface = iface;
                    pref.rouSrc = area;
                    pref.tag = pck.msbGetD(12); // route tag
                    rs.add(tabRoute.addType.better, pref, false, true);
                    break;
                case rtrOspf4lsa.lsaOpArea:
                    if ((segrouUsd == null) && !bierEna) {
                        continue;
                    }
                    if (checkPrfLsa(ntry)) {
                        continue;
                    }
                    typLenVal tlv = rtrOspfTe.getTlvHandler();
                    for (;;) {
                        if (tlv.getBytes(pck)) {
                            break;
                        }
                        pref = parsePrfTlv(tlv);
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
                        spf.addSegRouI(ntry.rtrID, pref.segrouIdx);
                        spf.addBierI(ntry.rtrID, pref.bierIdx, old.origin == 109);
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
                        lower.segrouLab[pref.segrouIdx].setFwdMpls(8, lower.fwdCore, iface, hop, lab);
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
        if (debugger.rtrOspf4evnt) {
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
        if (debugger.rtrOspf4evnt) {
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

}

class rtrOspf4areaLink {

    protected addrIPv4 linkID = new addrIPv4();

    protected addrIPv4 linkDat = new addrIPv4();

    protected int type;

    protected int metric;

}
