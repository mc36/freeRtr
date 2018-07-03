package rtr;

import java.util.Comparator;
import pack.packHolder;
import tab.tabGen;
import tab.tabListing;
import tab.tabPrfxlstN;
import tab.tabRoute;
import tab.tabRouteEntry;
import tab.tabRtrmapN;
import util.bits;
import util.debugger;
import util.logger;
import util.notifier;
import util.shrtPthFrst;
import util.typLenVal;
import addr.addrIP;
import addr.addrIsis;
import cfg.cfgAll;
import ip.ipCor4;
import ip.ipFwdIface;
import ip.ipMpls;
import java.util.List;
import tab.tabLabel;
import tab.tabLabelBier;
import tab.tabRtrplcN;
import util.state;
import util.syncInt;

/**
 * isis level
 *
 * @author matecsaba
 */
public class rtrIsisLevel implements Runnable {

    /**
     * level
     */
    public final int level;

    /**
     * list of lsps
     */
    protected final tabGen<rtrIsisLsp> lsps;

    /**
     * computed routes
     */
    protected final tabRoute<addrIP> routes;

    /**
     * advertise default route
     */
    public boolean defOrigin;

    /**
     * overloaded
     */
    public boolean overloaded;

    /**
     * set attached
     */
    public boolean attachedSet;

    /**
     * clear attached
     */
    public boolean attachedClr;

    /**
     * allow attached
     */
    public boolean attachedAlw;

    /**
     * clear attached
     */
    public boolean interLevels;

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
     * max lsp size
     */
    public int maxLspSize;

    /**
     * lsp refresh interval
     */
    public int lspRefresh;

    /**
     * lsp lifetime
     */
    public int lspLifetime;

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
    protected shrtPthFrst<rtrIsisLevelSpf> lastSpf;

    /**
     * segment routing usage
     */
    protected boolean[] segrouUsd;

    /**
     * bier results
     */
    protected tabLabelBier bierRes;

    private final rtrIsis lower;

    private syncInt todo = new syncInt(0); // 1=need2run, 2=running, 0xffff0=works

    private tabGen<rtrIsisLsp> need2adv;

    private notifier notif;

    /**
     * create one level
     *
     * @param parent the isis process
     * @param lev level number
     */
    public rtrIsisLevel(rtrIsis parent, int lev) {
        lower = parent;
        level = lev;
        lsps = new tabGen<rtrIsisLsp>();
        routes = new tabRoute<addrIP>("computed");
        need2adv = new tabGen<rtrIsisLsp>();
        notif = new notifier();
        attachedClr = level == 2;
        attachedAlw = level == 1;
        interLevels = true;
        maxLspSize = 1024;
        lspLifetime = 1200000;
        lspRefresh = lspLifetime / 3;
        hostname = true;
    }

    public String toString() {
        return "isis level" + level;
    }

    /**
     * get flags value
     *
     * @return flags
     */
    protected int getFlagsVal() {
        int i;
        if (level == 1) {
            i = 1;
        } else {
            i = 3;
        }
        if (!attachedClr) {
            if (lower.amIattach()) {
                i |= rtrIsisLsp.flgAttach;
            }
            if (attachedSet) {
                i |= rtrIsisLsp.flgAttach;
            }
        }
        if (overloaded) {
            i |= rtrIsisLsp.flgOver;
        }
        return i;
    }

    /**
     * advertise one lsp
     *
     * @param lsp lsp to advertise
     * @param purge set true to purge it out
     */
    protected synchronized void generateLsp(rtrIsisLsp lsp, boolean purge) {
        if (purge) {
            if (lsp.getTimeRemain(true) < 1) {
                return;
            }
            lsp.setTimeRemain(0);
        } else {
            lsp.setTimeRemain(lspLifetime / 1000);
        }
        rtrIsisLsp old = lsps.find(lsp);
        if (old == null) {
            lsp.sequence = 1;
        } else {
            lsp.sequence = old.sequence + 1;
        }
        if (debugger.rtrIsisEvnt) {
            logger.debug("generate lsp " + lsp);
        }
        lsp.generateCheckSum();
        lsps.put(lsp);
    }

    /**
     * advertise needed lsps
     */
    protected void advertiseLsps() {
        if (debugger.rtrIsisEvnt) {
            logger.debug("advertise lsps in level" + level);
        }
        for (int i = 0; i < lsps.size(); i++) {
            rtrIsisLsp ntry = lsps.get(i);
            if (ntry == null) {
                continue;
            }
            if (lower.routerID.compare(ntry.srcID, lower.routerID) != 0) {
                continue;
            }
            if (need2adv.find(ntry) != null) {
                continue;
            }
            if (ntry.getTimeRemain(true) < 1) {
                continue;
            }
            generateLsp(ntry, true);
        }
        for (int i = 0; i < need2adv.size(); i++) {
            rtrIsisLsp ntry = need2adv.get(i);
            if (ntry == null) {
                continue;
            }
            if (!ntry.contentDiffers(lsps.find(ntry))) {
                continue;
            }
            generateLsp(ntry, false);
        }
    }

    /**
     * purge out aged lsps
     */
    protected void purgeLsps() {
        if (debugger.rtrIsisEvnt) {
            logger.debug("purge lsps in level" + level);
        }
        for (int i = lsps.size(); i >= 0; i--) {
            rtrIsisLsp ntry = lsps.get(i);
            if (ntry == null) {
                continue;
            }
            int o = ntry.getTimeRemain(true);
            if (o < (-lspRefresh / 1000)) {
                if (debugger.rtrIsisEvnt) {
                    logger.debug("purge " + ntry);
                }
                lsps.del(ntry);
                continue;
            }
            if (lower.routerID.compare(ntry.srcID, lower.routerID) != 0) {
                continue;
            }
            if (o > (lspRefresh / 1000)) {
                continue;
            }
            if (need2adv.find(ntry) == null) {
                continue;
            }
            generateLsp(ntry, false);
            continue;
        }
    }

    /**
     * advertise one lsp
     *
     * @param pck payload
     */
    protected void advertiseLsp(packHolder pck) {
        pck.merge2beg();
        if (pck.dataSize() < 1) {
            return;
        }
        rtrIsisLsp lsp = new rtrIsisLsp();
        lsp.srcID = lower.routerID.copyBytes();
        lsp.nodID = pck.RTPsrc;
        lsp.lspNum = pck.RTPtyp;
        lsp.bufDat = pck.getCopy();
        lsp.flags = getFlagsVal();
        need2adv.put(lsp);
    }

    private void advertiseTlv(packHolder pck, typLenVal tlv) {
        if ((pck.headSize() + tlv.valSiz) > maxLspSize) {
            advertiseLsp(pck);
            pck.setDataSize(0);
            pck.RTPtyp++;
        }
        tlv.putThis(pck);
    }

    private void advertiseTlv(packHolder pck, int typ, byte[] buf) {
        typLenVal tlv = rtrIsis.getTlv();
        tlv.valDat = buf;
        tlv.valSiz = buf.length;
        tlv.valTyp = typ;
        advertiseTlv(pck, tlv);
    }

    private void createNeighs(packHolder pck, rtrIsisIface ifc, boolean subs) {
        for (int o = 0; o < ifc.neighs.size(); o++) {
            rtrIsisNeigh nei = ifc.neighs.get(o);
            if (nei == null) {
                continue;
            }
            if (nei.level.level != level) {
                continue;
            }
            if (pck.RTPsrc == 0) {
                byte[] buf = new byte[0];
                if (subs) {
                    buf = rtrIsisTe.putSubs(lower, ifc, nei);
                }
                if (nei.segrouLab != null) {
                    buf = bits.byteConcat(buf, rtrIsisSr.putAdj(lower.fwdCore.ipVersion == ipCor4.protocolVersion, nei.segrouLab.getValue()));
                }
                advertiseTlv(pck, lower.putISneigh(nei.rtrID, 0, ifc.metric, buf));
                if (subs) {
                    advertiseTlv(pck, rtrIsisTe.putSrlg(lower, nei.rtrID, 0, ifc.iface.addr, nei.ifcAddr, ifc.teSrlg));
                }
            } else {
                advertiseTlv(pck, lower.putISneigh(nei.rtrID, 0, 0, new byte[0]));
            }
        }
    }

    private void createIface(packHolder pck, rtrIsisIface ifc) {
        boolean subs = traffEng && !ifc.teSuppress;
        if (ifc.netPnt2pnt) {
            createNeighs(pck, ifc, subs);
            return;
        }
        byte[] buf = new byte[0];
        if (subs) {
            buf = rtrIsisTe.putSubs(lower, ifc, null);
        }
        advertiseTlv(pck, lower.putISneigh(ifc.getDisAddr(level), ifc.getDisCirc(level), ifc.metric, buf));
        if (subs) {
            advertiseTlv(pck, rtrIsisTe.putSrlg(lower, ifc.getDisAddr(level), ifc.getDisCirc(level), ifc.iface.addr, null, ifc.teSrlg));
        }
        if (!ifc.amIdis(level)) {
            return;
        }
        packHolder p = new packHolder(true, true);
        p.RTPsrc = ifc.circuitID;
        advertiseTlv(p, lower.putISneigh(lower.routerID, 0, 0, new byte[0]));
        createNeighs(p, ifc, false);
        advertiseLsp(p);
    }

    private void createAddrs(packHolder pck) {
        tabRoute<addrIP> rs = new tabRoute<addrIP>("rs");
        if (defOrigin) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = lower.getDefaultRoute();
            ntry.origin = 1;
            rs.add(tabRoute.addType.better, ntry, false, false);
        }
        for (int i = 0; i < lower.ifaces.size(); i++) {
            rtrIsisIface ifc = lower.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
                continue;
            }
            if (!ifc.suppressInt) {
                advertiseTlv(pck, lower.putAddrIface(ifc.iface.addr));
            }
            if (ifc.suppressAddr) {
                continue;
            }
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = ifc.iface.network.copyBytes();
            ntry.distance = tabRouteEntry.distanIfc;
            ntry.metric = ifc.metric;
            ntry.segrouIdx = ifc.srIndex;
            if (ifc.srNode) {
                ntry.rouSrc |= 8;
            }
            ntry.bierIdx = ifc.brIndex;
            rs.add(tabRoute.addType.better, ntry, false, false);
        }
        for (int i = 0; i < lower.routerRedistedU.size(); i++) {
            tabRouteEntry<addrIP> ntry = lower.routerRedistedU.get(i);
            if (ntry == null) {
                continue;
            }
            ntry = ntry.copyBytes();
            ntry.distance = tabRouteEntry.distanIfc + 1;
            ntry.rouSrc = 1;
            ntry.segrouIdx = 0;
            ntry.bierIdx = 0;
            rs.add(tabRoute.addType.better, ntry, false, false);
        }
        if (interLevels) {
            tabRoute<addrIP> other = lower.getLevel(3 - level).routes;
            for (int i = 0; i < other.size(); i++) {
                tabRouteEntry<addrIP> ntry = other.get(i);
                if (ntry == null) {
                    continue;
                }
                ntry = ntry.copyBytes();
                if ((ntry.rouSrc & 4) != 0) {
                    continue;
                }
                if (level == 2) {
                    if ((ntry.rouSrc & 2) != 0) {
                        continue;
                    }
                } else {
                    ntry.rouSrc |= 2;
                }
                ntry.rouSrc &= -1 - 8;
                rs.add(tabRoute.addType.better, ntry, false, false);
            }
        }
        tabRoute<addrIP> fl = new tabRoute<addrIP>("fl");
        tabRoute.addUpdatedTable(tabRoute.addType.better, rtrBgpUtil.safiUnicast, fl, rs, roumapInto, roupolInto, prflstInto);
        for (int i = 0; i < fl.size(); i++) {
            tabRouteEntry<addrIP> ntry = fl.get(i);
            if (ntry == null) {
                continue;
            }
            byte[] subs = new byte[0];
            if (segrouEna && (ntry.segrouIdx > 0)) {
                subs = rtrIsisSr.putPref(ntry.segrouIdx, (ntry.rouSrc & 3) != 0, (ntry.rouSrc & 8) != 0);
            }
            if (bierEna && (ntry.bierIdx > 0)) {
                subs = bits.byteConcat(subs, rtrIsisBr.putPref(lower, ntry.bierIdx));
            }
            advertiseTlv(pck, lower.putAddrReach(ntry.prefix, ntry.rouSrc, ntry.metric, subs));
        }
    }

    private void generateLsps() {
        if (debugger.rtrIsisEvnt) {
            logger.debug("generate lsps in level" + level);
        }
        need2adv.clear();
        packHolder pck = new packHolder(true, true);
        advertiseTlv(pck, rtrIsisLsp.tlvProtSupp, lower.getNLPIDlst());
        if (lower.multiTopo) {
            int i = getFlagsVal();
            byte[] buf = lower.getMTopoLst();
            if ((i & rtrIsisLsp.flgOver) != 0) {
                buf[0] |= 0x80;
            }
            if ((i & rtrIsisLsp.flgAttach) != 0) {
                buf[0] |= 0x40;
            }
            advertiseTlv(pck, rtrIsisLsp.tlvMultiTopo, buf);
        }
        advertiseTlv(pck, rtrIsisLsp.tlvAreaAddr, lower.areaID.getAddrDat(true));
        if (traffEng) {
            advertiseTlv(pck, rtrIsisTe.putAddr(lower));
        }
        if (segrouEna && (lower.segrouLab != null)) {
            advertiseTlv(pck, rtrIsisSr.putBase(lower));
        }
        if (hostname) {
            byte[] buf = cfgAll.hostName.getBytes();
            advertiseTlv(pck, rtrIsisLsp.tlvHostName, buf);
        }
        for (int i = 0; i < lower.ifaces.size(); i++) {
            rtrIsisIface ifc = lower.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            createIface(pck, ifc);
        }
        createAddrs(pck);
        advertiseLsp(pck);
        advertiseLsps();
    }

    private void calculateSpf() {
        if (debugger.rtrIsisEvnt) {
            logger.debug("calculate spf on level" + level);
        }
        if (segrouEna && (lower.segrouLab != null)) {
            segrouUsd = new boolean[lower.segrouMax];
        } else {
            segrouUsd = null;
        }
        shrtPthFrst<rtrIsisLevelSpf> spf = new shrtPthFrst<rtrIsisLevelSpf>(lastSpf);
        for (int i = 0; i < lsps.size(); i++) {
            rtrIsisLsp lsp = lsps.get(i);
            if (lsp == null) {
                continue;
            }
            if (lsp.getTimeRemain(true) < 1) {
                continue;
            }
            if ((lsp.flags & rtrIsisLsp.flgOver) != 0) {
                continue;
            }
            packHolder pck = lsp.getPayload();
            rtrIsisLevelSpf src = new rtrIsisLevelSpf(lsp.srcID, lsp.nodID);
            typLenVal tlv = rtrIsis.getTlv();
            for (;;) {
                if (tlv.getBytes(pck)) {
                    break;
                }
                int o = rtrIsisSr.getBase(tlv);
                if ((o > 0) && (segrouUsd != null)) {
                    spf.addSegRouB(src, o);
                    continue;
                }
                addrIsis adr = lower.getISalias(tlv);
                if (adr != null) {
                    rtrIsisLevelSpf trg = new rtrIsisLevelSpf(adr, 0);
                    spf.addConn(src, trg, 0, false);
                    spf.addConn(trg, src, 0, false);
                    continue;
                }
                tabGen<rtrIsisLsp> nel = lower.getISneigh(tlv);
                if (nel != null) {
                    for (o = 0; o < nel.size(); o++) {
                        rtrIsisLsp nei = nel.get(o);
                        if (nei == null) {
                            continue;
                        }
                        spf.addConn(src, new rtrIsisLevelSpf(nei.srcID, nei.nodID), nei.lspNum, nei.nodID == 0);
                    }
                    continue;
                }
                if (!bierEna) {
                    continue;
                }
                tabGen<tabRouteEntry<addrIP>> rou = lower.getAddrReach(tlv);
                if (rou == null) {
                    continue;
                }
                for (o = 0; o < rou.size(); o++) {
                    tabRouteEntry<addrIP> pref = rou.get(o);
                    if (pref == null) {
                        continue;
                    }
                    if (pref.bierBeg == 0) {
                        continue;
                    }
                    spf.addBierB(src, pref.bierBeg);
                }
            }
        }
        spf.doCalc(new rtrIsisLevelSpf(lower.routerID, 0), null);
        for (int i = 0; i < lower.ifaces.size(); i++) {
            rtrIsisIface ifc = lower.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if ((segrouUsd != null) && (ifc.srIndex > 0)) {
                segrouUsd[ifc.srIndex] = true;
                lower.segrouLab[ifc.srIndex].setFwdCommon(7, lower.fwdCore);
            }
            for (int o = 0; o < ifc.neighs.size(); o++) {
                rtrIsisNeigh nei = ifc.neighs.get(o);
                if (nei == null) {
                    continue;
                }
                if (nei.peerAdjState != rtrIsisNeigh.statUp) {
                    continue;
                }
                if (nei.level.level != level) {
                    continue;
                }
                spf.addNextHop(ifc.metric, new rtrIsisLevelSpf(nei.rtrID, 0), nei.ifcAddr.copyBytes(), ifc.iface);
            }
        }
        boolean needAttach = (!lower.haveNeighbor(2)) && attachedAlw;
        tabRoute<addrIP> rs = new tabRoute<addrIP>("rs");
        for (int i = 0; i < lsps.size(); i++) {
            rtrIsisLsp lsp = lsps.get(i);
            if (lsp == null) {
                continue;
            }
            if (lsp.getTimeRemain(true) < 1) {
                continue;
            }
            rtrIsisLevelSpf src = new rtrIsisLevelSpf(lsp.srcID, lsp.nodID);
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
            packHolder pck = lsp.getPayload();
            typLenVal tlv = rtrIsis.getTlv();
            if ((lsp.flags & rtrIsisLsp.flgAttach) != 0) {
                tabRouteEntry<addrIP> pref = new tabRouteEntry<addrIP>();
                pref.prefix = lower.getDefaultRoute();
                pref.metric = met;
                pref.distance = lower.distantInt;
                pref.rouSrc = 6;
                pref.nextHop = hop.copyBytes();
                pref.srcRtr = lsp.srcID.copyBytes();
                pref.iface = iface;
                if (needAttach && ((lsp.flags & rtrIsisLsp.flgOver) == 0)) {
                    rs.add(tabRoute.addType.better, pref, false, true);
                }
            }
            for (;;) {
                if (tlv.getBytes(pck)) {
                    break;
                }
                tabGen<tabRouteEntry<addrIP>> rou = lower.getAddrReach(tlv);
                if (rou == null) {
                    continue;
                }
                for (int o = 0; o < rou.size(); o++) {
                    tabRouteEntry<addrIP> pref = rou.get(o);
                    if (pref == null) {
                        continue;
                    }
                    pref.metric += met;
                    if ((pref.rouSrc & 1) == 0) {
                        pref.distance = lower.distantInt;
                    } else {
                        pref.distance = lower.distantExt;
                    }
                    pref.nextHop = hop.copyBytes();
                    pref.srcRtr = lsp.srcID.copyBytes();
                    pref.iface = iface;
                    spf.addSegRouI(src, pref.segrouIdx);
                    spf.addBierI(src, pref.bierIdx, (pref.rouSrc & 3) == 0);
                    pref.segrouBeg = srb;
                    pref.segrouOld = sro;
                    pref.bierBeg = brb;
                    pref.bierOld = bro;
                    if ((segrouUsd != null) && (pref.segrouIdx > 0) && (pref.segrouIdx < lower.segrouMax) && (srb > 0)) {
                        List<Integer> lab = tabLabel.int2labels(srb + pref.segrouIdx);
                        if (((pref.rouSrc & 16) != 0) && (hops <= 1)) {
                            lab = tabLabel.int2labels(ipMpls.labelImp);
                        }
                        lower.segrouLab[pref.segrouIdx].setFwdMpls(7, lower.fwdCore, iface, hop, lab);
                        segrouUsd[pref.segrouIdx] = true;
                        pref.labelRem = lab;
                    }
                    rs.add(tabRoute.addType.better, pref, false, true);
                }
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
        if (debugger.rtrIsisEvnt) {
            logger.debug("unreachable:" + spf.listUnreachables());
            logger.debug("reachable:" + spf.listReachables());
        }
        lastSpf = spf;
        lower.routerCreateComputed();
    }

    /**
     * schedule work
     *
     * @param i what to do: 1=genLsp, 2=calcSpf, 4=genAll, 7=all
     */
    public void schedWork(int i) {
        todo.or(i << 4);
        notif.wakeup();
    }

    public void run() {
        if (debugger.rtrIsisEvnt) {
            logger.debug("started level" + level);
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
                    generateLsps();
                }
                if ((val & 0x20) != 0) {
                    calculateSpf();
                }
                if ((val & 0x40) != 0) {
                    lower.genLsps(1);
                }
                purgeLsps();
            } catch (Exception e) {
                logger.traceback(e);
            }
        }
        todo.and(1);
        if (debugger.rtrIsisEvnt) {
            logger.debug("stopped level" + level);
        }
    }

    /**
     * start this level
     */
    public void startNow() {
        if ((todo.get() & 2) != 0) {
            return;
        }
        todo.or(1);
        new Thread(this).start();
    }

    /**
     * stop this level
     */
    public void stopNow() {
        todo.and(2);
    }

}
