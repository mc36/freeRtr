package org.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIsis;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.enc.encBase64;
import org.freertr.ip.ipCor4;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabGen;
import org.freertr.tab.tabIndex;
import org.freertr.tab.tabLabelBier;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
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
import org.freertr.user.userFormat;
import org.freertr.util.cmds;

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
     * ha mode
     */
    public boolean haMode;

    /**
     * list of lsps
     */
    protected final tabGen<rtrIsisLsp> lsps;

    /**
     * computed routes
     */
    protected final tabRoute<addrIP> routes;

    /**
     * computed routes
     */
    protected final tabRoute<addrIP> oroutes;

    /**
     * computed routes
     */
    protected List<tabRoute<addrIP>> algos;

    /**
     * computed routes
     */
    protected List<tabRoute<addrIP>> oalgos;

    /**
     * advertise default route
     */
    public boolean defOrigin;

    /**
     * advertise default route
     */
    public boolean odefOrigin;

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
     * segment routing v6 enabled
     */
    public boolean srv6ena;

    /**
     * bier enabled
     */
    public boolean bierEna;

    /**
     * suppress interface addresses
     */
    public boolean suppressAddr;

    /**
     * other suppress interface addresses
     */
    public boolean osuppressAddr;

    /**
     * lsp password
     */
    public String lspPassword;

    /**
     * authentication mode: 1=cleartext, 2=md5, 3=sha1, 4=sha224, 5=sha256,
     * 6=sha384, 7=sha512
     */
    public int authenMode;

    /**
     * authentication id
     */
    public int authenKey;

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
     * learn prefix list
     */
    public tabListing<tabPrfxlstN, addrIP> oprflstFrom;

    /**
     * learn prefix list
     */
    public tabListing<tabPrfxlstN, addrIP> oprflstInto;

    /**
     * advertise route map
     */
    public tabListing<tabRtrmapN, addrIP> oroumapFrom;

    /**
     * advertise route map
     */
    public tabListing<tabRtrmapN, addrIP> oroumapInto;

    /**
     * advertise route map
     */
    public tabListing<tabRtrplcN, addrIP> oroupolFrom;

    /**
     * advertise route map
     */
    public tabListing<tabRtrplcN, addrIP> oroupolInto;

    /**
     * last spf
     */
    protected spfCalc<rtrIsisLevelSpf> lastSpf;

    /**
     * segment routing usage
     */
    protected tabGen<tabIndex<addrIP>> segrouUsd;

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
        lastSpf = new spfCalc<rtrIsisLevelSpf>(null);
        lower = parent;
        level = lev;
        lsps = new tabGen<rtrIsisLsp>();
        routes = new tabRoute<addrIP>("computed");
        oroutes = new tabRoute<addrIP>("computed");
        need2adv = new tabGen<rtrIsisLsp>();
        notif = new notifier();
        authenMode = 1;
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

    private void updateAuthLsp(rtrIsisLsp lsp, int pos, byte[] buf) {
        if ((pos + buf.length) > lsp.bufDat.length) {
            return;
        }
        bits.byteCopy(buf, 0, lsp.bufDat, pos, buf.length);
    }

    /**
     * advertise one lsp
     *
     * @param lsp lsp to advertise
     * @param purge set true to purge it out
     * @param cntnt check content
     */
    protected synchronized void generateLsp(rtrIsisLsp lsp, boolean purge, boolean cntnt) {
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
        byte[] buf = getAuthen(new packHolder(true, true), 0, 0);
        if (buf != null) {
            encTlv tlv = rtrIsis.getTlv();
            packHolder pck = new packHolder(true, true);
            if (purge) {
                advertiseTlv(pck, rtrIsisLsp.tlvAuthen, buf);
                pck.merge2end();
                lsp.bufDat = pck.getCopy();
                lsp.generateCheckSum();
            }
            int siz = lsp.writeData(pck, 0);
            pck.msbPutW(2, 0); // lifetime
            pck.msbPutW(16, 0); // checksum
            pck.putSkip(siz);
            pck.merge2end();
            pck.getSkip(rtrIsisLsp.headSize);
            int pos = rtrIsisLsp.headSize;
            for (;;) {
                int ofs = pck.dataSize();
                if (tlv.getBytes(pck)) {
                    break;
                }
                if (tlv.valTyp != rtrIsisLsp.tlvAuthen) {
                    continue;
                }
                pos = siz - ofs;
            }
            pck.setBytesLeft(siz);
            int typ;
            if (level == 1) {
                typ = rtrIsisNeigh.msgTypL1lsp;
            } else {
                typ = rtrIsisNeigh.msgTypL2lsp;
            }
            buf = getAuthen(pck, typ, pos);
            pos -= rtrIsisLsp.headSize;
            pos += 2;
            updateAuthLsp(lsp, pos, buf);
            if (cntnt && (old != null)) {
                old = old.copyBytes(true);
                updateAuthLsp(old, pos, buf);
            }
        }
        if (cntnt) {
            if (!lsp.contentDiffers(old)) {
                return;
            }
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
        int done = 0;
        for (int i = 0; i < lsps.size(); i++) {
            rtrIsisLsp ntry = lsps.get(i);
            if (ntry == null) {
                continue;
            }
            if (ntry.srcID.compareTo(lower.routerID) != 0) {
                continue;
            }
            if (need2adv.find(ntry) != null) {
                continue;
            }
            if (ntry.getTimeRemain(true) < 1) {
                continue;
            }
            generateLsp(ntry, true, false);
            done++;
        }
        for (int i = 0; i < need2adv.size(); i++) {
            rtrIsisLsp ntry = need2adv.get(i);
            if (ntry == null) {
                continue;
            }
            generateLsp(ntry, false, true);
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
            rtrIsisIface iface = lower.ifaces.get(o);
            if (iface == null) {
                continue;
            }
            for (int i = 0; i < iface.neighs.size(); i++) {
                rtrIsisNeigh neigh = iface.neighs.get(i);
                if (neigh == null) {
                    continue;
                }
                if (neigh.level.level != level) {
                    continue;
                }
                neigh.notif.wakeup();
            }
        }
    }

    /**
     * purge out aged lsps
     */
    protected void purgeLsps() {
        if (debugger.rtrIsisEvnt) {
            logger.debug("purge lsps in level" + level);
        }
        int done = 0;
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
            if (ntry.srcID.compareTo(lower.routerID) != 0) {
                continue;
            }
            if (o > (lspRefresh / 1000)) {
                continue;
            }
            if (need2adv.find(ntry) == null) {
                continue;
            }
            generateLsp(ntry, false, false);
            done++;
        }
        if (done > 0) {
            wakeNeighs();
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
        lsp.nodID = pck.UDPsrc;
        lsp.lspNum = pck.UDPsiz;
        lsp.bufDat = pck.getCopy();
        lsp.flags = getFlagsVal();
        need2adv.put(lsp);
    }

    /**
     * get authentication data
     *
     * @param pck packet
     * @param typ message type
     * @param ofs offset of tlv
     * @return binary data, null if disabled
     */
    protected byte[] getAuthen(packHolder pck, int typ, int ofs) {
        return lower.calcAuthData(pck, typ, ofs, authenMode, authenKey, lspPassword);
    }

    private void advertiseTlv(packHolder pck, encTlv tlv) {
        if ((pck.headSize() + tlv.valSiz) > maxLspSize) {
            advertiseLsp(pck);
            pck.setDataSize(0);
            pck.UDPsiz++;
            byte[] buf = getAuthen(new packHolder(true, true), 0, 0);
            if (buf != null) {
                advertiseTlv(pck, rtrIsisLsp.tlvAuthen, buf);
            }
        }
        tlv.putThis(pck);
    }

    private void advertiseTlv(packHolder pck, int typ, byte[] buf) {
        encTlv tlv = rtrIsis.getTlv();
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
            if (nei.peerAdjState != rtrIsisNeigh.statUp) {
                continue;
            }
            if (nei.level.level != level) {
                continue;
            }
            if (pck.UDPsrc != 0) {
                if (!lower.multiTopo && !lower.other.multiTopo) {
                    advertiseTlv(pck, lower.putISneigh(false, lower.metricWide, lower.multiTopo, nei.rtrID, 0, 0, new byte[0]));
                } else {
                    advertiseTlv(pck, lower.putISneigh(false, lower.metricWide, lower.multiTopo, nei.rtrID, 0, 0, new byte[0]));
                    advertiseTlv(pck, lower.putISneigh(true, lower.other.metricWide, lower.other.multiTopo, nei.rtrID, 0, 0, new byte[0]));
                }
                continue;
            }
            byte[] buf = new byte[0];
            if (subs) {
                buf = rtrIsisTe.putSubs(lower, ifc, nei);
            }
            if (nei.segrouLab != null) {
                buf = bits.byteConcat(buf, rtrIsisSr.putAdj(lower.fwdCore.ipVersion == ipCor4.protocolVersion, nei.segrouLab.label));
            }
            if (nei.segrouOth != null) {
                buf = bits.byteConcat(buf, rtrIsisSr.putAdj(lower.fwdCore.ipVersion != ipCor4.protocolVersion, nei.segrouOth.label));
            }
            if (!lower.multiTopo && !lower.other.multiTopo) {
                advertiseTlv(pck, lower.putISneigh(false, lower.metricWide, lower.multiTopo, nei.rtrID, 0, nei.getMetric(), buf));
            } else {
                advertiseTlv(pck, lower.putISneigh(false, lower.metricWide, lower.multiTopo, nei.rtrID, 0, nei.getMetric(), buf));
                advertiseTlv(pck, lower.putISneigh(true, lower.other.metricWide, lower.other.multiTopo, nei.rtrID, 0, nei.getMetric(), buf));
            }
            if (subs) {
                advertiseTlv(pck, rtrIsisTe.putSrlg(lower, nei.rtrID, 0, ifc.iface.addr, nei.ifcAddr, ifc.teSrlg));
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
        if (!lower.multiTopo && !lower.other.multiTopo) {
            advertiseTlv(pck, lower.putISneigh(false, lower.metricWide, lower.multiTopo, ifc.getDisAddr(level), ifc.getDisCirc(level), ifc.metric, buf));
        } else {
            advertiseTlv(pck, lower.putISneigh(false, lower.metricWide, lower.multiTopo, ifc.getDisAddr(level), ifc.getDisCirc(level), ifc.metric, buf));
            advertiseTlv(pck, lower.putISneigh(true, lower.other.metricWide, lower.other.multiTopo, ifc.getDisAddr(level), ifc.getDisCirc(level), ifc.metric, buf));
        }
        if (subs) {
            advertiseTlv(pck, rtrIsisTe.putSrlg(lower, ifc.getDisAddr(level), ifc.getDisCirc(level), ifc.iface.addr, null, ifc.teSrlg));
        }
        if (!ifc.amIdis(level)) {
            return;
        }
        packHolder p = new packHolder(true, true);
        p.UDPsrc = ifc.circuitID;
        buf = getAuthen(new packHolder(true, true), 0, 0);
        if (buf != null) {
            advertiseTlv(pck, rtrIsisLsp.tlvAuthen, buf);
        }
        if (!lower.multiTopo && !lower.other.multiTopo) {
            advertiseTlv(p, lower.putISneigh(false, lower.metricWide, lower.multiTopo, lower.routerID, 0, 0, new byte[0]));
        } else {
            advertiseTlv(p, lower.putISneigh(false, lower.metricWide, lower.multiTopo, lower.routerID, 0, 0, new byte[0]));
            advertiseTlv(p, lower.putISneigh(true, lower.other.metricWide, lower.other.multiTopo, lower.routerID, 0, 0, new byte[0]));
        }
        createNeighs(p, ifc, false);
        advertiseLsp(p);
    }

    private void doIntLev(tabRoute<addrIP> rs, tabRoute<addrIP> ro, tabRoute<addrIP> other) {
        for (int i = 0; i < other.size(); i++) {
            tabRouteEntry<addrIP> ntry = other.get(i);
            if (ntry == null) {
                continue;
            }
            ntry = ntry.copyBytes(tabRoute.addType.notyet);
            if ((ntry.best.rouSrc & 4) != 0) {
                continue;
            }
            if (level == 2) {
                if ((ntry.best.rouSrc & 2) != 0) {
                    continue;
                }
            } else {
                ntry.best.rouSrc |= 2;
            }
            if (ro.find(ntry.prefix) != null) {
                continue;
            }
            ntry.best.rouSrc &= -1 - 8;
            rs.add(tabRoute.addType.better, ntry, false, false);
        }
    }

    private void doPrefs(boolean other, boolean wide, boolean multi, packHolder pck, tabRoute<addrIP> fl) {
        for (int i = 0; i < fl.size(); i++) {
            tabRouteEntry<addrIP> ntry = fl.get(i);
            if (ntry == null) {
                continue;
            }
            byte[] subs = new byte[0];
            if (ntry.best.tag != 0) {
                subs = bits.byteConcat(subs, lower.putAddrTag(ntry.best.tag));
            }
            if (segrouEna && (ntry.best.segrouIdx > 0)) {
                subs = bits.byteConcat(subs, rtrIsisSr.putPref(ntry.best.segrouIdx, ((ntry.best.rouSrc & 16) != 0), (ntry.best.rouSrc & 3) != 0, (ntry.best.rouSrc & 8) != 0));
            }
            if (bierEna && (ntry.best.bierIdx > 0)) {
                subs = bits.byteConcat(subs, rtrIsisBr.putPref(lower, ntry.best.bierIdx, ntry.best.bierSub));
            }
            advertiseTlv(pck, lower.putAddrReach(other, wide, multi, ntry.prefix, ntry.best.rouSrc, ntry.best.metric, subs));
        }
    }

    private void createAddrs(packHolder pck) {
        tabRoute<addrIP> rs = new tabRoute<addrIP>("rs");
        tabRoute<addrIP> ri = new tabRoute<addrIP>("rs");
        tabRoute<addrIP> os = new tabRoute<addrIP>("rs");
        tabRoute<addrIP> oi = new tabRoute<addrIP>("rs");
        if (defOrigin) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = lower.getDefaultRoute(false);
            ntry.best.origin = 1;
            rs.add(tabRoute.addType.better, ntry, false, false);
        }
        if (odefOrigin) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = lower.getDefaultRoute(true);
            ntry.best.origin = 1;
            os.add(tabRoute.addType.better, ntry, false, false);
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
                advertiseTlv(pck, lower.putAddrIface(false, ifc.iface.addr));
            }
            if (lower.other.enabled && ifc.otherEna && (!ifc.othSuppInt)) {
                advertiseTlv(pck, lower.putAddrIface(true, ifc.oface.addr));
            }
            if (!((suppressAddr || ifc.suppressAddr) && (!ifc.unsuppressAddr))) {
                tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
                ntry.prefix = ifc.iface.network.copyBytes();
                ntry.best.distance = tabRouteAttr.distanIfc;
                ntry.best.metric = ifc.metric;
                ntry.best.segrouIdx = ifc.srIndex;
                if (ifc.srNode) {
                    ntry.best.rouSrc |= 8;
                }
                if (ifc.srPop) {
                    ntry.best.rouSrc |= 16;
                }
                ntry.best.bierIdx = ifc.brIndex;
                ntry.best.bierSub = ifc.brSub;
                ri.add(tabRoute.addType.better, ntry, false, false);
                if ((ifc.circuitLevel & level) == 0) {
                    continue;
                }
                rs.add(tabRoute.addType.better, ntry, false, false);
            }
            if (lower.other.enabled && ifc.otherEna && !((osuppressAddr || ifc.othSuppAddr) && (!ifc.othUnsuppAddr))) {
                tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
                ntry.prefix = ifc.oface.network.copyBytes();
                ntry.best.distance = tabRouteAttr.distanIfc;
                ntry.best.metric = ifc.metric;
                ntry.best.segrouIdx = ifc.srOthIdx;
                if (ifc.srNode) {
                    ntry.best.rouSrc |= 8;
                }
                if (ifc.srPop) {
                    ntry.best.rouSrc |= 16;
                }
                ntry.best.bierIdx = ifc.brOthIdx;
                ntry.best.bierSub = ifc.brOthSub;
                oi.add(tabRoute.addType.better, ntry, false, false);
                if ((ifc.circuitLevel & level) == 0) {
                    continue;
                }
                os.add(tabRoute.addType.better, ntry, false, false);
            }
        }
        for (int i = 0; i < lower.routerRedistedU.size(); i++) {
            tabRouteEntry<addrIP> ntry = lower.routerRedistedU.get(i);
            if (ntry == null) {
                continue;
            }
            ntry = ntry.copyBytes(tabRoute.addType.notyet);
            ntry.best.distance = tabRouteAttr.distanIfc + 1;
            ntry.best.rouSrc = 1;
            ntry.best.segrouIdx = 0;
            ntry.best.bierIdx = 0;
            ntry.best.bierSub = 0;
            rs.add(tabRoute.addType.better, ntry, false, false);
        }
        if (lower.other.enabled) {
            for (int i = 0; i < lower.other.routerRedistedU.size(); i++) {
                tabRouteEntry<addrIP> ntry = lower.other.routerRedistedU.get(i);
                if (ntry == null) {
                    continue;
                }
                ntry = ntry.copyBytes(tabRoute.addType.notyet);
                ntry.best.distance = tabRouteAttr.distanIfc + 1;
                ntry.best.rouSrc = 1;
                ntry.best.segrouIdx = 0;
                ntry.best.bierIdx = 0;
                ntry.best.bierSub = 0;
                os.add(tabRoute.addType.better, ntry, false, false);
            }
        }
        if (interLevels) {
            doIntLev(rs, ri, lower.getLevel(3 - level).routes);
            if (lower.other.enabled) {
                doIntLev(os, oi, lower.getLevel(3 - level).oroutes);
            }
        }
        tabRoute<addrIP> fl = new tabRoute<addrIP>("fl");
        tabRoute.addUpdatedTable(tabRoute.addType.better, rtrBgpUtil.sfiUnicast, 0, fl, rs, true, roumapInto, roupolInto, prflstInto);
        doPrefs(false, lower.metricWide, lower.multiTopo, pck, fl);
        if (!lower.other.enabled) {
            return;
        }
        fl = new tabRoute<addrIP>("fl");
        tabRoute.addUpdatedTable(tabRoute.addType.better, rtrBgpUtil.sfiUnicast, 0, fl, os, true, oroumapInto, oroupolInto, oprflstInto);
        doPrefs(true, lower.other.metricWide, lower.other.multiTopo, pck, fl);
    }

    private void generateLsps() {
        if (debugger.rtrIsisEvnt) {
            logger.debug("generate lsps in level" + level);
        }
        if (lower.routerID.isFilled(0)) {
            return;
        }
        need2adv.clear();
        packHolder pck = new packHolder(true, true);
        byte[] buf = getAuthen(new packHolder(true, true), 0, 0);
        if (buf != null) {
            advertiseTlv(pck, rtrIsisLsp.tlvAuthen, buf);
        }
        advertiseTlv(pck, rtrIsisLsp.tlvProtSupp, lower.getNLPIDlst(lower.other.enabled));
        if (lower.multiTopo || lower.other.multiTopo) {
            int i = getFlagsVal();
            int o = 0;
            if ((i & rtrIsisLsp.flgOver) != 0) {
                o |= 0x80;
            }
            if ((i & rtrIsisLsp.flgAttach) != 0) {
                o |= 0x40;
            }
            buf = lower.getMTopoLst(lower.other.enabled, o);
            advertiseTlv(pck, rtrIsisLsp.tlvMultiTopo, buf);
        }
        advertiseTlv(pck, rtrIsisLsp.tlvAreaAddr, lower.areaID.getAddrDat(true));
        if (traffEng) {
            advertiseTlv(pck, rtrIsisTe.putAddr(lower));
        }
        if (segrouEna && (lower.segrouLab != null)) {
            advertiseTlv(pck, rtrIsisSr.putBase(lower));
        }
        if (srv6ena) {
            advertiseTlv(pck, rtrIsisSr.srv6base(lower));
            for (int i = 0; i < lower.srv6.size(); i++) {
                encTlv tlv = rtrIsisSr.srv6loc(lower.srv6.get(i), 0);
                if (tlv == null) {
                    continue;
                }
                advertiseTlv(pck, tlv);
            }
        }
        if (hostname) {
            buf = cfgAll.hostName.getBytes();
            advertiseTlv(pck, rtrIsisLsp.tlvHostName, buf);
        }
        for (int i = 0; i < lower.ifaces.size(); i++) {
            rtrIsisIface ifc = lower.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
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
            segrouUsd = new tabGen<tabIndex<addrIP>>();
        } else {
            segrouUsd = null;
        }
        spfCalc<rtrIsisLevelSpf> spf = new spfCalc<rtrIsisLevelSpf>(lastSpf);
        boolean needAttach = (!lower.haveNeighbor(2)) && attachedAlw;
        for (int i = 0; i < lsps.size(); i++) {
            rtrIsisLsp lsp = lsps.get(i);
            if (lsp == null) {
                continue;
            }
            if (lsp.getTimeRemain(true) < 1) {
                continue;
            }
            boolean stub = (lsp.flags & rtrIsisLsp.flgOver) != 0;
            packHolder pck = lsp.getPayload();
            rtrIsisLevelSpf src = new rtrIsisLevelSpf(lsp.srcID, lsp.nodID);
            if (needAttach && (!stub) && ((lsp.flags & rtrIsisLsp.flgAttach) != 0)) {
                tabRouteEntry<addrIP> pref = new tabRouteEntry<addrIP>();
                pref.prefix = lower.getDefaultRoute(false);
                pref.best.distance = lower.distantInt;
                pref.best.rouSrc = 6;
                spf.addPref(src, pref, false);
                pref = new tabRouteEntry<addrIP>();
                pref.prefix = lower.getDefaultRoute(true);
                pref.best.distance = lower.other.distantInt;
                pref.best.rouSrc = 6;
                spf.addOpref(src, pref, false);
            }
            encTlv tlv = rtrIsis.getTlv();
            for (;;) {
                if (tlv.getBytes(pck)) {
                    break;
                }
                spf.addIdent(src, rtrIsis.getHostname(tlv));
                if (tlv.valTyp == rtrIsisLsp.tlvRouterCapa) {
                    packHolder p = new packHolder(true, true);
                    byte[] b = tlv.copyBytes();
                    p.putCopy(b, 0, 0, b.length);
                    p.putSkip(b.length);
                    p.merge2end();
                    p.getSkip(5);
                    for (;;) {
                        if (tlv.getBytes(p)) {
                            break;
                        }
                        int o = rtrIsisSr.getBase(tlv);
                        spf.addSegRouB(src, o);
                        spf.addAlgo(src, rtrIsisSr.getAlgos(tlv));
                    }
                    continue;
                }
                addrIsis adr = lower.getISalias(tlv);
                if (adr != null) {
                    rtrIsisLevelSpf trg = new rtrIsisLevelSpf(adr, 0);
                    spf.addConn(src, trg, 0, false, stub, null);
                    spf.addConn(trg, src, 0, false, stub, null);
                    continue;
                }
                List<rtrIsisLsp> nel = lower.getISneigh(false, lower.metricWide, lower.multiTopo, tlv);
                if (nel != null) {
                    for (int o = 0; o < nel.size(); o++) {
                        rtrIsisLsp nei = nel.get(o);
                        if (nei == null) {
                            continue;
                        }
                        spf.addConn(src, new rtrIsisLevelSpf(nei.srcID, nei.nodID), nei.lspNum, nei.nodID == 0, stub, null);
                    }
                    continue;
                }
                List<tabRouteEntry<addrIP>> rou = lower.getAddrReach(false, lower.metricWide, lower.multiTopo, tlv);
                if (rou != null) {
                    for (int o = 0; o < rou.size(); o++) {
                        tabRouteEntry<addrIP> pref = rou.get(o);
                        spf.addBierB(src, pref.best.bierBeg);
                        spf.addSegRouI(src, pref.best.segrouIdx);
                        spf.addBierI(src, pref.best.bierIdx);
                        spf.addBierS(src, pref.best.bierSub);
                        if ((pref.best.rouSrc & 1) == 0) {
                            pref.best.distance = lower.distantInt;
                        } else {
                            pref.best.distance = lower.distantExt;
                        }
                        spf.addPref(src, pref, false);
                    }
                    continue;
                }
                if (!lower.other.enabled) {
                    continue;
                }
                rou = lower.getAddrReach(true, lower.other.metricWide, lower.other.multiTopo, tlv);
                if (rou == null) {
                    continue;
                }
                for (int o = 0; o < rou.size(); o++) {
                    tabRouteEntry<addrIP> pref = rou.get(o);
                    spf.addBierB(src, pref.best.bierBeg);
                    spf.addSegRouI(src, pref.best.segrouIdx);
                    spf.addBierI(src, pref.best.bierIdx);
                    spf.addBierS(src, pref.best.bierSub);
                    if ((pref.best.rouSrc & 1) == 0) {
                        pref.best.distance = lower.other.distantInt;
                    } else {
                        pref.best.distance = lower.other.distantExt;
                    }
                    spf.addOpref(src, pref, false);
                }
            }
        }
        spf.doWork(new rtrIsisLevelSpf(lower.routerID, 0));
        for (int i = 0; i < lower.ifaces.size(); i++) {
            rtrIsisIface ifc = lower.ifaces.get(i);
            if (ifc == null) {
                continue;
            }
            if (ifc.iface.lower.getState() != state.states.up) {
                continue;
            }
            if (segrouUsd != null) {
                if (ifc.srIndex > 0) {
                    tabIndex.add2table(segrouUsd, new tabIndex<addrIP>(ifc.srIndex, new addrPrefix<addrIP>(new addrIP(), 0)));
                    lower.segrouLab[ifc.srIndex].setFwdCommon(tabLabelEntry.owner.isisSrgb, lower.fwdCore);
                }
                if (lower.other.enabled && (ifc.otherEna) && (ifc.srOthIdx > 0)) {
                    tabIndex.add2table(segrouUsd, new tabIndex<addrIP>(ifc.srOthIdx, new addrPrefix<addrIP>(new addrIP(), 0)));
                    lower.segrouLab[ifc.srOthIdx].setFwdCommon(tabLabelEntry.owner.isisSrgb, lower.other.fwd);
                }
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
                spf.addNextHop(nei.getMetric(), new rtrIsisLevelSpf(nei.rtrID, 0), nei.ifcAddr, ifc.iface, nei.ofcAddr, ifc.oface);
            }
        }
        tabRoute<addrIP> rs = spf.getRoutes(lower.fwdCore, tabLabelEntry.owner.isisSrgb, lower.segrouLab, segrouUsd);
        routes.clear();
        tabRoute.addUpdatedTable(tabRoute.addType.ecmp, rtrBgpUtil.sfiUnicast, 0, routes, rs, true, roumapFrom, roupolFrom, prflstFrom);
        lower.routerDoAggregates(rtrBgpUtil.sfiUnicast, routes, routes, lower.fwdCore.commonLabel, null, 0);
        if (lower.other.enabled) {
            rs = spf.getOroutes(lower.other.fwd, tabLabelEntry.owner.isisSrgb, lower.segrouLab, segrouUsd);
            oroutes.clear();
            tabRoute.addUpdatedTable(tabRoute.addType.ecmp, rtrBgpUtil.sfiUnicast, 0, oroutes, rs, true, oroumapFrom, oroupolFrom, oprflstFrom);
            lower.other.routerDoAggregates(rtrBgpUtil.sfiUnicast, oroutes, oroutes, lower.other.fwd.commonLabel, null, 0);
        } else {
            oroutes.clear();
        }
        if (bierEna) {
            bierRes = spf.getBierI(lower.fwdCore, 0, 0);
        } else {
            bierRes = null;
        }
        if (debugger.rtrIsisEvnt) {
            logger.debug("unreachable:" + spf.listReachablility(false));
            logger.debug("reachable:" + spf.listReachablility(true));
        }
        lastSpf = spf;
        algos = new ArrayList<tabRoute<addrIP>>();
        oalgos = new ArrayList<tabRoute<addrIP>>();
        for (int p = 0; p < lower.algos.size(); p++) {
            rtrAlgo alg = lower.algos.get(p);
            if (alg == null) {
                continue;
            }
            spf = lastSpf.copyBytes();
            spf.justFlexAlgo(alg.num);
            spf.doWork(new rtrIsisLevelSpf(lower.routerID, 0));
            for (int i = 0; i < lower.ifaces.size(); i++) {
                rtrIsisIface ifc = lower.ifaces.get(i);
                if (ifc == null) {
                    continue;
                }
                if (ifc.iface.lower.getState() != state.states.up) {
                    continue;
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
                    spf.addNextHop(nei.getMetric(), new rtrIsisLevelSpf(nei.rtrID, 0), nei.ifcAddr, ifc.iface, nei.ofcAddr, ifc.oface);
                }
            }
            rs = spf.getRoutes(lower.fwdCore, null, null, null);
            if (debugger.rtrIsisEvnt) {
                logger.debug("algo" + alg.num + " unreachable:" + spf.listReachablility(false));
                logger.debug("algo" + alg.num + " reachable:" + spf.listReachablility(true));
            }
            tabRoute<addrIP> res = new tabRoute<addrIP>("rou");
            tabRoute.addUpdatedTable(tabRoute.addType.ecmp, rtrBgpUtil.sfiUnicast, 0, res, rs, true, roumapFrom, roupolFrom, prflstFrom);
            lower.routerDoAggregates(rtrBgpUtil.sfiUnicast, res, res, lower.fwdCore.commonLabel, null, 0);
            algos.add(res);
            if (!lower.other.enabled) {
                continue;
            }
            rs = spf.getOroutes(lower.other.fwd, null, null, null);
            res.clear();
            tabRoute.addUpdatedTable(tabRoute.addType.ecmp, rtrBgpUtil.sfiUnicast, 0, res, rs, true, oroumapFrom, oroupolFrom, oprflstFrom);
            lower.other.routerDoAggregates(rtrBgpUtil.sfiUnicast, res, res, lower.other.fwd.commonLabel, null, 0);
            oalgos.add(res);
        }
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

    /**
     * show afi inconsistency
     *
     * @return inconsistency list
     */
    public userFormat showAfiIncons() {
        int protoId = lower.getNLPIDval(false);
        int otherId = lower.getNLPIDval(true);
        userFormat res = new userFormat("|", "router|this|other");
        for (int i = 0; i < lsps.size(); i++) {
            rtrIsisLsp lsp = lsps.get(i);
            if (lsp == null) {
                continue;
            }
            packHolder pck = lsp.getPayload();
            rtrIsisLevelSpf src = new rtrIsisLevelSpf(lsp.srcID, lsp.nodID);
            encTlv tlv = rtrIsis.getTlv();
            boolean protoSupp = false;
            boolean otherSupp = false;
            for (;;) {
                if (tlv.getBytes(pck)) {
                    break;
                }
                if (tlv.valTyp != rtrIsisLsp.tlvProtSupp) {
                    continue;
                }
                for (int o = 0; o < tlv.valSiz; o++) {
                    int currId = bits.getByte(tlv.valDat, o);
                    protoSupp |= currId == protoId;
                    otherSupp |= currId == otherId;
                }
            }
            res.add(src + "|" + cmds.upDown(protoSupp) + "|" + cmds.upDown(otherSupp));
        }
        return res;
    }

    /**
     * get state information
     *
     * @param lst list to append
     * @param beg beginning to use
     */
    public void stateGet(List<String> lst, String beg) {
        if (!haMode) {
            return;
        }
        beg += level + " ";
        packHolder pck = new packHolder(true, true);
        for (int i = 0; i < lsps.size(); i++) {
            rtrIsisLsp ntry = lsps.get(i);
            if (ntry == null) {
                continue;
            }
            pck.clear();
            int o = ntry.writeData(pck, 0);
            pck.msbPutW(2, 1800); // remaining
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
        rtrIsisLsp ntry = new rtrIsisLsp();
        if (ntry.readData(pck, 0) < 0) {
            return true;
        }
        lsps.put(ntry);
        return false;
    }

}
