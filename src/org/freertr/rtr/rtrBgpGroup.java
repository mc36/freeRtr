package org.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.ip.ipFwd;
import org.freertr.ip.ipMpls;
import org.freertr.tab.tabLabel;
import org.freertr.tab.tabLabelBier;
import org.freertr.tab.tabLabelEntry;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabPrfxlstN;
import org.freertr.tab.tabRpkiUtil;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteAttr;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRouteUtil;
import org.freertr.tab.tabRtrmapN;
import org.freertr.tab.tabRtrplcN;
import org.freertr.user.userFormat;

/**
 * bgp4 update group
 *
 * @author matecsaba
 */
public class rtrBgpGroup extends rtrBgpParam {

    /**
     * group number
     */
    public final int groupNum;

    /**
     * minimum version
     */
    public int minversion;

    /**
     * willing prefixes
     */
    public tabRoute<addrIP>[] willing;

    /**
     * changed prefixes
     */
    public tabRoute<addrIP>[] changed;

    /**
     * local address
     */
    public addrIP localAddr;

    /**
     * local other address
     */
    public addrIP localOddr;

    /**
     * type of peer
     */
    protected int peerType;

    private static boolean[] specials;

    /**
     * create group
     *
     * @param parent bgp process
     * @param num group number
     */
    public rtrBgpGroup(rtrBgp parent, int num) {
        super(parent, false);
        willing = rtrBgpParam.freshTables();
        changed = rtrBgpParam.freshTables();
        groupNum = num;
        if (specials != null) {
            return;
        }
        boolean[] res = rtrBgpParam.boolsSet(false);
        for (int i = 0; i < res.length; i++) {
            int o = rtrBgpParam.indexAlias(i);
            if (o < 0) {
                continue;
            }
            res[i] = true;
            res[o] = true;
        }
        res[rtrBgpParam.idxMlt] = true;
        res[rtrBgpParam.idxOmlt] = true;
        specials = res;
    }

    public void doTempCfg(String cmd, boolean negated) {
    }

    public void getConfig(List<String> l, String beg, int filter) {
        l.addAll(getParamCfg(beg, "group " + groupNum + " ", filter));
    }

    /**
     * get status of peer
     *
     * @return status
     */
    public userFormat getStatus() {
        userFormat l = new userFormat("|", "category|value");
        String a = "";
        for (int i = 0; i < lower.neighs.size(); i++) {
            rtrBgpNeigh ntry = lower.neighs.get(i);
            if (ntry.groupMember != groupNum) {
                continue;
            }
            a += " " + ntry.peerAddr;
        }
        for (int i = 0; i < lower.lstnNei.size(); i++) {
            rtrBgpNeigh ntry = lower.lstnNei.get(i);
            if (ntry.groupMember != groupNum) {
                continue;
            }
            a += " " + ntry.peerAddr;
        }
        l.add("peers|" + a);
        l.add("type|" + rtrBgpUtil.peerType2string(peerType));
        l.add("leak role|" + rtrBgpUtil.leakRole2string(leakRole, leakAttr));
        l.add("rpki|" + rtrBgpUtil.rpkiMode2string(rpkiOut) + " vpn=" + rtrBgpUtil.rpkiMode2string(vpkiOut));
        l.add("safi|" + bools2string(addrFams));
        l.add("local|" + localAddr);
        l.add("other|" + localOddr);
        l.add("version|" + minversion + " of " + lower.compRound);
        return l;
    }

    /**
     * get status of peer
     *
     * @return status
     */
    public userFormat getAfis() {
        userFormat l = new userFormat("|", "afi|will|change");
        for (int i = 0; i < addrFams.length; i++) {
            l.add(rtrBgpParam.idx2string(i) + "|" + willing[i].size() + "|" + changed[i].size());
        }
        return l;
    }

    /**
     * get changed
     *
     * @param idx safi to query
     * @param mask safi to query
     * @param safi safi to query
     * @return table
     */
    public tabRoute<addrIP> getChanged(int idx, long mask, int safi) {
        return changed[idx];
    }

    /**
     * purge changed routes
     */
    protected void computeIncrPurge() {
        for (int idx = 0; idx < addrFams.length; idx++) {
            if (!addrFams[idx]) {
                continue;
            }
            tabRoute<addrIP> chg = changed[idx];
            for (int i = chg.size() - 1; i >= 0; i--) {
                tabRouteEntry<addrIP> ntry = chg.get(i);
                if (ntry.best.version >= minversion) {
                    continue;
                }
                chg.del(ntry);
            }
        }
    }

    private ipFwd getForwarder(int afi, tabRouteAttr<addrIP> ntry) {
        if (ntry.rouTab != null) {
            return ntry.rouTab;
        }
        if ((afi == lower.afiOmlt) || (afi == lower.afiOuni)) {
            return lower.other.fwd;
        } else {
            return lower.fwdCore;
        }
    }

    private void nextHopSelf(int afi, tabRouteAttr<addrIP> ntry, tabRouteEntry<addrIP> route) {
        boolean done = false;
        if (!nxtHopCapa) {
            ntry.hopCapa = null;
        }
        if (nxtHopMltlb && (ntry.nextHop != null)) {
            ipFwd tab = getForwarder(afi, ntry);
            tabRouteEntry<addrIP> org = tab.labeldR.route(ntry.nextHop);
            tabLabelEntry loc;
            if (org == null) {
                loc = tab.commonLabel;
            } else {
                loc = org.best.labelLoc;
            }
            ntry.labelRem = tabLabel.prependLabel(ntry.labelRem, loc.label);
            done = true;
        }
        if ((afi == lower.afiOmlt) || ((afi == lower.afiOuni) && lower.other.routerVpn && !addrFams[rtrBgpParam.idxOlab] && !addrFams[rtrBgpParam.idxOctp] && !addrFams[rtrBgpParam.idxOcar])) {
            ntry.nextHop = localOddr.copyBytes();
        } else {
            ntry.nextHop = localAddr.copyBytes();
        }
        if (!done) {
            ntry.labelRem = new ArrayList<Integer>();
            tabLabelEntry loc = ntry.labelLoc;
            if (loc == null) {
                ipFwd tab = getForwarder(afi, ntry);
                tabRouteEntry<addrIP> org = tab.labeldR.find(route);
                if (org == null) {
                    loc = tab.commonLabel;
                } else {
                    loc = org.best.labelLoc;
                }
            }
            int val = loc.label;
            if (labelPop) {
                if ((afi == lower.afiUni) && (val == lower.fwdCore.commonLabel.label)) {
                    val = ipMpls.labelImp;
                }
                if ((afi == lower.afiOuni) && (val == lower.other.fwd.commonLabel.label)) {
                    val = ipMpls.labelImp;
                }
            }
            ntry.labelRem.add(val);
        }
        if (lower.segrouLab != null) {
            ntry.segrouSiz = lower.segrouMax;
            ntry.segrouBeg = lower.segrouLab[0].label;
        }
        if (lower.bierLab != null) {
            ntry.bierHdr = tabLabelBier.num2bsl(lower.bierLen);
            ntry.bierSiz = lower.bierLab.length;
            ntry.bierBeg = lower.bierLab[0].label;
        }
    }

    private void nextHopSelf(int afi, tabRouteEntry<addrIP> ntry) {
        for (int i = 0; i < ntry.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(i);
            nextHopSelf(afi, attr, ntry);
        }
    }

    private void setCustOnly(tabRouteEntry<addrIP> ntry) {
        for (int i = 0; i < ntry.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(i);
            if (attr.onlyCust == 0) {
                attr.onlyCust = localAs;
            }
        }
    }

    /**
     * validate a prefix
     *
     * @param afi address family
     * @param ntry route entry
     */
    public void setValidity(int afi, tabRouteEntry<addrIP> ntry) {
        if (lower.rpkiR == null) {
            return;
        }
        if ((afi == lower.afiUni) || (afi == lower.afiMlt)) {
            tabRpkiUtil.setValidityRoute(localAs, ntry, lower.rpkiA, lower.rpkiP, rpkiOut);
        }
        if ((afi == lower.afiOuni) || (afi == lower.afiOmlt)) {
            tabRpkiUtil.setValidityRoute(localAs, ntry, lower.rpkiO, lower.rpkiP, rpkiOut);
        }
        if ((afi == lower.afiVpnU) || (afi == lower.afiVpnM)) {
            tabRpkiUtil.setValidityRoute(localAs, ntry, lower.rpkiA, lower.rpkiP, vpkiOut);
        }
        if ((afi == lower.afiVpoU) || (afi == lower.afiVpoM)) {
            tabRpkiUtil.setValidityRoute(localAs, ntry, lower.rpkiO, lower.rpkiP, vpkiOut);
        }
    }

    private void clearAttribs(tabRouteAttr<addrIP> ntry) {
        if ((sendCommunity & 1) == 0) {
            ntry.stdComm = null;
        }
        if ((sendCommunity & 2) == 0) {
            ntry.extComm = null;
        }
        if ((sendCommunity & 4) == 0) {
            ntry.lrgComm = null;
        }
        if (!accIgp) {
            ntry.accIgp = 0;
        }
        if (!entrLab) {
            ntry.entropyLabel = null;
        }
        if (!traffEng) {
            ntry.bandwidth = 0;
        }
        if (!pmsiTun) {
            ntry.pmsiLab = 0;
            ntry.pmsiTyp = 0;
            ntry.pmsiTun = null;
        }
        if (!connect) {
            ntry.connRtr = null;
        }
        if (!peDist) {
            ntry.pediRtr = null;
            ntry.pediLab = 0;
        }
        if (!pathLim) {
            ntry.pathLim = 0;
            ntry.pathAsn = 0;
        }
        if (!nshChain) {
            ntry.nshChain = null;
        }
        if (!domainPath) {
            ntry.domainPath = null;
        }
        if (!bfdDiscr) {
            ntry.bfdDiscr = null;
        }
        if (!lnkSta) {
            ntry.linkStat = null;
        }
        if (!tunEnc) {
            ntry.tunelTyp = 0;
            ntry.tunelVal = null;
        }
        tabRouteUtil.removeUnknowns(ntry, unknownsOut);
        if (!attribSet) {
            ntry.attribAs = 0;
            ntry.attribVal = null;
        }
        if (!segRout) {
            ntry.segrouIdx = 0;
            ntry.segrouBeg = 0;
            ntry.segrouOld = 0;
            ntry.segrouSiz = 0;
            ntry.segrouOfs = 0;
            ntry.segrouPrf = null;
        }
        if (!bier) {
            ntry.bierIdx = 0;
            ntry.bierSub = 0;
            ntry.bierBeg = 0;
            ntry.bierOld = 0;
            ntry.bierSiz = 0;
            ntry.bierHdr = 0;
        }
        if (!leakAttr) {
            ntry.onlyCust = 0;
        }
        if (removePrivAsOut) {
            tabRouteUtil.removePrivateAs(ntry.pathSeq);
            tabRouteUtil.removePrivateAs(ntry.pathSet);
        }
        if (overridePeerOut) {
            tabRouteUtil.replaceIntList(ntry.pathSeq, remoteAs, localAs);
            tabRouteUtil.replaceIntList(ntry.pathSet, remoteAs, localAs);
        }
        ntry.srcRtr = null;
        ntry.oldHop = null;
        ntry.iface = null;
        switch (peerType) {
            case rtrBgpUtil.peerServr:
            case rtrBgpUtil.peerExtrn:
                ntry.originator = null;
                ntry.clustList = null;
                ntry.confSeq = null;
                ntry.confSet = null;
                ntry.locPref = 0;
                break;
            case rtrBgpUtil.peerCnfed:
                ntry.originator = null;
                ntry.clustList = null;
                break;
        }
    }

    /**
     * originate prefix
     *
     * @param afi afi
     * @param ntry prefix
     * @return copy of prefix, null if forbidden
     */
    public tabRouteEntry<addrIP> originatePrefix(int afi, tabRouteEntry<addrIP> ntry) {
        ntry = ntry.copyBytes(tabRoute.addType.altEcmp);
        ntry.best.rouSrc = rtrBgpUtil.peerOriginate;
        if (intVpnClnt) {
            rtrBgpUtil.decodeAttribSet(lower.dummySpk, ntry);
        }
        if ((rtfilterUsed != null) && shouldRtfilter(afi)) {
            if (tabRouteUtil.findRtfilterTab(ntry.best.extComm, remoteAs, rtfilterUsed, false) && tabRouteUtil.findRtfilterTab(ntry.best.extComm, localAs, rtfilterUsed, false)) {
                return null;
            }
        }
        switch (leakRole) {
            case rtrBgpUtil.roleRs:
            case rtrBgpUtil.roleProv:
                if (ntry.best.onlyCust != 0) {
                    return null;
                }
                break;
            case rtrBgpUtil.roleRsc:
            case rtrBgpUtil.roleCust:
                setCustOnly(ntry);
                break;
            case rtrBgpUtil.rolePeer:
                if (ntry.best.onlyCust != 0) {
                    return null;
                }
                setCustOnly(ntry);
                break;
            default:
                break;
        }
        setValidity(afi, ntry);
        nextHopSelf(afi, ntry);
        switch (peerType) {
            case rtrBgpUtil.peerExtrn:
            case rtrBgpUtil.peerServr:
                for (int i = 0; i < ntry.alts.size(); i++) {
                    tabRouteAttr<addrIP> attr = ntry.alts.get(i);
                    attr.pathSeq = tabLabel.prependLabel(attr.pathSeq, localAs);
                }
                break;
            case rtrBgpUtil.peerIntrn:
                for (int i = 0; i < ntry.alts.size(); i++) {
                    tabRouteAttr<addrIP> attr = ntry.alts.get(i);
                    if (attr.locPref == 0) {
                        attr.locPref = preference;
                    }
                }
                break;
            case rtrBgpUtil.peerRflct:
                addrIP addr = new addrIP();
                addr.fromIPv4addr(lower.routerID);
                for (int i = 0; i < ntry.alts.size(); i++) {
                    tabRouteAttr<addrIP> attr = ntry.alts.get(i);
                    if (attr.locPref == 0) {
                        attr.locPref = preference;
                    }
                    if (attr.clustList == null) {
                        attr.clustList = new ArrayList<addrIP>();
                    }
                    attr.clustList.add(addr);
                }
                break;
            case rtrBgpUtil.peerCnfed:
                for (int i = 0; i < ntry.alts.size(); i++) {
                    tabRouteAttr<addrIP> attr = ntry.alts.get(i);
                    attr.confSeq = tabLabel.prependLabel(attr.confSeq, localAs);
                    if (attr.locPref == 0) {
                        attr.locPref = preference;
                    }
                }
                break;
        }
        for (int i = 0; i < ntry.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(i);
            attr.segrouIdx = lower.segrouIdx;
            attr.bierIdx = lower.bierIdx;
            attr.bierSub = lower.bierSub;
            clearAttribs(attr);
        }
        return ntry;
    }

    /**
     * readvertise prefix
     *
     * @param afi afi
     * @param ntry prefix
     * @return copy of prefix, null if forbidden
     */
    public tabRouteEntry<addrIP> readvertPrefix(int afi, tabRouteEntry<addrIP> ntry) {
        if (intVpnClnt) {
            ntry = ntry.copyBytes(tabRoute.addType.altEcmp);
            rtrBgpUtil.decodeAttribSet(lower.dummySpk, ntry);
        }
        if ((rtfilterUsed != null) && shouldRtfilter(afi)) {
            if (tabRouteUtil.findRtfilterTab(ntry.best.extComm, remoteAs, rtfilterUsed, false) && tabRouteUtil.findRtfilterTab(ntry.best.extComm, localAs, rtfilterUsed, false)) {
                return null;
            }
        }
        switch (leakRole) {
            case rtrBgpUtil.rolePeer:
            case rtrBgpUtil.roleProv:
            case rtrBgpUtil.roleRs:
                if (ntry.best.onlyCust != 0) {
                    return null;
                }
                break;
            default:
                break;
        }
        if (!allowAsOut) {
            if (tabRouteUtil.findIntList(ntry.best.pathSeq, remoteAs) >= 0) {
                return null;
            }
            if (tabRouteUtil.findIntList(ntry.best.pathSet, remoteAs) >= 0) {
                return null;
            }
        }
        if (tabRouteUtil.findIntList(ntry.best.stdComm, rtrBgpUtil.commNoAdvertise) >= 0) {
            return null;
        }
        switch (peerType) {
            case rtrBgpUtil.peerExtrn:
                if (ntry.best.pathLim > 0) {
                    if (ntry.best.asPathLen() >= ntry.best.pathLim) {
                        return null;
                    }
                }
                if (tabRouteUtil.findIntList(ntry.best.stdComm, rtrBgpUtil.commNoExport) >= 0) {
                    return null;
                }
                ntry = ntry.copyBytes(tabRoute.addType.altEcmp);
                for (int i = 0; i < ntry.alts.size(); i++) {
                    tabRouteAttr<addrIP> attr = ntry.alts.get(i);
                    attr.pathSeq = tabLabel.prependLabel(attr.pathSeq, localAs);
                    if (attr.pathSeq.size() > 1) {
                        attr.metric = 0;
                    }
                }
                if (!nxtHopUnchgd) {
                    nextHopSelf(afi, ntry);
                }
                break;
            case rtrBgpUtil.peerIntrn:
                switch (ntry.best.rouSrc) {
                    case rtrBgpUtil.peerIntrn:
                        return null;
                    case rtrBgpUtil.peerExtrn:
                    case rtrBgpUtil.peerServr:
                        ntry = ntry.copyBytes(tabRoute.addType.altEcmp);
                        if (!nxtHopUnchgd) {
                            nextHopSelf(afi, ntry);
                        }
                        break;
                    default:
                        ntry = ntry.copyBytes(tabRoute.addType.altEcmp);
                        break;
                }
                break;
            case rtrBgpUtil.peerRflct:
                ntry = ntry.copyBytes(tabRoute.addType.altEcmp);
                switch (ntry.best.rouSrc) {
                    case rtrBgpUtil.peerIntrn:
                        addrIP addr = new addrIP();
                        addr.fromIPv4addr(lower.routerID);
                        for (int i = 0; i < ntry.alts.size(); i++) {
                            tabRouteAttr<addrIP> attr = ntry.alts.get(i);
                            if (attr.clustList == null) {
                                attr.clustList = new ArrayList<addrIP>();
                            }
                            attr.clustList.add(addr);
                        }
                        break;
                    case rtrBgpUtil.peerRflct:
                        if (!lower.clientReflect) {
                            return null;
                        }
                        break;
                    case rtrBgpUtil.peerExtrn:
                    case rtrBgpUtil.peerServr:
                        if (!nxtHopUnchgd) {
                            nextHopSelf(afi, ntry);
                        }
                        break;
                }
                break;
            case rtrBgpUtil.peerServr:
                if (tabRouteUtil.findIntList(ntry.best.stdComm, rtrBgpUtil.commNoExport) >= 0) {
                    return null;
                }
                ntry = ntry.copyBytes(tabRoute.addType.altEcmp);
                switch (ntry.best.rouSrc) {
                    case rtrBgpUtil.peerServr:
                        break;
                    default:
                        for (int i = 0; i < ntry.alts.size(); i++) {
                            tabRouteAttr<addrIP> attr = ntry.alts.get(i);
                            attr.pathSeq = tabLabel.prependLabel(attr.pathSeq, localAs);
                            if (attr.pathSeq.size() > 1) {
                                attr.metric = 0;
                            }
                        }
                        if (!nxtHopUnchgd) {
                            nextHopSelf(afi, ntry);
                        }
                        break;
                }
                break;
            case rtrBgpUtil.peerCnfed:
                if (tabRouteUtil.findIntList(ntry.best.stdComm, rtrBgpUtil.commNoConfed) >= 0) {
                    return null;
                }
                ntry = ntry.copyBytes(tabRoute.addType.altEcmp);
                switch (ntry.best.rouSrc) {
                    case rtrBgpUtil.peerExtrn:
                    case rtrBgpUtil.peerServr:
                        if (!nxtHopUnchgd) {
                            nextHopSelf(afi, ntry);
                        }
                        break;
                }
                for (int i = 0; i < ntry.alts.size(); i++) {
                    tabRouteAttr<addrIP> attr = ntry.alts.get(i);
                    attr.confSeq = tabLabel.prependLabel(attr.confSeq, localAs);
                }
                break;
            default:
                return null;
        }
        switch (leakRole) {
            case rtrBgpUtil.roleRsc:
            case rtrBgpUtil.roleCust:
            case rtrBgpUtil.rolePeer:
                setCustOnly(ntry);
                break;
            default:
                break;
        }
        for (int i = 0; i < ntry.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(i);
            clearAttribs(attr);
        }
        setValidity(afi, ntry);
        if (nxtHopSelf) {
            nextHopSelf(afi, ntry);
            return ntry;
        }
        if ((afi != lower.afiUni)) {
            return ntry;
        }
        if (!addrFams[rtrBgpParam.idxLab] && !addrFams[rtrBgpParam.idxCtp] && !addrFams[rtrBgpParam.idxCar]) {
            return ntry;
        }
        for (int i = 0; i < ntry.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(i);
            if (attr.labelRem == null) {
                nextHopSelf(afi, attr, ntry);
            }
        }
        return ntry;
    }

    private void readvertTable(int afi, tabRoute<addrIP> tab, tabRoute<addrIP> cmp, tabListing<tabRtrmapN, addrIP> rouMap, tabListing<tabRtrplcN, addrIP> rouPlc, tabListing<tabPrfxlstN, addrIP> prfLst) {
        for (int i = 0; i < cmp.size(); i++) {
            tabRouteEntry<addrIP> ntry = cmp.get(i);
            if (ntry.best.rouSrc == rtrBgpUtil.peerOriginate) {
                ntry = originatePrefix(afi, ntry);
            } else {
                ntry = readvertPrefix(afi, ntry);
            }
            if (ntry == null) {
                continue;
            }
            tabRoute.addUpdatedEntry(tabRoute.addType.altEcmp, tab, afi, remoteAs, ntry, false, rouMap, rouPlc, prfLst);
        }
    }

    private void importTable(int afi, tabRoute<addrIP> tab, tabRoute<addrIP> imp, tabListing<tabRtrmapN, addrIP> rouMap, tabListing<tabRtrplcN, addrIP> rouPlc, tabListing<tabPrfxlstN, addrIP> prfLst) {
        for (int i = 0; i < imp.size(); i++) {
            tabRouteEntry<addrIP> ntry = imp.get(i);
            if (ntry.best.rouSrc == rtrBgpUtil.peerOriginate) {
                ntry = originatePrefix(afi, ntry);
            } else {
                ntry = readvertPrefix(afi, ntry);
            }
            if (ntry == null) {
                continue;
            }
            tabRoute.addUpdatedEntry(tabRoute.addType.altEcmp, tab, afi, remoteAs, ntry, false, rouMap, rouPlc, prfLst);
        }
    }

    private void updateTable(tabRoute<addrIP> src, tabRoute.addType mod, tabRoute<addrIP> trg, int afi, tabListing<tabRtrmapN, addrIP> rouMap, tabListing<tabRtrplcN, addrIP> rouPlc, tabListing<tabPrfxlstN, addrIP> prfLst) {
        for (int i = 0; i < src.size(); i++) {
            tabRouteEntry<addrIP> ntry = src.get(i);
            if (ntry == null) {
                continue;
            }
            ntry = originatePrefix(lower.afiUni, ntry);
            tabRoute.addUpdatedEntry(mod, trg, afi, remoteAs, ntry, true, rouMap, rouPlc, prfLst);
        }
    }

    /**
     * create needed prefix list
     */
    @SuppressWarnings({"unchecked", "rawtypes"})
    public void createNeeded() {
        tabRoute<addrIP>[] newly = rtrBgpParam.freshTables();
        if (sendDefRou || lower.defRou) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = rtrBgpUtil.defaultRoute(lower.afiUni);
            ntry.best.aggrRtr = new addrIP();
            ntry.best.aggrRtr.fromIPv4addr(lower.routerID);
            ntry.best.aggrAs = localAs;
            ntry = originatePrefix(lower.afiUni, ntry);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, newly[rtrBgpParam.idxUni], lower.afiUni, remoteAs, ntry, true, roumapOut, roupolOut, prflstOut);
            ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = rtrBgpUtil.defaultRoute(lower.afiUni);
            ntry.best.aggrRtr = new addrIP();
            ntry.best.aggrRtr.fromIPv4addr(lower.routerID);
            ntry.best.aggrAs = localAs;
            ntry = originatePrefix(lower.afiMlt, ntry);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, newly[rtrBgpParam.idxMlt], lower.afiMlt, remoteAs, ntry, true, roumapOut, roupolOut, prflstOut);
        }
        if (sendOtrDefRou || lower.other.defRou) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = rtrBgpUtil.defaultRoute(lower.afiOuni);
            ntry.best.aggrRtr = new addrIP();
            ntry.best.aggrRtr.fromIPv4addr(lower.routerID);
            ntry.best.aggrAs = localAs;
            ntry = originatePrefix(lower.afiOuni, ntry);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, newly[rtrBgpParam.idxOuni], lower.afiOuni, remoteAs, ntry, true, oroumapOut, oroupolOut, oprflstOut);
            ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = rtrBgpUtil.defaultRoute(lower.afiOuni);
            ntry.best.aggrRtr = new addrIP();
            ntry.best.aggrRtr.fromIPv4addr(lower.routerID);
            ntry.best.aggrAs = localAs;
            ntry = originatePrefix(lower.afiOmlt, ntry);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, newly[rtrBgpParam.idxOmlt], lower.afiOmlt, remoteAs, ntry, true, oroumapOut, oroupolOut, oprflstOut);
        }
        updateTable(lower.routerRedistedU, tabRoute.addType.altEcmp, newly[rtrBgpParam.idxUni], lower.afiUni, roumapOut, roupolOut, prflstOut);
        updateTable(lower.routerRedistedM, tabRoute.addType.altEcmp, newly[rtrBgpParam.idxMlt], lower.afiMlt, roumapOut, roupolOut, prflstOut);
        updateTable(lower.routerRedistedF, tabRoute.addType.altEcmp, newly[rtrBgpParam.idxFlw], lower.afiFlw, vroumapOut, vroupolOut, null);
        readvertTable(lower.afiUni, newly[rtrBgpParam.idxUni], lower.newlyUni, roumapOut, roupolOut, prflstOut);
        readvertTable(lower.afiMlt, newly[rtrBgpParam.idxMlt], lower.newlyMlt, roumapOut, roupolOut, prflstOut);
        readvertTable(lower.afiOuni, newly[rtrBgpParam.idxOuni], lower.newlyOuni, oroumapOut, oroupolOut, oprflstOut);
        readvertTable(lower.afiOmlt, newly[rtrBgpParam.idxOmlt], lower.newlyOmlt, oroumapOut, oroupolOut, oprflstOut);
        tabRoute<addrIP> tab = new tabRoute<addrIP>("agg");
        lower.routerDoAggregates(lower.afiUni, newly[rtrBgpParam.idxUni], tab, lower.fwdCore.commonLabel, lower.routerID, lower.localAs);
        updateTable(tab, tabRoute.addType.better, newly[rtrBgpParam.idxUni], lower.afiUni, roumapOut, roupolOut, prflstOut);
        tab = new tabRoute<addrIP>("agg");
        lower.routerDoAggregates(lower.afiMlt, newly[rtrBgpParam.idxMlt], tab, lower.fwdCore.commonLabel, lower.routerID, lower.localAs);
        updateTable(tab, tabRoute.addType.better, newly[rtrBgpParam.idxMlt], lower.afiMlt, roumapOut, roupolOut, prflstOut);
        tab = new tabRoute<addrIP>("agg");
        lower.other.routerDoAggregates(lower.afiOuni, newly[rtrBgpParam.idxOuni], tab, lower.other.fwd.commonLabel, lower.routerID, lower.localAs);
        updateTable(tab, tabRoute.addType.better, newly[rtrBgpParam.idxOuni], lower.afiOuni, oroumapOut, oroupolOut, oprflstOut);
        tab = new tabRoute<addrIP>("agg");
        lower.other.routerDoAggregates(lower.afiOmlt, newly[rtrBgpParam.idxOmlt], tab, lower.other.fwd.commonLabel, lower.routerID, lower.localAs);
        updateTable(tab, tabRoute.addType.better, newly[rtrBgpParam.idxOmlt], lower.afiOmlt, oroumapOut, oroupolOut, oprflstOut);
        for (int i = 0; i < specials.length; i++) {
            if (specials[i]) {
                continue;
            }
            int afi = lower.idx2safi(i);
            tabListing[] fltr = getOutFilters(i);
            importTable(afi, newly[i], lower.getNewly(i, 0, 0), fltr[0], fltr[1], fltr[2]);
        }
        if (peerType != rtrBgpUtil.peerRflct) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = new addrPrefix<addrIP>(new addrIP(), 0);
            newly[rtrBgpParam.idxRtf].del(ntry);
        }
        willing = newly;
    }

}
