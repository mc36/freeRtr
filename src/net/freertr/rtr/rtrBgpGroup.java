package net.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.ip.ipFwd;
import net.freertr.ip.ipMpls;
import net.freertr.tab.tabLabel;
import net.freertr.tab.tabLabelBier;
import net.freertr.tab.tabLabelEntry;
import net.freertr.tab.tabListing;
import net.freertr.tab.tabPrfxlstN;
import net.freertr.tab.tabRoute;
import net.freertr.tab.tabRouteAttr;
import net.freertr.tab.tabRouteEntry;
import net.freertr.tab.tabRtrmapN;
import net.freertr.tab.tabRtrplcN;
import net.freertr.user.userFormat;
import net.freertr.util.logger;

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
     * willing unicast prefixes
     */
    public tabRoute<addrIP> wilUni = new tabRoute<addrIP>("tx");

    /**
     * willing multicast prefixes
     */
    public tabRoute<addrIP> wilMlt = new tabRoute<addrIP>("tx");

    /**
     * willing other unicast prefixes
     */
    public tabRoute<addrIP> wilOtrU = new tabRoute<addrIP>("tx");

    /**
     * willing other multicast prefixes
     */
    public tabRoute<addrIP> wilOtrM = new tabRoute<addrIP>("tx");

    /**
     * willing other flowspec prefixes
     */
    public tabRoute<addrIP> wilOtrF = new tabRoute<addrIP>("tx");

    /**
     * willing other srte prefixes
     */
    public tabRoute<addrIP> wilOtrS = new tabRoute<addrIP>("tx");

    /**
     * willing flowspec prefixes
     */
    public tabRoute<addrIP> wilFlw = new tabRoute<addrIP>("tx");

    /**
     * willing vpnuni prefixes
     */
    public tabRoute<addrIP> wilVpnU = new tabRoute<addrIP>("tx");

    /**
     * willing vpnmulti prefixes
     */
    public tabRoute<addrIP> wilVpnM = new tabRoute<addrIP>("tx");

    /**
     * willing vpnflow prefixes
     */
    public tabRoute<addrIP> wilVpnF = new tabRoute<addrIP>("tx");

    /**
     * willing other vpnuni prefixes
     */
    public tabRoute<addrIP> wilVpoU = new tabRoute<addrIP>("tx");

    /**
     * willing other vpnmulti prefixes
     */
    public tabRoute<addrIP> wilVpoM = new tabRoute<addrIP>("tx");

    /**
     * willing other vpnflow prefixes
     */
    public tabRoute<addrIP> wilVpoF = new tabRoute<addrIP>("tx");

    /**
     * willing vpls prefixes
     */
    public tabRoute<addrIP> wilVpls = new tabRoute<addrIP>("tx");

    /**
     * willing mspw prefixes
     */
    public tabRoute<addrIP> wilMspw = new tabRoute<addrIP>("tx");

    /**
     * willing evpn prefixes
     */
    public tabRoute<addrIP> wilEvpn = new tabRoute<addrIP>("tx");

    /**
     * willing mdt prefixes
     */
    public tabRoute<addrIP> wilMdt = new tabRoute<addrIP>("tx");

    /**
     * willing nsh prefixes
     */
    public tabRoute<addrIP> wilNsh = new tabRoute<addrIP>("tx");

    /**
     * willing srte prefixes
     */
    public tabRoute<addrIP> wilSrte = new tabRoute<addrIP>("tx");

    /**
     * willing linkstate prefixes
     */
    public tabRoute<addrIP> wilLnks = new tabRoute<addrIP>("tx");

    /**
     * willing mvpn prefixes
     */
    public tabRoute<addrIP> wilMvpn = new tabRoute<addrIP>("tx");

    /**
     * willing other mvpn prefixes
     */
    public tabRoute<addrIP> wilMvpo = new tabRoute<addrIP>("tx");

    /**
     * changed unicast prefixes
     */
    public tabRoute<addrIP> chgUni = new tabRoute<addrIP>("chg");

    /**
     * changed multicast prefixes
     */
    public tabRoute<addrIP> chgMlt = new tabRoute<addrIP>("chg");

    /**
     * changed other unicast prefixes
     */
    public tabRoute<addrIP> chgOtrU = new tabRoute<addrIP>("chg");

    /**
     * changed other multicast prefixes
     */
    public tabRoute<addrIP> chgOtrM = new tabRoute<addrIP>("chg");

    /**
     * changed other flowspec prefixes
     */
    public tabRoute<addrIP> chgOtrF = new tabRoute<addrIP>("chg");

    /**
     * changed other srte prefixes
     */
    public tabRoute<addrIP> chgOtrS = new tabRoute<addrIP>("chg");

    /**
     * changed flowspec prefixes
     */
    public tabRoute<addrIP> chgFlw = new tabRoute<addrIP>("chg");

    /**
     * changed vpnuni prefixes
     */
    public tabRoute<addrIP> chgVpnU = new tabRoute<addrIP>("chg");

    /**
     * changed vpnmulti prefixes
     */
    public tabRoute<addrIP> chgVpnM = new tabRoute<addrIP>("chg");

    /**
     * changed vpnflow prefixes
     */
    public tabRoute<addrIP> chgVpnF = new tabRoute<addrIP>("chg");

    /**
     * changed other vpnuni prefixes
     */
    public tabRoute<addrIP> chgVpoU = new tabRoute<addrIP>("chg");

    /**
     * changed other vpnmulti prefixes
     */
    public tabRoute<addrIP> chgVpoM = new tabRoute<addrIP>("chg");

    /**
     * changed other vpnflow prefixes
     */
    public tabRoute<addrIP> chgVpoF = new tabRoute<addrIP>("chg");

    /**
     * changed vpls prefixes
     */
    public tabRoute<addrIP> chgVpls = new tabRoute<addrIP>("chg");

    /**
     * changed mspw prefixes
     */
    public tabRoute<addrIP> chgMspw = new tabRoute<addrIP>("chg");

    /**
     * changed evpn prefixes
     */
    public tabRoute<addrIP> chgEvpn = new tabRoute<addrIP>("chg");

    /**
     * changed mdt prefixes
     */
    public tabRoute<addrIP> chgMdt = new tabRoute<addrIP>("chg");

    /**
     * changed nsh prefixes
     */
    public tabRoute<addrIP> chgNsh = new tabRoute<addrIP>("chg");

    /**
     * changed srte prefixes
     */
    public tabRoute<addrIP> chgSrte = new tabRoute<addrIP>("chg");

    /**
     * changed linkstate prefixes
     */
    public tabRoute<addrIP> chgLnks = new tabRoute<addrIP>("chg");

    /**
     * changed mvpn prefixes
     */
    public tabRoute<addrIP> chgMvpn = new tabRoute<addrIP>("chg");

    /**
     * changed other mvpn prefixes
     */
    public tabRoute<addrIP> chgMvpo = new tabRoute<addrIP>("chg");

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

    /**
     * create group
     *
     * @param parent bgp process
     * @param num group number
     */
    public rtrBgpGroup(rtrBgp parent, int num) {
        super(parent, false);
        groupNum = num;
    }

    public void flapBgpConn() {
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
        l.add("safi|" + mask2string(addrFams));
        l.add("local|" + localAddr);
        l.add("other|" + localOddr);
        l.add("unicast advertise|" + wilUni.size() + ", list=" + chgUni.size());
        l.add("multicast advertise|" + wilMlt.size() + ", list=" + chgMlt.size());
        l.add("ouni advertise|" + wilOtrU.size() + ", list=" + chgOtrU.size());
        l.add("omlt advertise|" + wilOtrM.size() + ", list=" + chgOtrM.size());
        l.add("oflw advertise|" + wilOtrF.size() + ", list=" + chgOtrF.size());
        l.add("osrt advertise|" + wilOtrS.size() + ", list=" + chgOtrS.size());
        l.add("flowspec advertise|" + wilFlw.size() + ", list=" + chgFlw.size());
        l.add("vpnuni advertise|" + wilVpnU.size() + ", list=" + chgVpnU.size());
        l.add("vpnmlt advertise|" + wilVpnM.size() + ", list=" + chgVpnM.size());
        l.add("vpnflw advertise|" + wilVpnF.size() + ", list=" + chgVpnF.size());
        l.add("ovpnuni advertise|" + wilVpoU.size() + ", list=" + chgVpoU.size());
        l.add("ovpnmlt advertise|" + wilVpoM.size() + ", list=" + chgVpoM.size());
        l.add("ovpnflw advertise|" + wilVpoF.size() + ", list=" + chgVpoF.size());
        l.add("vpls advertise|" + wilVpls.size() + ", list=" + chgVpls.size());
        l.add("mspw advertise|" + wilMspw.size() + ", list=" + chgMspw.size());
        l.add("evpn advertise|" + wilEvpn.size() + ", list=" + chgEvpn.size());
        l.add("mdt advertise|" + wilMdt.size() + ", list=" + chgMdt.size());
        l.add("nsh advertise|" + wilNsh.size() + ", list=" + chgNsh.size());
        l.add("srte advertise|" + wilSrte.size() + ", list=" + chgSrte.size());
        l.add("linkstate advertise|" + wilLnks.size() + ", list=" + chgLnks.size());
        l.add("mvpn advertise|" + wilMvpn.size() + ", list=" + chgMvpn.size());
        l.add("omvpn advertise|" + wilMvpo.size() + ", list=" + chgMvpo.size());
        l.add("version|" + minversion + " of " + lower.compRound);
        return l;
    }

    /**
     * get willing
     *
     * @param safi safi to query
     * @return table
     */
    public tabRoute<addrIP> getWilling(int safi) {
        if (safi == lower.afiUni) {
            return wilUni;
        }
        if (safi == lower.afiLab) {
            return wilUni;
        }
        if (safi == lower.afiMlt) {
            return wilMlt;
        }
        if (safi == lower.afiOtrL) {
            return wilOtrU;
        }
        if (safi == lower.afiOtrU) {
            return wilOtrU;
        }
        if (safi == lower.afiOtrM) {
            return wilOtrM;
        }
        if (safi == lower.afiOtrF) {
            return wilOtrF;
        }
        if (safi == lower.afiOtrS) {
            return wilOtrS;
        }
        if (safi == lower.afiFlw) {
            return wilFlw;
        }
        if (safi == lower.afiVpnU) {
            return wilVpnU;
        }
        if (safi == lower.afiVpnM) {
            return wilVpnM;
        }
        if (safi == lower.afiVpnF) {
            return wilVpnF;
        }
        if (safi == lower.afiVpoU) {
            return wilVpoU;
        }
        if (safi == lower.afiVpoM) {
            return wilVpoM;
        }
        if (safi == lower.afiVpoF) {
            return wilVpoF;
        }
        if (safi == lower.afiVpls) {
            return wilVpls;
        }
        if (safi == lower.afiMspw) {
            return wilMspw;
        }
        if (safi == lower.afiEvpn) {
            return wilEvpn;
        }
        if (safi == lower.afiMdt) {
            return wilMdt;
        }
        if (safi == lower.afiNsh) {
            return wilNsh;
        }
        if (safi == lower.afiSrte) {
            return wilSrte;
        }
        if (safi == lower.afiLnks) {
            return wilLnks;
        }
        if (safi == lower.afiMvpn) {
            return wilMvpn;
        }
        if (safi == lower.afiMvpo) {
            return wilMvpo;
        }
        logger.info("unknown safi (" + safi + ") requested");
        return null;
    }

    /**
     * get changed
     *
     * @param safi safi to query
     * @return table
     */
    public tabRoute<addrIP> getChanged(int safi) {
        if (safi == lower.afiUni) {
            return chgUni;
        }
        if (safi == lower.afiLab) {
            return chgUni;
        }
        if (safi == lower.afiMlt) {
            return chgMlt;
        }
        if (safi == lower.afiOtrL) {
            return chgOtrU;
        }
        if (safi == lower.afiOtrU) {
            return chgOtrU;
        }
        if (safi == lower.afiOtrM) {
            return chgOtrM;
        }
        if (safi == lower.afiOtrF) {
            return chgOtrF;
        }
        if (safi == lower.afiOtrS) {
            return chgOtrS;
        }
        if (safi == lower.afiFlw) {
            return chgFlw;
        }
        if (safi == lower.afiVpnU) {
            return chgVpnU;
        }
        if (safi == lower.afiVpnM) {
            return chgVpnM;
        }
        if (safi == lower.afiVpnF) {
            return chgVpnF;
        }
        if (safi == lower.afiVpoU) {
            return chgVpoU;
        }
        if (safi == lower.afiVpoM) {
            return chgVpoM;
        }
        if (safi == lower.afiVpoF) {
            return chgVpoF;
        }
        if (safi == lower.afiVpls) {
            return chgVpls;
        }
        if (safi == lower.afiMspw) {
            return chgMspw;
        }
        if (safi == lower.afiEvpn) {
            return chgEvpn;
        }
        if (safi == lower.afiMdt) {
            return chgMdt;
        }
        if (safi == lower.afiNsh) {
            return chgNsh;
        }
        if (safi == lower.afiSrte) {
            return chgSrte;
        }
        if (safi == lower.afiLnks) {
            return chgLnks;
        }
        if (safi == lower.afiMvpn) {
            return chgMvpn;
        }
        if (safi == lower.afiMvpo) {
            return chgMvpo;
        }
        logger.info("unknown safi (" + safi + ") requested");
        return null;
    }

    private void nextHopSelf(int afi, tabRouteAttr<addrIP> ntry, tabRouteEntry<addrIP> route) {
        if ((afi == lower.afiOtrM) || ((afi == lower.afiOtrU) && ((addrFams & rtrBgpParam.mskOtrL) == 0))) {
            ntry.nextHop = localOddr.copyBytes();
        } else {
            ntry.nextHop = localAddr.copyBytes();
        }
        ntry.labelRem = new ArrayList<Integer>();
        tabLabelEntry loc = ntry.labelLoc;
        if (loc == null) {
            ipFwd tab;
            if (ntry.rouTab == null) {
                if (afi == lower.afiOtrU) {
                    tab = lower.other.fwd;
                } else {
                    tab = lower.fwdCore;
                }
            } else {
                tab = ntry.rouTab;
            }
            tabRouteEntry<addrIP> org = tab.labeldR.find(route);
            if (org == null) {
                loc = tab.commonLabel;
            } else {
                loc = org.best.labelLoc;
            }
        }
        int val = loc.label;
        if (labelPop) {
            if ((afi == lower.afiUni) && ((addrFams & rtrBgpParam.mskLab) != 0) && (val == lower.fwdCore.commonLabel.label)) {
                val = ipMpls.labelImp;
            }
            if ((afi == lower.afiOtrU) && ((addrFams & rtrBgpParam.mskOtrL) != 0) && (val == lower.other.fwd.commonLabel.label)) {
                val = ipMpls.labelImp;
            }
        }
        ntry.labelRem.add(val);
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
        if (!traffEng) {
            ntry.bandwidth = 0;
        }
        if (!pmsiTun) {
            ntry.pmsiLab = 0;
            ntry.pmsiTyp = 0;
            ntry.pmsiTun = null;
        }
        if (!lnkSta) {
            ntry.linkStat = null;
        }
        if (!tunEnc) {
            ntry.tunelTyp = 0;
            ntry.tunelVal = null;
        }
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
            ntry.bierBeg = 0;
            ntry.bierOld = 0;
            ntry.bierSiz = 0;
            ntry.bierHdr = 0;
        }
        if (!leakAttr) {
            ntry.onlyCust = 0;
        }
        if (removePrivAsOut) {
            rtrBgpUtil.removePrivateAs(ntry.pathSeq);
            rtrBgpUtil.removePrivateAs(ntry.pathSet);
        }
        if (overridePeerOut) {
            rtrBgpUtil.replaceIntList(ntry.pathSeq, remoteAs, localAs);
            rtrBgpUtil.replaceIntList(ntry.pathSet, remoteAs, localAs);
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
            rtrBgpUtil.decodeAttribSet(ntry);
        }
        switch (leakRole) {
            case rtrBgpUtil.roleProv:
                if (ntry.best.onlyCust != 0) {
                    return null;
                }
                break;
            case rtrBgpUtil.roleRs:
                if (ntry.best.onlyCust != 0) {
                    return null;
                }
                break;
            case rtrBgpUtil.roleRsc:
                setCustOnly(ntry);
                break;
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
        nextHopSelf(afi, ntry);
        switch (peerType) {
            case rtrBgpUtil.peerExtrn:
            case rtrBgpUtil.peerServr:
                for (int i = 0; i < ntry.alts.size(); i++) {
                    tabRouteAttr<addrIP> attr = ntry.alts.get(i);
                    attr.pathSeq = tabLabel.prependLabel(attr.pathSeq, localAs);
                }
                break;
            case rtrBgpUtil.peerCnfed:
                for (int i = 0; i < ntry.alts.size(); i++) {
                    tabRouteAttr<addrIP> attr = ntry.alts.get(i);
                    attr.confSeq = tabLabel.prependLabel(attr.confSeq, localAs);
                    if (attr.locPref == 0) {
                        attr.locPref = 100;
                    }
                }
                break;
            case rtrBgpUtil.peerIntrn:
            case rtrBgpUtil.peerRflct:
                for (int i = 0; i < ntry.alts.size(); i++) {
                    tabRouteAttr<addrIP> attr = ntry.alts.get(i);
                    if (attr.locPref == 0) {
                        attr.locPref = 100;
                    }
                }
                break;
        }
        for (int i = 0; i < ntry.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(i);
            attr.segrouIdx = lower.segrouIdx;
            attr.bierIdx = lower.bierIdx;
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
            rtrBgpUtil.decodeAttribSet(ntry);
        }
        switch (leakRole) {
            case rtrBgpUtil.roleProv:
                if (ntry.best.onlyCust != 0) {
                    return null;
                }
                break;
            case rtrBgpUtil.roleRs:
                if (ntry.best.onlyCust != 0) {
                    return null;
                }
                break;
            case rtrBgpUtil.roleRsc:
                ntry = ntry.copyBytes(tabRoute.addType.altEcmp);
                setCustOnly(ntry);
                break;
            case rtrBgpUtil.roleCust:
                ntry = ntry.copyBytes(tabRoute.addType.altEcmp);
                setCustOnly(ntry);
                break;
            case rtrBgpUtil.rolePeer:
                if (ntry.best.onlyCust != 0) {
                    return null;
                }
                ntry = ntry.copyBytes(tabRoute.addType.altEcmp);
                setCustOnly(ntry);
                break;
            default:
                break;
        }
        if (!allowAsOut) {
            if (rtrBgpUtil.findIntList(ntry.best.pathSeq, remoteAs) >= 0) {
                return null;
            }
            if (rtrBgpUtil.findIntList(ntry.best.pathSet, remoteAs) >= 0) {
                return null;
            }
        }
        if (rtrBgpUtil.findIntList(ntry.best.stdComm, rtrBgpUtil.commNoAdvertise) >= 0) {
            return null;
        }
        switch (peerType) {
            case rtrBgpUtil.peerExtrn:
                if (rtrBgpUtil.findIntList(ntry.best.stdComm, rtrBgpUtil.commNoExport) >= 0) {
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
            case rtrBgpUtil.peerCnfed:
                if (rtrBgpUtil.findIntList(ntry.best.stdComm, rtrBgpUtil.commNoConfed) >= 0) {
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
                    case rtrBgpUtil.peerExtrn:
                    case rtrBgpUtil.peerServr:
                        if (!nxtHopUnchgd) {
                            nextHopSelf(afi, ntry);
                        }
                        break;
                }
                break;
            case rtrBgpUtil.peerServr:
                if (rtrBgpUtil.findIntList(ntry.best.stdComm, rtrBgpUtil.commNoExport) >= 0) {
                    return null;
                }
                ntry = ntry.copyBytes(tabRoute.addType.altEcmp);
                switch (ntry.best.rouSrc) {
                    case rtrBgpUtil.peerExtrn:
                        if (!nxtHopUnchgd) {
                            nextHopSelf(afi, ntry);
                        }
                        break;
                }
                break;
            default:
                return null;
        }
        for (int i = 0; i < ntry.alts.size(); i++) {
            tabRouteAttr<addrIP> attr = ntry.alts.get(i);
            clearAttribs(attr);
        }
        if (nxtHopSelf) {
            nextHopSelf(afi, ntry);
            return ntry;
        }
        if ((afi != lower.afiUni) || ((addrFams & rtrBgpParam.mskLab) == 0)) {
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

    /**
     * create needed prefix list
     *
     * @param cUni unicast
     * @param cMlt multicast
     * @param cOtrU other uni
     * @param cOtrM other multi
     * @param cOtrF other flow
     * @param cOtrS other srte
     * @param cFlw flowspec
     * @param cVpnU vpn uni
     * @param cVpnM vpn multi
     * @param cVpnF vpn flow
     * @param cVpoU ovpn uni
     * @param cVpoM ovpn multi
     * @param cVpoF ovpn flow
     * @param cVpls vpls
     * @param cMspw mspw
     * @param cEvpn evpn
     * @param cMdt mdt
     * @param cNsh nsh
     * @param cSrte srte
     * @param cLnks lnks
     * @param cMvpn mvpn
     * @param cMvpo omvpn
     */
    public void createNeeded(tabRoute<addrIP> cUni, tabRoute<addrIP> cMlt, tabRoute<addrIP> cOtrU,
            tabRoute<addrIP> cOtrM, tabRoute<addrIP> cOtrF, tabRoute<addrIP> cOtrS, tabRoute<addrIP> cFlw,
            tabRoute<addrIP> cVpnU, tabRoute<addrIP> cVpnM, tabRoute<addrIP> cVpnF,
            tabRoute<addrIP> cVpoU, tabRoute<addrIP> cVpoM, tabRoute<addrIP> cVpoF,
            tabRoute<addrIP> cVpls, tabRoute<addrIP> cMspw, tabRoute<addrIP> cEvpn,
            tabRoute<addrIP> cMdt, tabRoute<addrIP> cNsh, tabRoute<addrIP> cSrte,
            tabRoute<addrIP> cLnks, tabRoute<addrIP> cMvpn, tabRoute<addrIP> cMvpo) {
        tabRoute<addrIP> nUni = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nMlt = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nOtrU = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nOtrM = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nOtrF = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nOtrS = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nFlw = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nVpnU = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nVpnM = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nVpnF = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nVpoU = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nVpoM = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nVpoF = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nVpls = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nMspw = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nEvpn = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nMdt = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nNsh = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nSrte = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nLnks = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nMvpn = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nMvpo = new tabRoute<addrIP>("bgp");
        if (sendDefRou) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = rtrBgpUtil.defaultRoute(lower.afiUni);
            ntry.best.aggrRtr = new addrIP();
            ntry.best.aggrRtr.fromIPv4addr(lower.routerID);
            ntry.best.aggrAs = localAs;
            ntry = originatePrefix(lower.afiUni, ntry);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, nUni, lower.afiUni, remoteAs, ntry, true, roumapOut, roupolOut, prflstOut);
            ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = rtrBgpUtil.defaultRoute(lower.afiUni);
            ntry.best.aggrRtr = new addrIP();
            ntry.best.aggrRtr.fromIPv4addr(lower.routerID);
            ntry.best.aggrAs = localAs;
            ntry = originatePrefix(lower.afiMlt, ntry);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, nMlt, lower.afiMlt, remoteAs, ntry, true, roumapOut, roupolOut, prflstOut);
        }
        if (sendOtrDefRou) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = rtrBgpUtil.defaultRoute(lower.afiOtrU);
            ntry.best.aggrRtr = new addrIP();
            ntry.best.aggrRtr.fromIPv4addr(lower.routerID);
            ntry.best.aggrAs = localAs;
            ntry = originatePrefix(lower.afiOtrU, ntry);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, nOtrU, lower.afiOtrU, remoteAs, ntry, true, oroumapOut, oroupolOut, oprflstOut);
            ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = rtrBgpUtil.defaultRoute(lower.afiOtrU);
            ntry.best.aggrRtr = new addrIP();
            ntry.best.aggrRtr.fromIPv4addr(lower.routerID);
            ntry.best.aggrAs = localAs;
            ntry = originatePrefix(lower.afiOtrM, ntry);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, nOtrM, lower.afiOtrM, remoteAs, ntry, true, oroumapOut, oroupolOut, oprflstOut);
        }
        for (int i = 0; i < lower.routerRedistedU.size(); i++) {
            tabRouteEntry<addrIP> ntry = lower.routerRedistedU.get(i);
            if (ntry == null) {
                continue;
            }
            ntry = originatePrefix(lower.afiUni, ntry);
            tabRoute.addUpdatedEntry(tabRoute.addType.altEcmp, nUni, lower.afiUni, remoteAs, ntry, true, roumapOut, roupolOut, prflstOut);
        }
        for (int i = 0; i < lower.routerRedistedM.size(); i++) {
            tabRouteEntry<addrIP> ntry = lower.routerRedistedM.get(i);
            if (ntry == null) {
                continue;
            }
            ntry = originatePrefix(lower.afiMlt, ntry);
            tabRoute.addUpdatedEntry(tabRoute.addType.altEcmp, nMlt, lower.afiMlt, remoteAs, ntry, true, roumapOut, roupolOut, prflstOut);
        }
        for (int i = 0; i < lower.routerRedistedF.size(); i++) {
            tabRouteEntry<addrIP> ntry = lower.routerRedistedF.get(i);
            if (ntry == null) {
                continue;
            }
            ntry = originatePrefix(lower.afiFlw, ntry);
            tabRoute.addUpdatedEntry(tabRoute.addType.altEcmp, nFlw, lower.afiFlw, remoteAs, ntry, true, vroumapOut, vroupolOut, null);
        }
        readvertTable(lower.afiUni, nUni, cUni, roumapOut, roupolOut, prflstOut);
        readvertTable(lower.afiMlt, nMlt, cMlt, roumapOut, roupolOut, prflstOut);
        readvertTable(lower.afiOtrU, nOtrU, cOtrU, oroumapOut, oroupolOut, oprflstOut);
        readvertTable(lower.afiOtrM, nOtrM, cOtrM, oroumapOut, oroupolOut, oprflstOut);
        tabRoute<addrIP> tab = new tabRoute<addrIP>("agg");
        lower.routerDoAggregates(lower.afiUni, nUni, tab, lower.fwdCore.commonLabel, lower.routerID, lower.localAs);
        for (int i = 0; i < tab.size(); i++) {
            tabRouteEntry<addrIP> ntry = originatePrefix(lower.afiUni, tab.get(i));
            tabRoute.addUpdatedEntry(tabRoute.addType.better, nUni, lower.afiUni, remoteAs, ntry, true, roumapOut, roupolOut, prflstOut);
        }
        tab = new tabRoute<addrIP>("agg");
        lower.routerDoAggregates(lower.afiMlt, nMlt, tab, lower.fwdCore.commonLabel, lower.routerID, lower.localAs);
        for (int i = 0; i < tab.size(); i++) {
            tabRouteEntry<addrIP> ntry = originatePrefix(lower.afiMlt, tab.get(i));
            tabRoute.addUpdatedEntry(tabRoute.addType.better, nMlt, lower.afiMlt, remoteAs, ntry, true, roumapOut, roupolOut, prflstOut);
        }
        tab = new tabRoute<addrIP>("agg");
        lower.routerDoAggregates(lower.afiOtrU, nOtrU, tab, lower.other.fwd.commonLabel, lower.routerID, lower.localAs);
        for (int i = 0; i < tab.size(); i++) {
            tabRouteEntry<addrIP> ntry = originatePrefix(lower.afiOtrU, tab.get(i));
            tabRoute.addUpdatedEntry(tabRoute.addType.better, nOtrU, lower.afiOtrU, remoteAs, ntry, true, oroumapOut, oroupolOut, oprflstOut);
        }
        tab = new tabRoute<addrIP>("agg");
        lower.routerDoAggregates(lower.afiOtrM, nOtrM, tab, lower.other.fwd.commonLabel, lower.routerID, lower.localAs);
        for (int i = 0; i < tab.size(); i++) {
            tabRouteEntry<addrIP> ntry = originatePrefix(lower.afiOtrM, tab.get(i));
            tabRoute.addUpdatedEntry(tabRoute.addType.better, nOtrM, lower.afiOtrM, remoteAs, ntry, true, oroumapOut, oroupolOut, oprflstOut);
        }
        importTable(lower.afiOtrF, nOtrF, cOtrF, wroumapOut, wroupolOut, null);
        importTable(lower.afiOtrS, nOtrS, cOtrS, wroumapOut, wroupolOut, null);
        importTable(lower.afiFlw, nFlw, cFlw, vroumapOut, vroupolOut, null);
        importTable(lower.afiVpnU, nVpnU, cVpnU, vroumapOut, vroupolOut, null);
        importTable(lower.afiVpnM, nVpnM, cVpnM, vroumapOut, vroupolOut, null);
        importTable(lower.afiVpnF, nVpnF, cVpnF, vroumapOut, vroupolOut, null);
        importTable(lower.afiVpoU, nVpoU, cVpoU, wroumapOut, wroupolOut, null);
        importTable(lower.afiVpoM, nVpoM, cVpoM, wroumapOut, wroupolOut, null);
        importTable(lower.afiVpoF, nVpoF, cVpoF, wroumapOut, wroupolOut, null);
        importTable(lower.afiVpls, nVpls, cVpls, vroumapOut, vroupolOut, null);
        importTable(lower.afiMspw, nMspw, cMspw, vroumapOut, vroupolOut, null);
        importTable(lower.afiEvpn, nEvpn, cEvpn, vroumapOut, vroupolOut, null);
        importTable(lower.afiMdt, nMdt, cMdt, vroumapOut, vroupolOut, null);
        importTable(lower.afiNsh, nNsh, cNsh, vroumapOut, vroupolOut, null);
        importTable(lower.afiSrte, nSrte, cSrte, vroumapOut, vroupolOut, null);
        importTable(lower.afiLnks, nLnks, cLnks, vroumapOut, vroupolOut, null);
        importTable(lower.afiMvpn, nMvpn, cMvpn, vroumapOut, vroupolOut, null);
        importTable(lower.afiMvpo, nMvpo, cMvpo, wroumapOut, wroupolOut, null);
        wilUni = nUni;
        wilMlt = nMlt;
        wilOtrU = nOtrU;
        wilOtrM = nOtrM;
        wilOtrF = nOtrF;
        wilOtrS = nOtrS;
        wilFlw = nFlw;
        wilVpnU = nVpnU;
        wilVpnM = nVpnM;
        wilVpnF = nVpnF;
        wilVpoU = nVpoU;
        wilVpoM = nVpoM;
        wilVpoF = nVpoF;
        wilVpls = nVpls;
        wilMspw = nMspw;
        wilEvpn = nEvpn;
        wilMdt = nMdt;
        wilNsh = nNsh;
        wilSrte = nSrte;
        wilLnks = nLnks;
        wilMvpn = nMvpn;
        wilMvpo = nMvpo;
    }

}
