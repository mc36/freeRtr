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
import org.freertr.util.logger;

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
    public tabRoute<addrIP> wilOuni = new tabRoute<addrIP>("tx");

    /**
     * willing other multicast prefixes
     */
    public tabRoute<addrIP> wilOmlt = new tabRoute<addrIP>("tx");

    /**
     * willing other flowspec prefixes
     */
    public tabRoute<addrIP> wilOflw = new tabRoute<addrIP>("tx");

    /**
     * willing other srte prefixes
     */
    public tabRoute<addrIP> wilOsrt = new tabRoute<addrIP>("tx");

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
     * willing rpd prefixes
     */
    public tabRoute<addrIP> wilRpd = new tabRoute<addrIP>("tx");

    /**
     * willing sdwan prefixes
     */
    public tabRoute<addrIP> wilSdw = new tabRoute<addrIP>("tx");

    /**
     * willing spf prefixes
     */
    public tabRoute<addrIP> wilSpf = new tabRoute<addrIP>("tx");

    /**
     * willing rtfilter prefixes
     */
    public tabRoute<addrIP> wilRtf = new tabRoute<addrIP>("tx");

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
     * willing mtree prefixes
     */
    public tabRoute<addrIP> wilMtre = new tabRoute<addrIP>("tx");

    /**
     * willing other mtree prefixes
     */
    public tabRoute<addrIP> wilMtro = new tabRoute<addrIP>("tx");

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
    public tabRoute<addrIP> chgOuni = new tabRoute<addrIP>("chg");

    /**
     * changed other multicast prefixes
     */
    public tabRoute<addrIP> chgOmlt = new tabRoute<addrIP>("chg");

    /**
     * changed other flowspec prefixes
     */
    public tabRoute<addrIP> chgOflw = new tabRoute<addrIP>("chg");

    /**
     * changed other srte prefixes
     */
    public tabRoute<addrIP> chgOsrt = new tabRoute<addrIP>("chg");

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
     * changed rpd prefixes
     */
    public tabRoute<addrIP> chgRpd = new tabRoute<addrIP>("chg");

    /**
     * changed sdwan prefixes
     */
    public tabRoute<addrIP> chgSdw = new tabRoute<addrIP>("chg");

    /**
     * changed spf prefixes
     */
    public tabRoute<addrIP> chgSpf = new tabRoute<addrIP>("chg");

    /**
     * changed rtfilter prefixes
     */
    public tabRoute<addrIP> chgRtf = new tabRoute<addrIP>("chg");

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
     * changed mtre prefixes
     */
    public tabRoute<addrIP> chgMtre = new tabRoute<addrIP>("chg");

    /**
     * changed other mtro prefixes
     */
    public tabRoute<addrIP> chgMtro = new tabRoute<addrIP>("chg");

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
        l.add("safi|" + mask2string(addrFams));
        l.add("local|" + localAddr);
        l.add("other|" + localOddr);
        l.add("unicast advertise|" + wilUni.size() + ", list=" + chgUni.size());
        l.add("multicast advertise|" + wilMlt.size() + ", list=" + chgMlt.size());
        l.add("ouni advertise|" + wilOuni.size() + ", list=" + chgOuni.size());
        l.add("omlt advertise|" + wilOmlt.size() + ", list=" + chgOmlt.size());
        l.add("oflw advertise|" + wilOflw.size() + ", list=" + chgOflw.size());
        l.add("osrt advertise|" + wilOsrt.size() + ", list=" + chgOsrt.size());
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
        l.add("rpd advertise|" + wilRpd.size() + ", list=" + chgRpd.size());
        l.add("sdwan advertise|" + wilSdw.size() + ", list=" + chgSdw.size());
        l.add("spf advertise|" + wilSpf.size() + ", list=" + chgSpf.size());
        l.add("rtfilter advertise|" + wilRtf.size() + ", list=" + chgRtf.size());
        l.add("srte advertise|" + wilSrte.size() + ", list=" + chgSrte.size());
        l.add("linkstate advertise|" + wilLnks.size() + ", list=" + chgLnks.size());
        l.add("mvpn advertise|" + wilMvpn.size() + ", list=" + chgMvpn.size());
        l.add("omvpn advertise|" + wilMvpo.size() + ", list=" + chgMvpo.size());
        l.add("mtree advertise|" + wilMtre.size() + ", list=" + chgMtre.size());
        l.add("omtree advertise|" + wilMtro.size() + ", list=" + chgMtro.size());
        l.add("version|" + minversion + " of " + lower.compRound);
        return l;
    }

    /**
     * get willing
     *
     * @param mask safi to query
     * @param safi safi to query
     * @return table
     */
    public tabRoute<addrIP> getWilling(long mask, int safi) {
        if (mask == rtrBgpParam.mskUni) {
            return wilUni;
        }
        if (mask == rtrBgpParam.mskLab) {
            return wilUni;
        }
        if (mask == rtrBgpParam.mskCtp) {
            return wilUni;
        }
        if (mask == rtrBgpParam.mskCar) {
            return wilUni;
        }
        if (mask == rtrBgpParam.mskMlt) {
            return wilMlt;
        }
        if (mask == rtrBgpParam.mskOlab) {
            return wilOuni;
        }
        if (mask == rtrBgpParam.mskOctp) {
            return wilOuni;
        }
        if (mask == rtrBgpParam.mskOcar) {
            return wilOuni;
        }
        if (mask == rtrBgpParam.mskOuni) {
            return wilOuni;
        }
        if (mask == rtrBgpParam.mskOmlt) {
            return wilOmlt;
        }
        if (mask == rtrBgpParam.mskOflw) {
            return wilOflw;
        }
        if (mask == rtrBgpParam.mskOsrt) {
            return wilOsrt;
        }
        if (mask == rtrBgpParam.mskFlw) {
            return wilFlw;
        }
        if (mask == rtrBgpParam.mskVpnU) {
            return wilVpnU;
        }
        if (mask == rtrBgpParam.mskVpnM) {
            return wilVpnM;
        }
        if (mask == rtrBgpParam.mskVpnF) {
            return wilVpnF;
        }
        if (mask == rtrBgpParam.mskVpoU) {
            return wilVpoU;
        }
        if (mask == rtrBgpParam.mskVpoM) {
            return wilVpoM;
        }
        if (mask == rtrBgpParam.mskVpoF) {
            return wilVpoF;
        }
        if (mask == rtrBgpParam.mskVpls) {
            return wilVpls;
        }
        if (mask == rtrBgpParam.mskMspw) {
            return wilMspw;
        }
        if (mask == rtrBgpParam.mskEvpn) {
            return wilEvpn;
        }
        if (mask == rtrBgpParam.mskMdt) {
            return wilMdt;
        }
        if (mask == rtrBgpParam.mskNsh) {
            return wilNsh;
        }
        if (mask == rtrBgpParam.mskRpd) {
            return wilRpd;
        }
        if (mask == rtrBgpParam.mskSdw) {
            return wilSdw;
        }
        if (mask == rtrBgpParam.mskSpf) {
            return wilSpf;
        }
        if (mask == rtrBgpParam.mskRtf) {
            return wilRtf;
        }
        if (mask == rtrBgpParam.mskSrte) {
            return wilSrte;
        }
        if (mask == rtrBgpParam.mskLnks) {
            return wilLnks;
        }
        if (mask == rtrBgpParam.mskMvpn) {
            return wilMvpn;
        }
        if (mask == rtrBgpParam.mskMvpo) {
            return wilMvpo;
        }
        if (mask == rtrBgpParam.mskMtre) {
            return wilMtre;
        }
        if (mask == rtrBgpParam.mskMtro) {
            return wilMtro;
        }
        logger.info("unknown safi (" + safi + ") requested");
        return null;
    }

    /**
     * get changed
     *
     * @param mask safi to query
     * @param safi safi to query
     * @return table
     */
    public tabRoute<addrIP> getChanged(long mask, int safi) {
        if (mask == rtrBgpParam.mskUni) {
            return chgUni;
        }
        if (mask == rtrBgpParam.mskLab) {
            return chgUni;
        }
        if (mask == rtrBgpParam.mskCtp) {
            return chgUni;
        }
        if (mask == rtrBgpParam.mskCar) {
            return chgUni;
        }
        if (mask == rtrBgpParam.mskMlt) {
            return chgMlt;
        }
        if (mask == rtrBgpParam.mskOlab) {
            return chgOuni;
        }
        if (mask == rtrBgpParam.mskOctp) {
            return chgOuni;
        }
        if (mask == rtrBgpParam.mskOcar) {
            return chgOuni;
        }
        if (mask == rtrBgpParam.mskOuni) {
            return chgOuni;
        }
        if (mask == rtrBgpParam.mskOmlt) {
            return chgOmlt;
        }
        if (mask == rtrBgpParam.mskOflw) {
            return chgOflw;
        }
        if (mask == rtrBgpParam.mskOsrt) {
            return chgOsrt;
        }
        if (mask == rtrBgpParam.mskFlw) {
            return chgFlw;
        }
        if (mask == rtrBgpParam.mskVpnU) {
            return chgVpnU;
        }
        if (mask == rtrBgpParam.mskVpnM) {
            return chgVpnM;
        }
        if (mask == rtrBgpParam.mskVpnF) {
            return chgVpnF;
        }
        if (mask == rtrBgpParam.mskVpoU) {
            return chgVpoU;
        }
        if (mask == rtrBgpParam.mskVpoM) {
            return chgVpoM;
        }
        if (mask == rtrBgpParam.mskVpoF) {
            return chgVpoF;
        }
        if (mask == rtrBgpParam.mskVpls) {
            return chgVpls;
        }
        if (mask == rtrBgpParam.mskMspw) {
            return chgMspw;
        }
        if (mask == rtrBgpParam.mskEvpn) {
            return chgEvpn;
        }
        if (mask == rtrBgpParam.mskMdt) {
            return chgMdt;
        }
        if (mask == rtrBgpParam.mskNsh) {
            return chgNsh;
        }
        if (mask == rtrBgpParam.mskRpd) {
            return chgRpd;
        }
        if (mask == rtrBgpParam.mskSdw) {
            return chgSdw;
        }
        if (mask == rtrBgpParam.mskSpf) {
            return chgSpf;
        }
        if (mask == rtrBgpParam.mskRtf) {
            return chgRtf;
        }
        if (mask == rtrBgpParam.mskSrte) {
            return chgSrte;
        }
        if (mask == rtrBgpParam.mskLnks) {
            return chgLnks;
        }
        if (mask == rtrBgpParam.mskMvpn) {
            return chgMvpn;
        }
        if (mask == rtrBgpParam.mskMvpo) {
            return chgMvpo;
        }
        if (mask == rtrBgpParam.mskMtre) {
            return chgMtre;
        }
        if (mask == rtrBgpParam.mskMtro) {
            return chgMtro;
        }
        logger.info("unknown safi (" + safi + ") requested");
        return null;
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
        if ((afi == lower.afiOmlt) || ((afi == lower.afiOuni) && lower.other.routerVpn && ((addrFams & (rtrBgpParam.mskOlab | rtrBgpParam.mskOctp | rtrBgpParam.mskOcar)) == 0))) {
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
        if ((afi != lower.afiUni) || ((addrFams & (rtrBgpParam.mskLab | rtrBgpParam.mskCtp | rtrBgpParam.mskCar)) == 0)) {
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
     */
    public void createNeeded() {
        tabRoute<addrIP> nUni = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nMlt = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nOuni = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nOmlt = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nOflw = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nOsrt = new tabRoute<addrIP>("bgp");
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
        tabRoute<addrIP> nRpd = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nSdw = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nSpf = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nRtf = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nSrte = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nLnks = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nMvpn = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nMvpo = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nMtre = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nMtro = new tabRoute<addrIP>("bgp");
        if (sendDefRou || lower.defRou) {
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
        if (sendOtrDefRou || lower.other.defRou) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = rtrBgpUtil.defaultRoute(lower.afiOuni);
            ntry.best.aggrRtr = new addrIP();
            ntry.best.aggrRtr.fromIPv4addr(lower.routerID);
            ntry.best.aggrAs = localAs;
            ntry = originatePrefix(lower.afiOuni, ntry);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, nOuni, lower.afiOuni, remoteAs, ntry, true, oroumapOut, oroupolOut, oprflstOut);
            ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = rtrBgpUtil.defaultRoute(lower.afiOuni);
            ntry.best.aggrRtr = new addrIP();
            ntry.best.aggrRtr.fromIPv4addr(lower.routerID);
            ntry.best.aggrAs = localAs;
            ntry = originatePrefix(lower.afiOmlt, ntry);
            tabRoute.addUpdatedEntry(tabRoute.addType.better, nOmlt, lower.afiOmlt, remoteAs, ntry, true, oroumapOut, oroupolOut, oprflstOut);
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
        readvertTable(lower.afiUni, nUni, lower.newlyUni, roumapOut, roupolOut, prflstOut);
        readvertTable(lower.afiMlt, nMlt, lower.newlyMlt, roumapOut, roupolOut, prflstOut);
        readvertTable(lower.afiOuni, nOuni, lower.newlyOuni, oroumapOut, oroupolOut, oprflstOut);
        readvertTable(lower.afiOmlt, nOmlt, lower.newlyOmlt, oroumapOut, oroupolOut, oprflstOut);
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
        lower.routerDoAggregates(lower.afiOuni, nOuni, tab, lower.other.fwd.commonLabel, lower.routerID, lower.localAs);
        for (int i = 0; i < tab.size(); i++) {
            tabRouteEntry<addrIP> ntry = originatePrefix(lower.afiOuni, tab.get(i));
            tabRoute.addUpdatedEntry(tabRoute.addType.better, nOuni, lower.afiOuni, remoteAs, ntry, true, oroumapOut, oroupolOut, oprflstOut);
        }
        tab = new tabRoute<addrIP>("agg");
        lower.routerDoAggregates(lower.afiOmlt, nOmlt, tab, lower.other.fwd.commonLabel, lower.routerID, lower.localAs);
        for (int i = 0; i < tab.size(); i++) {
            tabRouteEntry<addrIP> ntry = originatePrefix(lower.afiOmlt, tab.get(i));
            tabRoute.addUpdatedEntry(tabRoute.addType.better, nOmlt, lower.afiOmlt, remoteAs, ntry, true, oroumapOut, oroupolOut, oprflstOut);
        }
        importTable(lower.afiOflw, nOflw, lower.newlyOflw, wroumapOut, wroupolOut, null);
        importTable(lower.afiOsrt, nOsrt, lower.newlyOsrt, wroumapOut, wroupolOut, null);
        importTable(lower.afiFlw, nFlw, lower.newlyFlw, vroumapOut, vroupolOut, null);
        importTable(lower.afiVpnU, nVpnU, lower.newlyVpnU, vroumapOut, vroupolOut, null);
        importTable(lower.afiVpnM, nVpnM, lower.newlyVpnM, vroumapOut, vroupolOut, null);
        importTable(lower.afiVpnF, nVpnF, lower.newlyVpnF, vroumapOut, vroupolOut, null);
        importTable(lower.afiVpoU, nVpoU, lower.newlyVpoU, wroumapOut, wroupolOut, null);
        importTable(lower.afiVpoM, nVpoM, lower.newlyVpoM, wroumapOut, wroupolOut, null);
        importTable(lower.afiVpoF, nVpoF, lower.newlyVpoF, wroumapOut, wroupolOut, null);
        importTable(lower.afiVpls, nVpls, lower.newlyVpls, eroumapOut, eroupolOut, null);
        importTable(lower.afiMspw, nMspw, lower.newlyMspw, eroumapOut, eroupolOut, null);
        importTable(lower.afiEvpn, nEvpn, lower.newlyEvpn, eroumapOut, eroupolOut, null);
        importTable(lower.afiMdt, nMdt, lower.newlyMdt, vroumapOut, vroupolOut, null);
        importTable(lower.afiNsh, nNsh, lower.newlyNsh, vroumapOut, vroupolOut, null);
        importTable(lower.afiRpd, nRpd, lower.newlyRpd, vroumapOut, vroupolOut, null);
        importTable(lower.afiSdw, nSdw, lower.newlySdw, vroumapOut, vroupolOut, null);
        importTable(lower.afiSpf, nSpf, lower.newlySpf, vroumapOut, vroupolOut, null);
        importTable(lower.afiRtf, nRtf, lower.newlyRtf, vroumapOut, vroupolOut, null);
        importTable(lower.afiSrte, nSrte, lower.newlySrte, vroumapOut, vroupolOut, null);
        importTable(lower.afiLnks, nLnks, lower.newlyLnks, vroumapOut, vroupolOut, null);
        importTable(lower.afiMvpn, nMvpn, lower.newlyMvpn, vroumapOut, vroupolOut, null);
        importTable(lower.afiMvpo, nMvpo, lower.newlyMvpo, wroumapOut, wroupolOut, null);
        importTable(lower.afiMtre, nMtre, lower.newlyMtre, vroumapOut, vroupolOut, null);
        importTable(lower.afiMtro, nMtro, lower.newlyMtro, wroumapOut, wroupolOut, null);
        if (peerType != rtrBgpUtil.peerRflct) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = new addrPrefix<addrIP>(new addrIP(), 0);
            nRtf.del(ntry);
        }
        wilUni = nUni;
        wilMlt = nMlt;
        wilOuni = nOuni;
        wilOmlt = nOmlt;
        wilOflw = nOflw;
        wilOsrt = nOsrt;
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
        wilRpd = nRpd;
        wilSdw = nSdw;
        wilSpf = nSpf;
        wilRtf = nRtf;
        wilSrte = nSrte;
        wilLnks = nLnks;
        wilMvpn = nMvpn;
        wilMvpo = nMvpo;
        wilMtre = nMtre;
        wilMtro = nMtro;
    }

}
