package rtr;

import addr.addrIP;
import java.util.ArrayList;
import java.util.List;
import tab.tabLabel;
import tab.tabLabelBier;
import tab.tabLabelNtry;
import tab.tabRoute;
import tab.tabRouteEntry;

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
     * willing other prefixes
     */
    public tabRoute<addrIP> wilOtr = new tabRoute<addrIP>("tx");

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
     * changed other prefixes
     */
    public tabRoute<addrIP> chgOtr = new tabRoute<addrIP>("chg");

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

    public void getConfig(List<String> l, String beg, boolean filter) {
        l.addAll(getParamCfg(beg, "group " + groupNum + " ", filter));
    }

    /**
     * get status of group
     *
     * @param l list to append
     */
    public void getStatus(List<String> l) {
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
        l.add("peers =" + a);
        l.add("type = " + rtrBgpUtil.peerType2string(peerType));
        l.add("safi = " + mask2string(addrFams));
        l.add("local = " + localAddr);
        l.add("unicast advertise = " + wilUni.size() + ", list=" + chgUni.size());
        l.add("multicast advertise = " + wilMlt.size() + ", list=" + chgMlt.size());
        l.add("other advertise = " + wilOtr.size() + ", list=" + chgOtr.size());
        l.add("flowspec advertise = " + wilFlw.size() + ", list=" + chgFlw.size());
        l.add("vpnuni advertise = " + wilVpnU.size() + ", list=" + chgVpnU.size());
        l.add("vpnmlt advertise = " + wilVpnM.size() + ", list=" + chgVpnM.size());
        l.add("vpnflw advertise = " + wilVpnF.size() + ", list=" + chgVpnF.size());
        l.add("ovpnuni advertise = " + wilVpoU.size() + ", list=" + chgVpoU.size());
        l.add("ovpnmlt advertise = " + wilVpoM.size() + ", list=" + chgVpoM.size());
        l.add("ovpnflw advertise = " + wilVpoF.size() + ", list=" + chgVpoF.size());
        l.add("vpls advertise = " + wilVpls.size() + ", list=" + chgVpls.size());
        l.add("mspw advertise = " + wilMspw.size() + ", list=" + chgMspw.size());
        l.add("evpn advertise = " + wilEvpn.size() + ", list=" + chgEvpn.size());
        l.add("mdt advertise = " + wilMdt.size() + ", list=" + chgMdt.size());
        l.add("mvpn advertise = " + wilMvpn.size() + ", list=" + chgMvpn.size());
        l.add("omvpn advertise = " + wilMvpo.size() + ", list=" + chgMvpo.size());
        l.add("version = " + minversion + " of " + lower.compRound);
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
        if (safi == lower.afiOtr) {
            return wilOtr;
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
        if (safi == lower.afiMvpn) {
            return wilMvpn;
        }
        if (safi == lower.afiMvpo) {
            return wilMvpo;
        }
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
        if (safi == lower.afiOtr) {
            return chgOtr;
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
        if (safi == lower.afiMvpn) {
            return chgMvpn;
        }
        if (safi == lower.afiMvpo) {
            return chgMvpo;
        }
        return null;
    }

    private void nextHopSelf(tabRouteEntry<addrIP> ntry) {
        ntry.nextHop = localAddr.copyBytes();
        ntry.labelRem = new ArrayList<Integer>();
        tabLabelNtry loc = ntry.labelLoc;
        if (loc == null) {
            if (ntry.rouTab == null) {
                loc = lower.fwdCore.commonLabel;
            } else {
                loc = ntry.rouTab.commonLabel;
            }
        }
        ntry.labelRem.add(loc.getValue());
        if (lower.segrouLab != null) {
            ntry.segRoutS = lower.segrouMax;
            ntry.segRoutB = lower.segrouLab[0].getValue();
        }
        if (lower.bierLab != null) {
            ntry.bierS = tabLabelBier.num2bsl(lower.bierLen);
            ntry.bierR = lower.bierLab.length;
            ntry.bierB = lower.bierLab[0].getValue();
        }
    }

    private void updateAttribs(tabRouteEntry<addrIP> ntry) {
        if (intVpnClnt) {
            rtrBgpUtil.decodeAttribSet(ntry);
        }
    }

    private void clearAttribs(tabRouteEntry<addrIP> ntry) {
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
        if (!segRout) {
            ntry.segRoutI = 0;
            ntry.segRoutB = 0;
            ntry.segRoutS = 0;
        }
        if (!bier) {
            ntry.bierI = 0;
            ntry.bierB = 0;
            ntry.bierR = 0;
            ntry.bierS = 0;
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
        ntry.iface = null;
    }

    /**
     * originate prefix
     *
     * @param ntry prefix
     */
    public void originatePrefix(tabRouteEntry<addrIP> ntry) {
        updateAttribs(ntry);
        nextHopSelf(ntry);
        switch (peerType) {
            case rtrBgpUtil.peerExtrn:
            case rtrBgpUtil.peerServr:
                ntry.pathSeq = tabLabel.prependLabel(ntry.pathSeq, localAs);
                break;
            case rtrBgpUtil.peerCnfed:
                ntry.confSeq = tabLabel.prependLabel(ntry.confSeq, localAs);
                if (ntry.locPref == 0) {
                    ntry.locPref = 100;
                }
                break;
            case rtrBgpUtil.peerIntrn:
            case rtrBgpUtil.peerRflct:
                if (ntry.locPref == 0) {
                    ntry.locPref = 100;
                }
                break;
        }
        ntry.segRoutI = lower.segrouIdx;
        ntry.bierI = lower.bierIdx;
        clearAttribs(ntry);
    }

    /**
     * readvertise prefix
     *
     * @param nhs set next hop self
     * @param ntry prefix
     * @return false on success, true on failure
     */
    public boolean readvertPrefix(boolean nhs, tabRouteEntry<addrIP> ntry) {
        updateAttribs(ntry);
        if (!allowAsOut) {
            if (rtrBgpUtil.findIntList(ntry.pathSeq, remoteAs) >= 0) {
                return true;
            }
            if (rtrBgpUtil.findIntList(ntry.pathSet, remoteAs) >= 0) {
                return true;
            }
        }
        if (rtrBgpUtil.findIntList(ntry.stdComm, rtrBgpUtil.commNoAdvertise) >= 0) {
            return true;
        }
        switch (peerType) {
            case rtrBgpUtil.peerExtrn:
                if (rtrBgpUtil.findIntList(ntry.stdComm, rtrBgpUtil.commNoExport) >= 0) {
                    return true;
                }
                ntry.pathSeq = tabLabel.prependLabel(ntry.pathSeq, localAs);
                if (ntry.pathSeq.size() > 1) {
                    ntry.metric = 0;
                }
                ntry.locPref = 0;
                ntry.originator = null;
                ntry.clustList = null;
                ntry.confSeq = null;
                ntry.confSet = null;
                if (!nxtHopUnchgd) {
                    nextHopSelf(ntry);
                }
                break;
            case rtrBgpUtil.peerCnfed:
                if (rtrBgpUtil.findIntList(ntry.stdComm, rtrBgpUtil.commNoConfed) >= 0) {
                    return true;
                }
                switch (ntry.rouSrc) {
                    case rtrBgpUtil.peerExtrn:
                    case rtrBgpUtil.peerServr:
                        if (!nxtHopUnchgd) {
                            nextHopSelf(ntry);
                        }
                        break;
                }
                ntry.confSeq = tabLabel.prependLabel(ntry.confSeq, localAs);
                ntry.originator = null;
                ntry.clustList = null;
                break;
            case rtrBgpUtil.peerIntrn:
                switch (ntry.rouSrc) {
                    case rtrBgpUtil.peerIntrn:
                        return true;
                    case rtrBgpUtil.peerExtrn:
                    case rtrBgpUtil.peerServr:
                        if (!nxtHopUnchgd) {
                            nextHopSelf(ntry);
                        }
                        break;
                }
                break;
            case rtrBgpUtil.peerRflct:
                switch (ntry.rouSrc) {
                    case rtrBgpUtil.peerExtrn:
                    case rtrBgpUtil.peerServr:
                        if (!nxtHopUnchgd) {
                            nextHopSelf(ntry);
                        }
                        break;
                }
                break;
            case rtrBgpUtil.peerServr:
                if (rtrBgpUtil.findIntList(ntry.stdComm, rtrBgpUtil.commNoExport) >= 0) {
                    return true;
                }
                ntry.locPref = 0;
                ntry.originator = null;
                ntry.clustList = null;
                ntry.confSeq = null;
                ntry.confSet = null;
                switch (ntry.rouSrc) {
                    case rtrBgpUtil.peerExtrn:
                        if (!nxtHopUnchgd) {
                            nextHopSelf(ntry);
                        }
                        break;
                }
                break;
        }
        clearAttribs(ntry);
        if (nxtHopSelf) {
            nextHopSelf(ntry);
        }
        if (nhs && ((addrFams & rtrBgpParam.mskLab) != 0) && (ntry.labelRem == null)) {
            nextHopSelf(ntry);
        }
        return false;
    }

    private void readvertTable(boolean nhs, tabRoute<addrIP> tab, tabRoute<addrIP> cmp) {
        for (int i = 0; i < cmp.size(); i++) {
            tabRouteEntry<addrIP> ntry = cmp.get(i);
            ntry = ntry.copyBytes();
            if (ntry.rouSrc == rtrBgpUtil.peerOriginate) {
                originatePrefix(ntry);
            } else if (readvertPrefix(nhs, ntry)) {
                continue;
            }
            tabRoute.addUpdatedEntry(2, tab, ntry, roumapOut, roupolOut, prflstOut);
        }
    }

    private void importTable(tabRoute<addrIP> tab, tabRoute<addrIP> imp) {
        for (int i = 0; i < imp.size(); i++) {
            tabRouteEntry<addrIP> ntry = imp.get(i);
            ntry = ntry.copyBytes();
            if (ntry.rouSrc == rtrBgpUtil.peerOriginate) {
                originatePrefix(ntry);
            } else if (readvertPrefix(false, ntry)) {
                continue;
            }
            tabRoute.addUpdatedEntry(2, tab, ntry, voumapOut, voupolOut, null);
        }
    }

    /**
     * create needed prefix list
     *
     * @param cUni unicast
     * @param cMlt multicast
     * @param cOtr other
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
     * @param cMvpn mvpn
     * @param cMvpo omvpn
     */
    public void createNeeded(tabRoute<addrIP> cUni, tabRoute<addrIP> cMlt, tabRoute<addrIP> cOtr, tabRoute<addrIP> cFlw,
            tabRoute<addrIP> cVpnU, tabRoute<addrIP> cVpnM, tabRoute<addrIP> cVpnF,
            tabRoute<addrIP> cVpoU, tabRoute<addrIP> cVpoM, tabRoute<addrIP> cVpoF,
            tabRoute<addrIP> cVpls, tabRoute<addrIP> cMspw, tabRoute<addrIP> cEvpn,
            tabRoute<addrIP> cMdt, tabRoute<addrIP> cMvpn, tabRoute<addrIP> cMvpo) {
        tabRoute<addrIP> nUni = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nMlt = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nOtr = new tabRoute<addrIP>("bgp");
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
        tabRoute<addrIP> nMvpn = new tabRoute<addrIP>("bgp");
        tabRoute<addrIP> nMvpo = new tabRoute<addrIP>("bgp");
        if (sendDefRou) {
            tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = rtrBgpUtil.defaultRoute(lower.afiUni);
            ntry.aggrRtr = new addrIP();
            ntry.aggrRtr.fromIPv4addr(lower.routerID);
            ntry.aggrAs = localAs;
            ntry.rouSrc = rtrBgpUtil.peerOriginate;
            originatePrefix(ntry);
            tabRoute.addUpdatedEntry(2, nUni, ntry, roumapOut, roupolOut, prflstOut);
            ntry = new tabRouteEntry<addrIP>();
            ntry.prefix = rtrBgpUtil.defaultRoute(lower.afiUni);
            ntry.aggrRtr = new addrIP();
            ntry.aggrRtr.fromIPv4addr(lower.routerID);
            ntry.aggrAs = localAs;
            ntry.rouSrc = rtrBgpUtil.peerOriginate;
            originatePrefix(ntry);
            tabRoute.addUpdatedEntry(2, nMlt, ntry, roumapOut, roupolOut, prflstOut);
        }
        for (int i = 0; i < lower.routerRedistedU.size(); i++) {
            tabRouteEntry<addrIP> ntry = lower.routerRedistedU.get(i);
            if (ntry == null) {
                continue;
            }
            ntry = ntry.copyBytes();
            ntry.rouSrc = rtrBgpUtil.peerOriginate;
            originatePrefix(ntry);
            tabRoute.addUpdatedEntry(2, nUni, ntry, roumapOut, roupolOut, prflstOut);
        }
        for (int i = 0; i < lower.routerRedistedM.size(); i++) {
            tabRouteEntry<addrIP> ntry = lower.routerRedistedM.get(i);
            if (ntry == null) {
                continue;
            }
            ntry = ntry.copyBytes();
            ntry.rouSrc = rtrBgpUtil.peerOriginate;
            originatePrefix(ntry);
            tabRoute.addUpdatedEntry(2, nMlt, ntry, roumapOut, roupolOut, prflstOut);
        }
        for (int i = 0; i < lower.routerRedistedF.size(); i++) {
            tabRouteEntry<addrIP> ntry = lower.routerRedistedF.get(i);
            if (ntry == null) {
                continue;
            }
            ntry = ntry.copyBytes();
            ntry.rouSrc = rtrBgpUtil.peerOriginate;
            originatePrefix(ntry);
            tabRoute.addUpdatedEntry(2, nFlw, ntry, voumapOut, voupolOut, null);
        }
        readvertTable(true, nUni, cUni);
        readvertTable(false, nMlt, cMlt);
        importTable(nOtr, cOtr);
        importTable(nFlw, cFlw);
        importTable(nVpnU, cVpnU);
        importTable(nVpnM, cVpnM);
        importTable(nVpnF, cVpnF);
        importTable(nVpoU, cVpoU);
        importTable(nVpoM, cVpoM);
        importTable(nVpoF, cVpoF);
        importTable(nVpls, cVpls);
        importTable(nMspw, cMspw);
        importTable(nEvpn, cEvpn);
        importTable(nMdt, cMdt);
        importTable(nMvpn, cMvpn);
        importTable(nMvpo, cMvpo);
        lower.routerDoAggregates(nUni, localAddr, lower.fwdCore.commonLabel, rtrBgpUtil.peerOriginate, lower.routerID, lower.localAs);
        lower.routerDoAggregates(nMlt, localAddr, lower.fwdCore.commonLabel, rtrBgpUtil.peerOriginate, lower.routerID, lower.localAs);
        wilUni = nUni;
        wilMlt = nMlt;
        wilOtr = nOtr;
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
        wilMvpn = nMvpn;
        wilMvpo = nMvpo;
    }

}
