package net.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrIP;
import net.freertr.addr.addrPrefix;
import net.freertr.cfg.cfgAll;
import net.freertr.cfg.cfgIfc;
import net.freertr.cfg.cfgRtr;
import net.freertr.cfg.cfgTrack;
import net.freertr.pack.packHolder;
import net.freertr.rtr.rtrBgpUtil;
import net.freertr.util.bits;
import net.freertr.util.cmds;

/**
 * represents one route map entry
 *
 * @author matecsaba
 */
public class tabRtrmapN extends tabListingEntry<addrIP> {

    /**
     * create instance
     */
    public tabRtrmapN() {
    }

    /**
     * route distinguisher matcher
     */
    public long rouDstMatch;

    /**
     * afi matcher
     */
    public tabIntMatcher afiMatch = new tabIntMatcher();

    /**
     * safi matcher
     */
    public tabIntMatcher safiMatch = new tabIntMatcher();

    /**
     * network matcher
     */
    public tabPrfxlstN networkMatch;

    /**
     * prefix list matcher
     */
    public tabListing<tabPrfxlstN, addrIP> prfxlstMatch;

    /**
     * peer asn matcher
     */
    public tabIntMatcher peerasnMatch = new tabIntMatcher();

    /**
     * distance matcher
     */
    public tabIntMatcher distanceMatch = new tabIntMatcher();

    /**
     * distance updater
     */
    public tabIntUpdater distanceSet = new tabIntUpdater();

    /**
     * locPref matcher
     */
    public tabIntMatcher locPrefMatch = new tabIntMatcher();

    /**
     * locPref updater
     */
    public tabIntUpdater locPrefSet = new tabIntUpdater();

    /**
     * aigp matcher
     */
    public tabIntMatcher accIgpMatch = new tabIntMatcher();

    /**
     * bandwidth matcher
     */
    public tabIntMatcher bandwidthMatch = new tabIntMatcher();

    /**
     * validity matcher
     */
    public tabIntMatcher validityMatch = new tabIntMatcher();

    /**
     * aspath length matched
     */
    public tabIntMatcher pathlenMatch = new tabIntMatcher();

    /**
     * aspath end matched
     */
    public tabIntMatcher asendMatch = new tabIntMatcher();

    /**
     * aspath begin matched
     */
    public tabIntMatcher asbegMatch = new tabIntMatcher();

    /**
     * aspath mid matched
     */
    public tabIntMatcher asmidMatch = new tabIntMatcher();

    /**
     * aigp updater
     */
    public tabIntUpdater accIgpSet = new tabIntUpdater();

    /**
     * aigp updater
     */
    public tabIntUpdater bandwidthSet = new tabIntUpdater();

    /**
     * origin matcher
     */
    public tabIntMatcher originMatch = new tabIntMatcher();

    /**
     * origin updater
     */
    public tabIntUpdater originSet = new tabIntUpdater();

    /**
     * metric matcher
     */
    public tabIntMatcher metricMatch = new tabIntMatcher();

    /**
     * metric updater
     */
    public tabIntUpdater metricSet = new tabIntUpdater();

    /**
     * tag matcher
     */
    public tabIntMatcher tagMatch = new tabIntMatcher();

    /**
     * tag updater
     */
    public tabIntUpdater tagSet = new tabIntUpdater();

    /**
     * segrou matcher
     */
    public tabIntMatcher segrouMatch = new tabIntMatcher();

    /**
     * segrou updater
     */
    public tabIntUpdater segrouSet = new tabIntUpdater();

    /**
     * bier matcher
     */
    public tabIntMatcher bierMatch = new tabIntMatcher();

    /**
     * bier updater
     */
    public tabIntUpdater bierSet = new tabIntUpdater();

    /**
     * as path matcher
     */
    public String aspathMatch = null;

    /**
     * as path updater
     */
    public List<Integer> aspathSet;

    /**
     * as path updater
     */
    public List<Integer> aspathCnf;

    /**
     * next hop matcher
     */
    public addrIP nexthopMatch;

    /**
     * next hop updater
     */
    public addrIP nexthopSet;

    /**
     * old hop matcher
     */
    public addrIP oldhopMatch;

    /**
     * protocol type matcher
     */
    public tabRouteAttr.routeType protoTypMatch;

    /**
     * protocol number matcher
     */
    public int protoNumMatch;

    /**
     * interface matcher
     */
    public cfgIfc ifaceMatch;

    /**
     * tracker matcher
     */
    public String trackMatch;

    /**
     * private as matcher
     */
    public boolean privasMatch;

    /**
     * private as updates
     */
    public boolean privasClear;

    /**
     * peer as updates
     */
    public boolean peerasClear;

    /**
     * exact as updates
     */
    public int exactasClear;

    /**
     * first as updates
     */
    public boolean firstasClear;

    /**
     * community matcher
     */
    public int peerStdMatch;

    /**
     * community matcher
     */
    public tabLargeComm peerLrgMatch;

    /**
     * community matcher
     */
    public List<Integer> stdCommMatch;

    /**
     * community updater
     */
    public String stdCommClear;

    /**
     * community updater
     */
    public List<Integer> stdCommSet;

    /**
     * community matcher
     */
    public List<Long> extCommMatch;

    /**
     * community updater
     */
    public String extCommClear;

    /**
     * community updater
     */
    public List<Long> extCommSet;

    /**
     * community matcher
     */
    public List<tabLargeComm> lrgCommMatch;

    /**
     * community updater
     */
    public String lrgCommClear;

    /**
     * community updater
     */
    public List<tabLargeComm> lrgCommSet;

    /**
     * cluster list updater
     */
    public String clstLstClear;

    /**
     * community matcher
     */
    public boolean noStdComm;

    /**
     * community matcher
     */
    public boolean noExtComm;

    /**
     * community matcher
     */
    public boolean noLrgComm;

    /**
     * script to execute
     */
    public List<String> script;

    /**
     * list of routemaps
     */
    public tabListing<tabRtrmapN, addrIP> roumapMatch;

    /**
     * list of routemaps
     */
    public tabListing<tabRtrmapN, addrIP> roumapSet;

    /**
     * list of routemaps
     */
    public tabListing<tabRtrplcN, addrIP> rouplcMatch;

    /**
     * list of routemaps
     */
    public tabListing<tabRtrplcN, addrIP> rouplcSet;

    public String toString() {
        return description;
    }

    /**
     * get the config
     *
     * @param beg beginning
     * @return the config
     */
    public List<String> usrString(String beg) {
        beg += "sequence " + sequence + " ";
        List<String> l = new ArrayList<String>();
        l.add(beg + "description " + description);
        l.add(beg + "action " + action2string(action));
        if (script == null) {
            l.add(beg + "tcldel");
        } else {
            for (int i = 0; i < script.size(); i++) {
                l.add(beg + "tcladd " + script.get(i));
            }
        }
        cmds.cfgLine(l, prfxlstMatch == null, beg, "match prefix-list", "" + prfxlstMatch);
        cmds.cfgLine(l, roumapMatch == null, beg, "match route-map", "" + roumapMatch);
        cmds.cfgLine(l, rouplcMatch == null, beg, "match route-policy", "" + rouplcMatch);
        l.add(beg + "match afi " + afiMatch);
        l.add(beg + "match safi " + safiMatch);
        if (rouDstMatch == 0) {
            l.add(beg + "no match rd");
        } else {
            l.add(beg + "match rd " + tabRouteUtil.rd2string(rouDstMatch));
        }
        if (networkMatch == null) {
            l.add(beg + "no match network");
        } else {
            l.add(beg + "match network " + networkMatch);
        }
        if (ifaceMatch == null) {
            l.add(beg + "no match interface");
        } else {
            l.add(beg + "match interface " + ifaceMatch.name);
        }
        if (nexthopMatch == null) {
            l.add(beg + "no match nexthop");
        } else {
            l.add(beg + "match nexthop " + nexthopMatch);
        }
        if (oldhopMatch == null) {
            l.add(beg + "no match recursive");
        } else {
            l.add(beg + "match recursive " + oldhopMatch);
        }
        if (protoTypMatch == null) {
            l.add(beg + "no match protocol");
        } else {
            String a = "" + protoTypMatch;
            if (cfgRtr.num2proc(protoTypMatch)) {
                a += " " + protoNumMatch;
            }
            l.add(beg + "match protocol " + a);
        }
        if (aspathMatch == null) {
            l.add(beg + "no match aspath");
        } else {
            l.add(beg + "match aspath " + aspathMatch);
        }
        cmds.cfgLine(l, peerStdMatch == 0, beg, "match peerstd", tabRouteUtil.stdComm2string(peerStdMatch));
        cmds.cfgLine(l, peerLrgMatch == null, beg, "match peerlrg", "" + peerLrgMatch);
        if (stdCommMatch == null) {
            l.add(beg + "no match stdcomm");
        } else {
            l.add(beg + "match stdcomm " + tabRouteUtil.stdComms2string(stdCommMatch));
        }
        if (extCommMatch == null) {
            l.add(beg + "no match extcomm");
        } else {
            l.add(beg + "match extcomm " + tabRouteUtil.extComms2string(extCommMatch));
        }
        if (lrgCommMatch == null) {
            l.add(beg + "no match lrgcomm");
        } else {
            l.add(beg + "match lrgcomm " + tabRouteUtil.lrgComms2string(lrgCommMatch));
        }
        l.add(beg + "match peerasn " + peerasnMatch);
        l.add(beg + "match distance " + distanceMatch);
        l.add(beg + "match locpref " + locPrefMatch);
        l.add(beg + "match validity " + validityMatch);
        l.add(beg + "match pathlen " + pathlenMatch);
        l.add(beg + "match asend " + asendMatch);
        l.add(beg + "match asbeg " + asbegMatch);
        l.add(beg + "match asmid " + asmidMatch);
        l.add(beg + "match aigp " + accIgpMatch);
        l.add(beg + "match bandwidth " + bandwidthMatch);
        l.add(beg + "match origin " + originMatch);
        l.add(beg + "match metric " + metricMatch);
        l.add(beg + "match tag " + tagMatch);
        l.add(beg + "match segrout " + segrouMatch);
        l.add(beg + "match bier " + bierMatch);
        cmds.cfgLine(l, !noStdComm, beg, "match nostdcomm", "");
        cmds.cfgLine(l, !noExtComm, beg, "match noextcomm", "");
        cmds.cfgLine(l, !noLrgComm, beg, "match nolrgcomm", "");
        cmds.cfgLine(l, trackMatch == null, beg, "match tracker", "" + trackMatch);
        cmds.cfgLine(l, !privasMatch, beg, "match privateas", "");
        cmds.cfgLine(l, !logMatch, beg, "log", "");
        cmds.cfgLine(l, stdCommClear == null, beg, "clear stdcomm", stdCommClear);
        cmds.cfgLine(l, extCommClear == null, beg, "clear extcomm", extCommClear);
        cmds.cfgLine(l, lrgCommClear == null, beg, "clear lrgcomm", lrgCommClear);
        cmds.cfgLine(l, clstLstClear == null, beg, "clear clustlist", clstLstClear);
        cmds.cfgLine(l, !privasClear, beg, "clear privateas", "");
        cmds.cfgLine(l, !peerasClear, beg, "clear peeras", "");
        cmds.cfgLine(l, exactasClear == 0, beg, "clear exactas", "" + bits.num2str(exactasClear));
        cmds.cfgLine(l, !firstasClear, beg, "clear firstas", "");
        cmds.cfgLine(l, roumapSet == null, beg, "set route-map", "" + roumapSet);
        cmds.cfgLine(l, rouplcSet == null, beg, "set route-policy", "" + rouplcSet);
        if (aspathSet == null) {
            l.add(beg + "no set aspath");
        } else {
            l.add(beg + "set aspath " + tabRouteUtil.dumpIntList(aspathSet, "", ""));
        }
        if (aspathCnf == null) {
            l.add(beg + "no set asconfed");
        } else {
            l.add(beg + "set asconfed " + tabRouteUtil.dumpIntList(aspathCnf, "", ""));
        }
        if (stdCommSet == null) {
            l.add(beg + "no set stdcomm");
        } else {
            l.add(beg + "set stdcomm " + tabRouteUtil.stdComms2string(stdCommSet));
        }
        if (extCommSet == null) {
            l.add(beg + "no set extcomm");
        } else {
            l.add(beg + "set extcomm " + tabRouteUtil.extComms2string(extCommSet));
        }
        if (lrgCommSet == null) {
            l.add(beg + "no set lrgcomm");
        } else {
            l.add(beg + "set lrgcomm " + tabRouteUtil.lrgComms2string(lrgCommSet));
        }
        if (nexthopSet == null) {
            l.add(beg + "no set nexthop");
        } else {
            l.add(beg + "set nexthop " + nexthopSet);
        }
        l.add(beg + "set distance " + distanceSet);
        l.add(beg + "set locpref " + locPrefSet);
        l.add(beg + "set aigp " + accIgpSet);
        l.add(beg + "set bandwidth " + bandwidthSet);
        l.add(beg + "set origin " + originSet);
        l.add(beg + "set metric " + metricSet);
        l.add(beg + "set tag " + tagSet);
        l.add(beg + "set segrout " + segrouSet);
        l.add(beg + "set bier " + bierSet);
        return l;
    }

    public boolean matches(int afi, int asn, addrPrefix<addrIP> net) {
        tabRouteEntry<addrIP> nt = new tabRouteEntry<addrIP>();
        nt.prefix = net;
        return matches(afi, asn, nt);
    }

    public boolean matches(int afi, int asn, tabRouteEntry<addrIP> net) {
        if (rouDstMatch != 0) {
            if (rouDstMatch != net.rouDst) {
                return false;
            }
        }
        if (!afiMatch.matches(afi & rtrBgpUtil.afiMask)) {
            return false;
        }
        if (!safiMatch.matches(afi & rtrBgpUtil.sfiMask)) {
            return false;
        }
        if (!peerasnMatch.matches(asn)) {
            return false;
        }
        if (!distanceMatch.matches(net.best.distance)) {
            return false;
        }
        if (!locPrefMatch.matches(net.best.locPref)) {
            return false;
        }
        if (!accIgpMatch.matches(net.best.accIgp)) {
            return false;
        }
        if (!bandwidthMatch.matches(net.best.bandwidth)) {
            return false;
        }
        if (!originMatch.matches(net.best.origin)) {
            return false;
        }
        if (!metricMatch.matches(net.best.metric)) {
            return false;
        }
        if (!tagMatch.matches(net.best.tag)) {
            return false;
        }
        if (!segrouMatch.matches(net.best.segrouIdx)) {
            return false;
        }
        if (!bierMatch.matches(net.best.bierIdx)) {
            return false;
        }
        if (!validityMatch.matches(net.best.validity)) {
            return false;
        }
        if (!pathlenMatch.matches(net.best.asPathLen())) {
            return false;
        }
        if (!asendMatch.matches(net.best.asPathEnd())) {
            return false;
        }
        if (!asbegMatch.matches(net.best.asPathBeg())) {
            return false;
        }
        if (!net.best.asPathMid(asmidMatch)) {
            return false;
        }
        if (noStdComm) {
            if (net.best.stdComm != null) {
                if (net.best.stdComm.size() > 0) {
                    return false;
                }
            }
        }
        if (noExtComm) {
            if (net.best.extComm != null) {
                if (net.best.extComm.size() > 0) {
                    return false;
                }
            }
        }
        if (noLrgComm) {
            if (net.best.lrgComm != null) {
                if (net.best.lrgComm.size() > 0) {
                    return false;
                }
            }
        }
        if (trackMatch != null) {
            cfgTrack res = cfgAll.trackFind(trackMatch, false);
            if (res == null) {
                return false;
            }
            if (!res.worker.getStatus()) {
                return false;
            }
        }
        if (privasMatch) {
            int i = tabRouteUtil.removePrivateAs(tabLabel.copyLabels(net.best.pathSeq));
            i += tabRouteUtil.removePrivateAs(tabLabel.copyLabels(net.best.pathSet));
            if (i < 1) {
                return false;
            }
        }
        if (ifaceMatch != null) {
            if (net.best.iface == null) {
                return false;
            }
            if ((net.best.iface != ifaceMatch.fwdIf4) && (net.best.iface != ifaceMatch.fwdIf6)) {
                return false;
            }
        }
        if (nexthopMatch != null) {
            if (net.best.nextHop == null) {
                return false;
            }
            if (nexthopMatch.compare(nexthopMatch, net.best.nextHop) != 0) {
                return false;
            }
        }
        if (oldhopMatch != null) {
            if (net.best.oldHop == null) {
                return false;
            }
            if (oldhopMatch.compare(oldhopMatch, net.best.oldHop) != 0) {
                return false;
            }
        }
        if (protoTypMatch != null) {
            if (net.best.rouTyp != protoTypMatch) {
                return false;
            }
            if (net.best.protoNum != protoNumMatch) {
                return false;
            }
        }
        if (networkMatch != null) {
            if (!networkMatch.matches(afi, asn, net.prefix)) {
                return false;
            }
        }
        if (aspathMatch != null) {
            if (!net.best.asPathStr().matches(aspathMatch)) {
                return false;
            }
        }
        if (peerStdMatch != 0) {
            int i = tabRouteUtil.stdCommAsn(peerStdMatch, asn);
            if (tabRouteUtil.findIntList(net.best.stdComm, i) < 0) {
                return false;
            }
        }
        if (peerLrgMatch != null) {
            tabLargeComm lrg = peerLrgMatch.copyBytes();
            lrg.d2 = asn;
            if (tabRouteUtil.findLrgList(net.best.lrgComm, lrg) < 0) {
                return false;
            }
        }
        if (stdCommMatch != null) {
            for (int i = 0; i < stdCommMatch.size(); i++) {
                if (tabRouteUtil.findIntList(net.best.stdComm, stdCommMatch.get(i)) < 0) {
                    return false;
                }
            }
        }
        if (extCommMatch != null) {
            for (int i = 0; i < extCommMatch.size(); i++) {
                if (tabRouteUtil.findLongList(net.best.extComm, extCommMatch.get(i)) < 0) {
                    return false;
                }
            }
        }
        if (lrgCommMatch != null) {
            for (int i = 0; i < lrgCommMatch.size(); i++) {
                if (tabRouteUtil.findLrgList(net.best.lrgComm, lrgCommMatch.get(i)) < 0) {
                    return false;
                }
            }
        }
        if (prfxlstMatch != null) {
            if (!prfxlstMatch.matches(afi, asn, net.prefix)) {
                return false;
            }
        }
        if (roumapMatch != null) {
            if (!roumapMatch.matches(afi, asn, net)) {
                return false;
            }
        }
        if (rouplcMatch != null) {
            if (tabRtrplc.doRpl(afi, asn, net, rouplcMatch, true) == null) {
                return false;
            }
        }
        return true;
    }

    public boolean matches(packHolder pck) {
        return matches(rtrBgpUtil.sfiUnicast, 0, new addrPrefix<addrIP>(pck.IPsrc, new addrIP().maxBits()));
    }

    private void doUpdate(tabRouteAttr<addrIP> attr, int asn) {
        attr.distance = distanceSet.update(attr.distance);
        attr.locPref = locPrefSet.update(attr.locPref);
        attr.accIgp = accIgpSet.update(attr.accIgp);
        attr.bandwidth = bandwidthSet.update(attr.bandwidth);
        attr.origin = originSet.update(attr.origin);
        attr.metric = metricSet.update(attr.metric);
        attr.tag = tagSet.update(attr.tag);
        attr.segrouIdx = segrouSet.update(attr.segrouIdx);
        attr.bierIdx = bierSet.update(attr.bierIdx);
        attr.pathSeq = tabLabel.prependLabels(attr.pathSeq, aspathSet);
        attr.confSeq = tabLabel.prependLabels(attr.confSeq, aspathCnf);
        if (stdCommClear != null) {
            tabRouteUtil.removeStdComm(attr, stdCommClear);
        }
        if (extCommClear != null) {
            tabRouteUtil.removeExtComm(attr, extCommClear);
        }
        if (lrgCommClear != null) {
            tabRouteUtil.removeLrgComm(attr, lrgCommClear);
        }
        if (clstLstClear != null) {
            tabRouteUtil.removeClstLst(attr, clstLstClear);
        }
        if (privasClear) {
            tabRouteUtil.removePrivateAs(attr.pathSeq);
            tabRouteUtil.removePrivateAs(attr.pathSet);
        }
        if (peerasClear) {
            tabRouteUtil.removeIntList(attr.pathSeq, asn);
            tabRouteUtil.removeIntList(attr.pathSet, asn);
        }
        if (exactasClear != 0) {
            tabRouteUtil.removeIntList(attr.pathSeq, exactasClear);
            tabRouteUtil.removeIntList(attr.pathSet, exactasClear);
        }
        if (firstasClear) {
            tabRouteUtil.removeFirstAs(attr);
        }
        attr.stdComm = tabLabel.prependLabels(attr.stdComm, stdCommSet);
        if (nexthopSet != null) {
            attr.nextHop = nexthopSet.copyBytes();
        }
        if (extCommSet != null) {
            if (attr.extComm == null) {
                attr.extComm = new ArrayList<Long>();
            }
            attr.extComm.addAll(extCommSet);
        }
        if (lrgCommSet != null) {
            if (attr.lrgComm == null) {
                attr.lrgComm = new ArrayList<tabLargeComm>();
            }
            attr.lrgComm.addAll(lrgCommSet);
        }
    }

    public void update(int afi, int asn, tabRouteEntry<addrIP> net) {
        for (int i = 0; i < net.alts.size(); i++) {
            doUpdate(net.alts.get(i), asn);
        }
        net.selectBest();
        if (roumapSet != null) {
            roumapSet.update(afi, asn, net, false);
        }
        if (rouplcSet != null) {
            tabRtrplc.doRpl(afi, asn, net, rouplcSet, false);
        }
        if (script != null) {
            tabRouteUtil.doTcl(afi, asn, net.best, net, script);
        }
    }

}
