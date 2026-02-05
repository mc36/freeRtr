package org.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgIfc;
import org.freertr.cfg.cfgRtr;
import org.freertr.cfg.cfgTrack;
import org.freertr.ip.ipFwd;
import org.freertr.pack.packHolder;
import org.freertr.rtr.rtrBgpUtil;
import org.freertr.util.bits;

/**
 * represents one route policy entry
 *
 * @author matecsaba
 */
public class tabRtrplcN extends tabListingEntry<addrIP> {

    /**
     * create instance
     */
    public tabRtrplcN() {
    }

    /**
     * action type
     */
    public enum doType {
        /**
         * next
         */
        next,
        /**
         * description
         */
        description,
        /**
         * if
         */
        iff,
        /**
         * elseif
         */
        elsif,
        /**
         * else
         */
        els,
        /**
         * endif
         */
        enif,
        /**
         * pass
         */
        pass,
        /**
         * drop
         */
        drop,
        /**
         * log
         */
        log,
        /**
         * tcl
         */
        tcl,
        /**
         * clear stdcomm
         */
        clrStdcomm,
        /**
         * clear extcomm
         */
        clrExtcomm,
        /**
         * clear lrgcomm
         */
        clrLrgcomm,
        /**
         * clear originator
         */
        clrOrgntr,
        /**
         * clear clstlst
         */
        clrClstlst,
        /**
         * clear privas
         */
        clrPrivas,
        /**
         * clear entropy
         */
        clrEntropy,
        /**
         * clear peeras
         */
        clrPeeras,
        /**
         * clear exactas
         */
        clrExactas,
        /**
         * clear firstas
         */
        clrFirstas,
        /**
         * set stdcomm
         */
        setStdcomm,
        /**
         * set extcomm
         */
        setExtcomm,
        /**
         * set lrgcomm
         */
        setLrgcomm,
        /**
         * set vrf
         */
        setVrf,
        /**
         * set nexthop
         */
        setNexthop,
        /**
         * set aspath
         */
        setAspath,
        /**
         * set asconf
         */
        setAsconf,
        /**
         * set distance
         */
        setDistance,
        /**
         * set metric
         */
        setMetric,
        /**
         * set origin
         */
        setOrigin,
        /**
         * set locpref
         */
        setLocPref,
        /**
         * set accigp
         */
        setAccIgp,
        /**
         * set validity
         */
        setValidRoa,
        /**
         * set validity
         */
        setValidAspa,
        /**
         * set aggregator
         */
        setAggregator,
        /**
         * set connector
         */
        setConnect,
        /**
         * set path limit
         */
        setPathLimit,
        /**
         * set customer
         */
        setCustomer,
        /**
         * set destination preference
         */
        setDestPref,
        /**
         * set bandwidth
         */
        setBandwidth,
        /**
         * set tag
         */
        setTag,
        /**
         * set local label
         */
        setLabloc,
        /**
         * set remote label
         */
        setLabrem,
        /**
         * set segrou
         */
        setSegrou,
        /**
         * set bier
         */
        setBier,
        /**
         * set srv6
         */
        setSrv6,
        /**
         * set route distinguisher
         */
        setRoudst,
        /**
         * set routemap
         */
        setRoumap,
        /**
         * set routepolicy
         */
        setRouplc
    }

    /**
     * match type
     */
    public enum ifType {
        /**
         * never
         */
        never,
        /**
         * always
         */
        always,
        /**
         * aspath
         */
        aspath,
        /**
         * peer stdcomm
         */
        peerstd,
        /**
         * peer lrgcomm
         */
        peerlrg,
        /**
         * stdcomm
         */
        stdcomm,
        /**
         * extcomm
         */
        extcomm,
        /**
         * lrgcomm
         */
        lrgcomm,
        /**
         * route distinguisher
         */
        roudst,
        /**
         * network
         */
        network,
        /**
         * nostdcomm
         */
        nostdcomm,
        /**
         * noextcomm
         */
        noextcomm,
        /**
         * nolrgcomm
         */
        nolrgcomm,
        /**
         * tracker
         */
        track,
        /**
         * privas
         */
        privas,
        /**
         * entropy
         */
        entropy,
        /**
         * access list
         */
        aceslst,
        /**
         * prefix list
         */
        prfxlst,
        /**
         * route map
         */
        roumap,
        /**
         * route policy
         */
        rouplc,
        /**
         * peer asn
         */
        peerasn,
        /**
         * distance
         */
        distance,
        /**
         * metric
         */
        metric,
        /**
         * origin
         */
        origin,
        /**
         * locpref
         */
        locpref,
        /**
         * accigp
         */
        accigp,
        /**
         * validity
         */
        validroa,
        /**
         * validity
         */
        validaspa,
        /**
         * aggregator
         */
        aggregator,
        /**
         * customer
         */
        customer,
        /**
         * destination preference
         */
        destPref,
        /**
         * pathlen
         */
        pathlen,
        /**
         * unknown
         */
        unknown,
        /**
         * asend
         */
        asend,
        /**
         * asbeg
         */
        asbeg,
        /**
         * asmid
         */
        asmid,
        /**
         * asany
         */
        asany,
        /**
         * bandwidth
         */
        bandwidth,
        /**
         * tag
         */
        tag,
        /**
         * local label
         */
        labrem,
        /**
         * remote label
         */
        labloc,
        /**
         * segrou
         */
        segrou,
        /**
         * bier
         */
        bier,
        /**
         * srv6
         */
        srv6,
        /**
         * afi
         */
        afi,
        /**
         * safi
         */
        safi,
        /**
         * nexthop
         */
        nexthop,
        /**
         * interface
         */
        iface,
        /**
         * old nexthop
         */
        recursive,
        /**
         * protocol
         */
        protocol
    }

    /**
     * indentation level
     */
    public int indent = 0;

    /**
     * do mode
     */
    public doType doMode = doType.next;

    /**
     * if mode
     */
    public ifType ifMode = ifType.never;

    /**
     * string value
     */
    public String strVal = null;

    /**
     * integer value
     */
    public int intVal = 0;

    /**
     * long value
     */
    public long longVal = 0;

    /**
     * integer list
     */
    public List<Integer> intLst;

    /**
     * long list
     */
    public List<Long> lngLst;

    /**
     * large list
     */
    public List<tabLargeComm> lrgLst;

    /**
     * protocol type matcher
     */
    public tabRouteAttr.routeType protoMatch;

    /**
     * network matcher
     */
    public tabPrfxlstN networkMatch;

    /**
     * access list matcher
     */
    public tabListing<tabAceslstN<addrIP>, addrIP> aceslst;

    /**
     * prefix list matcher
     */
    public tabListing<tabPrfxlstN, addrIP> prfxlst;

    /**
     * route map matcher
     */
    public tabListing<tabRtrmapN, addrIP> roumap;

    /**
     * route policy matcher
     */
    public tabListing<tabRtrplcN, addrIP> rouplc;

    /**
     * integer matcher
     */
    public tabIntMatcher intMatch = new tabIntMatcher();

    /**
     * integer updater
     */
    public tabIntUpdater intSet = new tabIntUpdater();

    /**
     * integer updater
     */
    public tabIntUpdater int2set = new tabIntUpdater();

    /**
     * next hop updater
     */
    public addrIP addrSet;

    /**
     * vrf forwarder updater
     */
    public ipFwd vrfSetF;

    /**
     * vrf afi type updater
     */
    public boolean vrfSetT;

    /**
     * interface matcher
     */
    public cfgIfc ifaceMatch;

    public String toString() {
        switch (doMode) {
            case next:
                return "next";
            case description:
                return "description " + description;
            case iff:
                return "if " + ifString();
            case elsif:
                return "elsif " + ifString();
            case els:
                return "else";
            case enif:
                return "enif";
            case pass:
                return "pass";
            case drop:
                return "drop";
            case log:
                return "log";
            case tcl:
                return "tcl " + strVal;
            case clrStdcomm:
                return "clear stdcomm " + strVal;
            case clrExtcomm:
                return "clear extcomm " + strVal;
            case clrLrgcomm:
                return "clear lrgcomm " + strVal;
            case clrOrgntr:
                return "clear originator " + strVal;
            case clrClstlst:
                return "clear clustlist " + strVal;
            case clrPrivas:
                return "clear privateas";
            case clrEntropy:
                return "clear entropy";
            case clrPeeras:
                return "clear peeras";
            case clrExactas:
                return "clear exactas " + intVal;
            case clrFirstas:
                return "clear firstas";
            case setStdcomm:
                return "set stdcomm " + tabRouteUtil.stdComms2string(intLst);
            case setExtcomm:
                return "set extcomm " + tabRouteUtil.extComms2string(lngLst);
            case setLrgcomm:
                return "set lrgcomm " + tabRouteUtil.lrgComms2string(lrgLst);
            case setVrf:
                return "set vrf " + vrfSetF.cfgName + " " + (vrfSetT ? "ipv4" : "ipv6");
            case setNexthop:
                return "set nexthop " + addrSet;
            case setAspath:
                return "set aspath " + tabRouteUtil.dumpIntList(intLst, "", "");
            case setAsconf:
                return "set asconfed " + tabRouteUtil.dumpIntList(intLst, "", "");
            case setDistance:
                return "set distance " + intSet;
            case setMetric:
                return "set metric " + intSet;
            case setOrigin:
                return "set origin " + intSet;
            case setLocPref:
                return "set locpref " + intSet;
            case setAccIgp:
                return "set aigp " + intSet;
            case setValidRoa:
                return "set validroa " + intSet;
            case setValidAspa:
                return "set validaspa " + intSet;
            case setAggregator:
                return "set aggregator " + intSet + " " + addrSet;
            case setConnect:
                return "set connector " + addrSet;
            case setPathLimit:
                return "set pathlimit " + intSet + " " + int2set;
            case setCustomer:
                return "set customer " + intSet;
            case setDestPref:
                return "set destpref " + intSet + " " + int2set;
            case setBandwidth:
                return "set bandwidth " + intSet;
            case setTag:
                return "set tag " + intSet;
            case setLabloc:
                return "set label-local " + intSet;
            case setLabrem:
                return "set label-remote " + intSet;
            case setSegrou:
                return "set segrout " + intSet;
            case setBier:
                return "set bier " + intSet + " " + int2set;
            case setSrv6:
                return "set srv6 " + addrSet;
            case setRoudst:
                return "set rd " + tabRouteUtil.rd2string(longVal);
            case setRoumap:
                return "set route-map " + roumap;
            case setRouplc:
                return "set route-policy " + rouplc;
            default:
                return "unknown=" + doMode;
        }
    }

    /**
     * convert to string
     *
     * @param beg beginning
     * @param filter filter mode
     * @return string
     */
    public List<String> usrString(String beg, int filter) {
        beg += "sequence " + sequence + " ";
        for (int i = 0; i < indent; i++) {
            beg += "  ";
        }
        return bits.str2lst(beg + this);
    }

    public boolean matches(int afi, int asn, addrPrefix<addrIP> net) {
        tabRouteEntry<addrIP> nt = new tabRouteEntry<addrIP>();
        nt.prefix = net;
        return matches(afi, asn, nt);
    }

    private String ifString() {
        switch (ifMode) {
            case never:
                return "never";
            case always:
                return "always";
            case aspath:
                return "aspath " + strVal;
            case peerstd:
                return "peerstd " + tabRouteUtil.stdComms2string(intLst);
            case peerlrg:
                return "peerlrg " + tabRouteUtil.lrgComms2string(lrgLst);
            case stdcomm:
                return "stdcomm " + tabRouteUtil.stdComms2string(intLst);
            case extcomm:
                return "extcomm " + tabRouteUtil.extComms2string(lngLst);
            case lrgcomm:
                return "lrgcomm " + tabRouteUtil.lrgComms2string(lrgLst);
            case roudst:
                return "rd " + tabRouteUtil.rd2string(longVal);
            case network:
                return "network " + networkMatch;
            case nostdcomm:
                return "nostdcomm";
            case noextcomm:
                return "noextcomm";
            case nolrgcomm:
                return "nolrgcomm";
            case track:
                return "tracker " + strVal;
            case privas:
                return "privateas";
            case entropy:
                return "entropy";
            case aceslst:
                return "access-list " + aceslst;
            case prfxlst:
                return "prefix-list " + prfxlst;
            case roumap:
                return "route-map " + roumap;
            case rouplc:
                return "route-policy " + rouplc;
            case peerasn:
                return "peerasn " + intMatch;
            case distance:
                return "distance " + intMatch;
            case metric:
                return "metric " + intMatch;
            case origin:
                return "origin " + intMatch;
            case locpref:
                return "locpref " + intMatch;
            case accigp:
                return "aigp " + intMatch;
            case validroa:
                return "validroa " + intMatch;
            case validaspa:
                return "validaspa " + intMatch;
            case aggregator:
                return "aggregator " + intMatch;
            case customer:
                return "customer " + intMatch;
            case destPref:
                return "destpref " + intMatch;
            case pathlen:
                return "pathlen " + intMatch;
            case unknown:
                return "unknowns " + intMatch;
            case asend:
                return "asend " + intMatch;
            case asbeg:
                return "asbeg " + intMatch;
            case asmid:
                return "asmid " + intMatch;
            case asany:
                return "asany " + intMatch;
            case bandwidth:
                return "bandwidth " + intMatch;
            case tag:
                return "tag " + intMatch;
            case labloc:
                return "label-local " + intMatch;
            case labrem:
                return "label-remote " + intMatch;
            case segrou:
                return "segrout " + intMatch;
            case bier:
                return "bier " + intMatch;
            case srv6:
                return "srv6 " + addrSet;
            case afi:
                return "afi " + intMatch;
            case safi:
                return "safi " + intMatch;
            case nexthop:
                return "nexthop " + addrSet;
            case iface:
                return "interface " + ifaceMatch.name;
            case recursive:
                return "recursive " + addrSet;
            case protocol:
                String a = "" + protoMatch;
                if (cfgRtr.num2proc(protoMatch)) {
                    a += " " + intVal;
                }
                return "protocol " + a;
            default:
                return "unknown=" + ifMode;
        }
    }

    public boolean matches(int afi, int asn, tabRouteEntry<addrIP> net) {
        switch (ifMode) {
            case never:
                return false;
            case always:
                return true;
            case aspath:
                return net.best.asPathStr().matches(strVal);
            case peerstd:
                int i = tabRouteUtil.stdCommAsn(intLst.get(0), asn);
                if (tabRouteUtil.findIntList(net.best.stdComm, i) < 0) {
                    return false;
                }
                return true;
            case peerlrg:
                tabLargeComm lrg = lrgLst.get(0).copyBytes();
                lrg.d2 = asn;
                if (tabRouteUtil.findLrgList(net.best.lrgComm, lrg) < 0) {
                    return false;
                }
                return true;
            case stdcomm:
                for (i = 0; i < intLst.size(); i++) {
                    if (tabRouteUtil.findIntList(net.best.stdComm, intLst.get(i)) < 0) {
                        return false;
                    }
                }
                return true;
            case extcomm:
                for (i = 0; i < lngLst.size(); i++) {
                    if (tabRouteUtil.findLongList(net.best.extComm, lngLst.get(i)) < 0) {
                        return false;
                    }
                }
                return true;
            case lrgcomm:
                for (i = 0; i < lrgLst.size(); i++) {
                    if (tabRouteUtil.findLrgList(net.best.lrgComm, lrgLst.get(i)) < 0) {
                        return false;
                    }
                }
                return true;
            case roudst:
                return longVal == net.rouDst;
            case network:
                return networkMatch.matches(afi, asn, net.prefix);
            case nostdcomm:
                if (net.best.stdComm != null) {
                    if (net.best.stdComm.size() > 0) {
                        return false;
                    }
                }
                return true;
            case noextcomm:
                if (net.best.extComm != null) {
                    if (net.best.extComm.size() > 0) {
                        return false;
                    }
                }
                return true;
            case nolrgcomm:
                if (net.best.lrgComm != null) {
                    if (net.best.lrgComm.size() > 0) {
                        return false;
                    }
                }
                return true;
            case track:
                cfgTrack res = cfgAll.trackFind(strVal, false);
                if (res == null) {
                    return false;
                }
                return res.worker.getStatus();
            case privas:
                i = tabRouteUtil.removePrivateAs(tabLabel.copyLabels(net.best.pathSeq));
                i += tabRouteUtil.removePrivateAs(tabLabel.copyLabels(net.best.pathSet));
                return i > 0;
            case entropy:
                return net.best.entropyLabel != null;
            case aceslst:
                packHolder pck = new packHolder(false, false);
                if ((afi & rtrBgpUtil.sfiMask) != rtrBgpUtil.sfiEthVpn) {
                    pck.IPsrc.setAddr(net.prefix.network);
                    pck.IPtrg.setAddr(net.prefix.mask);
                } else {
                    addrPrefix<addrIP> pfx = tabRouteUtil.convertL3evpn(net.prefix);
                    if (pfx == null) {
                        return false;
                    }
                    pck.IPsrc.setAddr(pfx.network);
                    pck.IPtrg.setAddr(pfx.mask);
                }
                return aceslst.matches(false, false, pck);
            case prfxlst:
                return prfxlst.matches(afi, asn, net.prefix);
            case roumap:
                return roumap.matches(afi, asn, net);
            case rouplc:
                return tabRtrplc.doRpl(afi, asn, net, rouplc, true) != null;
            case peerasn:
                return intMatch.matches(asn);
            case distance:
                return intMatch.matches(net.best.distance);
            case metric:
                return intMatch.matches(net.best.metric);
            case origin:
                return intMatch.matches(net.best.originType);
            case locpref:
                return intMatch.matches(net.best.locPref);
            case accigp:
                return intMatch.matches(net.best.accIgp);
            case validroa:
                return intMatch.matches(net.best.validRoa);
            case validaspa:
                return intMatch.matches(net.best.validAspa);
            case aggregator:
                return intMatch.matches(net.best.aggrAs);
            case customer:
                return intMatch.matches(net.best.onlyCust);
            case destPref:
                return intMatch.matches(net.best.destPrefVal);
            case pathlen:
                return intMatch.matches(net.best.asPathLen());
            case unknown:
                return intMatch.matches(net.best.unkAttrCnt());
            case asend:
                return intMatch.matches(net.best.asPathEnd());
            case asbeg:
                return intMatch.matches(net.best.asPathBeg());
            case asmid:
                return net.best.asPathMid(intMatch, 0, 1);
            case asany:
                return net.best.asPathMid(intMatch, 0, 0);
            case bandwidth:
                return intMatch.matches(net.best.bandwidth);
            case tag:
                return intMatch.matches(net.best.tag);
            case labloc:
                if (net.best.labelLoc == null) {
                    return false;
                }
                return intMatch.matches(net.best.labelLoc.label);
            case labrem:
                if (net.best.labelRem == null) {
                    return false;
                }
                if (net.best.labelRem.size() < 1) {
                    return false;
                }
                return intMatch.matches(net.best.labelRem.get(0));
            case segrou:
                return intMatch.matches(net.best.segrouIdx);
            case bier:
                return intMatch.matches(net.best.bierIdx);
            case srv6:
                if (net.best.segrouPrf == null) {
                    return false;
                }
                return addrSet.compareTo(net.best.segrouPrf) == 0;
            case afi:
                return intMatch.matches(afi & rtrBgpUtil.afiMask);
            case safi:
                return intMatch.matches(afi & rtrBgpUtil.sfiMask);
            case nexthop:
                if (net.best.nextHop == null) {
                    return false;
                }
                return addrSet.compareTo(net.best.nextHop) == 0;
            case iface:
                if (net.best.iface == null) {
                    return false;
                }
                return ((net.best.iface == ifaceMatch.fwdIf4) || (net.best.iface == ifaceMatch.fwdIf6));
            case recursive:
                if (net.best.oldHop == null) {
                    return false;
                }
                return addrSet.compareTo(net.best.oldHop) == 0;
            case protocol:
                if (net.best.rouTyp != protoMatch) {
                    return false;
                }
                if (net.best.protoNum != intVal) {
                    return false;
                }
                return true;
            default:
                return true;
        }
    }

    public boolean matches(packHolder pck) {
        return matches(rtrBgpUtil.sfiUnicast, 0, new addrPrefix<addrIP>(pck.IPsrc, new addrIP().maxBits()));
    }

    private void doUpdate(tabRouteAttr<addrIP> attr, int asn) {
        switch (doMode) {
            case clrStdcomm:
                tabRouteUtil.removeStdComm(attr, strVal);
                return;
            case clrExtcomm:
                tabRouteUtil.removeExtComm(attr, strVal);
                return;
            case clrLrgcomm:
                tabRouteUtil.removeLrgComm(attr, strVal);
                return;
            case clrOrgntr:
                tabRouteUtil.removeOrgntr(attr, strVal);
                break;
            case clrClstlst:
                tabRouteUtil.removeClstLst(attr, strVal);
                return;
            case clrPrivas:
                tabRouteUtil.removePrivateAs(attr.pathSeq);
                tabRouteUtil.removePrivateAs(attr.pathSet);
                return;
            case clrEntropy:
                attr.entropyLabel = null;
                return;
            case clrPeeras:
                tabRouteUtil.removeIntList(attr.pathSeq, asn);
                tabRouteUtil.removeIntList(attr.pathSet, asn);
                return;
            case clrExactas:
                tabRouteUtil.removeIntList(attr.pathSeq, intVal);
                tabRouteUtil.removeIntList(attr.pathSet, intVal);
                return;
            case clrFirstas:
                tabRouteUtil.removeFirstAs(attr);
                return;
            case setStdcomm:
                attr.stdComm = tabLabel.prependLabels(attr.stdComm, intLst);
                return;
            case setExtcomm:
                if (attr.extComm == null) {
                    attr.extComm = new ArrayList<Long>();
                }
                attr.extComm.addAll(lngLst);
                return;
            case setLrgcomm:
                if (attr.lrgComm == null) {
                    attr.lrgComm = new ArrayList<tabLargeComm>();
                }
                attr.lrgComm.addAll(lrgLst);
                return;
            case setVrf:
                attr.rouTab = vrfSetF;
                return;
            case setNexthop:
                attr.nextHop = addrSet.copyBytes();
                return;
            case setAspath:
                attr.pathSeq = tabLabel.prependLabels(attr.pathSeq, intLst);
                return;
            case setAsconf:
                attr.confSeq = tabLabel.prependLabels(attr.confSeq, intLst);
                return;
            case setDistance:
                attr.distance = intSet.update(attr.distance);
                return;
            case setMetric:
                attr.metric = intSet.update(attr.metric);
                return;
            case setOrigin:
                attr.originType = intSet.update(attr.originType);
                return;
            case setLocPref:
                attr.locPref = intSet.update(attr.locPref);
                return;
            case setAccIgp:
                attr.accIgp = intSet.update(attr.accIgp);
                return;
            case setValidRoa:
                if (intSet.action == tabIntUpdater.actionType.nothing) {
                    return;
                }
                attr.validRoa = intSet.update(attr.validRoa);
                attr.extComm = tabRouteUtil.setValidExtCommRoa(attr.extComm, attr.validRoa);
                return;
            case setValidAspa:
                if (intSet.action == tabIntUpdater.actionType.nothing) {
                    return;
                }
                attr.validAspa = intSet.update(attr.validAspa);
                attr.extComm = tabRouteUtil.setValidExtCommAspa(attr.extComm, attr.validAspa);
                return;
            case setAggregator:
                if (intSet.action == tabIntUpdater.actionType.nothing) {
                    return;
                }
                attr.aggrAs = intSet.update(attr.aggrAs);
                attr.aggrRtr = addrSet.copyBytes();
                return;
            case setConnect:
                attr.connRtr = addrSet.copyBytes();
                return;
            case setPathLimit:
                attr.pathLimVal = intSet.update(attr.pathLimVal);
                attr.pathLimAsn = int2set.update(attr.pathLimAsn);
                return;
            case setCustomer:
                attr.onlyCust = intSet.update(attr.onlyCust);
                return;
            case setDestPref:
                attr.destPrefVal = intSet.update(attr.destPrefVal);
                attr.destPrefAsn = int2set.update(attr.destPrefAsn);
                return;
            case setBandwidth:
                attr.bandwidth = intSet.update(attr.bandwidth);
                return;
            case setTag:
                attr.tag = intSet.update(attr.tag);
                return;
            case setLabloc:
                tabRouteUtil.updateLabloc(attr, intSet);
                return;
            case setLabrem:
                tabRouteUtil.updateLabrem(attr, intSet);
                return;
            case setSegrou:
                attr.segrouIdx = intSet.update(attr.segrouIdx);
                return;
            case setBier:
                attr.bierIdx = intSet.update(attr.bierIdx);
                attr.bierSub = int2set.update(attr.bierSub);
                return;
            case setSrv6:
                attr.segrouPrf = addrSet.copyBytes();
                return;
            default:
                break;
        }
    }

    public void update(int afi, int asn, tabRouteEntry<addrIP> net) {
        switch (doMode) {
            case setRoumap:
                roumap.update(afi, asn, net, false);
                return;
            case setRouplc:
                tabRtrplc.doRpl(afi, asn, net, rouplc, false);
                return;
            case setRoudst:
                net.rouDst = longVal;
                return;
            default:
                break;
        }
        for (int i = 0; i < net.alts.size(); i++) {
            doUpdate(net.alts.get(i), asn);
        }
    }

}
