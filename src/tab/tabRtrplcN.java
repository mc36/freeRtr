package tab;

import addr.addrIP;
import addr.addrPrefix;
import cfg.cfgAll;
import cfg.cfgTrack;
import java.util.ArrayList;
import java.util.List;
import pack.packHolder;
import rtr.rtrBgpUtil;
import util.bits;

/**
 * represents one route policy entry
 *
 * @author matecsaba
 */
public class tabRtrplcN extends tabListingEntry<addrIP> {

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
         * clear privas
         */
        clrPrivas,
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
         * set bandwidth
         */
        setBandwidth,
        /**
         * set tag
         */
        setTag,
        /**
         * set segrou
         */
        setSegrou,
        /**
         * set bier
         */
        setBier,
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
        validity,
        /**
         * pathlen
         */
        pathlen,
        /**
         * bandwidth
         */
        bandwidth,
        /**
         * tag
         */
        tag,
        /**
         * segrou
         */
        segrou,
        /**
         * bier
         */
        bier,
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
        nexthop
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
     * route distinguisher matcher
     */
    public long rouDstMatch;

    /**
     * network matcher
     */
    public tabPrfxlstN networkMatch;

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
     * next hop updater
     */
    public addrIP nexthopSet;

    public List<String> usrString(String beg) {
        beg += "sequence " + sequence + " ";
        for (int i = 0; i < indent; i++) {
            beg += "  ";
        }
        switch (doMode) {
            case next:
                return bits.str2lst(beg + "next");
            case description:
                return bits.str2lst(beg + "description " + description);
            case iff:
                return bits.str2lst(beg + "if " + ifString());
            case elsif:
                return bits.str2lst(beg + "elsif " + ifString());
            case els:
                return bits.str2lst(beg + "else");
            case enif:
                return bits.str2lst(beg + "enif");
            case pass:
                return bits.str2lst(beg + "pass");
            case drop:
                return bits.str2lst(beg + "drop");
            case log:
                return bits.str2lst(beg + "log");
            case tcl:
                return bits.str2lst(beg + "tcl " + strVal);
            case clrStdcomm:
                return bits.str2lst(beg + "clear stdcomm");
            case clrExtcomm:
                return bits.str2lst(beg + "clear extcomm");
            case clrLrgcomm:
                return bits.str2lst(beg + "clear lrgcomm");
            case clrPrivas:
                return bits.str2lst(beg + "clear privateas");
            case setStdcomm:
                return bits.str2lst(beg + "set stdcomm " + tabRtrmapN.stdComms2string(intLst));
            case setExtcomm:
                return bits.str2lst(beg + "set extcomm " + tabRtrmapN.extComms2string(lngLst));
            case setLrgcomm:
                return bits.str2lst(beg + "set lrgcomm " + tabRtrmapN.lrgComms2string(lrgLst));
            case setNexthop:
                return bits.str2lst(beg + "set nexthop " + nexthopSet);
            case setAspath:
                return bits.str2lst(beg + "set aspath " + tabRouteAttr.dumpIntList(intLst, "", ""));
            case setAsconf:
                return bits.str2lst(beg + "set asconfed " + tabRouteAttr.dumpIntList(intLst, "", ""));
            case setDistance:
                return bits.str2lst(beg + "set distance " + intSet);
            case setMetric:
                return bits.str2lst(beg + "set metric " + intSet);
            case setOrigin:
                return bits.str2lst(beg + "set origin " + intSet);
            case setLocPref:
                return bits.str2lst(beg + "set locpref " + intSet);
            case setAccIgp:
                return bits.str2lst(beg + "set aigp " + intSet);
            case setBandwidth:
                return bits.str2lst(beg + "set bandwidth " + intSet);
            case setTag:
                return bits.str2lst(beg + "set tag " + intSet);
            case setSegrou:
                return bits.str2lst(beg + "set segrout " + intSet);
            case setBier:
                return bits.str2lst(beg + "set bier " + intSet);
            case setRoumap:
                return bits.str2lst(beg + "set route-map " + roumap);
            case setRouplc:
                return bits.str2lst(beg + "set route-policy " + rouplc);
            default:
                return bits.str2lst(beg + "unknown=" + doMode);
        }
    }

    public boolean matches(int afi, addrPrefix<addrIP> net) {
        tabRouteEntry<addrIP> nt = new tabRouteEntry<addrIP>();
        nt.prefix = net;
        return matches(afi, nt);
    }

    private String ifString() {
        switch (ifMode) {
            case never:
                return "never";
            case always:
                return "always";
            case aspath:
                return "aspath " + strVal;
            case stdcomm:
                return "stdcomm " + tabRtrmapN.stdComms2string(intLst);
            case extcomm:
                return "extcomm " + tabRtrmapN.extComms2string(lngLst);
            case lrgcomm:
                return "lrgcomm " + tabRtrmapN.lrgComms2string(lrgLst);
            case roudst:
                return "rd " + tabRtrmapN.rd2string(rouDstMatch);
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
            case prfxlst:
                return "prefix-list " + prfxlst;
            case roumap:
                return "route-map " + roumap;
            case rouplc:
                return "route-policy " + rouplc;
            case distance:
                return "distance " + intMatch;
            case metric:
                return "metric " + intMatch;
            case origin:
                return "origin " + intMatch;
            case locpref:
                return "locpref " + intMatch;
            case accigp:
                return "accigp " + intMatch;
            case validity:
                return "validity " + intMatch;
            case pathlen:
                return "pathlen " + intMatch;
            case bandwidth:
                return "bandwidth " + intMatch;
            case tag:
                return "tag " + intMatch;
            case segrou:
                return "segrout " + intMatch;
            case bier:
                return "bier " + intMatch;
            case afi:
                return "afi " + intMatch;
            case safi:
                return "safi " + intMatch;
            case nexthop:
                return "nexthop " + nexthopSet;
            default:
                return "unknown=" + ifMode;
        }
    }

    public boolean matches(int afi, tabRouteEntry<addrIP> net) {
        switch (ifMode) {
            case never:
                return false;
            case always:
                return true;
            case aspath:
                return net.best.asPathStr().matches(strVal);
            case stdcomm:
                for (int i = 0; i < intLst.size(); i++) {
                    if (rtrBgpUtil.findIntList(net.best.stdComm, intLst.get(i)) < 0) {
                        return false;
                    }
                }
                return true;
            case extcomm:
                for (int i = 0; i < lngLst.size(); i++) {
                    if (rtrBgpUtil.findLongList(net.best.extComm, lngLst.get(i)) < 0) {
                        return false;
                    }
                }
                return true;
            case lrgcomm:
                for (int i = 0; i < lrgLst.size(); i++) {
                    if (rtrBgpUtil.findLrgList(net.best.lrgComm, lrgLst.get(i)) < 0) {
                        return false;
                    }
                }
                return true;
            case roudst:
                return rouDstMatch == net.rouDst;
            case network:
                return networkMatch.matches(afi, net.prefix);
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
                int i = rtrBgpUtil.removePrivateAs(tabLabel.copyLabels(net.best.pathSeq));
                i += rtrBgpUtil.removePrivateAs(tabLabel.copyLabels(net.best.pathSet));
                return i > 0;
            case prfxlst:
                return prfxlst.matches(afi, net.prefix);
            case roumap:
                return roumap.matches(afi, net);
            case rouplc:
                return tabRtrplc.doRpl(afi, net, rouplc, true) != null;
            case distance:
                return intMatch.matches(net.best.distance);
            case metric:
                return intMatch.matches(net.best.metric);
            case origin:
                return intMatch.matches(net.best.origin);
            case locpref:
                return intMatch.matches(net.best.locPref);
            case accigp:
                return intMatch.matches(net.best.accIgp);
            case validity:
                return intMatch.matches(net.best.validity);
            case pathlen:
                return intMatch.matches(net.best.asPathLen());
            case bandwidth:
                return intMatch.matches(net.best.bandwidth);
            case tag:
                return intMatch.matches(net.best.tag);
            case segrou:
                return intMatch.matches(net.best.segrouIdx);
            case bier:
                return intMatch.matches(net.best.bierIdx);
            case afi:
                return intMatch.matches(afi & rtrBgpUtil.afiMask);
            case safi:
                return intMatch.matches(afi & rtrBgpUtil.safiMask);
            case nexthop:
                if (net.best.nextHop == null) {
                    return false;
                }
                return nexthopSet.compare(nexthopSet, net.best.nextHop) == 0;
            default:
                return true;
        }
    }

    public boolean matches(packHolder pck) {
        return matches(rtrBgpUtil.safiUnicast, new addrPrefix<addrIP>(pck.IPsrc, new addrIP().maxBits()));
    }

    public void update(int afi, tabRouteEntry<addrIP> net) {
        switch (doMode) {
            case clrStdcomm:
                net.best.stdComm = null;
                return;
            case clrExtcomm:
                net.best.extComm = null;
                return;
            case clrLrgcomm:
                net.best.lrgComm = null;
                return;
            case clrPrivas:
                rtrBgpUtil.removePrivateAs(net.best.pathSeq);
                rtrBgpUtil.removePrivateAs(net.best.pathSet);
                return;
            case setStdcomm:
                net.best.stdComm = tabLabel.prependLabels(net.best.stdComm, intLst);
                return;
            case setExtcomm:
                if (net.best.extComm == null) {
                    net.best.extComm = new ArrayList<Long>();
                }
                net.best.extComm.addAll(lngLst);
                return;
            case setLrgcomm:
                if (net.best.lrgComm == null) {
                    net.best.lrgComm = new ArrayList<tabLargeComm>();
                }
                net.best.lrgComm.addAll(lrgLst);
                return;
            case setNexthop:
                net.best.nextHop = nexthopSet.copyBytes();
                return;
            case setAspath:
                net.best.pathSeq = tabLabel.prependLabels(net.best.pathSeq, intLst);
                return;
            case setAsconf:
                net.best.confSeq = tabLabel.prependLabels(net.best.confSeq, intLst);
                return;
            case setDistance:
                net.best.distance = intSet.update(net.best.distance);
                return;
            case setMetric:
                net.best.metric = intSet.update(net.best.metric);
                return;
            case setOrigin:
                net.best.origin = intSet.update(net.best.origin);
                return;
            case setLocPref:
                net.best.locPref = intSet.update(net.best.locPref);
                return;
            case setAccIgp:
                net.best.accIgp = intSet.update(net.best.accIgp);
                return;
            case setBandwidth:
                net.best.bandwidth = intSet.update(net.best.bandwidth);
                return;
            case setTag:
                net.best.tag = intSet.update(net.best.tag);
                return;
            case setSegrou:
                net.best.segrouIdx = intSet.update(net.best.segrouIdx);
                return;
            case setBier:
                net.best.bierIdx = intSet.update(net.best.bierIdx);
                return;
            case setRoumap:
                roumap.update(afi, net, false);
                return;
            case setRouplc:
                tabRtrplc.doRpl(afi, net, rouplc, false);
                return;
            default:
                break;
        }
    }

}
