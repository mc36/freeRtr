package tab;

import addr.addrIP;
import addr.addrPrefix;
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
        tag
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
            case setStdcomm:
                return bits.str2lst(beg + "set stdcomm " + tabRtrmapN.stdComms2string(intLst));
            case setExtcomm:
                return bits.str2lst(beg + "set extcomm " + tabRtrmapN.extComms2string(lngLst));
            case setLrgcomm:
                return bits.str2lst(beg + "set lrgcomm " + tabRtrmapN.lrgComms2string(lrgLst));
            case setNexthop:
                return bits.str2lst(beg + "set nexthop " + nexthopSet);
            case setAspath:
                return bits.str2lst(beg + "set aspath " + tabRouteEntry.dumpIntList(intLst, "", ""));
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
            case setRoumap:
                return bits.str2lst(beg + "set route-map " + roumap);
            case setRouplc:
                return bits.str2lst(beg + "set route-policy " + rouplc);
            default:
                return bits.str2lst(beg + "unknown=" + doMode);
        }
    }

    public boolean matches(addrPrefix<addrIP> net) {
        tabRouteEntry<addrIP> nt = new tabRouteEntry<addrIP>();
        nt.prefix = net;
        return matches(nt);
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
            default:
                return "unknown=" + ifMode;
        }
    }

    public boolean matches(tabRouteEntry<addrIP> net) {
        switch (ifMode) {
            case never:
                return false;
            case always:
                return true;
            case aspath:
                return net.asPathStr().matches(strVal);
            case stdcomm:
                for (int i = 0; i < intLst.size(); i++) {
                    if (rtrBgpUtil.findIntList(net.stdComm, intLst.get(i)) < 0) {
                        return false;
                    }
                }
                return true;
            case extcomm:
                for (int i = 0; i < lngLst.size(); i++) {
                    if (rtrBgpUtil.findLongList(net.extComm, lngLst.get(i)) < 0) {
                        return false;
                    }
                }
                return true;
            case lrgcomm:
                for (int i = 0; i < lrgLst.size(); i++) {
                    if (rtrBgpUtil.findLrgList(net.lrgComm, lrgLst.get(i)) < 0) {
                        return false;
                    }
                }
                return true;
            case roudst:
                return rouDstMatch == net.rouDst;
            case network:
                return networkMatch.matches(net.prefix);
            case nostdcomm:
                if (net.stdComm != null) {
                    if (net.stdComm.size() > 0) {
                        return false;
                    }
                }
                return true;
            case noextcomm:
                if (net.extComm != null) {
                    if (net.extComm.size() > 0) {
                        return false;
                    }
                }
                return true;
            case nolrgcomm:
                if (net.lrgComm != null) {
                    if (net.lrgComm.size() > 0) {
                        return false;
                    }
                }
                return true;
            case prfxlst:
                return prfxlst.matches(net.prefix);
            case roumap:
                tabRtrmapN ntry = roumap.find(net);
                if (ntry == null) {
                    return false;
                }
                if (ntry.action == tabPlcmapN.actionType.actDeny) {
                    return false;
                }
                return true;
            case rouplc:
                return tabRtrplc.doRpl(net, rouplc, true) != null;
            case distance:
                return intMatch.matches(net.distance);
            case metric:
                return intMatch.matches(net.metric);
            case origin:
                return intMatch.matches(net.origin);
            case locpref:
                return intMatch.matches(net.locPref);
            case accigp:
                return intMatch.matches(net.accIgp);
            case validity:
                return intMatch.matches(net.validity);
            case pathlen:
                return intMatch.matches(net.asPathLen());
            case bandwidth:
                return intMatch.matches(net.bandwidth);
            case tag:
                return intMatch.matches(net.tag);
            default:
                return true;
        }
    }

    public boolean matches(packHolder pck) {
        return matches(new addrPrefix<addrIP>(pck.IPsrc, new addrIP().maxBits()));
    }

    public void update(tabRouteEntry<addrIP> net) {
        switch (doMode) {
            case clrStdcomm:
                net.stdComm = null;
                return;
            case clrExtcomm:
                net.extComm = null;
                return;
            case clrLrgcomm:
                net.lrgComm = null;
                return;
            case setStdcomm:
                net.stdComm = tabLabel.prependLabels(net.stdComm, intLst);
                return;
            case setExtcomm:
                if (net.extComm == null) {
                    net.extComm = new ArrayList<Long>();
                }
                net.extComm.addAll(lngLst);
                return;
            case setLrgcomm:
                if (net.lrgComm == null) {
                    net.lrgComm = new ArrayList<tabLargeComm>();
                }
                net.lrgComm.addAll(lrgLst);
                return;
            case setNexthop:
                net.nextHop = nexthopSet.copyBytes();
                return;
            case setAspath:
                net.pathSeq = tabLabel.prependLabels(net.pathSeq, intLst);
                return;
            case setDistance:
                net.distance = intSet.update(net.distance);
                return;
            case setMetric:
                net.metric = intSet.update(net.metric);
                return;
            case setOrigin:
                net.origin = intSet.update(net.origin);
                return;
            case setLocPref:
                net.locPref = intSet.update(net.locPref);
                return;
            case setAccIgp:
                net.accIgp = intSet.update(net.accIgp);
                return;
            case setBandwidth:
                net.bandwidth = intSet.update(net.bandwidth);
                return;
            case setTag:
                net.tag = intSet.update(net.tag);
                return;
            case setRoumap:
                roumap.update(net, false);
                return;
            case setRouplc:
                tabRtrplc.doRpl(net, rouplc, false);
                return;
        }
    }

}
