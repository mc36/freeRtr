package tab;

import addr.addrIP;
import addr.addrPrefix;
import addr.addrType;
import cfg.cfgAll;
import ip.ipFwd;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import util.bits;
import util.counter;

/**
 * represents one route table entry
 *
 * @param <T> class of address
 * @author matecsaba
 */
public class tabRouteEntry<T extends addrType> implements Comparator<tabRouteEntry<T>> {

    /**
     * route type
     */
    public enum routeType {

        /**
         * connected
         */
        conn,
        /**
         * local
         */
        local,
        /**
         * remote
         */
        remote,
        /**
         * auto mesh
         */
        automesh,
        /**
         * through gateway
         */
        defpref,
        /**
         * static
         */
        staticRoute,
        /**
         * ipv4 rip
         */
        rip4,
        /**
         * ipv6 rip
         */
        rip6,
        /**
         * ipv4 babel
         */
        babel4,
        /**
         * ipv6 babel
         */
        babel6,
        /**
         * ipv4 babel
         */
        olsr4,
        /**
         * ipv6 olsr
         */
        olsr6,
        /**
         * ipv4 ospf
         */
        ospf4,
        /**
         * ipv6 ospf
         */
        ospf6,
        /**
         * ipv4 isis
         */
        isis4,
        /**
         * ipv6 isis
         */
        isis6,
        /**
         * ipv4 pvrp
         */
        pvrp4,
        /**
         * ipv6 pvrp
         */
        pvrp6,
        /**
         * ipv4 lsrp
         */
        lsrp4,
        /**
         * ipv6 lsrp
         */
        lsrp6,
        /**
         * ipv4 eigrp
         */
        eigrp4,
        /**
         * ipv6 eigrp
         */
        eigrp6,
        /**
         * ipv4 bgp
         */
        bgp4,
        /**
         * ipv6 bgp
         */
        bgp6,
        /**
         * ipv4 msdp
         */
        msdp4,
        /**
         * ipv6 msdp
         */
        msdp6,
        /**
         * ipv4 flowspec
         */
        flwspc4,
        /**
         * ipv6 flowspec
         */
        flwspc6,
        /**
         * ipv4 unicast to multicast
         */
        uni2multi4,
        /**
         * ipv6 unicast to multicast
         */
        uni2multi6,
        /**
         * ipv4 unicast to flowspec
         */
        uni2flow4,
        /**
         * ipv6 unicast to flowspec
         */
        uni2flow6,
        /**
         * ipv4 logger
         */
        logger4,
        /**
         * ipv6 logger
         */
        logger6,
        /**
         * ipv4 download
         */
        download4,
        /**
         * ipv6 download
         */
        download6,
        /**
         * ipv4 deaggregation
         */
        deaggr4,
        /**
         * ipv6 deaggregation
         */
        deaggr6,
        /**
         * ipv4 mobile
         */
        mobile4,
        /**
         * ipv6 mobile
         */
        mobile6,

    }

    /**
     * distance limit
     */
    public final static int distanLim = 0x3fffffff;

    /**
     * max distance
     */
    public final static int distanMax = 255;

    /**
     * iface distance
     */
    public final static int distanIfc = 1;

    /**
     * distance of prefix
     */
    public int distance;

    /**
     * metric of prefix
     */
    public int metric;

    /**
     * tag of prefix
     */
    public int tag;

    /**
     * validity type
     */
    public int validity;

    /**
     * segment routing index
     */
    public int segrouIdx;

    /**
     * segment routing base
     */
    public int segrouBeg;

    /**
     * segment routing old base
     */
    public int segrouOld;

    /**
     * segment routing label block size
     */
    public int segrouSiz;

    /**
     * bier index
     */
    public int bierIdx;

    /**
     * bier base
     */
    public int bierBeg;

    /**
     * bier old base
     */
    public int bierOld;

    /**
     * bier bsl
     */
    public int bierHdr;

    /**
     * bier label block size
     */
    public int bierSiz;

    /**
     * origin type
     */
    public int origin;

    /**
     * local preference
     */
    public int locPref;

    /**
     * accumulated igp
     */
    public int accIgp;

    /**
     * evpn label
     */
    public int evpnLab;

    /**
     * pmsi flag & type
     */
    public int pmsiTyp;

    /**
     * pmsi label
     */
    public int pmsiLab;

    /**
     * pmsi tunnel id
     */
    public byte[] pmsiTun;

    /**
     * tunnel type
     */
    public int tunelTyp;

    /**
     * tunnel value
     */
    public byte[] tunelVal;

    /**
     * attrib as
     */
    public int attribAs;

    /**
     * attrib value
     */
    public byte[] attribVal;

    /**
     * bandwidth
     */
    public int bandwidth;

    /**
     * source router
     */
    public addrType srcRtr;

    /**
     * atomic aggregator
     */
    public boolean atomicAggr;

    /**
     * aggregator as
     */
    public int aggrAs;

    /**
     * aggregator router
     */
    public T aggrRtr;

    /**
     * standard community values
     */
    public List<Integer> stdComm;

    /**
     * extended community values
     */
    public List<Long> extComm;

    /**
     * large community values
     */
    public List<tabLargeComm> lrgComm;

    /**
     * as path sequence
     */
    public List<Integer> pathSeq;

    /**
     * as path set
     */
    public List<Integer> pathSet;

    /**
     * confederation sequence
     */
    public List<Integer> confSeq;

    /**
     * confederation set
     */
    public List<Integer> confSet;

    /**
     * originator id
     */
    public T originator;

    /**
     * cluster list
     */
    public List<T> clustList;

    /**
     * route type
     */
    public routeType rouTyp;

    /**
     * route source
     */
    public int rouSrc;

    /**
     * time when updated
     */
    public long time;

    /**
     * version when updated
     */
    public int version;

    /**
     * next hop address
     */
    public T nextHop;

    /**
     * old next hop
     */
    public T oldHop;

    /**
     * protocol identifier number
     */
    public int protoNum = -1;

    /**
     * interface id
     */
    public tabRouteIface iface;

    /**
     * local label
     */
    public tabLabelNtry labelLoc = null;

    /**
     * remote label
     */
    public List<Integer> labelRem = null;

    /**
     * route distinguisher
     */
    public long rouDst;

    /**
     * route table to use
     */
    public ipFwd rouTab;

    /**
     * prefix
     */
    public addrPrefix<T> prefix;

    /**
     * counter
     */
    public counter cntr;

    /**
     * convert route type to string
     *
     * @param i route type
     * @return displayable string
     */
    public static String rouTyp2string(routeType i) {
        if (i == null) {
            return "null";
        }
        switch (i) {
            case conn:
                return "C";
            case staticRoute:
                return "S";
            case rip4:
            case rip6:
                return "R";
            case babel4:
            case babel6:
                return "A";
            case olsr4:
            case olsr6:
                return "N";
            case ospf4:
            case ospf6:
                return "O";
            case isis4:
            case isis6:
                return "I";
            case pvrp4:
            case pvrp6:
                return "P";
            case lsrp4:
            case lsrp6:
                return "L";
            case eigrp4:
            case eigrp6:
                return "D";
            case bgp4:
            case bgp6:
                return "B";
            case local:
                return "LOC";
            case remote:
                return "RMT";
            case defpref:
                return "DEF";
            case automesh:
                return "MSH";
            case msdp4:
            case msdp6:
                return "MSD";
            case flwspc4:
            case flwspc6:
                return "FLW";
            case uni2multi4:
            case uni2multi6:
                return "UNI";
            case uni2flow4:
            case uni2flow6:
                return "UNI";
            case logger4:
            case logger6:
                return "LOG";
            case download4:
            case download6:
                return "DWN";
            case deaggr4:
            case deaggr6:
                return "DEA";
            case mobile4:
            case mobile6:
                return "MOB";
            default:
                return "?";
        }
    }

    /**
     * clone this table entry
     *
     * @return new instance containing same data
     */
    @SuppressWarnings("unchecked")
    public tabRouteEntry<T> copyBytes() {
        tabRouteEntry<T> prf = new tabRouteEntry<T>();
        prf.rouDst = rouDst;
        prf.rouTab = rouTab;
        if (prefix != null) {
            prf.prefix = prefix.copyBytes();
        }
        prf.distance = distance;
        prf.metric = metric;
        prf.tag = tag;
        prf.origin = origin;
        prf.validity = validity;
        prf.segrouIdx = segrouIdx;
        prf.segrouSiz = segrouSiz;
        prf.segrouBeg = segrouBeg;
        prf.segrouOld = segrouOld;
        prf.bierIdx = bierIdx;
        prf.bierHdr = bierHdr;
        prf.bierBeg = bierBeg;
        prf.bierOld = bierOld;
        prf.bierSiz = bierSiz;
        prf.locPref = locPref;
        prf.accIgp = accIgp;
        prf.tunelTyp = tunelTyp;
        prf.attribAs = attribAs;
        prf.pmsiTyp = pmsiTyp;
        prf.pmsiLab = pmsiLab;
        prf.evpnLab = evpnLab;
        prf.bandwidth = bandwidth;
        prf.atomicAggr = atomicAggr;
        prf.aggrAs = aggrAs;
        if (attribVal != null) {
            prf.attribVal = new byte[attribVal.length];
            bits.byteCopy(attribVal, 0, prf.attribVal, 0, attribVal.length);
        }
        if (tunelVal != null) {
            prf.tunelVal = new byte[tunelVal.length];
            bits.byteCopy(tunelVal, 0, prf.tunelVal, 0, tunelVal.length);
        }
        if (pmsiTun != null) {
            prf.pmsiTun = new byte[pmsiTun.length];
            bits.byteCopy(pmsiTun, 0, prf.pmsiTun, 0, pmsiTun.length);
        }
        if (aggrRtr != null) {
            prf.aggrRtr = (T) aggrRtr.copyBytes();
        }
        if (srcRtr != null) {
            prf.srcRtr = srcRtr.copyBytes();
        }
        prf.stdComm = tabLabel.copyLabels(stdComm);
        prf.extComm = copyLongList(extComm);
        prf.pathSeq = tabLabel.copyLabels(pathSeq);
        prf.pathSet = tabLabel.copyLabels(pathSet);
        prf.confSeq = tabLabel.copyLabels(confSeq);
        prf.confSet = tabLabel.copyLabels(confSet);
        if (originator != null) {
            prf.originator = (T) originator.copyBytes();
        }
        if (clustList != null) {
            prf.clustList = new ArrayList<T>();
            for (int i = 0; i < clustList.size(); i++) {
                prf.clustList.add((T) clustList.get(i).copyBytes());
            }
        }
        if (lrgComm != null) {
            prf.lrgComm = new ArrayList<tabLargeComm>();
            for (int i = 0; i < lrgComm.size(); i++) {
                prf.lrgComm.add(lrgComm.get(i).copyBytes());
            }
        }
        if (nextHop != null) {
            prf.nextHop = (T) nextHop.copyBytes();
        }
        if (oldHop != null) {
            prf.oldHop = (T) oldHop.copyBytes();
        }
        prf.labelRem = tabLabel.copyLabels(labelRem);
        prf.labelLoc = labelLoc;
        prf.protoNum = protoNum;
        prf.rouTyp = rouTyp;
        prf.rouSrc = rouSrc;
        prf.time = time;
        prf.version = version;
        prf.iface = iface;
        prf.cntr = cntr;
        return prf;
    }

    private static boolean diffIntList(List<Integer> l1, List<Integer> l2) {
        if (l1 == null) {
            return l2 != null;
        }
        if (l2 == null) {
            return true;
        }
        if (l1.size() != l2.size()) {
            return true;
        }
        for (int i = 0; i < l1.size(); i++) {
            if (l1.get(i).compareTo(l2.get(i)) != 0) {
                return true;
            }
        }
        return false;
    }

    private static List<Long> copyLongList(List<Long> src) {
        if (src == null) {
            return null;
        }
        List<Long> trg = new ArrayList<Long>();
        for (int i = 0; i < src.size(); i++) {
            trg.add((long) src.get(i));
        }
        return trg;
    }

    private static boolean diffLongList(List<Long> l1, List<Long> l2) {
        if (l1 == null) {
            return l2 != null;
        }
        if (l2 == null) {
            return true;
        }
        if (l1.size() != l2.size()) {
            return true;
        }
        for (int i = 0; i < l1.size(); i++) {
            if (l1.get(i).compareTo(l2.get(i)) != 0) {
                return true;
            }
        }
        return false;
    }

    /**
     * check if differs from other
     *
     * @param other other to test
     * @return false on equals, true on differs
     */
    public boolean differs(tabRouteEntry<T> other) {
        if (other == null) {
            return true;
        }
        if (rouTyp != other.rouTyp) {
            return true;
        }
        if (iface != other.iface) {
            return true;
        }
        if (rouSrc != other.rouSrc) {
            return true;
        }
        if (protoNum != other.protoNum) {
            return true;
        }
        if (distance != other.distance) {
            return true;
        }
        if (metric != other.metric) {
            return true;
        }
        if (tag != other.tag) {
            return true;
        }
        if (segrouIdx != other.segrouIdx) {
            return true;
        }
        if (segrouBeg != other.segrouBeg) {
            return true;
        }
        if (segrouOld != other.segrouOld) {
            return true;
        }
        if (segrouSiz != other.segrouSiz) {
            return true;
        }
        if (bierIdx != other.bierIdx) {
            return true;
        }
        if (bierBeg != other.bierBeg) {
            return true;
        }
        if (bierOld != other.bierOld) {
            return true;
        }
        if (bierSiz != other.bierSiz) {
            return true;
        }
        if (bierHdr != other.bierHdr) {
            return true;
        }
        if (validity != other.validity) {
            return true;
        }
        if (origin != other.origin) {
            return true;
        }
        if (locPref != other.locPref) {
            return true;
        }
        if (accIgp != other.accIgp) {
            return true;
        }
        if (attribAs != other.attribAs) {
            return true;
        }
        if (tunelTyp != other.tunelTyp) {
            return true;
        }
        if (pmsiTyp != other.pmsiTyp) {
            return true;
        }
        if (pmsiLab != other.pmsiLab) {
            return true;
        }
        if (evpnLab != other.evpnLab) {
            return true;
        }
        if (bandwidth != other.bandwidth) {
            return true;
        }
        if (atomicAggr != other.atomicAggr) {
            return true;
        }
        if (aggrAs != other.aggrAs) {
            return true;
        }
        if (labelLoc != null) {
            if (other.labelLoc == null) {
                return true;
            }
            if (labelLoc.compare(labelLoc, other.labelLoc) != 0) {
                return true;
            }
        } else if (other.labelLoc != null) {
            return true;
        }
        if (srcRtr != null) {
            if (other.srcRtr == null) {
                return true;
            }
            if (srcRtr.getSize() != other.srcRtr.getSize()) {
                return true;
            }
            if (srcRtr.compare(srcRtr, other.srcRtr) != 0) {
                return true;
            }
        } else if (other.srcRtr != null) {
            return true;
        }
        if (aggrRtr != null) {
            if (other.aggrRtr == null) {
                return true;
            }
            if (aggrRtr.compare(aggrRtr, other.aggrRtr) != 0) {
                return true;
            }
        } else if (other.aggrRtr != null) {
            return true;
        }
        if (compare(this, other) != 0) {
            return true;
        }
        if (nextHop != null) {
            if (other.nextHop == null) {
                return true;
            }
            if (nextHop.compare(nextHop, other.nextHop) != 0) {
                return true;
            }
        } else if (other.nextHop != null) {
            return true;
        }
        if (oldHop != null) {
            if (other.oldHop == null) {
                return true;
            }
            if (oldHop.compare(oldHop, other.oldHop) != 0) {
                return true;
            }
        } else if (other.oldHop != null) {
            return true;
        }
        if (originator != null) {
            if (other.originator == null) {
                return true;
            }
            if (originator.compare(originator, other.originator) != 0) {
                return true;
            }
        } else if (other.originator != null) {
            return true;
        }
        if (diffIntList(labelRem, other.labelRem)) {
            return true;
        }
        if (diffIntList(stdComm, other.stdComm)) {
            return true;
        }
        if (diffLongList(extComm, other.extComm)) {
            return true;
        }
        if (diffIntList(pathSeq, other.pathSeq)) {
            return true;
        }
        if (diffIntList(pathSet, other.pathSet)) {
            return true;
        }
        if (diffIntList(confSeq, other.confSeq)) {
            return true;
        }
        if (diffIntList(confSet, other.confSet)) {
            return true;
        }
        if (attribVal != null) {
            if (other.attribVal == null) {
                return true;
            }
            if (attribVal.length != other.attribVal.length) {
                return true;
            }
            if (bits.byteComp(attribVal, 0, other.attribVal, 0, attribVal.length) != 0) {
                return true;
            }
        } else if (other.attribVal != null) {
            return true;
        }
        if (tunelVal != null) {
            if (other.tunelVal == null) {
                return true;
            }
            if (tunelVal.length != other.tunelVal.length) {
                return true;
            }
            if (bits.byteComp(tunelVal, 0, other.tunelVal, 0, tunelVal.length) != 0) {
                return true;
            }
        } else if (other.tunelVal != null) {
            return true;
        }
        if (pmsiTun != null) {
            if (other.pmsiTun == null) {
                return true;
            }
            if (pmsiTun.length != other.pmsiTun.length) {
                return true;
            }
            if (bits.byteComp(pmsiTun, 0, other.pmsiTun, 0, pmsiTun.length) != 0) {
                return true;
            }
        } else if (other.pmsiTun != null) {
            return true;
        }
        if (clustList != null) {
            if (other.clustList == null) {
                return true;
            }
            if (clustList.size() != other.clustList.size()) {
                return true;
            }
            for (int i = 0; i < clustList.size(); i++) {
                T cmp = clustList.get(i);
                if (cmp.compare(cmp, other.clustList.get(i)) != 0) {
                    return true;
                }
            }
        } else if (other.clustList != null) {
            return true;
        }
        if (lrgComm != null) {
            if (other.lrgComm == null) {
                return true;
            }
            if (lrgComm.size() != other.lrgComm.size()) {
                return true;
            }
            for (int i = 0; i < lrgComm.size(); i++) {
                tabLargeComm cmp = lrgComm.get(i);
                if (cmp.compare(cmp, other.lrgComm.get(i)) != 0) {
                    return true;
                }
            }
        } else if (other.lrgComm != null) {
            return true;
        }
        return false;
    }

    /**
     * need to update with this prefix
     *
     * @param imp new prefix
     * @return true if yes, false if not
     */
    public boolean isOtherBetter(tabRouteEntry<T> imp) {
        if (imp.distance < distance) {
            return true;
        }
        if (imp.distance > distance) {
            return false;
        }
        if (imp.validity < validity) {
            return true;
        }
        if (imp.validity > validity) {
            return false;
        }
        if (imp.locPref > locPref) {
            return true;
        }
        if (imp.locPref < locPref) {
            return false;
        }
        if (imp.accIgp < accIgp) {
            return true;
        }
        if (imp.accIgp > accIgp) {
            return false;
        }
        int il = imp.asPathLen();
        int ol = asPathLen();
        if (il < ol) {
            return true;
        }
        if (il > ol) {
            return false;
        }
        if (imp.origin < origin) {
            return true;
        }
        if (imp.origin > origin) {
            return false;
        }
        if (imp.metric < metric) {
            return true;
        }
        if (imp.metric > metric) {
            return false;
        }
        il = listLen(imp.clustList);
        ol = listLen(clustList);
        if (il < ol) {
            return true;
        }
        if (il > ol) {
            return false;
        }
        return imp.time < time;
    }

    public String toString() {
        return "" + prefix;
    }

    /**
     * convert to route format
     *
     * @param prf entry to dump
     * @return converted
     */
    public static String toShSrRoute(tabRouteEntry<addrIP> prf) {
        if (prf.segrouIdx < 1) {
            return null;
        }
        return addrPrefix.ip2str(prf.prefix) + "|" + prf.segrouIdx + "|" + prf.segrouBeg + "|" + prf.segrouOld;
    }

    /**
     * convert to route format
     *
     * @param prf entry to dump
     * @return converted
     */
    public static String toShBrRoute(tabRouteEntry<addrIP> prf) {
        if (prf.bierIdx < 1) {
            return null;
        }
        return addrPrefix.ip2str(prf.prefix) + "|" + prf.bierIdx + "|" + prf.bierBeg + "|" + prf.bierOld + "|" + prf.bierHdr + "-" + tabLabelBier.bsl2num(prf.bierHdr);
    }

    /**
     * convert to route format
     *
     * @param prf entry to dump
     * @return converted
     */
    public static String toShRoute(tabRouteEntry<addrIP> prf) {
        String s = "";
        if (prf.rouTab != null) {
            s = "@" + prf.rouTab.vrfName;
        }
        return rouTyp2string(prf.rouTyp) + "|" + addrPrefix.ip2str(prf.prefix)
                + "|" + prf.distance + "/" + prf.metric + "|"
                + prf.iface + s + "|"
                + prf.nextHop + "|" + bits.timePast(prf.time);
    }

    /**
     * convert to bgp format
     *
     * @param prf entry to dump
     * @return converted
     */
    public static String toShBgp(tabRouteEntry<addrIP> prf) {
        return toShBgpFirst(prf) + toShBgpLast(prf);
    }

    /**
     * convert to evpn format
     *
     * @param prf entry to dump
     * @return converted
     */
    public static String toShEvpn(tabRouteEntry<addrIP> prf) {
        return addrPrefix.ip2evpn(prf.prefix) + " " + tabRtrmapN.rd2string(prf.rouDst) + toShBgpLast(prf);
    }

    /**
     * convert to bgp format
     *
     * @param prf entry to dump
     * @param evpn evpn
     * @return converted
     */
    public static String toShBgpLabels(tabRouteEntry<addrIP> prf, boolean evpn) {
        String a;
        if (evpn) {
            a = addrPrefix.ip2evpn(prf.prefix);
        } else {
            a = toShBgpFirst(prf);
        }
        return a + "|" + prf.labelLoc + "|" + prf.evpnLab + "|" + prf.pmsiLab + "|" + dumpIntList(prf.labelRem, "", "") + "|" + prf.nextHop;
    }

    /**
     * convert to counter format
     *
     * @param prf entry to dump
     * @return converted
     */
    public static String toShCntr(tabRouteEntry<addrIP> prf) {
        if (prf.cntr == null) {
            return null;
        }
        return addrPrefix.ip2str(prf.prefix) + "|" + bits.timePast(prf.time) + "|" + prf.cntr.getShStat();
    }

    /**
     * convert to bgp format
     *
     * @param prf entry to dump
     * @return converted
     */
    public static String toShBgpFirst(tabRouteEntry<addrIP> prf) {
        String s = "";
        if (prf.rouDst != 0) {
            s = " " + tabRtrmapN.rd2string(prf.rouDst);
        }
        return addrPrefix.ip2str(prf.prefix) + s;
    }

    /**
     * convert to bgp format
     *
     * @param prf entry to dump
     * @return converted
     */
    private static String toShBgpLast(tabRouteEntry<addrIP> prf) {
        return "|" + prf.nextHop + "|"
                + prf.distance + "/" + prf.locPref + "/" + prf.origin + "/"
                + prf.metric + "|" + prf.asPathStr();
    }

    /**
     * convert to ldp format
     *
     * @param prf entry to dump
     * @return converted
     */
    public static String toShLdp(tabRouteEntry<addrIP> prf) {
        return addrPrefix.ip2str(prf.prefix) + "|" + prf.labelLoc + "|" + dumpIntList(prf.labelRem, "", "") + "|" + prf.nextHop;
    }

    /**
     * convert to rpki format
     *
     * @param prf entry to dump
     * @return converted
     */
    public static String toShRpki(tabRouteEntry<addrIP> prf) {
        return addrPrefix.ip2str(prf.prefix) + "|" + prf.metric + "|" + prf.rouSrc;
    }

    /**
     * size of list
     *
     * @param lst list to check
     * @return size of list
     */
    public static int listLen(List<?> lst) {
        if (lst == null) {
            return 0;
        }
        return lst.size();
    }

    /**
     * size of as path
     *
     * @return size of as path
     */
    public int asPathLen() {
        int i = listLen(pathSeq);
        if (listLen(pathSet) > 0) {
            i++;
        }
        return i;
    }

    /**
     * convert as path to string
     *
     * @return converted string
     */
    public String asPathStr() {
        return dumpIntList(confSeq, "(", ") ")
                + dumpIntList(confSet, "[", "] ")
                + dumpIntList(pathSeq, "", "")
                + dumpIntList(pathSet, " {", "}");
    }

    /**
     * dump integer list
     *
     * @param l list to dump
     * @param beg beginning
     * @param end ending
     * @return dumped list
     */
    public static String dumpIntList(List<Integer> l, String beg, String end) {
        if (l == null) {
            return "";
        }
        if (l.size() < 1) {
            return "";
        }
        String s = "";
        for (int i = 0; i < l.size(); i++) {
            s += " " + bits.num2str(l.get(i));
        }
        return beg + s.substring(1, s.length()) + end;
    }

    /**
     * dump address list
     *
     * @param <T> type of address
     * @param l address list
     * @return dumped list
     */
    public static <T extends addrType> String dumpAddrList(List<T> l) {
        if (l == null) {
            return "";
        }
        if (l.size() < 1) {
            return "";
        }
        String s = "";
        for (int i = 0; i < l.size(); i++) {
            s += " " + l.get(i);
        }
        return s.substring(1, s.length());
    }

    /**
     * full dump of this prefix
     *
     * @param fwd forwarding core to use
     * @return list describes this prefix
     */
    public List<String> fullDump(ipFwd fwd) {
        List<String> l = new ArrayList<String>();
        if (fwd != null) {
            l.add("vrf = " + fwd.vrfName);
            l.add("ipver = " + fwd.ipVersion);
        }
        l.add("rd = " + tabRtrmapN.rd2string(rouDst));
        l.add("prefix = " + prefix);
        l.add("prefix network = " + prefix.network);
        l.add("prefix broadcast = " + prefix.broadcast);
        l.add("prefix wildcard = " + prefix.wildcard);
        l.add("prefix netmask = " + prefix.mask);
        l.add("type = " + rouTyp + " " + protoNum);
        l.add("source = " + srcRtr);
        l.add("validity = " + validity);
        l.add("segment routing index = " + segrouIdx);
        l.add("segment routing old base = " + segrouOld);
        l.add("segment routing base = " + segrouBeg);
        l.add("segment routing size = " + segrouSiz);
        l.add("bier index = " + bierIdx);
        l.add("bier old base = " + bierOld);
        l.add("bier base = " + bierBeg);
        l.add("bier range = " + bierSiz);
        l.add("bier size = " + bierHdr + "-" + tabLabelBier.bsl2num(bierHdr));
        l.add("updated = " + bits.time2str(cfgAll.timeZoneName, time + cfgAll.timeServerOffset, 3) + " (" + bits.timePast(time) + " ago)");
        l.add("version = " + version);
        l.add("distance = " + distance);
        l.add("metric = " + metric);
        l.add("interface = " + iface);
        l.add("table = " + rouTab);
        l.add("nexthop = " + nextHop);
        l.add("original nexthop = " + oldHop);
        l.add("tag = " + tag);
        l.add("origin type = " + origin);
        l.add("local preference = " + locPref);
        l.add("evpn label*16 = " + evpnLab);
        l.add("attribute as = " + bits.num2str(attribAs));
        l.add("attribute value = " + bits.byteDump(attribVal, 0, -1));
        l.add("tunnel type = " + tunelTyp);
        l.add("tunnel value = " + bits.byteDump(tunelVal, 0, -1));
        l.add("pmsi type = " + pmsiTyp);
        l.add("pmsi label*16 = " + pmsiLab);
        l.add("pmsi tunnel = " + bits.byteDump(pmsiTun, 0, -1));
        l.add("accumulated igp = " + accIgp);
        l.add("bandwidth = " + bandwidth);
        l.add("atomic aggregator = " + atomicAggr);
        l.add("aggregator as = " + bits.num2str(aggrAs));
        l.add("aggregator router = " + aggrRtr);
        l.add("originator = " + originator);
        l.add("cluster list = " + dumpAddrList(clustList));
        l.add("as path (len=" + asPathLen() + ") = " + asPathStr());
        l.add("standard community = " + tabRtrmapN.stdComms2string(stdComm));
        l.add("extended community = " + tabRtrmapN.extComms2string(extComm));
        l.add("large community = " + tabRtrmapN.lrgComms2string(lrgComm));
        l.add("internal source = " + rouSrc);
        l.add("local label = " + labelLoc);
        l.add("remote label = " + dumpIntList(labelRem, "", ""));
        l.add("counter = " + counter.getShStat(cntr));
        return l;
    }

    public int compare(tabRouteEntry<T> o1, tabRouteEntry<T> o2) {
        if (o1.rouDst < o2.rouDst) {
            return -1;
        }
        if (o1.rouDst > o2.rouDst) {
            return +1;
        }
        return prefix.compare(o1.prefix, o2.prefix);
    }

}
