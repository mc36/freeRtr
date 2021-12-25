package net.freertr.tab;

import java.util.ArrayList;
import java.util.List;
import net.freertr.addr.addrType;
import net.freertr.cfg.cfgAll;
import net.freertr.ip.ipFwd;
import net.freertr.user.userFormat;
import net.freertr.util.bits;

/**
 * represents one route table entry attribute
 *
 * @param <T> class of address
 * @author matecsaba
 */
public class tabRouteAttr<T extends addrType> {

    /**
     * create instance
     */
    public tabRouteAttr() {
    }

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
        blackhole4,
        /**
         * ipv6 blackhole
         */
        blackhole6,
        /**
         * ipv4 blackhole
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
         * ipv4 aggregation
         */
        aggreg4,
        /**
         * ipv6 aggregation
         */
        aggreg6,
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
     * identifier of prefix
     */
    public int ident;

    /**
     * hop count of prefix
     */
    public int hops;

    /**
     * tag of prefix
     */
    public int tag;

    /**
     * validity type
     */
    public int validity;

    /**
     * only to customer
     */
    public int onlyCust;

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
     * segment routing label block offset
     */
    public int segrouOfs;

    /**
     * segment routing prefix sid
     */
    public T segrouPrf;

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
     * pmsi flag and type
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
     * link state
     */
    public byte[] linkStat;

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
    public tabLabelEntry labelLoc = null;

    /**
     * remote label
     */
    public List<Integer> labelRem = null;

    /**
     * route table to use
     */
    public ipFwd rouTab;

    /**
     * convert route type to string
     *
     * @param <T> class of address
     * @param ntry route
     * @return displayable string
     */
    public static <T extends addrType> String rouTyp2string(tabRouteAttr<T> ntry) {
        if (ntry.rouTyp == null) {
            return "null";
        }
        String a = "";
        switch (ntry.rouTyp) {
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
            case blackhole4:
            case blackhole6:
                return "BLK";
            case olsr4:
            case olsr6:
                return "N";
            case ospf4:
            case ospf6:
                switch (ntry.origin) {
                    case 109:
                        a += "";
                        break;
                    case 110:
                        a += " IA";
                        break;
                    case 111:
                        a += " E1";
                        break;
                    case 112:
                        a += " E2";
                        break;
                }
                return "O" + a;
            case isis4:
            case isis6:
                if ((ntry.rouSrc & 2) != 0) {
                    a += " IA";
                }
                if ((ntry.rouSrc & 1) != 0) {
                    a += " EX";
                }
                return "I" + a;
            case pvrp4:
            case pvrp6:
                if ((ntry.rouSrc & 1) != 0) {
                    a += " EX";
                }
                return "P" + a;
            case lsrp4:
            case lsrp6:
                if ((ntry.rouSrc & 1) != 0) {
                    a += " EX";
                }
                return "L" + a;
            case eigrp4:
            case eigrp6:
                if (ntry.aggrRtr != null) {
                    a += " EX";
                }
                return "D" + a;
            case bgp4:
            case bgp6:
                return "B";
            case local:
                return "LOC";
            case remote:
                return "REM";
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
            case aggreg4:
            case aggreg6:
                return "AGR";
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
     * @param atr target
     * @param fwd forwarding info also
     */
    @SuppressWarnings("unchecked")
    public void copyBytes(tabRouteAttr<T> atr, boolean fwd) {
        atr.rouTab = rouTab;
        atr.distance = distance;
        atr.metric = metric;
        atr.ident = ident;
        atr.hops = hops;
        atr.tag = tag;
        atr.origin = origin;
        atr.validity = validity;
        atr.onlyCust = onlyCust;
        atr.segrouIdx = segrouIdx;
        atr.segrouSiz = segrouSiz;
        atr.segrouOfs = segrouOfs;
        atr.segrouBeg = segrouBeg;
        atr.segrouOld = segrouOld;
        atr.bierIdx = bierIdx;
        atr.bierHdr = bierHdr;
        atr.bierBeg = bierBeg;
        atr.bierOld = bierOld;
        atr.bierSiz = bierSiz;
        atr.locPref = locPref;
        atr.accIgp = accIgp;
        atr.tunelTyp = tunelTyp;
        atr.attribAs = attribAs;
        atr.pmsiTyp = pmsiTyp;
        atr.pmsiLab = pmsiLab;
        atr.evpnLab = evpnLab;
        atr.bandwidth = bandwidth;
        atr.atomicAggr = atomicAggr;
        atr.aggrAs = aggrAs;
        if (segrouPrf != null) {
            atr.segrouPrf = (T) segrouPrf.copyBytes();
        } else {
            atr.segrouPrf = null;
        }
        if (attribVal != null) {
            atr.attribVal = new byte[attribVal.length];
            bits.byteCopy(attribVal, 0, atr.attribVal, 0, attribVal.length);
        } else {
            atr.attribVal = null;
        }
        if (tunelVal != null) {
            atr.tunelVal = new byte[tunelVal.length];
            bits.byteCopy(tunelVal, 0, atr.tunelVal, 0, tunelVal.length);
        } else {
            atr.tunelVal = null;
        }
        if (linkStat != null) {
            atr.linkStat = new byte[linkStat.length];
            bits.byteCopy(linkStat, 0, atr.linkStat, 0, linkStat.length);
        } else {
            atr.linkStat = null;
        }
        if (pmsiTun != null) {
            atr.pmsiTun = new byte[pmsiTun.length];
            bits.byteCopy(pmsiTun, 0, atr.pmsiTun, 0, pmsiTun.length);
        } else {
            atr.pmsiTun = null;
        }
        if (aggrRtr != null) {
            atr.aggrRtr = (T) aggrRtr.copyBytes();
        } else {
            atr.aggrRtr = null;
        }
        if (srcRtr != null) {
            atr.srcRtr = srcRtr.copyBytes();
        } else {
            atr.srcRtr = null;
        }
        atr.stdComm = tabLabel.copyLabels(stdComm);
        atr.pathSeq = tabLabel.copyLabels(pathSeq);
        atr.pathSet = tabLabel.copyLabels(pathSet);
        atr.confSeq = tabLabel.copyLabels(confSeq);
        atr.confSet = tabLabel.copyLabels(confSet);
        if (originator != null) {
            atr.originator = (T) originator.copyBytes();
        } else {
            atr.originator = null;
        }
        if (clustList != null) {
            atr.clustList = new ArrayList<T>();
            for (int i = 0; i < clustList.size(); i++) {
                atr.clustList.add((T) clustList.get(i).copyBytes());
            }
        } else {
            atr.clustList = null;
        }
        if (extComm != null) {
            atr.extComm = new ArrayList<Long>();
            for (int i = 0; i < extComm.size(); i++) {
                atr.extComm.add(extComm.get(i));
            }
        } else {
            atr.extComm = null;
        }
        if (lrgComm != null) {
            atr.lrgComm = new ArrayList<tabLargeComm>();
            for (int i = 0; i < lrgComm.size(); i++) {
                atr.lrgComm.add(lrgComm.get(i).copyBytes());
            }
        } else {
            atr.lrgComm = null;
        }
        atr.protoNum = protoNum;
        atr.rouTyp = rouTyp;
        atr.rouSrc = rouSrc;
        atr.time = time;
        atr.version = version;
        atr.labelLoc = labelLoc;
        if (!fwd) {
            return;
        }
        atr.iface = iface;
        atr.labelRem = tabLabel.copyLabels(labelRem);
        if (nextHop != null) {
            atr.nextHop = (T) nextHop.copyBytes();
        } else {
            atr.nextHop = null;
        }
        if (oldHop != null) {
            atr.oldHop = (T) oldHop.copyBytes();
        } else {
            atr.oldHop = null;
        }
    }

    /**
     * compare two integer lists
     *
     * @param l1 first list
     * @param l2 second list
     * @return true if differs
     */
    public static boolean diffIntList(List<Integer> l1, List<Integer> l2) {
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
    public boolean differs(tabRouteAttr<T> other) {
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
        if (ident != other.ident) {
            return true;
        }
        if (hops != other.hops) {
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
        if (segrouOfs != other.segrouOfs) {
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
        if (onlyCust != other.onlyCust) {
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
        if (segrouPrf != null) {
            if (other.segrouPrf == null) {
                return true;
            }
            if (segrouPrf.compare(segrouPrf, other.segrouPrf) != 0) {
                return true;
            }
        } else if (other.segrouPrf != null) {
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
        if (linkStat != null) {
            if (other.linkStat == null) {
                return true;
            }
            if (linkStat.length != other.linkStat.length) {
                return true;
            }
            if (bits.byteComp(linkStat, 0, other.linkStat, 0, linkStat.length) != 0) {
                return true;
            }
        } else if (other.linkStat != null) {
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
        if (extComm != null) {
            if (other.extComm == null) {
                return true;
            }
            if (extComm.size() != other.extComm.size()) {
                return true;
            }
            for (int i = 0; i < extComm.size(); i++) {
                if (extComm.get(i).compareTo(other.extComm.get(i)) != 0) {
                    return true;
                }
            }
        } else if (other.extComm != null) {
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
     * @param tim consider time
     * @return true if yes, false if not
     */
    public boolean isOtherBetter(tabRouteAttr<T> imp, boolean tim) {
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
        if (!tim) {
            return false;
        }
        return imp.time < time;
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
     * full dump of this attribute
     *
     * @param l list to append
     */
    public void fullDump(userFormat l) {
        l.add("type|" + rouTyp + " " + protoNum);
        l.add("source|" + srcRtr);
        l.add("validity|" + validity);
        l.add("only to customer|" + bits.num2str(onlyCust));
        l.add("segrout index|" + segrouIdx);
        l.add("segrout old base|" + segrouOld);
        l.add("segrout base|" + segrouBeg);
        l.add("segrout size|" + segrouSiz);
        l.add("segrout offset|" + segrouOfs);
        l.add("segrout prefix|" + segrouPrf);
        l.add("bier index|" + bierIdx);
        l.add("bier old base|" + bierOld);
        l.add("bier base|" + bierBeg);
        l.add("bier range|" + bierSiz);
        l.add("bier size|" + bierHdr + "-" + tabLabelBier.bsl2num(bierHdr));
        l.add("updated|" + bits.time2str(cfgAll.timeZoneName, time + cfgAll.timeServerOffset, 3) + " (" + bits.timePast(time) + " ago)");
        l.add("version|" + version);
        l.add("distance|" + distance);
        l.add("metric|" + metric);
        l.add("ident|" + ident);
        l.add("hops|" + hops);
        l.add("interface|" + iface);
        l.add("table|" + rouTab);
        l.add("nexthop|" + nextHop);
        l.add("original nexthop|" + oldHop);
        l.add("tag|" + tag);
        l.add("origin type|" + origin);
        l.add("local preference|" + locPref);
        l.add("evpn label*16|" + evpnLab);
        l.add("attribute as|" + bits.num2str(attribAs));
        l.add("attribute value|" + bits.byteDump(attribVal, 0, -1));
        l.add("tunnel type|" + tunelTyp);
        l.add("tunnel value|" + bits.byteDump(tunelVal, 0, -1));
        l.add("link state|" + bits.byteDump(linkStat, 0, -1));
        l.add("pmsi type|" + pmsiTyp);
        l.add("pmsi label*16|" + pmsiLab);
        l.add("pmsi tunnel|" + bits.byteDump(pmsiTun, 0, -1));
        l.add("accumulated igp|" + accIgp);
        l.add("bandwidth|" + bandwidth);
        l.add("atomic aggregator|" + atomicAggr);
        l.add("aggregator as|" + bits.num2str(aggrAs));
        l.add("aggregator router|" + aggrRtr);
        l.add("originator|" + originator);
        l.add("cluster list|" + dumpAddrList(clustList));
        l.add("as path|" + asPathStr());
        l.add("as path length|" + asPathLen());
        l.add("standard community|" + tabRtrmapN.stdComms2string(stdComm));
        l.add("extended community|" + tabRtrmapN.extComms2string(extComm));
        l.add("large community|" + tabRtrmapN.lrgComms2string(lrgComm));
        l.add("internal source|" + rouSrc);
        l.add("local label|" + labelLoc);
        l.add("remote label|" + dumpIntList(labelRem, "", ""));
    }

    /**
     * convert to route format
     *
     * @return converted
     */
    public String toShSrRoute() {
        return segrouIdx + "|" + segrouBeg + "|" + segrouOld;
    }

    /**
     * convert to route format
     *
     * @return converted
     */
    public String toShBrRoute() {
        return bierIdx + "|" + bierBeg + "|" + bierOld + "|" + bierHdr + "-" + tabLabelBier.bsl2num(bierHdr);
    }

    /**
     * convert to route format
     *
     * @return converted
     */
    public String toShRoute() {
        String s = "";
        if (rouTab != null) {
            s = "@" + rouTab.vrfName;
        }
        return distance + "/" + metric + "|" + iface + s + "|" + nextHop + "|" + bits.timePast(time);
    }

    /**
     * convert to bgp format
     *
     * @return converted
     */
    public String toShBgpLabels() {
        return labelLoc + "|" + evpnLab + "|" + pmsiLab + "|" + dumpIntList(labelRem, "", "") + "|" + nextHop;
    }

    /**
     * convert to bgp format
     *
     * @return converted
     */
    public String toShBgpLast() {
        return "|" + nextHop + "|" + distance + "/" + locPref + "/" + origin + "/" + metric + "|" + asPathStr();
    }

    /**
     * convert to ldp format
     *
     * @return converted
     */
    public String toShLdp() {
        return labelLoc + "|" + dumpIntList(labelRem, "", "") + "|" + nextHop;
    }

    /**
     * convert to rpki format
     *
     * @return converted
     */
    public String toShRpki() {
        return metric + "|" + rouSrc;
    }

    /**
     * convert to ecmp format
     *
     * @return converted
     */
    public String toShEcmp() {
        return rouTyp + " " + protoNum + "|" + srcRtr;
    }

}
