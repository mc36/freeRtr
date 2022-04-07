package net.freertr.tab;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import net.freertr.addr.addrType;
import net.freertr.cfg.cfgAll;
import net.freertr.ip.ipFwd;
import net.freertr.user.userFormat;
import net.freertr.user.userHelping;
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
         * ipv4 ghost hunter
         */
        ghosthunt4,
        /**
         * ipv6 ghost hunter
         */
        ghosthunt6,
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
            case ghosthunt4:
            case ghosthunt6:
                return "GHT";
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
     * @return 0 if equals, greater than 0 if differs
     */
    public int differs(tabRouteAttr<T> other) {
        if (other == null) {
            return 1;
        }
        if (rouTyp != other.rouTyp) {
            return 2;
        }
        if (iface != other.iface) {
            return 3;
        }
        if (rouSrc != other.rouSrc) {
            return 4;
        }
        if (protoNum != other.protoNum) {
            return 5;
        }
        if (distance != other.distance) {
            return 6;
        }
        if (metric != other.metric) {
            return 7;
        }
        if (ident != other.ident) {
            return 8;
        }
        if (hops != other.hops) {
            return 9;
        }
        if (tag != other.tag) {
            return 10;
        }
        if (segrouIdx != other.segrouIdx) {
            return 11;
        }
        if (segrouBeg != other.segrouBeg) {
            return 12;
        }
        if (segrouOld != other.segrouOld) {
            return 13;
        }
        if (segrouSiz != other.segrouSiz) {
            return 14;
        }
        if (segrouOfs != other.segrouOfs) {
            return 15;
        }
        if (bierIdx != other.bierIdx) {
            return 16;
        }
        if (bierBeg != other.bierBeg) {
            return 17;
        }
        if (bierOld != other.bierOld) {
            return 18;
        }
        if (bierSiz != other.bierSiz) {
            return 19;
        }
        if (bierHdr != other.bierHdr) {
            return 20;
        }
        if (validity != other.validity) {
            return 21;
        }
        if (onlyCust != other.onlyCust) {
            return 21;
        }
        if (origin != other.origin) {
            return 22;
        }
        if (locPref != other.locPref) {
            return 23;
        }
        if (accIgp != other.accIgp) {
            return 24;
        }
        if (attribAs != other.attribAs) {
            return 25;
        }
        if (tunelTyp != other.tunelTyp) {
            return 26;
        }
        if (pmsiTyp != other.pmsiTyp) {
            return 27;
        }
        if (pmsiLab != other.pmsiLab) {
            return 28;
        }
        if (evpnLab != other.evpnLab) {
            return 29;
        }
        if (bandwidth != other.bandwidth) {
            return 30;
        }
        if (atomicAggr != other.atomicAggr) {
            return 31;
        }
        if (aggrAs != other.aggrAs) {
            return 32;
        }
        if (segrouPrf != null) {
            if (other.segrouPrf == null) {
                return 33;
            }
            if (segrouPrf.compare(segrouPrf, other.segrouPrf) != 0) {
                return 34;
            }
        } else if (other.segrouPrf != null) {
            return 35;
        }
        if (labelLoc != null) {
            if (other.labelLoc == null) {
                return 36;
            }
            if (labelLoc.compare(labelLoc, other.labelLoc) != 0) {
                return 37;
            }
        } else if (other.labelLoc != null) {
            return 38;
        }
        if (srcRtr != null) {
            if (other.srcRtr == null) {
                return 39;
            }
            if (srcRtr.getSize() != other.srcRtr.getSize()) {
                return 40;
            }
            if (srcRtr.compare(srcRtr, other.srcRtr) != 0) {
                return 41;
            }
        } else if (other.srcRtr != null) {
            return 42;
        }
        if (aggrRtr != null) {
            if (other.aggrRtr == null) {
                return 43;
            }
            if (aggrRtr.compare(aggrRtr, other.aggrRtr) != 0) {
                return 44;
            }
        } else if (other.aggrRtr != null) {
            return 45;
        }
        if (nextHop != null) {
            if (other.nextHop == null) {
                return 46;
            }
            if (nextHop.compare(nextHop, other.nextHop) != 0) {
                return 47;
            }
        } else if (other.nextHop != null) {
            return 48;
        }
        if (oldHop != null) {
            if (other.oldHop == null) {
                return 49;
            }
            if (oldHop.compare(oldHop, other.oldHop) != 0) {
                return 50;
            }
        } else if (other.oldHop != null) {
            return 51;
        }
        if (originator != null) {
            if (other.originator == null) {
                return 52;
            }
            if (originator.compare(originator, other.originator) != 0) {
                return 53;
            }
        } else if (other.originator != null) {
            return 54;
        }
        if (diffIntList(labelRem, other.labelRem)) {
            return 55;
        }
        if (diffIntList(stdComm, other.stdComm)) {
            return 56;
        }
        if (diffIntList(pathSeq, other.pathSeq)) {
            return 57;
        }
        if (diffIntList(pathSet, other.pathSet)) {
            return 58;
        }
        if (diffIntList(confSeq, other.confSeq)) {
            return 59;
        }
        if (diffIntList(confSet, other.confSet)) {
            return 60;
        }
        if (attribVal != null) {
            if (other.attribVal == null) {
                return 61;
            }
            if (attribVal.length != other.attribVal.length) {
                return 62;
            }
            if (bits.byteComp(attribVal, 0, other.attribVal, 0, attribVal.length) != 0) {
                return 63;
            }
        } else if (other.attribVal != null) {
            return 64;
        }
        if (tunelVal != null) {
            if (other.tunelVal == null) {
                return 65;
            }
            if (tunelVal.length != other.tunelVal.length) {
                return 66;
            }
            if (bits.byteComp(tunelVal, 0, other.tunelVal, 0, tunelVal.length) != 0) {
                return 67;
            }
        } else if (other.tunelVal != null) {
            return 68;
        }
        if (linkStat != null) {
            if (other.linkStat == null) {
                return 69;
            }
            if (linkStat.length != other.linkStat.length) {
                return 70;
            }
            if (bits.byteComp(linkStat, 0, other.linkStat, 0, linkStat.length) != 0) {
                return 71;
            }
        } else if (other.linkStat != null) {
            return 72;
        }
        if (pmsiTun != null) {
            if (other.pmsiTun == null) {
                return 73;
            }
            if (pmsiTun.length != other.pmsiTun.length) {
                return 74;
            }
            if (bits.byteComp(pmsiTun, 0, other.pmsiTun, 0, pmsiTun.length) != 0) {
                return 75;
            }
        } else if (other.pmsiTun != null) {
            return 76;
        }
        if (clustList != null) {
            if (other.clustList == null) {
                return 77;
            }
            if (clustList.size() != other.clustList.size()) {
                return 78;
            }
            for (int i = 0; i < clustList.size(); i++) {
                T cmp = clustList.get(i);
                if (cmp.compare(cmp, other.clustList.get(i)) != 0) {
                    return 79;
                }
            }
        } else if (other.clustList != null) {
            return 80;
        }
        if (extComm != null) {
            if (other.extComm == null) {
                return 90;
            }
            if (extComm.size() != other.extComm.size()) {
                return 91;
            }
            for (int i = 0; i < extComm.size(); i++) {
                if (extComm.get(i).compareTo(other.extComm.get(i)) != 0) {
                    return 92;
                }
            }
        } else if (other.extComm != null) {
            return 93;
        }
        if (lrgComm != null) {
            if (other.lrgComm == null) {
                return 94;
            }
            if (lrgComm.size() != other.lrgComm.size()) {
                return 95;
            }
            for (int i = 0; i < lrgComm.size(); i++) {
                tabLargeComm cmp = lrgComm.get(i);
                if (cmp.compare(cmp, other.lrgComm.get(i)) != 0) {
                    return 96;
                }
            }
        } else if (other.lrgComm != null) {
            return 97;
        }
        return 0;
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
     * end of as path
     *
     * @return end of as path, -1 if none
     */
    public int asPathEnd() {
        if (pathSeq == null) {
            return -1;
        }
        int i = pathSeq.size();
        if (i < 1) {
            return -1;
        }
        return pathSeq.get(i - 1);
    }

    /**
     * begin of as path
     *
     * @return end of as path, -1 if none
     */
    public int asPathBeg() {
        if (pathSeq == null) {
            return -1;
        }
        int i = pathSeq.size();
        if (i < 1) {
            return -1;
        }
        return pathSeq.get(0);
    }

    /**
     * end of as path
     *
     * @param match matcher
     * @return true on match, false if not
     */
    public boolean asPathMid(tabIntMatcher match) {
        if (match.action == tabIntMatcher.actionType.always) {
            return true;
        }
        if (pathSeq == null) {
            return false;
        }
        for (int i = 0; i < pathSeq.size() - 1; i++) {
            if (match.matches(pathSeq.get(i))) {
                return true;
            }
        }
        return false;
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
     * get ignore help
     *
     * @param hl help to append
     * @param lv level to add
     */
    public static void ignoreHelp(userHelping hl, int lv) {
        hl.add(null, lv + " " + lv + ",.    bier         ignore bier");
        hl.add(null, lv + " " + lv + ",.    attrset      ignore attribute set");
        hl.add(null, lv + " " + lv + ",.    cluster      ignore cluster list");
        hl.add(null, lv + " " + lv + ",.    nexthop      ignore nexthop");
        hl.add(null, lv + " " + lv + ",.    origin       ignore origin");
        hl.add(null, lv + " " + lv + ",.    metric       ignore metric");
        hl.add(null, lv + " " + lv + ",.    locpref      ignore local preference");
        hl.add(null, lv + " " + lv + ",.    distance     ignore distance");
        hl.add(null, lv + " " + lv + ",.    tag          ignore tag");
        hl.add(null, lv + " " + lv + ",.    validity     ignore validity");
        hl.add(null, lv + " " + lv + ",.    aspath       ignore as path");
        hl.add(null, lv + " " + lv + ",.    asconf       ignore confed path");
        hl.add(null, lv + " " + lv + ",.    stdcomm      ignore standard community");
        hl.add(null, lv + " " + lv + ",.    extcomm      ignore extended community");
        hl.add(null, lv + " " + lv + ",.    lrgcomm      ignore large community");
        hl.add(null, lv + " " + lv + ",.    sortcomm     sort communities");
        hl.add(null, lv + " " + lv + ",.    lnksta       ignore link state");
        hl.add(null, lv + " " + lv + ",.    aigp         ignore accumulated igp");
        hl.add(null, lv + " " + lv + ",.    bandwidth    ignore bandwidth");
        hl.add(null, lv + " " + lv + ",.    label        ignore labels");
        hl.add(null, lv + " " + lv + ",.    aggregate    ignore aggregator");
        hl.add(null, lv + " " + lv + ",.    orignted     ignore originator");
        hl.add(null, lv + " " + lv + ",.    pmsi         ignore pmsi");
        hl.add(null, lv + " " + lv + ",.    segrout      ignore segment routing");
        hl.add(null, lv + " " + lv + ",.    tunnel       ignore tunnel");
        hl.add(null, lv + " " + lv + ",.    empty        ignore empty lists");
    }

    /**
     * convert string to ignore bitmap
     *
     * @param a string
     * @return bitmap
     */
    public static int string2ignore(String a) {
        if (a.equals("cluster")) {
            return 0x1;
        }
        if (a.equals("nexthop")) {
            return 0x2;
        }
        if (a.equals("origin")) {
            return 0x4;
        }
        if (a.equals("metric")) {
            return 0x8;
        }
        if (a.equals("locpref")) {
            return 0x10;
        }
        if (a.equals("distance")) {
            return 0x20;
        }
        if (a.equals("tag")) {
            return 0x40;
        }
        if (a.equals("validity")) {
            return 0x80;
        }
        if (a.equals("aspath")) {
            return 0x100;
        }
        if (a.equals("asconf")) {
            return 0x200;
        }
        if (a.equals("stdcomm")) {
            return 0x400;
        }
        if (a.equals("extcomm")) {
            return 0x800;
        }
        if (a.equals("aigp")) {
            return 0x1000;
        }
        if (a.equals("bandwidth")) {
            return 0x2000;
        }
        if (a.equals("label")) {
            return 0x4000;
        }
        if (a.equals("aggregate")) {
            return 0x8000;
        }
        if (a.equals("orignted")) {
            return 0x10000;
        }
        if (a.equals("pmsi")) {
            return 0x20000;
        }
        if (a.equals("segrout")) {
            return 0x40000;
        }
        if (a.equals("lrgcomm")) {
            return 0x80000;
        }
        if (a.equals("tunnel")) {
            return 0x100000;
        }
        if (a.equals("attrset")) {
            return 0x200000;
        }
        if (a.equals("bier")) {
            return 0x400000;
        }
        if (a.equals("sortcomm")) {
            return 0x800000;
        }
        if (a.equals("lnksta")) {
            return 0x1000000;
        }
        if (a.equals("empty")) {
            return 0x2000000;
        }
        return 0;
    }

    /**
     * convert ignore bitmap to string
     *
     * @param i bitmap
     * @return string
     */
    public static String ignore2string(int i) {
        if (i == 0) {
            return "";
        }
        String a = "";
        if ((i & 0x1) != 0) {
            a += " cluster";
        }
        if ((i & 0x2) != 0) {
            a += " nexthop";
        }
        if ((i & 0x4) != 0) {
            a += " origin";
        }
        if ((i & 0x8) != 0) {
            a += " metric";
        }
        if ((i & 0x10) != 0) {
            a += " locpref";
        }
        if ((i & 0x20) != 0) {
            a += " distance";
        }
        if ((i & 0x40) != 0) {
            a += " tag";
        }
        if ((i & 0x80) != 0) {
            a += " validity";
        }
        if ((i & 0x100) != 0) {
            a += " aspath";
        }
        if ((i & 0x200) != 0) {
            a += " asconf";
        }
        if ((i & 0x400) != 0) {
            a += " stdcomm";
        }
        if ((i & 0x800) != 0) {
            a += " extcomm";
        }
        if ((i & 0x1000) != 0) {
            a += " aigp";
        }
        if ((i & 0x2000) != 0) {
            a += " bandwidth";
        }
        if ((i & 0x4000) != 0) {
            a += " label";
        }
        if ((i & 0x8000) != 0) {
            a += " aggregate";
        }
        if ((i & 0x10000) != 0) {
            a += " orignted";
        }
        if ((i & 0x20000) != 0) {
            a += " pmsi";
        }
        if ((i & 0x40000) != 0) {
            a += " segrout";
        }
        if ((i & 0x80000) != 0) {
            a += " lrgcomm";
        }
        if ((i & 0x100000) != 0) {
            a += " tunnel";
        }
        if ((i & 0x200000) != 0) {
            a += " attrset";
        }
        if ((i & 0x400000) != 0) {
            a += " bier";
        }
        if ((i & 0x800000) != 0) {
            a += " sortcomm";
        }
        if ((i & 0x1000000) != 0) {
            a += " lnksta";
        }
        if ((i & 0x2000000) != 0) {
            a += " empty";
        }
        return a.substring(1, a.length());
    }

    /**
     * ignore attributes
     *
     * @param <T> class of address
     * @param ntry route entry
     * @param ign ignore bitmap
     */
    public static <T extends addrType> void ignoreAttribs(tabRouteAttr<T> ntry, int ign) {
        ntry.srcRtr = null;
        ntry.rouSrc = 0;
        ntry.rouTyp = tabRouteAttr.routeType.bgp4;
        ntry.protoNum = 0;
        ntry.iface = null;
        ntry.ident = 0;
        if ((ign & 0x1) != 0) {
            ntry.clustList = null;
        }
        if ((ign & 0x2) != 0) {
            ntry.nextHop = null;
            ntry.oldHop = null;
        }
        if ((ign & 0x4) != 0) {
            ntry.origin = 0;
        }
        if ((ign & 0x8) != 0) {
            ntry.metric = 0;
        }
        if ((ign & 0x10) != 0) {
            ntry.locPref = 0;
        }
        if ((ign & 0x20) != 0) {
            ntry.distance = 0;
        }
        if ((ign & 0x40) != 0) {
            ntry.tag = 0;
        }
        if ((ign & 0x80) != 0) {
            ntry.validity = 0;
        }
        if ((ign & 0x100) != 0) {
            ntry.pathSeq = null;
            ntry.pathSet = null;
        }
        if ((ign & 0x200) != 0) {
            ntry.confSeq = null;
            ntry.confSet = null;
        }
        if ((ign & 0x400) != 0) {
            ntry.stdComm = null;
        }
        if ((ign & 0x800) != 0) {
            ntry.extComm = null;
        }
        if ((ign & 0x1000) != 0) {
            ntry.accIgp = 0;
        }
        if ((ign & 0x2000) != 0) {
            ntry.bandwidth = 0;
        }
        if ((ign & 0x4000) != 0) {
            ntry.labelLoc = null;
            ntry.labelRem = null;
        }
        if ((ign & 0x8000) != 0) {
            ntry.atomicAggr = false;
            ntry.aggrAs = 0;
            ntry.aggrRtr = null;
        }
        if ((ign & 0x10000) != 0) {
            ntry.originator = null;
        }
        if ((ign & 0x20000) != 0) {
            ntry.pmsiLab = 0;
            ntry.pmsiTyp = 0;
            ntry.pmsiTun = null;
        }
        if ((ign & 0x40000) != 0) {
            ntry.segrouIdx = 0;
            ntry.segrouBeg = 0;
            ntry.segrouOld = 0;
            ntry.segrouSiz = 0;
        }
        if ((ign & 0x80000) != 0) {
            ntry.lrgComm = null;
        }
        if ((ign & 0x100000) != 0) {
            ntry.tunelTyp = 0;
            ntry.tunelVal = null;
        }
        if ((ign & 0x200000) != 0) {
            ntry.attribAs = 0;
            ntry.attribVal = null;
        }
        if ((ign & 0x400000) != 0) {
            ntry.bierIdx = 0;
            ntry.bierBeg = 0;
            ntry.bierOld = 0;
            ntry.bierSiz = 0;
            ntry.bierHdr = 0;
        }
        if ((ign & 0x800000) != 0) {
            if (ntry.stdComm != null) {
                Collections.sort(ntry.stdComm);
            }
            if (ntry.extComm != null) {
                Collections.sort(ntry.extComm);
            }
            if (ntry.lrgComm != null) {
                Collections.sort(ntry.lrgComm, new tabLargeComm());
            }
        }
        if ((ign & 0x1000000) != 0) {
            ntry.linkStat = null;
        }
        if ((ign & 0x2000000) != 0) {
            ntry.clustList = nullEmptyList(ntry.clustList);
            ntry.confSeq = nullEmptyList(ntry.confSeq);
            ntry.confSet = nullEmptyList(ntry.confSet);
            ntry.extComm = nullEmptyList(ntry.extComm);
            ntry.labelRem = nullEmptyList(ntry.labelRem);
            ntry.lrgComm = nullEmptyList(ntry.lrgComm);
            ntry.pathSeq = nullEmptyList(ntry.pathSeq);
            ntry.pathSet = nullEmptyList(ntry.pathSet);
            ntry.stdComm = nullEmptyList(ntry.stdComm);
        }
    }

    private static <E extends Object> List<E> nullEmptyList(List<E> o) {
        if (o == null) {
            return null;
        }
        if (o.size() < 1) {
            return null;
        }
        return o;
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
        l.add("aspath|" + asPathStr());
        l.add("path length|" + asPathLen());
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
