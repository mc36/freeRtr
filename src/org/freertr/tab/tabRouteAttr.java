package org.freertr.tab;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.freertr.addr.addrType;
import org.freertr.cfg.cfgAll;
import org.freertr.clnt.clntWhois;
import org.freertr.ip.ipFwd;
import org.freertr.user.userFormat;
import org.freertr.user.userHelp;
import org.freertr.util.bits;

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
         * ipv4 rift
         */
        rift4,
        /**
         * ipv6 rift
         */
        rift6,
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
         * ipv4 rpki
         */
        rpki4,
        /**
         * ipv6 rpki
         */
        rpki6,
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
     * roa validity type
     */
    public int validRoa;

    /**
     * aspa validity type
     */
    public int validAspa;

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
     * segment routing endpoint behaviour
     */
    public int segrouBeh;

    /**
     * segment routing layer2 endpoint
     */
    public boolean segrouEth;

    /**
     * segment routing prefix sid
     */
    public T segrouPrf;

    /**
     * bier index
     */
    public int bierIdx;

    /**
     * bier subdomain
     */
    public int bierSub;

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
    public int originType;

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
     * entropy label
     */
    public byte[] entropyLabel;

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
     * nsh service chain
     */
    public byte[] nshChain;

    /**
     * domain path
     */
    public byte[] domainPath;

    /**
     * bfd discriminator
     */
    public byte[] bfdDiscr;

    /**
     * next hop capability
     */
    public byte[] hopCapa;

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
     * connector router
     */
    public T connRtr;

    /**
     * pe distinguisher router
     */
    public T pediRtr;

    /**
     * pe distinguisher label
     */
    public int pediLab;

    /**
     * aspath limit
     */
    public int pathLim;

    /**
     * aspath originator
     */
    public int pathAsn;

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
     * ipv6 community values
     */
    public List<tabIpv6comm> ip6comm;

    /**
     * unknown attributes
     */
    public List<tabRouteBlob> unknown;

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
                switch (ntry.originType) {
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
            case rift4:
            case rift6:
                if ((ntry.rouSrc & 2) != 0) {
                    a += " DA";
                }
                if ((ntry.rouSrc & 1) != 0) {
                    a += " EX";
                }
                return "F" + a;
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
            case rpki4:
            case rpki6:
                return "RPK";
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
        atr.originType = originType;
        atr.validRoa = validRoa;
        atr.validAspa = validAspa;
        atr.onlyCust = onlyCust;
        atr.segrouIdx = segrouIdx;
        atr.segrouSiz = segrouSiz;
        atr.segrouOfs = segrouOfs;
        atr.segrouBeh = segrouBeh;
        atr.segrouEth = segrouEth;
        atr.segrouBeg = segrouBeg;
        atr.segrouOld = segrouOld;
        atr.bierSub = bierSub;
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
        atr.pediLab = pediLab;
        atr.pathAsn = pathAsn;
        atr.pathLim = pathLim;
        if (segrouPrf != null) {
            atr.segrouPrf = (T) segrouPrf.copyBytes();
        } else {
            atr.segrouPrf = null;
        }
        if (entropyLabel != null) {
            atr.entropyLabel = new byte[entropyLabel.length];
            bits.byteComp(entropyLabel, 0, atr.entropyLabel, 0, entropyLabel.length);
        } else {
            atr.entropyLabel = null;
        }
        if (attribVal != null) {
            atr.attribVal = new byte[attribVal.length];
            bits.byteCopy(attribVal, 0, atr.attribVal, 0, attribVal.length);
        } else {
            atr.attribVal = null;
        }
        if (nshChain != null) {
            atr.nshChain = new byte[nshChain.length];
            bits.byteCopy(nshChain, 0, atr.nshChain, 0, nshChain.length);
        } else {
            atr.nshChain = null;
        }
        if (domainPath != null) {
            atr.domainPath = new byte[domainPath.length];
            bits.byteCopy(domainPath, 0, atr.domainPath, 0, domainPath.length);
        } else {
            atr.domainPath = null;
        }
        if (bfdDiscr != null) {
            atr.bfdDiscr = new byte[bfdDiscr.length];
            bits.byteCopy(bfdDiscr, 0, atr.bfdDiscr, 0, bfdDiscr.length);
        } else {
            atr.bfdDiscr = null;
        }
        if (hopCapa != null) {
            atr.hopCapa = new byte[hopCapa.length];
            bits.byteCopy(hopCapa, 0, atr.hopCapa, 0, hopCapa.length);
        } else {
            atr.hopCapa = null;
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
        if (connRtr != null) {
            atr.connRtr = (T) connRtr.copyBytes();
        } else {
            atr.connRtr = null;
        }
        if (pediRtr != null) {
            atr.pediRtr = (T) pediRtr.copyBytes();
        } else {
            atr.pediRtr = null;
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
        if (ip6comm != null) {
            atr.ip6comm = new ArrayList<tabIpv6comm>();
            for (int i = 0; i < ip6comm.size(); i++) {
                atr.ip6comm.add(ip6comm.get(i).copyBytes());
            }
        } else {
            atr.ip6comm = null;
        }
        if (unknown != null) {
            atr.unknown = new ArrayList<tabRouteBlob>();
            for (int i = 0; i < unknown.size(); i++) {
                atr.unknown.add(unknown.get(i).copyBytes());
            }
        } else {
            atr.unknown = null;
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
        if (segrouBeh != other.segrouBeh) {
            return 133;
        }
        if (segrouEth != other.segrouEth) {
            return 134;
        }
        if (bierIdx != other.bierIdx) {
            return 16;
        }
        if (bierSub != other.bierSub) {
            return 106;
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
        if (validRoa != other.validRoa) {
            return 21;
        }
        if (validAspa != other.validAspa) {
            return 128;
        }
        if (onlyCust != other.onlyCust) {
            return 21;
        }
        if (originType != other.originType) {
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
        if (pediLab != other.pediLab) {
            return 110;
        }
        if (pathLim != other.pathLim) {
            return 114;
        }
        if (pathAsn != other.pathAsn) {
            return 115;
        }
        if (segrouPrf != null) {
            if (other.segrouPrf == null) {
                return 33;
            }
            if (segrouPrf.compareTo(other.segrouPrf) != 0) {
                return 34;
            }
        } else if (other.segrouPrf != null) {
            return 35;
        }
        if (labelLoc != null) {
            if (other.labelLoc == null) {
                return 36;
            }
            if (labelLoc.compareTo(other.labelLoc) != 0) {
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
            if (srcRtr.compareTo(other.srcRtr) != 0) {
                return 41;
            }
        } else if (other.srcRtr != null) {
            return 42;
        }
        if (aggrRtr != null) {
            if (other.aggrRtr == null) {
                return 43;
            }
            if (aggrRtr.compareTo(other.aggrRtr) != 0) {
                return 44;
            }
        } else if (other.aggrRtr != null) {
            return 45;
        }
        if (connRtr != null) {
            if (other.connRtr == null) {
                return 107;
            }
            if (connRtr.compareTo(other.connRtr) != 0) {
                return 108;
            }
        } else if (other.connRtr != null) {
            return 109;
        }
        if (pediRtr != null) {
            if (other.pediRtr == null) {
                return 111;
            }
            if (pediRtr.compareTo(other.pediRtr) != 0) {
                return 112;
            }
        } else if (other.pediRtr != null) {
            return 113;
        }
        if (nextHop != null) {
            if (other.nextHop == null) {
                return 46;
            }
            if (nextHop.compareTo(other.nextHop) != 0) {
                return 47;
            }
        } else if (other.nextHop != null) {
            return 48;
        }
        if (oldHop != null) {
            if (other.oldHop == null) {
                return 49;
            }
            if (oldHop.compareTo(other.oldHop) != 0) {
                return 50;
            }
        } else if (other.oldHop != null) {
            return 51;
        }
        if (originator != null) {
            if (other.originator == null) {
                return 52;
            }
            if (originator.compareTo(other.originator) != 0) {
                return 53;
            }
        } else if (other.originator != null) {
            return 54;
        }
        if (tabRouteUtil.diffIntList(labelRem, other.labelRem)) {
            return 55;
        }
        if (tabRouteUtil.diffIntList(stdComm, other.stdComm)) {
            return 56;
        }
        if (tabRouteUtil.diffIntList(pathSeq, other.pathSeq)) {
            return 57;
        }
        if (tabRouteUtil.diffIntList(pathSet, other.pathSet)) {
            return 58;
        }
        if (tabRouteUtil.diffIntList(confSeq, other.confSeq)) {
            return 59;
        }
        if (tabRouteUtil.diffIntList(confSet, other.confSet)) {
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
        if (nshChain != null) {
            if (other.nshChain == null) {
                return 116;
            }
            if (nshChain.length != other.nshChain.length) {
                return 117;
            }
            if (bits.byteComp(nshChain, 0, other.nshChain, 0, nshChain.length) != 0) {
                return 118;
            }
        } else if (other.nshChain != null) {
            return 119;
        }
        if (domainPath != null) {
            if (other.domainPath == null) {
                return 124;
            }
            if (domainPath.length != other.domainPath.length) {
                return 125;
            }
            if (bits.byteComp(domainPath, 0, other.domainPath, 0, domainPath.length) != 0) {
                return 126;
            }
        } else if (other.domainPath != null) {
            return 127;
        }
        if (bfdDiscr != null) {
            if (other.bfdDiscr == null) {
                return 120;
            }
            if (bfdDiscr.length != other.bfdDiscr.length) {
                return 121;
            }
            if (bits.byteComp(bfdDiscr, 0, other.bfdDiscr, 0, bfdDiscr.length) != 0) {
                return 122;
            }
        } else if (other.bfdDiscr != null) {
            return 123;
        }
        if (hopCapa != null) {
            if (other.hopCapa == null) {
                return 129;
            }
            if (hopCapa.length != other.hopCapa.length) {
                return 130;
            }
            if (bits.byteComp(hopCapa, 0, other.hopCapa, 0, hopCapa.length) != 0) {
                return 131;
            }
        } else if (other.hopCapa != null) {
            return 132;
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
                if (cmp.compareTo(other.clustList.get(i)) != 0) {
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
                if (cmp.compareTo(other.lrgComm.get(i)) != 0) {
                    return 96;
                }
            }
        } else if (other.lrgComm != null) {
            return 97;
        }
        if (ip6comm != null) {
            if (other.ip6comm == null) {
                return 135;
            }
            if (ip6comm.size() != other.ip6comm.size()) {
                return 136;
            }
            for (int i = 0; i < ip6comm.size(); i++) {
                tabIpv6comm cmp = ip6comm.get(i);
                if (cmp.compareTo(other.ip6comm.get(i)) != 0) {
                    return 137;
                }
            }
        } else if (other.ip6comm != null) {
            return 138;
        }
        if (unknown != null) {
            if (other.unknown == null) {
                return 98;
            }
            if (unknown.size() != other.unknown.size()) {
                return 99;
            }
            for (int i = 0; i < unknown.size(); i++) {
                tabRouteBlob cmp = unknown.get(i);
                if (cmp.compareTo(other.unknown.get(i)) != 0) {
                    return 100;
                }
            }
        } else if (other.unknown != null) {
            return 101;
        }
        if (entropyLabel != null) {
            if (other.entropyLabel == null) {
                return 102;
            }
            if (entropyLabel.length != other.entropyLabel.length) {
                return 103;
            }
            if (bits.byteComp(entropyLabel, 0, other.entropyLabel, 0, entropyLabel.length) != 0) {
                return 104;
            }
        } else if (other.entropyLabel != null) {
            return 105;
        }
        return 0;
    }

    /**
     * need to update with this prefix
     *
     * @param imp new prefix
     * @return true if yes, false if not
     */
    public boolean isOtherBetter(tabRouteAttr<T> imp) {
        if (imp.distance < distance) {
            return true;
        }
        if (imp.distance > distance) {
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
        if (imp.originType < originType) {
            return true;
        }
        if (imp.originType > originType) {
            return false;
        }
        if (imp.metric < metric) {
            return true;
        }
        if (imp.metric > metric) {
            return false;
        }
        il = tabRouteUtil.listLen(imp.clustList);
        ol = tabRouteUtil.listLen(clustList);
        if (il < ol) {
            return true;
        }
        if (il > ol) {
            return false;
        }
        return false;
    }

    /**
     * size of as path
     *
     * @return size of as path
     */
    public int asPathLen() {
        int i = tabRouteUtil.listLen(pathSeq);
        if (tabRouteUtil.listLen(pathSet) > 0) {
            i++;
        }
        return i;
    }

    /**
     * number of as path prepends
     *
     * @return size of prepend
     */
    public int asPathPrep() {
        int i = tabRouteUtil.countPrepends(pathSeq);
        i += tabRouteUtil.countPrepends(pathSet);
        return i;
    }

    /**
     * number of as path loops
     *
     * @return size of loops
     */
    public int asPathLoop() {
        int i = tabRouteUtil.countLoops(pathSeq);
        i += tabRouteUtil.countLoops(pathSet);
        return i;
    }

    /**
     * number of unknown attributes
     *
     * @return count
     */
    public int unkAttrCnt() {
        if (unknown == null) {
            return 0;
        }
        return unknown.size();
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
     * @param skbeg skip beginning
     * @param skend skip ending
     * @return true on match, false if not
     */
    public boolean asPathMid(tabIntMatcher match, int skbeg, int skend) {
        if (match.action == tabIntMatcher.actionType.always) {
            return true;
        }
        if (pathSeq == null) {
            return false;
        }
        for (int i = skbeg; i < (pathSeq.size() - skend); i++) {
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
        return tabRouteUtil.dumpIntList(confSeq, "(", ") ")
                + tabRouteUtil.dumpIntList(confSet, "[", "] ")
                + tabRouteUtil.dumpIntList(pathSeq, "", "")
                + tabRouteUtil.dumpIntList(pathSet, " {", "}");
    }

    /**
     * get the as name string
     *
     * @return string
     */
    public String asNameStr() {
        return clntWhois.asnList2str(confSeq, "(", ") ")
                + clntWhois.asnList2str(confSet, "[", "] ")
                + clntWhois.asnList2str(pathSeq, "", "")
                + clntWhois.asnList2str(pathSet, " {", "}");
    }

    /**
     * get the as info string
     *
     * @return string
     */
    public String asInfoStr() {
        return clntWhois.asnList2info(confSeq, "(", ") ")
                + clntWhois.asnList2info(confSet, "[", "] ")
                + clntWhois.asnList2info(pathSeq, "", "")
                + clntWhois.asnList2info(pathSet, " {", "}");
    }

    /**
     * get the as info string
     *
     * @return string
     */
    public String asMixedStr() {
        return clntWhois.asnList2mixed(confSeq, "(", ") ")
                + clntWhois.asnList2mixed(confSet, "[", "] ")
                + clntWhois.asnList2mixed(pathSeq, "", "")
                + clntWhois.asnList2mixed(pathSet, " {", "}");
    }

    /**
     * get the as path integers
     *
     * @param beg first as, -1 if nothing
     * @return string
     */
    public List<Integer> asPathInts(int beg) {
        List<Integer> res = new ArrayList<Integer>();
        if (beg != -1) {
            res.add(beg);
        }
        appendIntList(res, confSeq);
        appendIntList(res, confSet);
        appendIntList(res, pathSeq);
        appendIntList(res, pathSet);
        return res;
    }

    private static void appendIntList(List<Integer> trg, List<Integer> src) {
        if (src == null) {
            return;
        }
        int p = src.size();
        for (int i = 0; i < p; i++) {
            Integer o = src.get(i);
            trg.add(0 + o);
        }
    }

    /**
     * get ignore help
     *
     * @param hl help to append
     * @param lv level to add
     */
    public static void ignoreHelp(userHelp hl, int lv) {
        hl.add(null, false, lv, new int[]{lv, -1}, "bier", "ignore bier");
        hl.add(null, false, lv, new int[]{lv, -1}, "attrset", "ignore attribute set");
        hl.add(null, false, lv, new int[]{lv, -1}, "cluster", "ignore cluster list");
        hl.add(null, false, lv, new int[]{lv, -1}, "nexthop", "ignore nexthop");
        hl.add(null, false, lv, new int[]{lv, -1}, "origin", "ignore origin");
        hl.add(null, false, lv, new int[]{lv, -1}, "metric", "ignore metric");
        hl.add(null, false, lv, new int[]{lv, -1}, "locpref", "ignore local preference");
        hl.add(null, false, lv, new int[]{lv, -1}, "distance", "ignore distance");
        hl.add(null, false, lv, new int[]{lv, -1}, "tag", "ignore tag");
        hl.add(null, false, lv, new int[]{lv, -1}, "validity", "ignore validity");
        hl.add(null, false, lv, new int[]{lv, -1}, "aspath", "ignore as path");
        hl.add(null, false, lv, new int[]{lv, -1}, "asconf", "ignore confed path");
        hl.add(null, false, lv, new int[]{lv, -1}, "asset", "ignore path set");
        hl.add(null, false, lv, new int[]{lv, -1}, "stdcomm", "ignore standard community");
        hl.add(null, false, lv, new int[]{lv, -1}, "extcomm", "ignore extended community");
        hl.add(null, false, lv, new int[]{lv, -1}, "lrgcomm", "ignore large community");
        hl.add(null, false, lv, new int[]{lv, -1}, "ip6comm", "ignore ipv6 community");
        hl.add(null, false, lv, new int[]{lv, -1}, "unknown", "ignore unknown attribute");
        hl.add(null, false, lv, new int[]{lv, -1}, "sortcomm", "sort communities");
        hl.add(null, false, lv, new int[]{lv, -1}, "lnksta", "ignore link state");
        hl.add(null, false, lv, new int[]{lv, -1}, "aigp", "ignore accumulated igp");
        hl.add(null, false, lv, new int[]{lv, -1}, "bandwidth", "ignore bandwidth");
        hl.add(null, false, lv, new int[]{lv, -1}, "label", "ignore labels");
        hl.add(null, false, lv, new int[]{lv, -1}, "aggregate", "ignore aggregator");
        hl.add(null, false, lv, new int[]{lv, -1}, "connector", "ignore connector");
        hl.add(null, false, lv, new int[]{lv, -1}, "pedisting", "ignore pe distinguisher");
        hl.add(null, false, lv, new int[]{lv, -1}, "pathlimit", "ignore aspath limit");
        hl.add(null, false, lv, new int[]{lv, -1}, "nshchain", "ignore nsh service chain");
        hl.add(null, false, lv, new int[]{lv, -1}, "domainpath", "ignore domain path");
        hl.add(null, false, lv, new int[]{lv, -1}, "bfddiscr", "ignore bfd discriminator");
        hl.add(null, false, lv, new int[]{lv, -1}, "hopcapa", "ignore next hop capability");
        hl.add(null, false, lv, new int[]{lv, -1}, "orignted", "ignore originator");
        hl.add(null, false, lv, new int[]{lv, -1}, "pmsi", "ignore pmsi");
        hl.add(null, false, lv, new int[]{lv, -1}, "segrout", "ignore segment routing");
        hl.add(null, false, lv, new int[]{lv, -1}, "tunnel", "ignore tunnel");
        hl.add(null, false, lv, new int[]{lv, -1}, "entropy", "ignore entropy");
        hl.add(null, false, lv, new int[]{lv, -1}, "empty", "ignore empty lists");
    }

    /**
     * convert string to ignore bitmap
     *
     * @param a string
     * @return bitmap
     */
    public static long string2ignore(String a) {
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
        if (a.equals("unknown")) {
            return 0x4000000;
        }
        if (a.equals("entropy")) {
            return 0x8000000;
        }
        if (a.equals("connector")) {
            return 0x10000000;
        }
        if (a.equals("pedisting")) {
            return 0x20000000;
        }
        if (a.equals("pathlimit")) {
            return 0x40000000;
        }
        if (a.equals("nshchain")) {
            return 0x80000000;
        }
        if (a.equals("bfddiscr")) {
            return 0x100000000L;
        }
        if (a.equals("domainpath")) {
            return 0x200000000L;
        }
        if (a.equals("hopcapa")) {
            return 0x400000000L;
        }
        if (a.equals("asset")) {
            return 0x800000000L;
        }
        if (a.equals("ip6comm")) {
            return 0x1000000000L;
        }
        return 0;
    }

    /**
     * convert ignore bitmap to string
     *
     * @param i bitmap
     * @return string
     */
    public static String ignore2string(long i) {
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
        if ((i & 0x4000000) != 0) {
            a += " unknown";
        }
        if ((i & 0x8000000) != 0) {
            a += " entropy";
        }
        if ((i & 0x10000000) != 0) {
            a += " connector";
        }
        if ((i & 0x20000000) != 0) {
            a += " pedisting";
        }
        if ((i & 0x40000000) != 0) {
            a += " pathlimit";
        }
        if ((i & 0x80000000) != 0) {
            a += " nshchain";
        }
        if ((i & 0x100000000L) != 0) {
            a += " bfddiscr";
        }
        if ((i & 0x200000000L) != 0) {
            a += " domainpath";
        }
        if ((i & 0x400000000L) != 0) {
            a += " hopcapa";
        }
        if ((i & 0x800000000L) != 0) {
            a += " asset";
        }
        if ((i & 0x1000000000L) != 0) {
            a += " ip6comm";
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
    public static <T extends addrType> void ignoreAttribs(tabRouteAttr<T> ntry, long ign) {
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
            ntry.originType = 0;
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
            ntry.validRoa = 0;
            ntry.validAspa = 0;
        }
        if ((ign & 0x100) != 0) {
            ntry.pathSeq = new ArrayList<Integer>();
            ntry.pathSet = new ArrayList<Integer>();
        }
        if ((ign & 0x200) != 0) {
            ntry.confSeq = new ArrayList<Integer>();
            ntry.confSet = new ArrayList<Integer>();
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
            ntry.bierSub = 0;
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
                Collections.sort(ntry.lrgComm);
            }
            if (ntry.ip6comm != null) {
                Collections.sort(ntry.ip6comm);
            }
        }
        if ((ign & 0x1000000) != 0) {
            ntry.linkStat = null;
        }
        if ((ign & 0x2000000) != 0) {
            ntry.clustList = tabRouteUtil.nullEmptyList(ntry.clustList);
            ntry.confSeq = tabRouteUtil.nullEmptyList(ntry.confSeq);
            ntry.confSet = tabRouteUtil.nullEmptyList(ntry.confSet);
            ntry.extComm = tabRouteUtil.nullEmptyList(ntry.extComm);
            ntry.labelRem = tabRouteUtil.nullEmptyList(ntry.labelRem);
            ntry.lrgComm = tabRouteUtil.nullEmptyList(ntry.lrgComm);
            ntry.ip6comm = tabRouteUtil.nullEmptyList(ntry.ip6comm);
            ntry.pathSeq = tabRouteUtil.nullEmptyList(ntry.pathSeq);
            ntry.pathSet = tabRouteUtil.nullEmptyList(ntry.pathSet);
            ntry.stdComm = tabRouteUtil.nullEmptyList(ntry.stdComm);
            ntry.unknown = tabRouteUtil.nullEmptyList(ntry.unknown);
        }
        if ((ign & 0x4000000) != 0) {
            ntry.unknown = null;
        }
        if ((ign & 0x8000000) != 0) {
            ntry.entropyLabel = null;
        }
        if ((ign & 0x10000000) != 0) {
            ntry.connRtr = null;
        }
        if ((ign & 0x20000000) != 0) {
            ntry.pediRtr = null;
            ntry.pediLab = 0;
        }
        if ((ign & 0x40000000) != 0) {
            ntry.pathLim = 0;
            ntry.pathAsn = 0;
        }
        if ((ign & 0x80000000) != 0) {
            ntry.nshChain = null;
        }
        if ((ign & 0x100000000L) != 0) {
            ntry.bfdDiscr = null;
        }
        if ((ign & 0x200000000L) != 0) {
            ntry.domainPath = null;
        }
        if ((ign & 0x400000000L) != 0) {
            ntry.hopCapa = null;
        }
        if ((ign & 0x800000000L) != 0) {
            ntry.pathSet = new ArrayList<Integer>();
            ntry.confSet = new ArrayList<Integer>();
        }
        if ((ign & 0x1000000000L) != 0) {
            ntry.ip6comm = null;
        }
    }

    /**
     * full dump of this attribute
     *
     * @param lst list to append
     * @param beg beginning
     */
    public void fullDump(userFormat lst, String beg) {
        lst.add(beg + "type|" + rouTyp + " " + protoNum);
        lst.add(beg + "source|" + srcRtr);
        lst.add(beg + "validity roa|" + tabRpkiUtil.validity2string(validRoa));
        lst.add(beg + "validity aspa|" + tabRpkiUtil.validity2string(validAspa));
        lst.add(beg + "segrout index|" + segrouIdx);
        lst.add(beg + "segrout old base|" + segrouOld);
        lst.add(beg + "segrout base|" + segrouBeg);
        lst.add(beg + "segrout size|" + segrouSiz);
        lst.add(beg + "segrout offset|" + segrouOfs);
        lst.add(beg + "segrout behavior|" + segrouEth + "-" + segrouBeh);
        lst.add(beg + "segrout prefix|" + segrouPrf);
        lst.add(beg + "bier index|" + bierIdx);
        lst.add(beg + "bier subdomain|" + bierSub);
        lst.add(beg + "bier old base|" + bierOld);
        lst.add(beg + "bier base|" + bierBeg);
        lst.add(beg + "bier range|" + bierSiz);
        lst.add(beg + "bier size|" + bierHdr + "-" + tabLabelBier.bsl2num(bierHdr));
        lst.add(beg + "updated|" + bits.time2str(cfgAll.timeZoneName, time + cfgAll.timeServerOffset, 3) + " (" + bits.timePast(time) + " ago)");
        lst.add(beg + "version|" + version);
        lst.add(beg + "distance|" + distance);
        lst.add(beg + "metric|" + metric);
        lst.add(beg + "ident|" + ident);
        lst.add(beg + "hops|" + hops);
        lst.add(beg + "interface|" + iface);
        lst.add(beg + "table|" + rouTab);
        lst.add(beg + "nexthop|" + nextHop);
        lst.add(beg + "original nexthop|" + oldHop);
        lst.add(beg + "route tag|" + tag);
        lst.add(beg + "origin type|" + originType);
        lst.add(beg + "local preference|" + locPref);
        lst.add(beg + "accumulated igp|" + accIgp);
        lst.add(beg + "bandwidth|" + bandwidth);
        lst.add(beg + "to customer asnum|" + bits.num2str(onlyCust));
        lst.add(beg + "to customer asnam|" + clntWhois.asn2name(onlyCust, true));
        lst.add(beg + "attribute asnum|" + bits.num2str(attribAs));
        lst.add(beg + "attribute asnam|" + clntWhois.asn2name(attribAs, true));
        lst.add(beg + "attribute value|" + bits.byteDump(attribVal, 0, -1));
        lst.add(beg + "nsh chain value|" + bits.byteDump(nshChain, 0, -1));
        lst.add(beg + "domain path value|" + bits.byteDump(domainPath, 0, -1));
        lst.add(beg + "bfd discr value|" + bits.byteDump(bfdDiscr, 0, -1));
        lst.add(beg + "hop capability value|" + bits.byteDump(hopCapa, 0, -1));
        lst.add(beg + "tunnel type|" + tunelTyp);
        lst.add(beg + "tunnel value|" + bits.byteDump(tunelVal, 0, -1));
        lst.add(beg + "link state|" + bits.byteDump(linkStat, 0, -1));
        lst.add(beg + "pmsi type|" + pmsiTyp);
        lst.add(beg + "pmsi label*16|" + pmsiLab);
        lst.add(beg + "pmsi tunnel|" + bits.byteDump(pmsiTun, 0, -1));
        lst.add(beg + "evpn label*16|" + evpnLab);
        lst.add(beg + "entropy label|" + bits.byteDump(entropyLabel, 0, -1));
        lst.add(beg + "atomic aggregator|" + atomicAggr);
        lst.add(beg + "aggregator asnum|" + bits.num2str(aggrAs));
        lst.add(beg + "aggregator asnam|" + clntWhois.asn2name(aggrAs, true));
        lst.add(beg + "aggregator router|" + aggrRtr);
        lst.add(beg + "connector router|" + connRtr);
        lst.add(beg + "distinguish pe|" + pediRtr);
        lst.add(beg + "distinguish label|" + pediLab);
        lst.add(beg + "path limit|" + pathLim);
        lst.add(beg + "path asnum|" + bits.num2str(pathAsn));
        lst.add(beg + "path asnam|" + clntWhois.asn2name(pathAsn, true));
        lst.add(beg + "originator|" + originator);
        lst.add(beg + "cluster list|" + tabRouteUtil.dumpAddrList(clustList));
        lst.add(beg + "aspath|" + asPathStr());
        lst.add(beg + "asname|" + asNameStr());
        lst.add(beg + "asinfo|" + asInfoStr());
        lst.add(beg + "asmixed|" + asMixedStr());
        lst.add(beg + "path length|" + asPathLen());
        lst.add(beg + "standard community|" + tabRouteUtil.stdComms2string(stdComm));
        lst.add(beg + "extended community|" + tabRouteUtil.extComms2string(extComm));
        lst.add(beg + "large community|" + tabRouteUtil.lrgComms2string(lrgComm));
        lst.add(beg + "ipv6 community|" + tabRouteUtil.ip6comms2string(ip6comm));
        lst.add(beg + "internal source|" + rouSrc);
        lst.add(beg + "local label|" + labelLoc);
        lst.add(beg + "remote label|" + tabRouteUtil.dumpIntList(labelRem, "", ""));
        if (unknown == null) {
            return;
        }
        for (int i = 0; i < unknown.size(); i++) {
            lst.add(beg + "unknown attribute|" + unknown.get(i));
        }
    }

    /**
     * convert to route format
     *
     * @return converted
     */
    public String toShSrRoute() {
        return segrouIdx + "|" + segrouBeg + "|" + segrouOld + "|" + segrouPrf + "|" + segrouBeh;
    }

    /**
     * convert to route format
     *
     * @return converted
     */
    public String toShBrRoute() {
        return bierIdx + "|" + bierSub + "|" + bierBeg + "|" + bierOld + "|" + bierHdr + "-" + tabLabelBier.bsl2num(bierHdr);
    }

    /**
     * convert to route format
     *
     * @return converted
     */
    public String toShChgRoute() {
        return nextHop + "|" + bits.timePast(time) + "|" + bits.time2str(cfgAll.timeZoneName, time, 3);
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
        return labelLoc + "|" + evpnLab + "|" + pmsiLab + "|" + tabRouteUtil.dumpIntList(labelRem, "", "") + "|" + nextHop;
    }

    /**
     * convert to bgp format
     *
     * @return converted
     */
    public String toShBgpLast() {
        return "|" + nextHop + "|" + distance + "/" + locPref + "/" + originType + "/" + metric + "|" + asPathStr();
    }

    /**
     * convert to route format
     *
     * @return converted
     */
    public String toShAsName() {
        return "|" + nextHop + "|" + distance + "/" + locPref + "/" + originType + "/" + metric + "|" + asNameStr();
    }

    /**
     * convert to route format
     *
     * @return converted
     */
    public String toShAsInfo() {
        return "|" + nextHop + "|" + distance + "/" + locPref + "/" + originType + "/" + metric + "|" + asInfoStr();
    }

    /**
     * convert to route format
     *
     * @return converted
     */
    public String toShAsMixed() {
        return "|" + nextHop + "|" + distance + "/" + locPref + "/" + originType + "/" + metric + "|" + asMixedStr();
    }

    /**
     * convert to ldp format
     *
     * @return converted
     */
    public String toShLdp() {
        return labelLoc + "|" + tabRouteUtil.dumpIntList(labelRem, "", "") + "|" + nextHop;
    }

    /**
     * convert to rpki format
     *
     * @return converted
     */
    public String toShRpki() {
        int i = asPathEnd();
        int o = tabRouteUtil.getValidExtCommRoa(extComm);
        int p = tabRouteUtil.getValidExtCommAspa(extComm);
        return bits.num2str(i) + "|" + clntWhois.asn2name(i, true) + "|" + tabRpkiUtil.validity2string(validRoa) + "|" + tabRpkiUtil.validity2string(o) + "|" + tabRpkiUtil.validity2string(validAspa) + "|" + tabRpkiUtil.validity2string(p) + "|" + bits.timePast(time) + "|" + bits.time2str(cfgAll.timeZoneName, time + cfgAll.timeServerOffset, 3);
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
