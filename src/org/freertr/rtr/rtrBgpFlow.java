package org.freertr.rtr;

import java.util.ArrayList;
import org.freertr.addr.addrAfi;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrPrefix;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgVrf;
import org.freertr.ip.ipFwd;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabAceslstN;
import org.freertr.tab.tabIntMatcher;
import org.freertr.tab.tabListing;
import org.freertr.tab.tabListingEntry;
import org.freertr.tab.tabPlcmapN;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteEntry;
import org.freertr.tab.tabRouteUtil;
import org.freertr.util.bits;

/**
 * bgp4 flowspec
 *
 * @author matecsaba
 */
public class rtrBgpFlow {

    private rtrBgpFlow() {
    }

    /**
     * decode rules
     *
     * @param tab list of routes
     * @param ipv6 set to ipv6 addresses
     * @return list of rules
     */
    public static tabListing<tabPlcmapN, addrIP> doDecode(tabRoute<addrIP> tab, boolean ipv6) {
        tabListing<tabPlcmapN, addrIP> lst = new tabListing<tabPlcmapN, addrIP>();
        for (int i = 0; i < tab.size(); i++) {
            tabRouteEntry<addrIP> rou = tab.get(i);
            tabPlcmapN res = decodePolicy(rou, ipv6);
            res.sequence = lst.size() + 1;
            lst.add(res);
        }
        tabPlcmapN res = new tabPlcmapN();
        res.description = "default";
        res.action = tabListingEntry.actionType.actPermit;
        res.sequence = lst.size() + 1;
        lst.add(res);
        return lst;
    }

    /**
     * advertise flowspec policy
     *
     * @param tab table to update
     * @param plcy policy to advertise
     * @param attr entry to update
     * @param ipv6 set to ipv6 addresses
     * @param as as number
     * @return false on success, true on error
     */
    public static boolean doAdvertise(tabRoute<addrIP> tab, tabListing<tabPlcmapN, addrIP> plcy, tabRouteEntry<addrIP> attr, boolean ipv6, int as) {
        if (plcy == null) {
            return true;
        }
        attr.best.nextHop = new addrIP();
        attr.best.rouSrc = rtrBgpUtil.peerOriginate;
        boolean res = true;
        for (int i = 0; i < plcy.size(); i++) {
            res &= advertPolicy(tab, plcy.get(i), attr, ipv6, as);
        }
        return res;
    }

    private static boolean advertPolicy(tabRoute<addrIP> tab, tabPlcmapN plcy, tabRouteEntry<addrIP> attr, boolean ipv6, int as) {
        packHolder pck = new packHolder(true, true);
        long rate;
        switch (plcy.action) {
            case actDeny:
                rate = 0;
                break;
            case actPermit:
                rate = -1;
                break;
            default:
                rate = plcy.accessRate;
                break;
        }
        if (plcy.aclMatch == null) {
            encodeOthers(pck, plcy);
            return advertEntry(tab, pck, attr, as, rate, plcy.vrfSet4);
        }
        boolean res = true;
        for (int i = 0; i < plcy.aclMatch.size(); i++) {
            pck.clear();
            encodeAceslst(pck, plcy.aclMatch.get(i), ipv6);
            encodeOthers(pck, plcy);
            res &= advertEntry(tab, pck, attr, as, rate, plcy.vrfSet4);
        }
        return res;
    }

    /**
     * advertise target network
     *
     * @param trg target to match
     * @param ipv6 ipv6 route
     * @param dir direction
     * @param attr attributes
     * @return route on success, null on error
     */
    public static tabRouteEntry<addrIP> advertNetwork(addrPrefix<addrIP> trg, boolean ipv6, int dir, tabRouteEntry<addrIP> attr) {
        packHolder pck = new packHolder(true, true);
        encodeAddrMtch(pck, dir, ipv6, trg.network, trg.mask);
        return convertNetwork(pck, attr, 0, -1, null);
    }

    private static tabRouteEntry<addrIP> convertNetwork(packHolder pck, tabRouteEntry<addrIP> attr, int as, long rate, ipFwd dvrt) {
        int o = pck.dataSize();
        if (o < 1) {
            return null;
        }
        attr = attr.copyBytes(tabRoute.addType.notyet);
        for (int i = 0; i < ((4 * addrIP.size) - o); i++) {
            pck.putByte(0, 0);
            pck.putSkip(1);
        }
        pck.merge2end();
        byte[] buf = pck.getCopy();
        byte[] adr = new byte[addrIP.size];
        attr.prefix = new addrPrefix<addrIP>(new addrIP(), adr.length * 8);
        adr[0] = (byte) o;
        bits.byteCopy(buf, 0, adr, 1, 15);
        attr.prefix.network.fromBuf(adr, 0);
        bits.byteCopy(buf, 15, adr, 0, 16);
        attr.prefix.broadcast.fromBuf(adr, 0);
        bits.byteCopy(buf, 31, adr, 0, 16);
        attr.prefix.wildcard.fromBuf(adr, 0);
        bits.byteCopy(buf, 47, adr, 0, 16);
        attr.prefix.mask.fromBuf(adr, 0);
        if (attr.best.extComm == null) {
            attr.best.extComm = new ArrayList<Long>();
        }
        if (rate >= 0) {
            attr.best.extComm.add(tabRouteUtil.rate2comm(as, rate));
        }
        if (dvrt != null) {
            attr.best.extComm.add(tabRouteUtil.divert2comm(dvrt.rd));
        }
        return attr;
    }

    private static boolean advertEntry(tabRoute<addrIP> tab, packHolder pck, tabRouteEntry<addrIP> attr, int as, long rate, ipFwd dvrt) {
        attr = convertNetwork(pck, attr, as, rate, dvrt);
        if (attr == null) {
            return true;
        }
        tab.add(tabRoute.addType.better, attr, true, true);
        return false;
    }

    private static void encodeAceslst(packHolder pck, tabAceslstN<addrIP> plcy, boolean ipv6) {
        encodeAddrMtch(pck, 1, ipv6, plcy.trgAddr, plcy.trgMask);
        encodeAddrMtch(pck, 2, ipv6, plcy.srcAddr, plcy.srcMask);
        encodeIntMtch(pck, 3, plcy.proto);
        encodeIntMtch(pck, 5, plcy.trgPort);
        encodeIntMtch(pck, 6, plcy.srcPort);
        encodeIntMtch(pck, 9, plcy.flag);
        encodeIntMtch(pck, 10, plcy.len);
        encodeIntMtch(pck, 11, plcy.dscp);
    }

    private static void encodeOthers(packHolder pck, tabPlcmapN plcy) {
        encodeIntMtch(pck, 10, plcy.lengthMatch);
        encodeIntMtch(pck, 11, plcy.dscpMatch);
    }

    private static void encodeAddrMtch(packHolder pck, int typ, boolean ipv6, addrIP addr, addrIP mask) {
        if (mask.isEmpty() && addr.isEmpty()) {
            return;
        }
        pck.putByte(0, typ);
        pck.putSkip(1);
        addrPrefix<addrIP> prf;
        int sfi;
        if (ipv6) {
            addrIPv6 a6 = addr.toIPv6();
            addrIPv6 m6 = mask.toIPv6();
            prf = addrPrefix.ip6toIP(new addrPrefix<addrIPv6>(a6, m6.toNetmask()));
            sfi = rtrBgpUtil.afiIpv6;
        } else {
            addrIPv4 a4 = addr.toIPv4();
            addrIPv4 m4 = mask.toIPv4();
            prf = addrPrefix.ip4toIP(new addrPrefix<addrIPv4>(a4, m4.toNetmask()));
            sfi = rtrBgpUtil.afiIpv4;
        }
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = prf;
        rtrBgpUtil.writePrefix(sfi, true, pck, ntry);
        if (!ipv6) {
            pck.merge2end();
            return;
        }
        int i = pck.headSize();
        byte[] buf = pck.getHeadArray();
        bits.byteCopy(buf, 2, buf, 3, i - 2);
        buf[2] = 0;
        pck.putSkip(1);
        pck.merge2end();
    }

    private static void encodeIntMtch(packHolder pck, int typ, tabIntMatcher mtc) {
        switch (mtc.action) {
            case xact:
                pck.putByte(0, typ);
                pck.putSkip(1);
                encodeValue(pck, 0x81, mtc.rangeMin);
                break;
            case range:
                pck.putByte(0, typ);
                pck.putSkip(1);
                encodeValue(pck, 0x03, mtc.rangeMin);
                encodeValue(pck, 0xc5, mtc.rangeMax);
                break;
            case always:
                break;
            default:
                break;
        }
        pck.merge2end();
    }

    private static void encodeValue(packHolder pck, int flg, int val) {
        if ((val >= 0) && (val <= 0xff)) {
            pck.putByte(0, flg);
            pck.putByte(1, val);
            pck.putSkip(2);
            return;
        }
        if ((val >= 0) && (val <= 0xffff)) {
            pck.putByte(0, flg | 0x10);
            pck.msbPutW(1, val);
            pck.putSkip(3);
            return;
        }
        pck.putByte(0, flg | 0x20);
        pck.msbPutD(1, val);
        pck.putSkip(5);
    }

    private static tabPlcmapN decodePolicy(tabRouteEntry<addrIP> rou, boolean ipv6) {
        packHolder pck = new packHolder(true, true);
        pck.putAddr(addrIP.size * 0, rou.prefix.network);
        pck.putAddr(addrIP.size * 1, rou.prefix.broadcast);
        pck.putAddr(addrIP.size * 2, rou.prefix.wildcard);
        pck.putAddr(addrIP.size * 3, rou.prefix.mask);
        pck.putSkip(addrIP.size * 4);
        pck.merge2end();
        int i = pck.getByte(0);
        pck.getSkip(1);
        pck.setDataSize(i);
        tabPlcmapN res = new tabPlcmapN();
        res.description = pck.dump();
        res.action = tabListingEntry.actionType.actPermit;
        for (;;) {
            if (pck.dataSize() < 1) {
                break;
            }
            i = pck.getByte(0);
            pck.getSkip(1);
            switch (i) {
                case 1:
                    tabAceslstN<addrIP> ace = getAce(res);
                    tabRouteEntry<addrIP> prf = decodeAddrMtch(pck, ipv6);
                    ace.trgAddr = prf.prefix.network;
                    ace.trgMask = prf.prefix.mask;
                    break;
                case 2:
                    ace = getAce(res);
                    prf = decodeAddrMtch(pck, ipv6);
                    ace.srcAddr = prf.prefix.network;
                    ace.srcMask = prf.prefix.mask;
                    break;
                case 3:
                    ace = getAce(res);
                    ace.proto = decodeIntMtch(pck);
                    break;
                case 4:
                    ace = getAce(res);
                    ace.srcPort = decodeIntMtch(pck);
                    ace.trgPort = ace.srcPort;
                    break;
                case 5:
                    ace = getAce(res);
                    ace.trgPort = decodeIntMtch(pck);
                    break;
                case 6:
                    ace = getAce(res);
                    ace.srcPort = decodeIntMtch(pck);
                    break;
                case 7:
                    decodeIntMtch(pck); // icmp type
                    break;
                case 8:
                    decodeIntMtch(pck); // icmp code
                    break;
                case 9:
                    ace = getAce(res);
                    ace.flag = decodeIntMtch(pck);
                    break;
                case 10:
                    ace = getAce(res);
                    ace.len = decodeIntMtch(pck);
                    break;
                case 11:
                    ace = getAce(res);
                    ace.dscp = decodeIntMtch(pck);
                    break;
                case 12:
                    pck.getSkip(1); // fragments
                    break;
            }
        }
        if (rou.best.extComm == null) {
            return res;
        }
        for (i = 0; i < rou.best.extComm.size(); i++) {
            long p = rou.best.extComm.get(i);
            long o = tabRouteUtil.comm2divert(p);
            if (o > 0) {
                cfgVrf vrf = cfgAll.findRd(!ipv6, o);
                if (vrf == null) {
                    continue;
                }
                res.vrfSet4 = vrf.fwd4;
                res.vrfSet6 = vrf.fwd6;
                continue;
            }
            o = tabRouteUtil.comm2rate(p);
            if (o < 0) {
                continue;
            }
            if (o == 0) {
                res.action = tabListingEntry.actionType.actDeny;
                continue;
            }
            res.action = tabListingEntry.actionType.actPolice;
            res.accessRate = o;
        }
        return res;
    }

    private static tabAceslstN<addrIP> getAce(tabPlcmapN res) {
        if (res.aclMatch != null) {
            return res.aclMatch.get(0);
        }
        tabAceslstN<addrIP> ntry = new tabAceslstN<addrIP>(new addrIP());
        ntry.action = tabListingEntry.actionType.actPermit;
        ntry.sequence = 1;
        res.aclMatch = new tabListing<tabAceslstN<addrIP>, addrIP>();
        res.aclMatch.add(ntry);
        return ntry;
    }

    private static tabIntMatcher decodeIntMtch(packHolder pck) {
        tabIntMatcher res = new tabIntMatcher();
        res.action = tabIntMatcher.actionType.range;
        res.rangeMin = Integer.MIN_VALUE;
        res.rangeMax = Integer.MAX_VALUE;
        for (;;) {
            int flg = pck.getByte(0);
            pck.getSkip(1);
            int val = 0;
            switch ((flg >>> 4) & 3) {
                case 0:
                    val = pck.getByte(0);
                    pck.getSkip(1);
                    break;
                case 1:
                    val = pck.msbGetW(0);
                    pck.getSkip(2);
                    break;
                case 2:
                    val = pck.msbGetD(0);
                    pck.getSkip(4);
                    break;
                case 3:
                    val = pck.msbGetD(4);
                    pck.getSkip(8);
                    break;
            }
            switch (flg & 7) {
                case 1:
                    res.rangeMin = val;
                    res.rangeMax = val;
                    break;
                case 2:
                case 3:
                    res.rangeMin = val;
                    break;
                case 4:
                case 5:
                    res.rangeMax = val;
                    break;
            }
            if ((flg & 0x80) != 0) {
                break;
            }
        }
        return res;
    }

    private static tabRouteEntry<addrIP> decodeAddrMtch(packHolder pck, boolean ipv6) {
        if (ipv6) {
            pck.putByte(0, pck.getByte(0));
            pck.getSkip(2);
            pck.putSkip(1);
            pck.merge2beg();
            return addrAfi.ipv6uni.readPrefix(true, pck);
        } else {
            return addrAfi.ipv4uni.readPrefix(true, pck);
        }
    }

}
