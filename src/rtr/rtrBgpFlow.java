package rtr;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrPrefix;
import java.util.ArrayList;
import pack.packHolder;
import tab.tabAceslstN;
import tab.tabIntMatcher;
import tab.tabListing;
import tab.tabPlcmapN;
import tab.tabRoute;
import tab.tabRouteEntry;
import tab.tabRtrmapN;
import util.bits;

/**
 * bgp4 flowspec
 *
 * @author matecsaba
 */
public class rtrBgpFlow {

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
        attr.nextHop = new addrIP();
        attr.rouSrc = rtrBgpUtil.peerOriginate;
        boolean res = true;
        for (int i = 0; i < plcy.size(); i++) {
            res &= advertPolicy(tab, plcy.get(i), attr, ipv6, as);
        }
        return res;
    }

    private static boolean advertPolicy(tabRoute<addrIP> tab, tabPlcmapN plcy, tabRouteEntry<addrIP> attr, boolean ipv6, int as) {
        packHolder pck = new packHolder(true, true);
        if (plcy.aclMatch == null) {
            encodeOthers(pck, plcy);
            return advertEntry(tab, pck, attr, as, plcy.accessRate);
        }
        boolean res = true;
        for (int i = 0; i < plcy.aclMatch.size(); i++) {
            pck.clear();
            encodeAceslst(pck, plcy.aclMatch.get(i), ipv6);
            encodeOthers(pck, plcy);
            res &= advertEntry(tab, pck, attr, as, plcy.accessRate);
        }
        return res;
    }

    /**
     * advertise target network
     *
     * @param tab table to update
     * @param trg target to match
     * @param ipv6 ipv6 route
     * @param dir direction
     * @param attr attributes
     * @return false on success, true on error
     */
    public static boolean advertNetwork(tabRoute<addrIP> tab, addrPrefix<addrIP> trg, boolean ipv6, int dir, tabRouteEntry<addrIP> attr) {
        packHolder pck = new packHolder(true, true);
        encodeAddrMtch(pck, dir, ipv6, trg.network, trg.mask);
        return advertEntry(tab, pck, attr, 0, 0);
    }

    private static boolean advertEntry(tabRoute<addrIP> tab, packHolder pck, tabRouteEntry<addrIP> attr, int as, int rate) {
        int o = pck.dataSize();
        if (o < 1) {
            return true;
        }
        attr = attr.copyBytes();
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
        if (attr.extComm == null) {
            attr.extComm = new ArrayList<Long>();
        }
        if (rate > 0) {
            attr.extComm.add(tabRtrmapN.rate2comm(as, rate));
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
        if (mask.isFilled(0) && addr.isFilled(0)) {
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
        rtrBgpUtil.writePrefix(sfi, pck, ntry);
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

}
