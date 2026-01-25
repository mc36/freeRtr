package org.freertr.addr;

import java.util.ArrayList;
import org.freertr.cry.cryHashMd5;
import org.freertr.pack.packHolder;
import org.freertr.rtr.rtrBgpUtil;
import org.freertr.tab.tabRouteEntry;
import org.freertr.util.bits;
import org.freertr.util.logger;

/**
 * one sub address family
 *
 * @author matecsaba
 */
public interface addrSafi {

    /**
     * ipv4 unicast
     */
    public addrSafi ipv4uni = new addrSafiIpv4uni();

    /**
     * ipv6 unicast
     */
    public addrSafi ipv6uni = new addrSafiIpv6uni();

    /**
     * ipv4 labeled
     */
    public addrSafi ipv4lab = new addrSafiIpv4lab();

    /**
     * ipv6 labeled
     */
    public addrSafi ipv6lab = new addrSafiIpv6lab();

    /**
     * ipv4 colored
     */
    public addrSafi ipv4car = new addrSafiIpv4car();

    /**
     * ipv6 colored
     */
    public addrSafi ipv6car = new addrSafiIpv6car();

    /**
     * vpnv4 unicast
     */
    public addrSafi vpnv4uni = new addrSafiVpnv4uni();

    /**
     * vpnv6 unicast
     */
    public addrSafi vpnv6uni = new addrSafiVpnv6uni();

    /**
     * vpnv4 multicast
     */
    public addrSafi vpnv4mul = new addrSafiVpnv4mul();

    /**
     * vpnv6 multicast
     */
    public addrSafi vpnv6mul = new addrSafiVpnv6mul();

    /**
     * link state
     */
    public addrSafi linkState = new addrSafiLnkSt();

    /**
     * sdwan
     */
    public addrSafi sdWan = new addrSafiSdwan();

    /**
     * mup
     */
    public addrSafi mup = new addrSafiMup();

    /**
     * evpn
     */
    public addrSafi evpn = new addrSafiEvpn();

    /**
     * nsh
     */
    public addrSafi nsh = new addrSafiNsh();

    /**
     * rpd
     */
    public addrSafi rpd = new addrSafiRpd();

    /**
     * vpls
     */
    public addrSafi vpls = new addrSafiVpls();

    /**
     * mspw
     */
    public addrSafi mspw = new addrSafiMspw();

    /**
     * mdt
     */
    public addrSafi mdt = new addrSafiMdt();

    /**
     * rtfilter
     */
    public addrSafi rtf = new addrSafiRtf();

    /**
     * flowspec
     */
    public addrSafi flow = new addrSafiFlowspec();

    /**
     * vpn flowspec
     */
    public addrSafi vpnFlw = new addrSafiVpnFlow();

    /**
     * mvpn
     */
    public addrSafi mvpn = new addrSafiMvpn();

    /**
     * read address from packet
     *
     * @param oneLab just one label
     * @param pck packet to use
     * @return address read, null if nothing
     */
    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck);

    /**
     * write prefix
     *
     * @param oneLab just one label
     * @param pck packet to use
     * @param ntry prefix to write
     */
    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry);

    /**
     * read one ipvX unicast
     *
     * @param <T> address kind
     * @param adr dummy address
     * @param pck packet to use
     * @return address read
     */
    public static <T extends addrType> addrPrefix<T> readIpvXuni(T adr, packHolder pck) {
        int i = pck.getByte(0);
        pck.getSkip(1);
        pck.getAddr(adr, 0);
        pck.getSkip((i + 7) / 8);
        return new addrPrefix<T>(adr, i);
    }

    /**
     * write one ipvX unicast
     *
     * @param <T> address kind
     * @param pfx address to write
     * @param pck packet to use
     */
    public static <T extends addrType> void writeIpvXuni(addrPrefix<T> pfx, packHolder pck) {
        pck.putByte(0, pfx.maskLen);
        pck.putSkip(1);
        pck.putAddr(0, pfx.network);
        pck.putSkip((pfx.maskLen + 7) / 8);
    }

    /**
     * read one vpnvX unicast
     *
     * @param <T> address kind
     * @param adr dummy address
     * @param oneLab just one label
     * @param ntry route entry
     * @param pck packet to use
     * @return address read
     */
    public static <T extends addrType> addrPrefix<T> readVpnvXuni(T adr, boolean oneLab, tabRouteEntry<addrIP> ntry, packHolder pck) {
        int i = pck.getByte(0);
        pck.getSkip(1);
        ntry.best.labelRem = new ArrayList<Integer>();
        for (;;) {
            if (i < 24) {
                break;
            }
            int o = pck.msbGetD(0) >>> 8;
            pck.getSkip(3);
            i -= 24;
            ntry.best.labelRem.add(o >>> 4);
            if (oneLab) {
                break;
            }
            if ((o & 1) != 0) {
                break;
            }
        }
        ntry.rouDst = pck.msbGetQ(0);
        pck.getSkip(8);
        i -= 64;
        pck.getAddr(adr, 0);
        pck.getSkip((i + 7) / 8);
        return new addrPrefix<T>(adr, i);
    }

    /**
     * write one vpnvX unicast
     *
     * @param <T> address kind
     * @param pfx address to write
     * @param oneLab just one label
     * @param ntry route entry
     * @param pck packet to use
     */
    public static <T extends addrType> void writeVpnvXuni(addrPrefix<T> pfx, boolean oneLab, tabRouteEntry<addrIP> ntry, packHolder pck) {
        int p = 1;
        int i = 0;
        for (int q = 0; q < ntry.best.labelRem.size(); q++) {
            pck.msbPutD(p, ntry.best.labelRem.get(q) << 12);
            p += 3;
            i += 24;
            if (oneLab) {
                break;
            }
        }
        pck.putBit(p - 1, 0, true);
        pck.msbPutQ(p, ntry.rouDst);
        p += 8;
        i += 64;
        pck.putByte(0, i + pfx.maskLen);
        pck.putSkip(p);
        pck.putAddr(0, pfx.network);
        pck.putSkip((pfx.maskLen + 7) / 8);
    }

    /**
     * read one ipvX labeled
     *
     * @param <T> address kind
     * @param adr dummy address
     * @param oneLab just one label
     * @param ntry route entry
     * @param pck packet to use
     * @return address read
     */
    public static <T extends addrType> addrPrefix<T> readIpvXlab(T adr, boolean oneLab, tabRouteEntry<addrIP> ntry, packHolder pck) {
        int i = pck.getByte(0);
        pck.getSkip(1);
        ntry.best.labelRem = new ArrayList<Integer>();
        for (;;) {
            if (i < 24) {
                break;
            }
            int o = pck.msbGetD(0) >>> 8;
            pck.getSkip(3);
            i -= 24;
            ntry.best.labelRem.add(o >>> 4);
            if (oneLab) {
                break;
            }
            if ((o & 1) != 0) {
                break;
            }
        }
        pck.getAddr(adr, 0);
        pck.getSkip((i + 7) / 8);
        return new addrPrefix<T>(adr, i);
    }

    /**
     * write one ipvX labeled
     *
     * @param <T> address kind
     * @param pfx address to write
     * @param oneLab just one label
     * @param ntry route entry
     * @param pck packet to use
     */
    public static <T extends addrType> void writeIpvXlab(addrPrefix<T> pfx, boolean oneLab, tabRouteEntry<addrIP> ntry, packHolder pck) {
        int p = 1;
        int i = 0;
        for (int q = 0; q < ntry.best.labelRem.size(); q++) {
            pck.msbPutD(p, ntry.best.labelRem.get(q) << 12);
            p += 3;
            i += 24;
            if (oneLab) {
                break;
            }
        }
        pck.putBit(p - 1, 0, true);
        pck.putByte(0, i + pfx.maskLen);
        pck.putSkip(p);
        pck.putAddr(0, pfx.network);
        pck.putSkip((pfx.maskLen + 7) / 8);
    }

    /**
     * read one ipvX colored
     *
     * @param <T> address kind
     * @param adr dummy address
     * @param ntry route entry
     * @param pck packet to use
     * @return address read
     */
    public static <T extends addrType> addrPrefix<T> readIpvXcar(T adr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        int i = pck.getByte(0);
        int p = pck.getByte(1);
        i = pck.getByte(2);
        pck.getSkip(3);
        if (i == 2) {
            i = pck.getByte(0);
            pck.getAddr(adr, 1);
            pck.getSkip(p);
            return new addrPrefix<T>(adr, i);
        }
        if (i != 1) {
            return null;
        }
        int o = pck.getByte(0);
        pck.getAddr(adr, 1);
        ntry.rouDst = pck.msbGetD(p - 4);
        pck.getSkip(p);
        if (pck.getByte(0) != 1) {
            return null;
        }
        i = pck.getByte(1);
        pck.getSkip(2);
        ntry.best.labelRem = new ArrayList<Integer>();
        for (;;) {
            if (i < 3) {
                break;
            }
            p = pck.msbGetD(0) >>> 8;
            pck.getSkip(3);
            i -= 3;
            ntry.best.labelRem.add(p >>> 4);
        }
        return new addrPrefix<T>(adr, o);
    }

    /**
     * write one ipvX colored
     *
     * @param <T> address kind
     * @param pfx address to write
     * @param ntry route entry
     * @param pck packet to use
     */
    public static <T extends addrType> void writeIpvXcar(addrPrefix<T> pfx, tabRouteEntry<addrIP> ntry, packHolder pck) {
        int i = (pfx.maskLen + 7) / 8;
        int o = ntry.best.labelRem.size() * 3;
        pck.putByte(0, i + o + 9); // nlri length
        pck.putByte(1, i + 5); // key length
        pck.putByte(2, 1); // nlri type
        pck.putByte(3, pfx.maskLen); // mask length
        pck.putSkip(4);
        pck.putAddr(0, pfx.network); // mask
        pck.putSkip(i);
        pck.msbPutD(0, (int) ntry.rouDst);
        pck.putSkip(4);
        pck.putByte(0, 1); // tlv type
        pck.putByte(1, o); // tlv length
        pck.putSkip(2);
        o = 0;
        for (i = 0; i < ntry.best.labelRem.size(); i++) {
            pck.msbPutD(o, ntry.best.labelRem.get(i) << 12);
            o += 3;
        }
        pck.putBit(o - 1, 0, true);
        pck.putSkip(o);
    }

    /**
     * read one vpnvX multicast
     *
     * @param <T> address kind
     * @param adr dummy address
     * @param ntry route entry
     * @param pck packet to use
     * @return address read
     */
    public static <T extends addrType> addrPrefix<T> readVpnvXmul(T adr, tabRouteEntry<addrIP> ntry, packHolder pck) {
        int i = pck.getByte(0);
        pck.getSkip(1);
        ntry.rouDst = pck.msbGetQ(0);
        pck.getSkip(8);
        i -= 64;
        pck.getAddr(adr, 0);
        pck.getSkip((i + 7) / 8);
        return new addrPrefix<T>(adr, i);
    }

    /**
     * write one vpnvX multicast
     *
     * @param <T> address kind
     * @param pfx address to write
     * @param ntry route entry
     * @param pck packet to use
     */
    public static <T extends addrType> void writeVpnvXmul(addrPrefix<T> pfx, tabRouteEntry<addrIP> ntry, packHolder pck) {
        pck.msbPutQ(1, ntry.rouDst);
        pck.putByte(0, 64 + pfx.maskLen);
        pck.putSkip(9);
        pck.putAddr(0, pfx.network);
        pck.putSkip((pfx.maskLen + 7) / 8);
    }

    /**
     * read one flowspec
     *
     * @param pck packet to use
     * @return bytes to read
     */
    public static int sizeFlowspec(packHolder pck) {
        int i = pck.getByte(0);
        if (i >= 0xf0) {
            i = pck.msbGetW(0) & 0xfff;
            pck.getSkip(2);
        } else {
            pck.getSkip(1);
        }
        return i;
    }

    /**
     * read one flowspec
     *
     * @param ntry route entry
     * @param pck packet to use
     * @param len length in bytes
     */
    public static void readFlowspec(tabRouteEntry<addrIP> ntry, packHolder pck, int len) {
        byte[] adr = new byte[addrIP.size];
        ntry.prefix = new addrPrefix<addrIP>(new addrIP(), adr.length * 8);
        adr[0] = (byte) len;
        pck.getCopy(adr, 1, 0, 15);
        ntry.prefix.network.fromBuf(adr, 0);
        pck.getCopy(adr, 0, 15, 16);
        ntry.prefix.broadcast.fromBuf(adr, 0);
        pck.getCopy(adr, 0, 31, 16);
        ntry.prefix.wildcard.fromBuf(adr, 0);
        pck.getCopy(adr, 0, 47, 16);
        ntry.prefix.mask.fromBuf(adr, 0);
        pck.getSkip(len);
    }

    /**
     * write one flowspec
     *
     * @param ntry route entry
     * @param pck packet to use
     * @param ofs offset to use
     * @return length in bytes
     */
    public static int writeFlowspec(tabRouteEntry<addrIP> ntry, packHolder pck, int ofs) {
        pck.putCopy(ntry.prefix.network.getBytes(), 1, ofs + 0, 15);
        pck.putCopy(ntry.prefix.broadcast.getBytes(), 0, ofs + 15, 16);
        pck.putCopy(ntry.prefix.wildcard.getBytes(), 0, ofs + 31, 16);
        pck.putCopy(ntry.prefix.mask.getBytes(), 0, ofs + 47, 16);
        return ntry.prefix.network.getBytes()[0] & 0xff;
    }

    /**
     * read address from packet
     *
     * @param safi safi to read
     * @param pck packet to use
     * @return address read, null if nothing
     */
    public static tabRouteEntry<addrIP> readPrefix(int safi, packHolder pck) {
        switch (safi & rtrBgpUtil.afiMask) {
            case rtrBgpUtil.afiIpv4:
                return ipv4uni.readPrefix(true, pck);
            case rtrBgpUtil.afiIpv6:
                return ipv6uni.readPrefix(true, pck);
            default:
                logger.info("unknown safi (" + safi + ") requested");
                return null;
        }
    }

    /**
     * read address from packet
     *
     * @param safi safi to read
     * @param pck packet to use
     * @return address read, null if nothing
     */
    public static addrIP readAddress(int safi, packHolder pck) {
        addrIP ax = new addrIP();
        switch (safi & rtrBgpUtil.afiMask) {
            case rtrBgpUtil.afiIpv4:
                addrIPv4 a4 = new addrIPv4();
                pck.getAddr(a4, 0);
                pck.getSkip(addrIPv4.size);
                ax.fromIPv4addr(a4);
                return ax;
            case rtrBgpUtil.afiIpv6:
                addrIPv6 a6 = new addrIPv6();
                pck.getAddr(a6, 0);
                pck.getSkip(addrIPv6.size);
                ax.fromIPv6addr(a6);
                return ax;
            default:
                logger.info("unknown safi (" + safi + ") requested");
                return null;
        }
    }

    /**
     * write address to packet
     *
     * @param safi safi to write
     * @param pck packet to use
     * @param addr address to write
     */
    public static void writeAddress(int safi, packHolder pck, addrIP addr) {
        switch (safi & rtrBgpUtil.afiMask) {
            case rtrBgpUtil.afiIpv4:
                addrIPv4 a4 = addr.toIPv4();
                pck.putAddr(0, a4);
                pck.putSkip(addrIPv4.size);
                break;
            case rtrBgpUtil.afiIpv6:
                addrIPv6 a6 = addr.toIPv6();
                pck.putAddr(0, a6);
                pck.putSkip(addrIPv6.size);
                break;
            default:
                logger.info("unknown safi (" + safi + ") requested");
                break;
        }
    }

}

class addrSafiIpv4uni implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = addrPrefix.ip4toIP(addrSafi.readIpvXuni(new addrIPv4(), pck));
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        addrPrefix<addrIPv4> a4 = addrPrefix.ip2ip4(ntry.prefix);
        addrSafi.writeIpvXuni(a4, pck);
    }

}

class addrSafiIpv6uni implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = addrPrefix.ip6toIP(addrSafi.readIpvXuni(new addrIPv6(), pck));
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        addrPrefix<addrIPv6> a6 = addrPrefix.ip2ip6(ntry.prefix);
        addrSafi.writeIpvXuni(a6, pck);
    }

}

class addrSafiIpv4lab implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = addrPrefix.ip4toIP(addrSafi.readIpvXlab(new addrIPv4(), oneLab, ntry, pck));
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        addrPrefix<addrIPv4> a4 = addrPrefix.ip2ip4(ntry.prefix);
        addrSafi.writeIpvXlab(a4, oneLab, ntry, pck);
    }

}

class addrSafiIpv6lab implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = addrPrefix.ip6toIP(addrSafi.readIpvXlab(new addrIPv6(), oneLab, ntry, pck));
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        addrPrefix<addrIPv6> a6 = addrPrefix.ip2ip6(ntry.prefix);
        addrSafi.writeIpvXlab(a6, oneLab, ntry, pck);
    }

}

class addrSafiIpv4car implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = addrPrefix.ip4toIP(addrSafi.readIpvXcar(new addrIPv4(), ntry, pck));
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        addrPrefix<addrIPv4> a4 = addrPrefix.ip2ip4(ntry.prefix);
        addrSafi.writeIpvXcar(a4, ntry, pck);
    }

}

class addrSafiIpv6car implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = addrPrefix.ip6toIP(addrSafi.readIpvXcar(new addrIPv6(), ntry, pck));
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        addrPrefix<addrIPv6> a6 = addrPrefix.ip2ip6(ntry.prefix);
        addrSafi.writeIpvXcar(a6, ntry, pck);
    }

}

class addrSafiVpnv4uni implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = addrPrefix.ip4toIP(addrSafi.readVpnvXuni(new addrIPv4(), oneLab, ntry, pck));
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        addrPrefix<addrIPv4> a4 = addrPrefix.ip2ip4(ntry.prefix);
        addrSafi.writeVpnvXuni(a4, oneLab, ntry, pck);
    }

}

class addrSafiVpnv6uni implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = addrPrefix.ip6toIP(addrSafi.readVpnvXuni(new addrIPv6(), oneLab, ntry, pck));
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        addrPrefix<addrIPv6> a6 = addrPrefix.ip2ip6(ntry.prefix);
        addrSafi.writeVpnvXuni(a6, oneLab, ntry, pck);
    }

}

class addrSafiVpnv4mul implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = addrPrefix.ip4toIP(addrSafi.readVpnvXmul(new addrIPv4(), ntry, pck));
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        addrPrefix<addrIPv4> a4 = addrPrefix.ip2ip4(ntry.prefix);
        addrSafi.writeVpnvXmul(a4, ntry, pck);
    }

}

class addrSafiVpnv6mul implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = addrPrefix.ip6toIP(addrSafi.readVpnvXmul(new addrIPv6(), ntry, pck));
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        addrPrefix<addrIPv6> a6 = addrPrefix.ip2ip6(ntry.prefix);
        addrSafi.writeVpnvXmul(a6, ntry, pck);
    }

}

class addrSafiLnkSt implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        int p = pck.msbGetW(0);
        int i = pck.msbGetW(2);
        pck.getSkip(4);
        ntry.nlri = new byte[i + 2];
        bits.msbPutW(ntry.nlri, 0, p);
        pck.getCopy(ntry.nlri, 2, 0, i);
        pck.getSkip(i);
        addrIP adr = new addrIP();
        adr.fromBuf(cryHashMd5.compute(new cryHashMd5(), ntry.nlri), 0);
        ntry.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        int i = ntry.nlri.length - 2;
        pck.putCopy(ntry.nlri, 0, 0, 2);
        pck.msbPutW(2, i);
        pck.putSkip(4);
        pck.putCopy(ntry.nlri, 2, 0, i);
        pck.putSkip(i);
    }

}

class addrSafiSdwan implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        if (pck.msbGetW(0) != 1) {
            pck.getSkip(1);
            return null;
        }
        ntry.prefix = new addrPrefix<addrIP>(new addrIP(), addrIP.size * 8);
        pck.getCopy(ntry.prefix.broadcast.getBytes(), 0, 4, 8);
        int i = pck.msbGetW(2);
        pck.getSkip(12);
        if (i > (8 * 12)) {
            ntry.prefix.network = addrSafi.readAddress(rtrBgpUtil.afiIpv6, pck);
        } else {
            ntry.prefix.network = addrSafi.readAddress(rtrBgpUtil.afiIpv4, pck);
        }
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        pck.msbPutW(0, 1);
        pck.putCopy(ntry.prefix.broadcast.getBytes(), 0, 4, 8);
        if (ntry.prefix.network.isIPv4()) {
            pck.msbPutW(2, 8 * 12);
            pck.putAddr(12, ntry.prefix.network.toIPv4());
            pck.putSkip(16);
        } else {
            pck.msbPutW(2, 8 * 24);
            pck.putAddr(12, ntry.prefix.network.toIPv6());
            pck.putSkip(28);
        }
    }

}

class addrSafiMup implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        if (pck.getByte(0) != 1) {
            pck.getSkip(1);
            return null;
        }
        ntry.prefix = new addrPrefix<addrIP>(new addrIP(), addrIP.size * 8);
        int p = pck.msbGetW(1);
        int i = pck.getByte(3);
        ntry.rouDst = pck.msbGetQ(4);
        i -= 8;
        pck.getSkip(12);
        pck.putByte(0, p);
        pck.putSkip(1);
        pck.merge2beg();
        i++;
        addrSafi.readFlowspec(ntry, pck, i);
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        pck.putByte(0, 1); // arch
        int i = addrSafi.writeFlowspec(ntry, pck, 11);
        int o = pck.headByte(11);
        i--;
        pck.msbPutQ(4, ntry.rouDst);
        i += 8;
        pck.msbPutW(1, o); // type
        pck.putByte(3, i); // size
        pck.putSkip(4 + i);
    }

}

class addrSafiEvpn implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = new addrPrefix<addrIP>(new addrIP(), addrIP.size * 8);
        int typ = pck.getByte(0);
        int len = pck.getByte(1);
        pck.getSkip(2);
        len = pck.dataSize() - len;
        ntry.rouDst = pck.msbGetQ(0);
        pck.getSkip(8);
        byte[] buf;
        switch (typ) {
            case 1: // ethernet auto discovery
                buf = new byte[addrIP.size];
                pck.getCopy(buf, 2, 0, 14); // esi + eti
                ntry.prefix.network.fromBuf(buf, 0);
                ntry.best.evpnLab = pck.msbGetD(14) >>> 8;
                break;
            case 2: // mac ip advertisement
                buf = new byte[addrIP.size];
                pck.getCopy(buf, 0, 0, 10); // esi
                ntry.prefix.wildcard.fromBuf(buf, 0);
                buf = new byte[addrIP.size];
                pck.getCopy(buf, 2, 10, 4); // eti
                pck.getSkip(14);
                addrMac mac = new addrMac();
                if (pck.getByte(0) != mac.maxBits()) {
                    return null;
                }
                pck.getSkip(1);
                pck.getAddr(mac, 0);
                pck.getSkip(addrMac.size);
                mac.toBuffer(buf, 10);
                ntry.prefix.network.fromBuf(buf, 0);
                int i = pck.getByte(0);
                pck.getSkip(1);
                switch (i) {
                    case 0:
                        ntry.prefix.broadcast = new addrIP();
                        break;
                    case addrIPv4.size * 8:
                        ntry.prefix.broadcast = addrSafi.readAddress(rtrBgpUtil.afiIpv4, pck);
                        break;
                    case addrIPv6.size * 8:
                        ntry.prefix.broadcast = addrSafi.readAddress(rtrBgpUtil.afiIpv6, pck);
                        break;
                    default:
                        return null;
                }
                ntry.best.evpnLab = pck.msbGetD(0) >>> 8;
                break;
            case 3: // inclusive multicast ethernet tag
                buf = new byte[addrIP.size];
                pck.getCopy(buf, 2, 0, 4); // eti
                pck.getSkip(4);
                ntry.prefix.network.fromBuf(buf, 0);
                i = pck.getByte(0);
                pck.getSkip(1);
                switch (i) {
                    case addrIPv4.size * 8:
                        ntry.prefix.broadcast = addrSafi.readAddress(rtrBgpUtil.afiIpv4, pck);
                        break;
                    case addrIPv6.size * 8:
                        ntry.prefix.broadcast = addrSafi.readAddress(rtrBgpUtil.afiIpv6, pck);
                        break;
                    default:
                        return null;
                }
                break;
            case 4: // ethernet segment
                buf = new byte[addrIP.size];
                pck.getCopy(buf, 2, 0, 10); // esi
                pck.getSkip(10);
                ntry.prefix.network.fromBuf(buf, 0);
                i = pck.getByte(0);
                pck.getSkip(1);
                switch (i) {
                    case addrIPv4.size * 8:
                        ntry.prefix.broadcast = addrSafi.readAddress(rtrBgpUtil.afiIpv4, pck);
                        break;
                    case addrIPv6.size * 8:
                        ntry.prefix.broadcast = addrSafi.readAddress(rtrBgpUtil.afiIpv6, pck);
                        break;
                    default:
                        return null;
                }
                break;
            case 5: // ip prefix
                buf = new byte[addrIP.size];
                pck.getCopy(buf, 0, 0, 10); // esi
                ntry.prefix.wildcard.fromBuf(buf, 0);
                buf = new byte[addrIP.size];
                pck.getCopy(buf, 2, 10, 4); // eti
                pck.getSkip(14);
                ntry.prefix.network.fromBuf(buf, 0);
                i = pck.getByte(0);
                pck.getSkip(1);
                switch (i) {
                    case addrIPv4.size * 8:
                        ntry.prefix.broadcast = addrSafi.readAddress(rtrBgpUtil.afiIpv4, pck);
                        ntry.prefix.mask = addrSafi.readAddress(rtrBgpUtil.afiIpv4, pck);
                        break;
                    case addrIPv6.size * 8:
                        ntry.prefix.broadcast = addrSafi.readAddress(rtrBgpUtil.afiIpv6, pck);
                        ntry.prefix.mask = addrSafi.readAddress(rtrBgpUtil.afiIpv6, pck);
                        break;
                    default:
                        return null;
                }
                ntry.best.evpnLab = pck.msbGetD(0) >>> 8;
                break;
            default:
                return null;
        }
        ntry.prefix.network.getBytes()[0] = (byte) typ;
        pck.setBytesLeft(len);
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        int typ = ntry.prefix.network.getBytes()[0];
        pck.msbPutQ(2, ntry.rouDst);
        int pos = 10;
        byte[] buf = new byte[addrIP.size];
        switch (typ) {
            case 1: // ethernet auto discovery
                ntry.prefix.network.toBuffer(buf, 0);
                pck.putCopy(buf, 2, pos, 14); // esi + eti
                pos += 14;
                pck.msbPutD(pos, ntry.best.evpnLab << 8);
                pos += 3;
                break;
            case 2: // mac ip advertisement
                ntry.prefix.wildcard.toBuffer(buf, 0);
                pck.putCopy(buf, 0, pos, 10); // esi
                pos += 10;
                ntry.prefix.network.toBuffer(buf, 0);
                pck.putCopy(buf, 2, pos, 4); // eti
                pos += 4;
                pck.putByte(pos, addrMac.size * 8); // size;
                pos++;
                pck.putCopy(buf, 10, pos, addrMac.size); // eti
                pos += addrMac.size;
                addrType adr;
                if (ntry.prefix.broadcast.isIPv4()) {
                    adr = ntry.prefix.broadcast.toIPv4();
                } else {
                    adr = ntry.prefix.broadcast.toIPv6();
                }
                if (ntry.prefix.broadcast.isEmpty()) {
                    adr = new addrEmpty();
                }
                pck.putByte(pos, adr.maxBits());
                pos++;
                pck.putAddr(pos, adr);
                pos += adr.getSize();
                pck.msbPutD(pos, ntry.best.evpnLab << 8);
                pos += 3;
                break;
            case 3: // inclusive multicast ethernet tag
                ntry.prefix.network.toBuffer(buf, 0);
                pck.putCopy(buf, 2, pos, 4); // eti
                pos += 4;
                if (ntry.prefix.broadcast.isIPv4()) {
                    adr = ntry.prefix.broadcast.toIPv4();
                } else {
                    adr = ntry.prefix.broadcast.toIPv6();
                }
                pck.putByte(pos, adr.maxBits());
                pos++;
                pck.putAddr(pos, adr);
                pos += adr.getSize();
                break;
            case 4: // ethernet segment
                ntry.prefix.network.toBuffer(buf, 0);
                pck.putCopy(buf, 2, pos, 10); // esi
                pos += 10;
                if (ntry.prefix.broadcast.isIPv4()) {
                    adr = ntry.prefix.broadcast.toIPv4();
                } else {
                    adr = ntry.prefix.broadcast.toIPv6();
                }
                pck.putByte(pos, adr.maxBits());
                pos++;
                pck.putAddr(pos, adr);
                pos += adr.getSize();
                break;
            case 5: // ip prefix
                ntry.prefix.wildcard.toBuffer(buf, 0);
                pck.putCopy(buf, 0, pos, 10); // esi
                pos += 10;
                ntry.prefix.network.toBuffer(buf, 0);
                pck.putCopy(buf, 2, pos, 4); // eti
                pos += 4;
                if (ntry.prefix.broadcast.isIPv4()) {
                    adr = ntry.prefix.broadcast.toIPv4();
                } else {
                    adr = ntry.prefix.broadcast.toIPv6();
                }
                pck.putByte(pos, adr.maxBits());
                pos++;
                pck.putAddr(pos, adr);
                pos += adr.getSize();
                if (ntry.prefix.broadcast.isIPv4()) {
                    adr = ntry.prefix.mask.toIPv4();
                } else {
                    adr = ntry.prefix.mask.toIPv6();
                }
                pck.putAddr(pos, adr);
                pos += adr.getSize();
                pck.msbPutD(pos, ntry.best.evpnLab << 8);
                pos += 3;
                break;
            default:
                return;
        }
        pck.putByte(0, typ);
        pck.putByte(1, pos - 2);
        pck.putSkip(pos);
    }

}

class addrSafiNsh implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = new addrPrefix<addrIP>(new addrIP(), addrIP.size * 8);
        int p = pck.msbGetW(0);
        int i = pck.msbGetW(2);
        pck.getSkip(4);
        ntry.prefix.network.getBytes()[0] = (byte) p;
        ntry.prefix.network.getBytes()[1] = (byte) i;
        pck.getCopy(ntry.prefix.network.getBytes(), 2, 0, i);
        pck.getSkip(i);
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        int o = ntry.prefix.network.getBytes()[0];
        int i = ntry.prefix.network.getBytes()[1];
        pck.msbPutW(0, o); // type
        pck.msbPutW(2, i); // size
        pck.putSkip(4);
        pck.putCopy(ntry.prefix.network.getBytes(), 2, 0, i);
        pck.putSkip(i);
    }

}

class addrSafiRpd implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = new addrPrefix<addrIP>(new addrIP(), addrIP.size * 8);
        int i = pck.getByte(0);
        ntry.prefix.wildcard.getBytes()[0] = (byte) pck.getByte(1);
        ntry.rouDst = pck.msbGetD(2);
        pck.getSkip(6);
        if (i > 9) {
            ntry.prefix.network = addrSafi.readAddress(rtrBgpUtil.afiIpv6, pck);
        } else {
            ntry.prefix.network = addrSafi.readAddress(rtrBgpUtil.afiIpv4, pck);
        }
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        int i;
        pck.putByte(1, ntry.prefix.wildcard.getBytes()[0]);
        pck.msbPutD(2, (int) ntry.rouDst);
        if (ntry.prefix.network.isIPv4()) {
            pck.putAddr(6, ntry.prefix.network.toIPv4());
            i = addrIPv4.size;
        } else {
            pck.putAddr(6, ntry.prefix.network.toIPv6());
            i = addrIPv6.size;
        }
        pck.putByte(0, i + 5);
        pck.putSkip(i + 6);
    }

}

class addrSafiVpls implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        int i = pck.msbGetW(0) * 8;
        pck.getSkip(2);
        ntry.rouDst = pck.msbGetQ(0);
        pck.getSkip(8);
        i -= 64;
        int o = (i + 7) / 8;
        if (o == 9) {
            byte[] adr = new byte[addrIP.size];
            ntry.prefix = new addrPrefix<addrIP>(new addrIP(), adr.length * 8);
            pck.getCopy(adr, 0, 0, 4);
            ntry.prefix.network.fromBuf(adr, 0);
            adr[0] = 5;
            pck.getCopy(adr, 1, 4, 5);
            ntry.prefix.wildcard.fromBuf(adr, 0);
            pck.getSkip(o);
            return ntry;
        }
        if (o >= addrIPv6.size) {
            addrIPv6 a6 = new addrIPv6();
            pck.getAddr(a6, 0);
            ntry.prefix = addrPrefix.ip6toIP(new addrPrefix<addrIPv6>(a6, i));
        } else {
            addrIPv4 a4 = new addrIPv4();
            pck.getAddr(a4, 0);
            ntry.prefix = addrPrefix.ip4toIP(new addrPrefix<addrIPv4>(a4, i));
        }
        pck.getSkip(o);
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        pck.msbPutQ(2, ntry.rouDst);
        int i;
        if (ntry.prefix.wildcard.getBytes()[0] == 5) {
            pck.putCopy(ntry.prefix.network.getBytes(), 0, 10, 4);
            pck.putCopy(ntry.prefix.wildcard.getBytes(), 1, 14, 5);
            i = 17;
            pck.msbPutW(0, i);
            pck.putSkip(i + 2);
            return;
        }
        if (ntry.prefix.network.isIPv4()) {
            addrPrefix<addrIPv4> a4 = a4 = addrPrefix.ip2ip4(ntry.prefix);
            i = a4.maskLen;
            pck.putAddr(10, a4.network);
        } else {
            addrPrefix<addrIPv6> a6 = addrPrefix.ip2ip6(ntry.prefix);
            i = a6.maskLen;
            pck.putAddr(10, a6.network);
        }
        i = (i + 7) / 8;
        i += 8;
        pck.msbPutW(0, i);
        pck.putSkip(i + 2);
    }

}

class addrSafiMspw implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        int i = pck.getByte(0);
        pck.getSkip(1);
        ntry.rouDst = pck.msbGetQ(0);
        pck.getSkip(8);
        i -= 64;
        i = (i + 7) / 8;
        addrSafi.readFlowspec(ntry, pck, i);
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        pck.msbPutQ(1, ntry.rouDst);
        int i = addrSafi.writeFlowspec(ntry, pck, 9);
        i += 8;
        pck.putByte(0, i * 8);
        pck.putSkip(1 + i);
    }

}

class addrSafiMdt implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        int i = addrSafi.sizeFlowspec(pck);
        ntry.rouDst = pck.msbGetQ(0);
        pck.getSkip(8);
        i -= 64;
        i = (i + 7) / 8;
        int o = i / 2;
        byte[] adr = new byte[addrIP.size];
        ntry.prefix = new addrPrefix<addrIP>(new addrIP(), adr.length * 8);
        pck.getCopy(adr, 0, 0, o);
        ntry.prefix.network.fromBuf(adr, 0);
        pck.getCopy(adr, 0, o, o);
        ntry.prefix.broadcast.fromBuf(adr, 0);
        pck.getSkip(i);
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        pck.putByte(0, 128); // length
        pck.msbPutQ(1, ntry.rouDst);
        pck.putAddr(9, ntry.prefix.network);
        pck.putAddr(13, ntry.prefix.broadcast);
        pck.putSkip(17);
    }

}

class addrSafiRtf implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        int i = pck.getByte(0);
        pck.getSkip(1);
        int o = (i + 7) / 8;
        addrIP adr = new addrIP();
        pck.getAddr(adr, 0);
        ntry.prefix = new addrPrefix<addrIP>(adr, i);
        pck.getSkip(o);
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        pck.putByte(0, ntry.prefix.maskLen);
        pck.putSkip(1);
        pck.putAddr(0, ntry.prefix.network);
        pck.putSkip((ntry.prefix.maskLen + 7) / 8);
    }

}

class addrSafiFlowspec implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        int i = addrSafi.sizeFlowspec(pck);
        addrSafi.readFlowspec(ntry, pck, i);
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        int i = addrSafi.writeFlowspec(ntry, pck, 1);
        pck.putByte(0, i);
        pck.putSkip(1 + i);
    }

}

class addrSafiVpnFlow implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        int i = addrSafi.sizeFlowspec(pck);
        ntry.rouDst = pck.msbGetQ(0);
        pck.getSkip(8);
        i -= 8;
        addrSafi.readFlowspec(ntry, pck, i);
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        pck.msbPutQ(1, ntry.rouDst);
        int i = addrSafi.writeFlowspec(ntry, pck, 9);
        i += 8;
        pck.putByte(0, i);
        pck.putSkip(1 + i);
    }

}

class addrSafiMvpn implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        int p = pck.getByte(0);
        int i = pck.getByte(1);
        pck.getSkip(2);
        ntry.rouDst = pck.msbGetQ(0);
        pck.getSkip(8);
        i -= 8;
        pck.putByte(0, p);
        pck.putSkip(1);
        pck.merge2beg();
        i++;
        addrSafi.readFlowspec(ntry, pck, i);
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        int i = addrSafi.writeFlowspec(ntry, pck, 9);
        int o = pck.headByte(9);
        i--;
        pck.msbPutQ(2, ntry.rouDst);
        i += 8;
        pck.putByte(0, o);
        pck.putByte(1, i);
        pck.putSkip(2 + i);
    }

}
