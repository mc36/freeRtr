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
     * read address from packet
     *
     * @param safi safi to read
     * @param oneLab just one label
     * @param pck packet to use
     * @return address read, null if nothing
     */
    public static tabRouteEntry<addrIP> readPrefix(int safi, boolean oneLab, packHolder pck) {
        switch (safi & rtrBgpUtil.afiMask) {
            case rtrBgpUtil.afiIpv4:
                return ipv4uni.readPrefix(oneLab, pck);
            case rtrBgpUtil.afiIpv6:
                return ipv6uni.readPrefix(oneLab, pck);
            default:
                logger.info("unknown safi (" + safi + ") requested");
                return null;
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
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

}

class addrSafiIpv6uni implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = addrPrefix.ip6toIP(addrSafi.readIpvXuni(new addrIPv6(), pck));
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

}

class addrSafiIpv4lab implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = addrPrefix.ip4toIP(addrSafi.readIpvXlab(new addrIPv4(), oneLab, ntry, pck));
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

}

class addrSafiIpv6lab implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = addrPrefix.ip6toIP(addrSafi.readIpvXlab(new addrIPv6(), oneLab, ntry, pck));
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

}

class addrSafiIpv4car implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = addrPrefix.ip4toIP(addrSafi.readIpvXcar(new addrIPv4(), ntry, pck));
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

}

class addrSafiIpv6car implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = addrPrefix.ip6toIP(addrSafi.readIpvXcar(new addrIPv6(), ntry, pck));
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

}

class addrSafiVpnv4uni implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = addrPrefix.ip4toIP(addrSafi.readVpnvXuni(new addrIPv4(), oneLab, ntry, pck));
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

}

class addrSafiVpnv6uni implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = addrPrefix.ip6toIP(addrSafi.readVpnvXuni(new addrIPv6(), oneLab, ntry, pck));
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

}

class addrSafiVpnv4mul implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = addrPrefix.ip4toIP(addrSafi.readVpnvXmul(new addrIPv4(), ntry, pck));
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

}

class addrSafiVpnv6mul implements addrSafi {

    public tabRouteEntry<addrIP> readPrefix(boolean oneLab, packHolder pck) {
        tabRouteEntry<addrIP> ntry = new tabRouteEntry<addrIP>();
        ntry.prefix = addrPrefix.ip6toIP(addrSafi.readVpnvXmul(new addrIPv6(), ntry, pck));
        return ntry;
    }

    public void writePrefix(boolean oneLab, packHolder pck, tabRouteEntry<addrIP> ntry) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
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
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

}
