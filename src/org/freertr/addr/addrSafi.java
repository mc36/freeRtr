package org.freertr.addr;

import org.freertr.pack.packHolder;
import org.freertr.rtr.rtrBgpUtil;
import org.freertr.tab.tabRouteEntry;
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
        int len = pck.getByte(0);
        pck.getSkip(1);
        pck.getAddr(adr, 0);
        pck.getSkip((len + 7) / 8);
        return new addrPrefix<T>(adr, len);
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
