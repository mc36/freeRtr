package org.freertr.pack;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrSafi;
import org.freertr.rtr.rtrBgpUtil;
import org.freertr.util.bits;

/**
 * (multi)point to multipoint (rfc6388) fec
 *
 * @author matecsaba
 */
public class packLdpMp implements Comparable<packLdpMp> {

    /**
     * create instance
     */
    public packLdpMp() {
    }

    /**
     * label value
     */
    public int label;

    /**
     * fec type, fecT*
     */
    public int typ;

    /**
     * root node address
     */
    public addrIP root;

    /**
     * opaque value
     */
    public byte[] opaque;

    public String toString() {
        return typ + "|" + root + "|" + label + "|" + bits.byteDump(opaque, 0, -1);
    }

    /**
     * get reverse type
     *
     * @param i type
     * @return reverse type
     */
    public static int getReverse(int i) {
        switch (i) {
            case packLdp.fecTmp2mpDn:
                return packLdp.fecTmp2mpUp;
            case packLdp.fecTmp2mpUp:
                return packLdp.fecTmp2mpDn;
            default:
                return -1;
        }
    }

    public int compareTo(packLdpMp o) {
        if (typ < o.typ) {
            return -1;
        }
        if (typ > o.typ) {
            return +1;
        }
        int i = root.compareTo(o.root);
        if (i != 0) {
            return i;
        }
        if (opaque.length < o.opaque.length) {
            return -1;
        }
        if (opaque.length > o.opaque.length) {
            return +1;
        }
        return bits.byteComp(opaque, 0, o.opaque, 0, opaque.length);
    }

    /**
     * parse fec element
     *
     * @param pck packet to use
     */
    public void parseFEC(packHolder pck) {
        int i = pck.msbGetW(0) << 16; // afi
        pck.getSkip(3);
        root = addrSafi.readAddress(i, pck);
        i = pck.msbGetW(0); // opaque length
        pck.getSkip(2);
        if (i > pck.dataSize()) {
            i = pck.dataSize();
        }
        opaque = new byte[i];
        pck.getCopy(opaque, 0, 0, opaque.length);
        pck.getSkip(opaque.length);
    }

    /**
     * create fec element
     *
     * @param pck packet to use
     */
    public void createFEC(packHolder pck) {
        if (root.isIPv4()) {
            addrIPv4 a4 = root.toIPv4();
            pck.msbPutW(0, rtrBgpUtil.afiIpv4 >>> 16);
            pck.putByte(2, addrIPv4.size);
            pck.putSkip(3);
            pck.putAddr(0, a4);
            pck.putSkip(addrIPv4.size);
        } else {
            addrIPv6 a6 = root.toIPv6();
            pck.msbPutW(0, rtrBgpUtil.afiIpv6 >>> 16);
            pck.putByte(2, addrIPv6.size);
            pck.putSkip(3);
            pck.putAddr(0, a6);
            pck.putSkip(addrIPv6.size);
        }
        pck.msbPutW(0, opaque.length);
        pck.putSkip(2);
        pck.putCopy(opaque, 0, 0, opaque.length);
        pck.putSkip(opaque.length);
    }

}
