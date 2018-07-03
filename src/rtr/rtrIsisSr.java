package rtr;

import addr.addrIP;
import ip.ipCor4;
import pack.packHolder;
import tab.tabRouteEntry;
import util.bits;
import util.typLenVal;

/**
 * isis segment routing
 *
 * @author matecsaba
 */
public class rtrIsisSr {

    private rtrIsisSr() {
    }

    /**
     * create sr base
     *
     * @param lower lower layer to use
     * @return tlv generated
     */
    protected static typLenVal putBase(rtrIsis lower) {
        typLenVal tlv = rtrIsis.getTlv();
        lower.traffEngID.toIPv4().toBuffer(tlv.valDat, 0); // addr
        tlv.valDat[4] = 0; // flags
        tlv.valDat[5] = 2; // type
        tlv.valDat[6] = 9; // length
        if (lower.fwdCore.ipVersion == ipCor4.protocolVersion) {
            tlv.valDat[7] = (byte) 0x80; // flags
        } else {
            tlv.valDat[7] = (byte) 0x40; // flags
        }
        bits.msbPutD(tlv.valDat, 8, lower.segrouMax << 8); // range
        tlv.valDat[11] = 1; // type
        tlv.valDat[12] = 3; // length
        bits.msbPutD(tlv.valDat, 13, lower.segrouLab[0].getValue() << 8); // base
        tlv.valTyp = rtrIsisLsp.tlvRouterCapa;
        tlv.valSiz = 16;
        return tlv;
    }

    /**
     * get sr base
     *
     * @param tlv data
     * @return base, -1 if not found
     */
    protected static int getBase(typLenVal tlv) {
        if (tlv.valTyp != rtrIsisLsp.tlvRouterCapa) {
            return -1;
        }
        if (tlv.valDat[5] != 2) { // type
            return -1;
        }
        if (tlv.valDat[11] != 1) { // type
            return -1;
        }
        if (tlv.valDat[12] != 3) { // length
            return -1;
        }
        return bits.msbGetD(tlv.valDat, 13) >>> 8; // base
    }

    /**
     * generate sr prefix
     *
     * @param idx index
     * @param red redistribute
     * @param nod node
     * @return bytes generated
     */
    protected static byte[] putPref(int idx, boolean red, boolean nod) {
        packHolder pck = new packHolder(true, true);
        typLenVal tlv = rtrIsis.getTlv();
        tlv.valDat[0] = 0x20; // no-php
        if (red) {
            tlv.valDat[0] |= 0x80; // redistributed
        }
        if (nod) {
            tlv.valDat[0] |= 0x40; // node
        }
        tlv.valDat[1] = 0; // algorithm
        bits.msbPutD(tlv.valDat, 2, idx); // index
        tlv.valTyp = 3;
        tlv.valSiz = 6;
        tlv.putThis(pck);
        pck.merge2beg();
        return pck.getCopy();
    }

    /**
     * read sr prefix
     *
     * @param tlv data
     * @param prf prefix
     */
    protected static void getPref(typLenVal tlv, tabRouteEntry<addrIP> prf) {
        if (tlv.valTyp != 3) {
            return;
        }
        if ((tlv.valDat[0] & 0x04) != 0) { // local
            return;
        }
        if ((tlv.valDat[0] & 0x08) != 0) { // value
            return;
        }
        if ((tlv.valDat[0] & 0x20) == 0) { // no-php
            prf.rouSrc |= 16;
        }
        if ((tlv.valDat[0] & 0x10) != 0) { // expnull
            prf.rouSrc |= 16;
        }
        prf.segrouIdx = bits.msbGetD(tlv.valDat, 2);
    }

    /**
     * generate sr adjacency
     *
     * @param ip4 ipv4
     * @param lab label
     * @return bytes generated
     */
    protected static byte[] putAdj(boolean ip4, int lab) {
        packHolder pck = new packHolder(true, true);
        typLenVal tlv = rtrIsis.getTlv();
        tlv.valDat[0] = 0x30; // local, value
        if (!ip4) {
            tlv.valDat[0] |= 0x80; // address family
        }
        tlv.valDat[1] = 0; // weight
        bits.msbPutD(tlv.valDat, 2, lab << 8); // label
        tlv.valTyp = 31;
        tlv.valSiz = 5;
        tlv.putThis(pck);
        pck.merge2beg();
        return pck.getCopy();
    }

}
