package rtr;

import addr.addrIP;
import addr.addrIPv4;
import pack.packHolder;
import tab.tabLabelNtry;
import tab.tabRouteEntry;
import util.bits;
import util.typLenVal;

/**
 * ospf segment routing
 *
 * @author matecsaba
 */
public class rtrOspfSr {

    private rtrOspfSr() {
    }

    /**
     * put sr base
     *
     * @param pck packet to update
     * @param lab labels
     */
    public static void putBase(packHolder pck, tabLabelNtry[] lab) {
        if (lab == null) {
            return;
        }
        typLenVal tlv = rtrOspfTe.getTlvHandler();
        tlv.valDat[0] = 0; // algo
        tlv.valTyp = rtrOspfRi.typSrAlgo;
        tlv.valSiz = 1;
        tlv.putThis(pck);
        bits.msbPutD(tlv.valDat, 0, lab.length << 8);
        bits.msbPutW(tlv.valDat, 4, 1); // type
        bits.msbPutW(tlv.valDat, 6, 3); // length
        bits.msbPutD(tlv.valDat, 8, lab[0].getValue() << 8);
        tlv.valTyp = rtrOspfRi.typSrBase;
        tlv.valSiz = 12;
        tlv.putThis(pck);
    }

    /**
     * get sr base
     *
     * @param tlv data
     * @return base, -1 if not found
     */
    protected static int getBase(typLenVal tlv) {
        if (tlv.valTyp != rtrOspfRi.typSrBase) {
            return -1;
        }
        if (bits.msbGetW(tlv.valDat, 4) != 1) { // type
            return -1;
        }
        if (bits.msbGetW(tlv.valDat, 6) != 3) { // length
            return -1;
        }
        return bits.msbGetD(tlv.valDat, 8) >>> 8; // base
    }

    /**
     * generate sr prefix
     *
     * @param idx index
     * @return bytes generated
     */
    protected static byte[] putPref(int idx) {
        packHolder pck = new packHolder(true, true);
        typLenVal tlv = rtrOspfTe.getTlvHandler();
        tlv.valDat[0] = 0x40; // no-php
        tlv.valDat[1] = 0; // reserved
        tlv.valDat[2] = 0; // mtid
        tlv.valDat[3] = 0; // algorithm
        bits.msbPutD(tlv.valDat, 4, idx); // index
        tlv.valTyp = 2;
        tlv.valSiz = 8;
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
        if (tlv.valTyp != 2) {
            return;
        }
        if ((tlv.valDat[0] & 0x04) != 0) { // local
            return;
        }
        if ((tlv.valDat[0] & 0x08) != 0) { // value
            return;
        }
        if ((tlv.valDat[0] & 0x40) == 0) { // no-php
            prf.rouSrc |= 16;
        }
        if ((tlv.valDat[0] & 0x10) != 0) { // expnull
            prf.rouSrc |= 16;
        }
        prf.segrouIdx = bits.msbGetD(tlv.valDat, 4);
    }

    /**
     * generate sr adjacency
     *
     * @param lab label
     * @return bytes generated
     */
    protected static byte[] putAdj(int lab) {
        packHolder pck = new packHolder(true, true);
        typLenVal tlv = rtrOspfTe.getTlvHandler();
        tlv.valDat[0] = 0x60; // local value
        tlv.valDat[1] = 0; // reserved
        tlv.valDat[2] = 0; // mtid
        tlv.valDat[3] = 0; // weight
        bits.msbPutD(tlv.valDat, 4, lab << 8); // label
        tlv.valTyp = 2;
        tlv.valSiz = 7;
        tlv.putThis(pck);
        pck.merge2beg();
        return pck.getCopy();
    }

    /**
     * generate sr remote
     *
     * @param adr address
     * @return bytes generated
     */
    protected static byte[] putRem(addrIPv4 adr) {
        packHolder pck = new packHolder(true, true);
        typLenVal tlv = rtrOspfTe.getTlvHandler();
        adr.toBuffer(tlv.valDat, 0);
        tlv.valTyp = 0x8000;
        tlv.valSiz = 4;
        tlv.putThis(pck);
        pck.merge2beg();
        return pck.getCopy();
    }

}
