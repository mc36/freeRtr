package rtr;

import addr.addrIP;
import pack.packHolder;
import tab.tabLabelBier;
import tab.tabLabelNtry;
import tab.tabRouteEntry;
import util.bits;
import util.typLenVal;

/**
 * ospf bier
 *
 * @author matecsaba
 */
public class rtrOspfBr {

    /**
     * generate br prefix
     *
     * @param lab labels
     * @param bsl bitstring length
     * @param idx index
     * @return bytes generated
     */
    protected static byte[] putPref(tabLabelNtry[] lab, int bsl, int idx) {
        if (lab == null) {
            return new byte[0];
        }
        packHolder pck = new packHolder(true, true);
        typLenVal tlv = rtrOspfTe.getTlvHandler();
        tlv.valDat[0] = 0; // subdomain
        tlv.valDat[1] = 0; // mtid
        bits.msbPutW(tlv.valDat, 2, idx); // bfr id
        tlv.valDat[4] = 0; // algorithm
        tlv.valSiz = 8;
        tlv.valTyp = 9;
        tlv.putThis(pck);
        tlv = rtrOspfTe.getTlvHandler();
        bits.msbPutD(tlv.valDat, 0, lab[0].getValue()); // label
        tlv.valDat[0] = (byte) lab.length; // length
        tlv.valDat[4] = (byte) (tabLabelBier.num2bsl(bsl) << 4); // bsl
        tlv.valSiz = 8;
        tlv.valTyp = 10;
        tlv.putThis(pck);
        pck.merge2beg();
        return pck.getCopy();
    }

    /**
     * read br prefix
     *
     * @param tlv data
     * @param prf prefix
     */
    protected static void getPref(typLenVal tlv, tabRouteEntry<addrIP> prf) {
        switch (tlv.valTyp) {
            case 9: // bier info
                prf.bierIdx = bits.msbGetW(tlv.valDat, 2); // brf id
                break;
            case 10: // bier mpls
                prf.bierBeg = bits.msbGetD(tlv.valDat, 0) & 0xfffff; // label
                prf.bierHdr = (tlv.valDat[4] >>> 4) & 0xf; // bsl
                break;
        }
    }

}
