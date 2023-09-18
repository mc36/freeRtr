package net.freertr.rtr;

import net.freertr.addr.addrIP;
import net.freertr.pack.packHolder;
import net.freertr.tab.tabLabelBier;
import net.freertr.tab.tabLabelEntry;
import net.freertr.tab.tabRouteEntry;
import net.freertr.util.bits;
import net.freertr.enc.encTlv;

/**
 * ospf bier
 *
 * @author matecsaba
 */
public class rtrOspfBr {

    private rtrOspfBr() {
    }

    /**
     * bier info
     */
    public final static int typBierInfo = 9;

    /**
     * bier mpls
     */
    public final static int typBierMpls = 10;

    /**
     * generate br prefix
     *
     * @param lab labels
     * @param bsl bitstring length
     * @param idx index
     * @return bytes generated
     */
    protected static byte[] putPref(tabLabelEntry[] lab, int bsl, int idx) {
        if (lab == null) {
            return new byte[0];
        }
        packHolder pck = new packHolder(true, true);
        encTlv tlv = rtrOspfTe.getTlvHandler();
        tlv.valDat[0] = 0; // subdomain
        tlv.valDat[1] = 0; // mtid
        bits.msbPutW(tlv.valDat, 2, idx); // bfr id
        tlv.valDat[4] = 0; // bier algorithm
        tlv.valDat[5] = 0; // igp algorithm
        bits.msbPutW(tlv.valDat, 8, typBierMpls);
        bits.msbPutW(tlv.valDat, 10, 8);
        bits.msbPutD(tlv.valDat, 12 + 0, lab[0].label); // label
        tlv.valDat[12 + 0] = (byte) lab.length; // length
        tlv.valDat[12 + 4] = (byte) (tabLabelBier.num2bsl(bsl) << 4); // bsl
        tlv.valSiz = 12 + 8;
        tlv.valTyp = typBierInfo;
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
    protected static void getPref(encTlv tlv, tabRouteEntry<addrIP> prf) {
        if (tlv.valTyp != typBierInfo) {
            return;
        }
        prf.best.bierIdx = bits.msbGetW(tlv.valDat, 2); // brf id
        if (bits.msbGetW(tlv.valDat, 8) != typBierMpls) {
            return;
        }
        prf.best.bierBeg = bits.msbGetD(tlv.valDat, 12 + 0) & 0xfffff; // label
        prf.best.bierHdr = (tlv.valDat[12 + 4] >>> 4) & 0xf; // bsl
    }

}
