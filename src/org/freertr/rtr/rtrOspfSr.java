package org.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.cfg.cfgIfc;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabLabelEntry;
import org.freertr.tab.tabRouteEntry;
import org.freertr.util.bits;
import org.freertr.enc.encTlv;
import org.freertr.tab.tabGen;

/**
 * ospf segment routing
 *
 * @author matecsaba
 */
public class rtrOspfSr {

    private rtrOspfSr() {
    }

    /**
     * sid/label
     */
    public final static int prfSidLab = 1;

    /**
     * prefix sid
     */
    public final static int prfPrfSid = 2;

    /**
     * sid/label
     */
    public final static int lnkSidLab = 1;

    /**
     * adj sid
     */
    public final static int lnkAdjSid = 2;

    /**
     * lan adj sid
     */
    public final static int lnkLanAdjSid = 3;

    /**
     * remote
     */
    public final static int lnkRemote = 0x8000;

    /**
     * put sr base
     *
     * @param pck packet to update
     * @param lab labels
     * @param alg algorithms
     */
    public static void putBase(packHolder pck, tabLabelEntry[] lab, tabGen<rtrAlgo> alg) {
        if (lab == null) {
            return;
        }
        encTlv tlv = rtrOspfTe.getTlvHandler();
        tlv.valDat[0] = 0; // algo
        tlv.valTyp = rtrOspfRi.typSrAlgo;
        tlv.valSiz = 1 + alg.size();
        for (int i = 0; i < alg.size(); i++) {
            rtrAlgo a = alg.get(i);
            if (a == null) {
                continue;
            }
            tlv.valDat[i + 1] = (byte) a.num;
        }
        tlv.putThis(pck);
        bits.msbPutD(tlv.valDat, 0, lab.length << 8);
        bits.msbPutW(tlv.valDat, 4, 1); // type
        bits.msbPutW(tlv.valDat, 6, 3); // length
        bits.msbPutD(tlv.valDat, 8, lab[0].label << 8);
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
    protected static int getBase(encTlv tlv) {
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
     * get sr algo
     *
     * @param tlv data
     * @return algo, empty if not found
     */
    protected static List<Integer> getAlgos(encTlv tlv) {
        List<Integer> res = new ArrayList<Integer>();
        if (tlv.valTyp != rtrOspfRi.typSrAlgo) {
            return res;
        }
        for (int i = 0; i < tlv.valSiz; i++) {
            res.add(bits.getByte(tlv.valDat, i));
        }
        return res;
    }

    /**
     * generate sr prefix
     *
     * @param pop php
     * @param idx index
     * @return bytes generated
     */
    protected static byte[] putPref(int idx, boolean pop) {
        packHolder pck = new packHolder(true, true);
        encTlv tlv = rtrOspfTe.getTlvHandler();
        tlv.valDat[0] = 0;
        if (!pop) {
            tlv.valDat[0] |= 0x40; // no-php
        }
        tlv.valDat[1] = 0; // reserved
        tlv.valDat[2] = 0; // mtid
        tlv.valDat[3] = 0; // algorithm
        bits.msbPutD(tlv.valDat, 4, idx); // index
        tlv.valTyp = prfPrfSid;
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
    protected static void getPref(encTlv tlv, tabRouteEntry<addrIP> prf) {
        if (tlv.valTyp != prfPrfSid) {
            return;
        }
        if ((tlv.valDat[0] & 0x04) != 0) { // local
            return;
        }
        if ((tlv.valDat[0] & 0x08) != 0) { // value
            return;
        }
        if ((tlv.valDat[0] & 0x40) == 0) { // no-php
            prf.best.rouSrc |= 16;
        }
        if ((tlv.valDat[0] & 0x10) != 0) { // expnull
            prf.best.rouSrc |= 16;
        }
        prf.best.segrouIdx = bits.msbGetD(tlv.valDat, 4);
    }

    /**
     * generate sr adjacency
     *
     * @param lab label
     * @return bytes generated
     */
    protected static byte[] putAdj(int lab) {
        packHolder pck = new packHolder(true, true);
        encTlv tlv = rtrOspfTe.getTlvHandler();
        tlv.valDat[0] = 0x60; // local value
        tlv.valDat[1] = 0; // reserved
        tlv.valDat[2] = 0; // mtid
        tlv.valDat[3] = 0; // weight
        bits.msbPutD(tlv.valDat, 4, lab << 8); // label
        tlv.valTyp = lnkAdjSid;
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
        encTlv tlv = rtrOspfTe.getTlvHandler();
        adr.toBuffer(tlv.valDat, 0);
        tlv.valTyp = lnkRemote;
        tlv.valSiz = 4;
        tlv.putThis(pck);
        pck.merge2beg();
        return pck.getCopy();
    }

    /**
     * generate srv6 locator
     *
     * @param ifc interface
     * @param met metric
     * @return lsa generated
     */
    protected static packHolder srv6loc(cfgIfc ifc, int met) {
        if (ifc == null) {
            return null;
        }
        if (ifc.addr6 == null) {
            return null;
        }
        packHolder pck = new packHolder(true, true);
        encTlv tlv = rtrOspfTe.getTlvHandler();
        tlv.valTyp = 1;
        tlv.valDat[0] = 1; // route type
        tlv.valDat[1] = 0; // algo
        int len = ifc.mask6.toNetmask();
        tlv.valDat[2] = (byte) len; // loc len
        tlv.valDat[3] = 0; // flags
        bits.msbPutD(tlv.valDat, 4, met); // metric
        ifc.addr6.toBuffer(tlv.valDat, 8); // locator
        tlv.valSiz = (len + 7) / 8;
        tlv.valSiz += 8;
        bits.msbPutW(tlv.valDat, tlv.valSiz + 0, 1); // type
        bits.msbPutW(tlv.valDat, tlv.valSiz + 2, 20); // length
        tlv.valDat[tlv.valSiz + 4] = 0; // flags
        tlv.valDat[tlv.valSiz + 5] = 0; // reserved
        bits.msbPutW(tlv.valDat, tlv.valSiz + 6, 29); // endpoint behavior
        tlv.valSiz += 8;
        ifc.addr6.toBuffer(tlv.valDat, tlv.valSiz); // locator
        tlv.valSiz += addrIPv6.size;
        tlv.putThis(pck);
        pck.merge2beg();
        return pck;
    }

}
