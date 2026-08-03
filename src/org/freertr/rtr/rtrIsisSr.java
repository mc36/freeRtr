package org.freertr.rtr;

import java.util.ArrayList;
import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv6;
import org.freertr.cfg.cfgIfc;
import org.freertr.ip.ipCor4;
import org.freertr.pack.packHolder;
import org.freertr.tab.tabRouteEntry;
import org.freertr.util.bits;
import org.freertr.enc.encTlv;

/**
 * isis segment routing
 *
 * @author matecsaba
 */
public class rtrIsisSr {

    private rtrIsisSr() {
    }

    /**
     * adjacency sid
     */
    public final static int typAdjSid = 31;

    /**
     * lan adjacency sid
     */
    public final static int typLanAdjSid = 32;

    /**
     * bundle adjacency sid
     */
    public final static int typBunAdjSid = 41;

    /**
     * bundle lan adjacency sid
     */
    public final static int typBunLanAdjSid = 42;

    /**
     * prefix segment id
     */
    public final static int typPrfSeg = 3;

    /**
     * segment routing capability
     */
    public final static int typSrCapa = 2;

    /**
     * segment routing algorithms
     */
    public final static int typSrAlgo = 19;

    /**
     * end segment id
     */
    public final static int typEndSid = 5;

    /**
     * segment routing v6 capability
     */
    public final static int typSrv6capa = 25;

    /**
     * create sr base
     *
     * @param lower lower layer to use
     * @return tlv generated
     */
    protected static encTlv putBase(rtrIsis lower) {
        encTlv tlv = rtrIsis.getTlv();
        lower.traffEngID.toIPv4().toBuffer(tlv.valDat, 0); // addr
        tlv.valDat[4] = 0; // flags
        tlv.valDat[5] = typSrCapa; // type
        tlv.valDat[6] = 9; // length
        if (lower.fwdCore.ipVersion == ipCor4.protocolVersion) {
            tlv.valDat[7] = (byte) 0x80; // flags
        } else {
            tlv.valDat[7] = (byte) 0x40; // flags
        }
        if (lower.other.enabled) {
            tlv.valDat[7] = (byte) 0xc0; // flags
        }
        bits.msbPutD(tlv.valDat, 8, lower.segrouMax << 8); // range
        tlv.valDat[11] = 1; // type
        tlv.valDat[12] = 3; // length
        bits.msbPutD(tlv.valDat, 13, lower.segrouLab[0].label << 8); // base
        tlv.valDat[16] = typSrAlgo;
        tlv.valDat[17] = (byte) (1 + lower.algos.size()); // length
        tlv.valDat[18] = 0; // algorithm
        for (int i = 0; i < lower.algos.size(); i++) {
            rtrAlgo alg = lower.algos.get(i);
            if (alg == null) {
                continue;
            }
            tlv.valDat[19 + i] = (byte) alg.num;
        }
        tlv.valTyp = rtrIsisLsp.tlvRouterCapa;
        tlv.valSiz = 19 + lower.algos.size();
        return tlv;
    }

    /**
     * get sr base
     *
     * @param tlv data
     * @return base, -1 if not found
     */
    protected static int getBase(encTlv tlv) {
        if (tlv.valTyp != typSrCapa) { // type
            return -1;
        }
        if (tlv.valDat[4] != 1) { // type
            return -1;
        }
        if (tlv.valDat[5] != 3) { // length
            return -1;
        }
        return bits.msbGetD(tlv.valDat, 6) >>> 8; // base
    }

    /**
     * get algorithms
     *
     * @param tlv tlv
     * @return result, empty if none
     */
    protected static List<Integer> getAlgos(encTlv tlv) {
        List<Integer> res = new ArrayList<Integer>();
        if (tlv.valTyp != typSrAlgo) {
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
     * @param red redistribute
     * @param nod node
     * @return bytes generated
     */
    protected static byte[] putPref(int idx, boolean pop, boolean red, boolean nod) {
        packHolder pck = new packHolder(true, true);
        encTlv tlv = rtrIsis.getTlv();
        tlv.valDat[0] = 0;
        if (!pop) {
            tlv.valDat[0] |= 0x20; // no-php
        }
        if (red) {
            tlv.valDat[0] |= (byte) 0x80; // redistributed
        }
        if (nod) {
            tlv.valDat[0] |= 0x40; // node
        }
        tlv.valDat[1] = 0; // algorithm
        bits.msbPutD(tlv.valDat, 2, idx); // index
        tlv.valTyp = typPrfSeg;
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
    protected static void getPref(encTlv tlv, tabRouteEntry<addrIP> prf) {
        if (tlv.valTyp != typPrfSeg) {
            return;
        }
        if ((tlv.valDat[0] & 0x04) != 0) { // local
            return;
        }
        if ((tlv.valDat[0] & 0x08) != 0) { // value
            return;
        }
        if ((tlv.valDat[0] & 0x20) == 0) { // no-php
            prf.best.rouSrc |= 16;
        }
        if ((tlv.valDat[0] & 0x10) != 0) { // expnull
            prf.best.rouSrc |= 16;
        }
        prf.best.segrouIdx = bits.msbGetD(tlv.valDat, 2);
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
        encTlv tlv = rtrIsis.getTlv();
        tlv.valDat[0] = 0x30; // local, value
        if (!ip4) {
            tlv.valDat[0] |= (byte) 0x80; // address family
        }
        tlv.valDat[1] = 0; // weight
        bits.msbPutD(tlv.valDat, 2, lab << 8); // label
        tlv.valTyp = typAdjSid;
        tlv.valSiz = 5;
        tlv.putThis(pck);
        pck.merge2beg();
        return pck.getCopy();
    }

    /**
     * generate srv6 base
     *
     * @param lower lower layer to use
     * @return tlv generated
     */
    protected static encTlv srv6base(rtrIsis lower) {
        encTlv tlv = rtrIsis.getTlv();
        lower.traffEngID.toIPv4().toBuffer(tlv.valDat, 0); // addr
        tlv.valDat[4] = 0; // flags
        tlv.valDat[5] = typSrv6capa; // subtlv type
        tlv.valDat[6] = 2; // subtlv length
        bits.msbPutW(tlv.valDat, 7, 0); // flags
        tlv.valDat[9] = typSrAlgo;
        tlv.valDat[10] = (byte) (1 + lower.algos.size()); // length
        tlv.valDat[11] = 0; // algorithm
        for (int i = 0; i < lower.algos.size(); i++) {
            rtrAlgo alg = lower.algos.get(i);
            if (alg == null) {
                continue;
            }
            tlv.valDat[12 + i] = (byte) alg.num;
        }
        tlv.valTyp = rtrIsisLsp.tlvRouterCapa;
        tlv.valSiz = 12 + lower.algos.size();
        return tlv;
    }

    /**
     * generate srv6 locator
     *
     * @param ifc interface
     * @param met metric
     * @return tlv generated
     */
    protected static encTlv srv6loc(cfgIfc ifc, int met) {
        if (ifc == null) {
            return null;
        }
        if (ifc.addr6 == null) {
            return null;
        }
        encTlv tlv = rtrIsis.getTlv();
        tlv.valTyp = rtrIsisLsp.tlvSegRoutV6;
        bits.msbPutW(tlv.valDat, 0, 2); // mtid
        bits.msbPutD(tlv.valDat, 2, met); // metric
        tlv.valDat[6] = 0; // flags
        tlv.valDat[7] = 0; // algo
        int len = ifc.mask6.toNetmask();
        tlv.valDat[8] = (byte) len; // loc len
        ifc.addr6.toBuffer(tlv.valDat, 9); // locator
        tlv.valSiz = (len + 7) / 8;
        tlv.valSiz += 9;
        tlv.valDat[tlv.valSiz] = 22; // subtlv len
        tlv.valSiz += 1;
        tlv.valDat[tlv.valSiz + 0] = 5; // type
        tlv.valDat[tlv.valSiz + 1] = 20; // length
        tlv.valDat[tlv.valSiz + 2] = 0; // flags
        bits.msbPutW(tlv.valDat, tlv.valSiz + 3, 29); // endpoint behavior
        tlv.valSiz += 5;
        ifc.addr6.toBuffer(tlv.valDat, tlv.valSiz); // locator
        tlv.valSiz += addrIPv6.size;
        tlv.valDat[tlv.valSiz] = 0; // subtlv len
        tlv.valSiz += 1;
        return tlv;
    }

}
