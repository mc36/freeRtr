package net.freertr.rtr;

import net.freertr.addr.addrIP;
import net.freertr.cfg.cfgIfc;
import net.freertr.ip.ipCor4;
import net.freertr.pack.packHolder;
import net.freertr.tab.tabRouteEntry;
import net.freertr.util.bits;
import net.freertr.util.typLenVal;

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
    public static final int typAdjSid = 31;

    /**
     * lan adjacency sid
     */
    public static final int typLanAdjSid = 32;

    /**
     * bundle adjacency sid
     */
    public static final int typBunAdjSid = 41;

    /**
     * bundle lan adjacency sid
     */
    public static final int typBunLanAdjSid = 42;

    /**
     * prefix segment id
     */
    public static final int typPrfSeg = 3;

    /**
     * segment routing capability
     */
    public static final int typSrCapa = 2;

    /**
     * end segment id
     */
    public static final int typEndSid = 5;

    /**
     * segment routing v6 capability
     */
    public static final int typSrv6capa = 25;

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
        if (tlv.valDat[5] != typSrCapa) { // type
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
     * @param pop php
     * @param idx index
     * @param red redistribute
     * @param nod node
     * @return bytes generated
     */
    protected static byte[] putPref(int idx, boolean pop, boolean red, boolean nod) {
        packHolder pck = new packHolder(true, true);
        typLenVal tlv = rtrIsis.getTlv();
        tlv.valDat[0] = 0;
        if (!pop) {
            tlv.valDat[0] |= 0x20; // no-php
        }
        if (red) {
            tlv.valDat[0] |= 0x80; // redistributed
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
    protected static void getPref(typLenVal tlv, tabRouteEntry<addrIP> prf) {
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
        typLenVal tlv = rtrIsis.getTlv();
        tlv.valDat[0] = 0x30; // local, value
        if (!ip4) {
            tlv.valDat[0] |= 0x80; // address family
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
    protected static typLenVal srv6base(rtrIsis lower) {
        typLenVal tlv = rtrIsis.getTlv();
        lower.traffEngID.toIPv4().toBuffer(tlv.valDat, 0); // addr
        tlv.valDat[4] = 0; // flags
        tlv.valDat[5] = typSrv6capa; // subtlv type
        tlv.valDat[6] = 2; // subtlv length
        bits.msbPutW(tlv.valDat, 7, 0); // flags
        tlv.valTyp = rtrIsisLsp.tlvRouterCapa;
        tlv.valSiz = 9;
        return tlv;
    }

    /**
     * generate srv6 locator
     *
     * @param ifc interface
     * @param met metric
     * @return tlv generated
     */
    protected static typLenVal srv6loc(cfgIfc ifc, int met) {
        if (ifc == null) {
            return null;
        }
        if (ifc.addr6 == null) {
            return null;
        }
        typLenVal tlv = rtrIsis.getTlv();
        tlv.valTyp = rtrIsisLsp.tlvSegRoutV6;
        bits.msbPutW(tlv.valDat, 0, 0); // mtid
        bits.msbPutD(tlv.valDat, 2, met); // metric
        tlv.valDat[6] = 0; // flags
        tlv.valDat[7] = 0; // algo
        int len = ifc.mask6.toNetmask();
        tlv.valDat[8] = (byte) len; // loc len
        ifc.addr6.toBuffer(tlv.valDat, 9); // locator
        tlv.valSiz = (len + 7) / 8;
        tlv.valSiz += 9;
        tlv.valDat[tlv.valSiz] = 31; // subtlv len
        tlv.valSiz += 1;
        tlv.valDat[tlv.valSiz + 0] = 4; // subtlv type
        tlv.valDat[tlv.valSiz + 1] = 1; // subtlv length
        tlv.valDat[tlv.valSiz + 2] = 0; // flags
        tlv.valSiz += 3;
        tlv.valDat[tlv.valSiz + 0] = typEndSid; // subtlv type
        tlv.valDat[tlv.valSiz + 1] = 26; // subtlv length
        tlv.valDat[tlv.valSiz + 2] = 0; // flags
        bits.msbPutW(tlv.valDat, tlv.valSiz + 3, 29); // behavior
        ifc.addr6.toBuffer(tlv.valDat, tlv.valSiz + 5); // sid
        tlv.valDat[tlv.valSiz + 21] = 6; // subsubtlvs len
        tlv.valDat[tlv.valSiz + 22] = 1; // subsubtlv type
        tlv.valDat[tlv.valSiz + 23] = 4; // subsubtlv length
        tlv.valDat[tlv.valSiz + 24] = 40; // locator block length
        tlv.valDat[tlv.valSiz + 25] = 24; // locator node length
        tlv.valDat[tlv.valSiz + 26] = 16; // locator function length
        tlv.valDat[tlv.valSiz + 27] = 0; // locator arguments length
        tlv.valSiz += 28;
        return tlv;
    }

}
