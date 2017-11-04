package rtr;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrIsis;
import ip.ipCor4;
import pack.packHolder;
import util.bits;
import util.typLenVal;

/**
 * isis traffic engineering
 *
 * @author matecsaba
 */
public class rtrIsisTe {

    private rtrIsisTe() {
    }

    /**
     * administrative group
     */
    public static final int typAdminGrp = 3;

    /**
     * link local/remote identifier
     */
    public static final int typBothId = 4;

    /**
     * local ipv4 address
     */
    public static final int typLoc4adr = 6;

    /**
     * remote ipv4 address
     */
    public static final int typRem4adr = 8;

    /**
     * max bandwidth
     */
    public static final int typMaxBndwdt = 9;

    /**
     * max reservable bandwidth
     */
    public static final int typMaxReserv = 10;

    /**
     * unreservable bandwidth
     */
    public static final int typUnReserv = 11;

    /**
     * local ipv6 address
     */
    public static final int typLoc6adr = 12;

    /**
     * remote ipv6 address
     */
    public static final int typRem6adr = 13;

    /**
     * te metric
     */
    public static final int typMetric = 18;

    /**
     * link attributes
     */
    public static final int typLnkAttr = 19;

    /**
     * link protection type
     */
    public static final int typLnkProt = 20;

    /**
     * interface switching capability
     */
    public static final int typSwchCapa = 21;

    /**
     * bandwidth constraints
     */
    public static final int typBndwdtCnst = 22;

    /**
     * unconstrained te lsp count
     */
    public static final int typUncnstLsp = 23;

    /**
     * remote as number
     */
    public static final int typASnum = 24;

    /**
     * remote ipv4 asbr number
     */
    public static final int typAsbr4id = 25;

    /**
     * remote ipv6 asbr number
     */
    public static final int typAsbr6id = 26;

    /**
     * interface adjustment capability descriptor
     */
    public static final int typIntAdjCapa = 27;

    /**
     * maximum transmission unit
     */
    public static final int typMtu = 28;

    /**
     * create te router id
     *
     * @param lower lower layer to use
     * @return tlv generated
     */
    protected static typLenVal putAddr(rtrIsis lower) {
        typLenVal tlv = rtrIsis.getTlv();
        if (lower.fwdCore.ipVersion == ipCor4.protocolVersion) {
            addrIPv4 a = lower.traffEngID.toIPv4();
            tlv.valTyp = rtrIsisLsp.tlvIpv4teId;
            tlv.valSiz = addrIPv4.size;
            a.toBuffer(tlv.valDat, 0);
        } else {
            addrIPv6 a = lower.traffEngID.toIPv6();
            tlv.valTyp = rtrIsisLsp.tlvIpv6teId;
            tlv.valSiz = addrIPv6.size;
            a.toBuffer(tlv.valDat, 0);
        }
        return tlv;
    }

    /**
     * generate te subtlvs
     *
     * @param lower lower layer to use
     * @param ifc interface to use
     * @param nei neighbor to use
     * @return bytes generated
     */
    protected static byte[] putSubs(rtrIsis lower, rtrIsisIface ifc, rtrIsisNeigh nei) {
        int bw = Float.floatToIntBits(ifc.teBandwidth / 8);
        packHolder pck = new packHolder(true, true);
        typLenVal tlv = rtrIsis.getTlv();
        tlv.valTyp = typAdminGrp;
        tlv.valSiz = 4;
        bits.msbPutD(tlv.valDat, 0, ifc.teAffinity);
        tlv.putThis(pck);
        tlv.valTyp = typMetric;
        tlv.valSiz = 3;
        bits.msbPutD(tlv.valDat, 0, ifc.teMetric << 8);
        tlv.putThis(pck);
        if (lower.fwdCore.ipVersion == ipCor4.protocolVersion) {
            tlv.putAddr(pck, typLoc4adr, ifc.iface.addr.toIPv4());
        } else {
            tlv.putAddr(pck, typLoc6adr, ifc.iface.addr.toIPv6());
        }
        if (nei != null) {
            if (lower.fwdCore.ipVersion == ipCor4.protocolVersion) {
                tlv.putAddr(pck, typRem4adr, nei.ifcAddr.toIPv4());
            } else {
                tlv.putAddr(pck, typRem6adr, nei.ifcAddr.toIPv6());
            }
        }
        tlv.valTyp = typMaxBndwdt;
        tlv.valSiz = 4;
        bits.msbPutD(tlv.valDat, 0, bw);
        tlv.putThis(pck);
        tlv.valTyp = typMaxReserv;
        tlv.valSiz = 4;
        bits.msbPutD(tlv.valDat, 0, bw);
        tlv.putThis(pck);
        tlv.valTyp = typUnReserv;
        tlv.valSiz = 32;
        for (int i = 0; i < 8; i++) {
            bits.msbPutD(tlv.valDat, i * 4, bw);
        }
        tlv.putThis(pck);
        pck.merge2beg();
        return pck.getCopy();
    }

    /**
     * generate te srlg tlv
     *
     * @param lower lower layer to use
     * @param nei neighbor id
     * @param nod node address
     * @param loc local addr
     * @param rem remote addr
     * @param srlg srlg
     * @return tlv generated
     */
    protected static typLenVal putSrlg(rtrIsis lower, addrIsis nei, int nod, addrIP loc, addrIP rem, int srlg) {
        if (rem == null) {
            rem = new addrIP();
        }
        typLenVal tlv = rtrIsis.getTlv();
        nei.toBuffer(tlv.valDat, 0);
        bits.putByte(tlv.valDat, 6, nod);
        bits.putByte(tlv.valDat, 7, 1); // flags
        if (lower.fwdCore.ipVersion == ipCor4.protocolVersion) {
            tlv.valTyp = rtrIsisLsp.tlvIpv4srlg;
            tlv.valSiz = 20;
            loc.toIPv4().toBuffer(tlv.valDat, 8);
            rem.toIPv4().toBuffer(tlv.valDat, 12);
            bits.msbPutD(tlv.valDat, 16, srlg);
        } else {
            tlv.valTyp = rtrIsisLsp.tlvIpv6srlg;
            tlv.valSiz = 44;
            loc.toIPv6().toBuffer(tlv.valDat, 8);
            rem.toIPv6().toBuffer(tlv.valDat, 24);
            bits.msbPutD(tlv.valDat, 40, srlg);
        }
        return tlv;
    }

}
