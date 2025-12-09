package org.freertr.rtr;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.addr.addrIsis;
import org.freertr.ip.ipCor4;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;
import org.freertr.enc.encTlv;

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
    public final static int typAdminGrp = 3;

    /**
     * link local/remote identifier
     */
    public final static int typBothId = 4;

    /**
     * local ipv4 address
     */
    public final static int typLoc4adr = 6;

    /**
     * remote ipv4 address
     */
    public final static int typRem4adr = 8;

    /**
     * max bandwidth
     */
    public final static int typMaxBndwdt = 9;

    /**
     * max reservable bandwidth
     */
    public final static int typMaxReserv = 10;

    /**
     * unreservable bandwidth
     */
    public final static int typUnReserv = 11;

    /**
     * local ipv6 address
     */
    public final static int typLoc6adr = 12;

    /**
     * remote ipv6 address
     */
    public final static int typRem6adr = 13;

    /**
     * extended administrative group
     */
    public final static int typExtAdmin = 14;

    /**
     * link maximum sid depth
     */
    public final static int typLinkMsd = 15;

    /**
     * te metric
     */
    public final static int typMetric = 18;

    /**
     * link attributes
     */
    public final static int typLnkAttr = 19;

    /**
     * link protection type
     */
    public final static int typLnkProt = 20;

    /**
     * interface switching capability
     */
    public final static int typSwchCapa = 21;

    /**
     * bandwidth constraints
     */
    public final static int typBndwdtCnst = 22;

    /**
     * unconstrained te lsp count
     */
    public final static int typUncnstLsp = 23;

    /**
     * remote as number
     */
    public final static int typASnum = 24;

    /**
     * remote ipv4 asbr number
     */
    public final static int typAsbr4id = 25;

    /**
     * remote ipv6 asbr number
     */
    public final static int typAsbr6id = 26;

    /**
     * interface adjustment capability descriptor
     */
    public final static int typIntAdjCapa = 27;

    /**
     * maximum transmission unit
     */
    public final static int typMtu = 28;

    /**
     * create te router id
     *
     * @param lower lower layer to use
     * @return tlv generated
     */
    protected static encTlv putAddr(rtrIsis lower) {
        encTlv tlv = rtrIsis.getTlv();
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
        encTlv tlv = rtrIsis.getTlv();
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
    protected static encTlv putSrlg(rtrIsis lower, addrIsis nei, int nod, addrIP loc, addrIP rem, int srlg) {
        if (rem == null) {
            rem = new addrIP();
        }
        encTlv tlv = rtrIsis.getTlv();
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
