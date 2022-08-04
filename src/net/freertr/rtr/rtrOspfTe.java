package net.freertr.rtr;

import net.freertr.pack.packHolder;
import net.freertr.util.bits;
import net.freertr.enc.encTlv;

/**
 * ospf traffic engineering
 *
 * @author matecsaba
 */
public class rtrOspfTe {

    private rtrOspfTe() {
    }

    /**
     * ipv4 router lsa
     */
    public static final int typRouter4 = 1;

    /**
     * te link lsa
     */
    public static final int typLink = 2;

    /**
     * ipv6 router lsa
     */
    public static final int typRouter6 = 3;

    /**
     * link local lsa
     */
    public static final int typLnkLoc = 4;

    /**
     * node attribute lsa
     */
    public static final int typNodAtr = 5;

    /**
     * optical node property lsa
     */
    public static final int typOptNod = 6;

    /**
     * link type
     */
    public static final int typLnkTyp = 1;

    /**
     * link id
     */
    public static final int typLnkId = 2;

    /**
     * local ipv4 address
     */
    public static final int typLoc4adr = 3;

    /**
     * remote ipv4 address
     */
    public static final int typRem4adr = 4;

    /**
     * te metric
     */
    public static final int typMetric = 5;

    /**
     * max bandwidth
     */
    public static final int typMaxBndwdt = 6;

    /**
     * max reservable bandwidth
     */
    public static final int typMaxReserv = 7;

    /**
     * unreservable bandwidth
     */
    public static final int typUnReserv = 8;

    /**
     * administrative group
     */
    public static final int typAdminGrp = 9;

    /**
     * link local/remote te identifier
     */
    public static final int typBothTe = 10;

    /**
     * link local/remote identifier
     */
    public static final int typBothId = 11;

    /**
     * inter-ra export upward
     */
    public static final int typIntRaUp = 12;

    /**
     * inter-ra export downward
     */
    public static final int typIntRaDn = 13;

    /**
     * link protection type
     */
    public static final int typLnkProt = 14;

    /**
     * interface switching capability
     */
    public static final int typSwchCapa = 15;

    /**
     * shared risk link group
     */
    public static final int typSrlg = 16;

    /**
     * bandwidth constraints
     */
    public static final int typBndwdtCnst = 17;

    /**
     * neighbor id
     */
    public static final int typNeighId = 18;

    /**
     * local ipv6 address
     */
    public static final int typLoc6adr = 19;

    /**
     * remote ipv6 address
     */
    public static final int typRem6adr = 20;

    /**
     * remote as number
     */
    public static final int typASnum = 21;

    /**
     * remote ipv4 asbr number
     */
    public static final int typAsbr4id = 22;

    /**
     * unconstrained te lsp count
     */
    public static final int typUncnstLsp = 23;

    /**
     * remote ipv6 asbr number
     */
    public static final int typAsbr6id = 24;

    /**
     * interface adjustment capability descriptor
     */
    public static final int typIntAdjCapa = 25;

    /**
     * extended administrative group
     */
    public static final int typExtAdmin = 26;

    /**
     * unidirectional link delay
     */
    public static final int typUniLnkDly = 27;

    /**
     * min/max unidirectional link delay
     */
    public static final int typRngUniLnkDly = 28;

    /**
     * unidirectional link delay variation
     */
    public static final int typUniLnkDlyVar = 29;

    /**
     * unidirectional link loss
     */
    public static final int typUniLnkLos = 30;

    /**
     * unidirectional residual bandwidth
     */
    public static final int typUniResBwd = 31;

    /**
     * unidirectional available bandwidth
     */
    public static final int typUniAvaBwd = 32;

    /**
     * unidirectional utilized bandwidth
     */
    public static final int typUniUtlBwd = 33;

    /**
     * port label restrictions
     */
    public static final int typPrtLabRes = 34;

    /**
     * network to router te metric
     */
    public static final int typNetRtrMet = 35;

    /**
     * point to point link
     */
    public static final int lnkP2p = 1;

    /**
     * broadcast link
     */
    public static final int lnkBrd = 2;

    /**
     * get tlv handler
     *
     * @return tlv handler
     */
    public static encTlv getTlvHandler() {
        return new encTlv(0, 16, 16, 16, 1, 0, 4, 4, 0, 1024, true);
    }

    /**
     * put general tlvs
     *
     * @param pck packet to update
     * @param brd broadcast
     */
    public static void putGenTlv1(packHolder pck, boolean brd) {
        encTlv tlv = getTlvHandler();
        tlv.valTyp = typLnkTyp;
        tlv.valSiz = 1;
        if (brd) {
            tlv.valDat[0] = lnkBrd;
        } else {
            tlv.valDat[0] = lnkP2p;
        }
        tlv.putThis(pck);
    }

    /**
     * put general tlvs
     *
     * @param pck packet to update
     * @param met metric
     * @param bwd bandwidth
     * @param aff affinity
     * @param srl srlg
     */
    public static void putGenTlv2(packHolder pck, int met, long bwd, int aff, int srl) {
        encTlv tlv = getTlvHandler();
        int bw = Float.floatToIntBits(bwd / 8);
        tlv.valTyp = typMetric;
        tlv.valSiz = 4;
        bits.msbPutD(tlv.valDat, 0, met);
        tlv.putThis(pck);
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
        tlv.valTyp = typAdminGrp;
        tlv.valSiz = 4;
        bits.msbPutD(tlv.valDat, 0, aff);
        tlv.putThis(pck);
        tlv.valTyp = typSrlg;
        tlv.valSiz = 4;
        bits.msbPutD(tlv.valDat, 0, srl);
        tlv.putThis(pck);
    }

}
