package org.freertr.rtr;

import org.freertr.pack.packHolder;
import org.freertr.util.bits;
import org.freertr.enc.encTlv;

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
    public final static int typRouter4 = 1;

    /**
     * te link lsa
     */
    public final static int typLink = 2;

    /**
     * ipv6 router lsa
     */
    public final static int typRouter6 = 3;

    /**
     * link local lsa
     */
    public final static int typLnkLoc = 4;

    /**
     * node attribute lsa
     */
    public final static int typNodAtr = 5;

    /**
     * optical node property lsa
     */
    public final static int typOptNod = 6;

    /**
     * link type
     */
    public final static int typLnkTyp = 1;

    /**
     * link id
     */
    public final static int typLnkId = 2;

    /**
     * local ipv4 address
     */
    public final static int typLoc4adr = 3;

    /**
     * remote ipv4 address
     */
    public final static int typRem4adr = 4;

    /**
     * te metric
     */
    public final static int typMetric = 5;

    /**
     * max bandwidth
     */
    public final static int typMaxBndwdt = 6;

    /**
     * max reservable bandwidth
     */
    public final static int typMaxReserv = 7;

    /**
     * unreservable bandwidth
     */
    public final static int typUnReserv = 8;

    /**
     * administrative group
     */
    public final static int typAdminGrp = 9;

    /**
     * link local/remote te identifier
     */
    public final static int typBothTe = 10;

    /**
     * link local/remote identifier
     */
    public final static int typBothId = 11;

    /**
     * inter-ra export upward
     */
    public final static int typIntRaUp = 12;

    /**
     * inter-ra export downward
     */
    public final static int typIntRaDn = 13;

    /**
     * link protection type
     */
    public final static int typLnkProt = 14;

    /**
     * interface switching capability
     */
    public final static int typSwchCapa = 15;

    /**
     * shared risk link group
     */
    public final static int typSrlg = 16;

    /**
     * bandwidth constraints
     */
    public final static int typBndwdtCnst = 17;

    /**
     * neighbor id
     */
    public final static int typNeighId = 18;

    /**
     * local ipv6 address
     */
    public final static int typLoc6adr = 19;

    /**
     * remote ipv6 address
     */
    public final static int typRem6adr = 20;

    /**
     * remote as number
     */
    public final static int typASnum = 21;

    /**
     * remote ipv4 asbr number
     */
    public final static int typAsbr4id = 22;

    /**
     * unconstrained te lsp count
     */
    public final static int typUncnstLsp = 23;

    /**
     * remote ipv6 asbr number
     */
    public final static int typAsbr6id = 24;

    /**
     * interface adjustment capability descriptor
     */
    public final static int typIntAdjCapa = 25;

    /**
     * extended administrative group
     */
    public final static int typExtAdmin = 26;

    /**
     * unidirectional link delay
     */
    public final static int typUniLnkDly = 27;

    /**
     * min/max unidirectional link delay
     */
    public final static int typRngUniLnkDly = 28;

    /**
     * unidirectional link delay variation
     */
    public final static int typUniLnkDlyVar = 29;

    /**
     * unidirectional link loss
     */
    public final static int typUniLnkLos = 30;

    /**
     * unidirectional residual bandwidth
     */
    public final static int typUniResBwd = 31;

    /**
     * unidirectional available bandwidth
     */
    public final static int typUniAvaBwd = 32;

    /**
     * unidirectional utilized bandwidth
     */
    public final static int typUniUtlBwd = 33;

    /**
     * port label restrictions
     */
    public final static int typPrtLabRes = 34;

    /**
     * network to router te metric
     */
    public final static int typNetRtrMet = 35;

    /**
     * point to point link
     */
    public final static int lnkP2p = 1;

    /**
     * broadcast link
     */
    public final static int lnkBrd = 2;

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
