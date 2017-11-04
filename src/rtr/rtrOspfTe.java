package rtr;

import pack.packHolder;
import util.bits;
import util.typLenVal;

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
     * link local/remote identifier
     */
    public static final int typBothId = 11;

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
     * point to point link
     */
    public static final int lnkP2p = 1;

    /**
     * broadcast link
     */
    public static final int lnkBrd = 2;

    /**
     * link type
     */
    public static final int typLnkTyp = 1;

    /**
     * link id
     */
    public static final int typLnkId = 2;

    /**
     * get tlv handler
     *
     * @return tlv handler
     */
    public static typLenVal getTlvHandler() {
        return new typLenVal(0, 16, 16, 16, 1, 0, 4, 4, 0, 1024, true);
    }

    /**
     * put general tlvs
     *
     * @param pck packet to update
     * @param brd broadcast
     */
    public static void putGenTlv1(packHolder pck, boolean brd) {
        typLenVal tlv = getTlvHandler();
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
        typLenVal tlv = getTlvHandler();
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
