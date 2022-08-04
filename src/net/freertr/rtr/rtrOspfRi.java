package net.freertr.rtr;

import net.freertr.addr.addrIPv4;
import net.freertr.cfg.cfgAll;
import net.freertr.pack.packHolder;
import net.freertr.util.bits;
import net.freertr.enc.encTlv;

/**
 * ospf router information
 *
 * @author matecsaba
 */
public class rtrOspfRi {

    private rtrOspfRi() {
    }

    /**
     * router informational capabilities
     */
    public static final int typInfCapa = 1;

    /**
     * router functional capabilities
     */
    public static final int typFncCapa = 2;

    /**
     * te mesh group ipv4
     */
    public static final int typMshGrp4 = 3;

    /**
     * te mesh group ipv6
     */
    public static final int typMshGrp6 = 4;

    /**
     * te capabilities
     */
    public static final int typTeCapa = 5;

    /**
     * pce discovery
     */
    public static final int typPceD = 6;

    /**
     * dynamic hostname
     */
    public static final int typHstnam = 7;

    /**
     * sr algorithm
     */
    public static final int typSrAlgo = 8;

    /**
     * sr base
     */
    public static final int typSrBase = 9;

    /**
     * node admin tag
     */
    public static final int typNodAdm = 10;

    /**
     * s-bfd discriminator
     */
    public static final int typSbfd = 11;

    /**
     * node msd
     */
    public static final int typNodMsd = 12;

    /**
     * tunnel encapsulation
     */
    public static final int typTunEnc = 13;

    /**
     * sr local block
     */
    public static final int typSrLocBlk = 14;

    /**
     * sr ms preference
     */
    public static final int typSrMsPref = 15;

    /**
     * graceful restart capable
     */
    public static final int capGrcCap = 0x80000000;

    /**
     * graceful restart helper
     */
    public static final int capGrcHlp = 0x40000000;

    /**
     * stub router
     */
    public static final int capStub = 0x20000000;

    /**
     * traffic engineering
     */
    public static final int capTe = 0x10000000;

    /**
     * point2point lan
     */
    public static final int capPpLan = 0x08000000;

    /**
     * experimental traffic engineering
     */
    public static final int capExpTe = 0x04000000;

    /**
     * get opaque id
     *
     * @return lsa id
     */
    public static addrIPv4 getOpaque() {
        byte[] buf = new byte[4];
        buf[0] = 4;
        addrIPv4 a = new addrIPv4();
        a.fromBuf(buf, 0);
        return a;
    }

    /**
     * put capability tlv
     *
     * @param pck packet to update
     * @param te traffic engineering
     */
    public static void putCapa(packHolder pck, boolean te) {
        encTlv tlv = rtrOspfTe.getTlvHandler();
        tlv.valTyp = typInfCapa;
        tlv.valSiz = 4;
        int i = capStub | capPpLan;
        if (te) {
            i |= capTe;
        }
        bits.msbPutD(tlv.valDat, 0, i);
        tlv.putThis(pck);
    }

    /**
     * put hostname tlv
     *
     * @param pck packet to update
     */
    public static void putHstnam(packHolder pck) {
        encTlv tlv = rtrOspfTe.getTlvHandler();
        tlv.putStr(pck, typHstnam, cfgAll.hostName);
    }

    /**
     * get hostname
     *
     * @param tlv tlv to read
     * @return hostname, null if nothing
     */
    public static String getHstnam(encTlv tlv) {
        if (tlv.valTyp != typHstnam) {
            return null;
        }
        return tlv.getStr();
    }

}
