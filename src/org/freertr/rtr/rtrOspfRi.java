package org.freertr.rtr;

import org.freertr.addr.addrIPv4;
import org.freertr.cfg.cfgAll;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;
import org.freertr.enc.encTlv;

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
    public final static int typInfCapa = 1;

    /**
     * router functional capabilities
     */
    public final static int typFncCapa = 2;

    /**
     * te mesh group ipv4
     */
    public final static int typMshGrp4 = 3;

    /**
     * te mesh group ipv6
     */
    public final static int typMshGrp6 = 4;

    /**
     * te capabilities
     */
    public final static int typTeCapa = 5;

    /**
     * pce discovery
     */
    public final static int typPceD = 6;

    /**
     * dynamic hostname
     */
    public final static int typHstnam = 7;

    /**
     * sr algorithm
     */
    public final static int typSrAlgo = 8;

    /**
     * sr base
     */
    public final static int typSrBase = 9;

    /**
     * node admin tag
     */
    public final static int typNodAdm = 10;

    /**
     * s-bfd discriminator
     */
    public final static int typSbfd = 11;

    /**
     * node msd
     */
    public final static int typNodMsd = 12;

    /**
     * tunnel encapsulation
     */
    public final static int typTunEnc = 13;

    /**
     * sr local block
     */
    public final static int typSrLocBlk = 14;

    /**
     * sr ms preference
     */
    public final static int typSrMsPref = 15;

    /**
     * graceful restart capable
     */
    public final static int capGrcCap = 0x80000000;

    /**
     * graceful restart helper
     */
    public final static int capGrcHlp = 0x40000000;

    /**
     * stub router
     */
    public final static int capStub = 0x20000000;

    /**
     * traffic engineering
     */
    public final static int capTe = 0x10000000;

    /**
     * point2point lan
     */
    public final static int capPpLan = 0x08000000;

    /**
     * experimental traffic engineering
     */
    public final static int capExpTe = 0x04000000;

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
