package org.freertr.spf;

import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrPrefix;
import org.freertr.addr.addrType;
import org.freertr.cry.cryHashMd5;
import org.freertr.enc.encTlv;
import org.freertr.pack.packHolder;
import org.freertr.rtr.rtrBgpUtil;
import org.freertr.tab.tabRoute;
import org.freertr.tab.tabRouteEntry;
import org.freertr.util.bits;

/**
 * link state encoder
 *
 * @author matecsaba
 */
public class spfLnkst {

    private spfLnkst() {
    }

    /**
     * get tlv encoder
     *
     * @return tlv
     */
    public static encTlv listLinkStateTlv() {
        return new encTlv(0, 16, 16, 16, 1, 0, 4, 1, 0, 1024, true);
    }

    /**
     * put header
     *
     * @param tlv tlv to use
     * @param pck packet to use
     * @param prt protocol to use
     * @param typ identifier
     */
    public static void listLinkStateHdr(encTlv tlv, packHolder pck, int prt, int typ) {
        pck.clear();
        pck.msbPutW(0, typ); // type
        pck.putByte(2, prt); // protocol
        pck.msbPutQ(3, 0); // identifier
        pck.putSkip(11);
    }

    /**
     * add a node
     *
     * @param <Ta> type to use
     * @param tlv tlv to use
     * @param pck packet to use
     * @param hlp helper to use
     * @param siz size of router id
     * @param asn asn to use
     * @param adv ls id
     * @param par area id
     * @param nod advertising router
     * @param typ node type
     */
    public static <Ta extends addrType> void listLinkStateNod(encTlv tlv, packHolder pck, packHolder hlp, int siz, int asn, addrIPv4 adv, int par, spfNode<Ta> nod, int typ) {
        hlp.clear();
        tlv.valSiz = 4;
        tlv.valTyp = 512; // asn
        bits.msbPutD(tlv.valDat, 0, asn);
        tlv.putThis(hlp);
        tlv.putAddr(hlp, 513, adv); // ls id
        hlp.merge2end();
        if (par != -1) {
            byte[] buf = new byte[4];
            bits.msbPutD(buf, 0, par);
            tlv.putBytes(hlp, 514, buf); // area id
        }
        nod.name.toBuffer(tlv.valDat, 0);
        tlv.putBytes(hlp, 515, siz, tlv.valDat); // router id
        hlp.merge2end();
        byte[] buf = hlp.getCopy();
        bits.byteCopy(buf, 0, tlv.valDat, 0, buf.length);
        tlv.valSiz = buf.length;
        tlv.putBytes(pck, typ); // node type
        pck.merge2end();
    }

    /**
     * add a node
     *
     * @param tlv tlv to use
     * @param pck packet to use
     * @param hlp helper to use
     * @param asn asn to use
     * @param adv router id
     * @param typ node type
     */
    public static void listSpfNod(encTlv tlv, packHolder pck, packHolder hlp, int asn, addrIPv4 adv, int typ) {
        hlp.clear();
        tlv.valSiz = 4;
        tlv.valTyp = 512; // asn
        bits.msbPutD(tlv.valDat, 0, asn);
        tlv.putThis(hlp);
        tlv.putAddr(hlp, 516, adv); // router id
        hlp.merge2end();
        byte[] buf = hlp.getCopy();
        bits.byteCopy(buf, 0, tlv.valDat, 0, buf.length);
        tlv.valSiz = buf.length;
        tlv.putBytes(pck, typ); // node type
        pck.merge2end();
    }

    /**
     * add prefix
     *
     * @param tab table to use
     * @param tlv tlv to use
     * @param pck packet to use
     * @param hlp helper to use
     * @param ntry route entry
     */
    public static void listLinkStatePrf(tabRoute<addrIP> tab, encTlv tlv, packHolder pck, packHolder hlp, tabRouteEntry<addrIP> ntry) {
        hlp.clear();
        if (ntry.prefix.network.isIPv4()) {
            rtrBgpUtil.writePrefix(rtrBgpUtil.safiIp4uni, true, hlp, ntry);
        } else {
            rtrBgpUtil.writePrefix(rtrBgpUtil.safiIp6uni, true, hlp, ntry);
        }
        hlp.merge2end();
        tlv.putBytes(pck, 265, hlp.getCopy());
        pck.merge2end();
        tabRouteEntry<addrIP> rou = new tabRouteEntry<addrIP>();
        rou.best.rouSrc = rtrBgpUtil.peerOriginate;
        addrIP adr = new addrIP();
        rou.nlri = pck.getCopy();
        adr.fromBuf(cryHashMd5.compute(new cryHashMd5(), rou.nlri), 0);
        rou.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
        pck.clear();
        if (ntry.best.metric > 0) {
            bits.msbPutD(tlv.valDat, 0, ntry.best.metric);
            tlv.putBytes(pck, 1155, 4, tlv.valDat); // metric
            pck.merge2end();
        }
        if (ntry.best.tag > 0) {
            bits.msbPutD(tlv.valDat, 0, ntry.best.tag);
            tlv.putBytes(pck, 1153, 4, tlv.valDat); // tag
            pck.merge2end();
        }
        if (pck.dataSize() > 0) {
            rou.best.linkStat = pck.getCopy();
        }
        tab.add(tabRoute.addType.better, rou, true, true);
    }

    /**
     * add a link state entry
     *
     * @param tab table to update
     * @param tlv tlv to use
     * @param pck packet to use
     * @param met metric
     * @param seq sequence
     */
    public static void listLinkStateAdd(tabRoute<addrIP> tab, encTlv tlv, packHolder pck, int met, long seq) {
        tabRouteEntry<addrIP> rou = new tabRouteEntry<addrIP>();
        rou.best.rouSrc = rtrBgpUtil.peerOriginate;
        addrIP adr = new addrIP();
        rou.nlri = pck.getCopy();
        adr.fromBuf(cryHashMd5.compute(new cryHashMd5(), rou.nlri), 0);
        rou.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
        pck.clear();
        if (met >= 0) {
            bits.msbPutD(tlv.valDat, 0, met << 8);
            tlv.putBytes(pck, 1095, 3, tlv.valDat); // metric
            pck.merge2end();
        }
        if (seq >= 0) {
            bits.msbPutQ(tlv.valDat, 0, seq);
            tlv.putBytes(pck, 1181, 8, tlv.valDat); // sequence
            pck.merge2end();
        }
        if (pck.dataSize() > 0) {
            rou.best.linkStat = pck.getCopy();
        }
        tab.add(tabRoute.addType.better, rou, true, true);
    }

}
