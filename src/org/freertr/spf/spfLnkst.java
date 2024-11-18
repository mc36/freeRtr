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
     * add a link
     *
     * @param tlv tlv to use
     * @param pck packet to use
     * @param loc local ip
     * @param rem remote ip
     */
    public static void listSpfLnk(encTlv tlv, packHolder pck, addrIP loc, addrIP rem) {
        if (loc == null) {
            return;
        }
        if (rem == null) {
            return;
        }
        if (loc.isIPv4()) {
            tlv.putAddr(pck, 259, loc.toIPv4());
            tlv.putAddr(pck, 260, rem.toIPv4());
        } else {
            tlv.putAddr(pck, 261, loc.toIPv6());
            tlv.putAddr(pck, 262, rem.toIPv6());
        }
    }

    /**
     * get prefix type
     *
     * @param ntry route entry
     * @return type to use
     */
    public static int getPrefixType(tabRouteEntry<addrIP> ntry) {
        if (ntry.prefix.network.isIPv4()) {
            return 3;
        } else {
            return 4;
        }
    }

    /**
     * add prefix
     *
     * @param tab table to use
     * @param tlv tlv to use
     * @param pck packet to use
     * @param hlp helper to use
     * @param ntry route entry
     * @param seq sequence
     */
    public static void listLinkStatePrf(tabRoute<addrIP> tab, encTlv tlv, packHolder pck, packHolder hlp, tabRouteEntry<addrIP> ntry, long seq) {
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
        if (seq >= 0) {
            bits.msbPutQ(tlv.valDat, 0, seq);
            tlv.putBytes(pck, 1181, 8, tlv.valDat); // sequence
            pck.merge2end();
        }
        if (ntry.best.metric >= 0) {
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
     * @param siz size of metric
     * @param met metric
     * @param seq sequence
     */
    public static void listLinkStateAdd(tabRoute<addrIP> tab, encTlv tlv, packHolder pck, int siz, int met, long seq) {
        tabRouteEntry<addrIP> rou = new tabRouteEntry<addrIP>();
        rou.best.rouSrc = rtrBgpUtil.peerOriginate;
        addrIP adr = new addrIP();
        rou.nlri = pck.getCopy();
        adr.fromBuf(cryHashMd5.compute(new cryHashMd5(), rou.nlri), 0);
        rou.prefix = new addrPrefix<addrIP>(adr, addrIP.size * 8);
        pck.clear();
        switch (siz) {
            case 1:
                tlv.valDat[0] = (byte) met;
                break;
            case 2:
                bits.msbPutW(tlv.valDat, 0, met);
                break;
            case 3:
                bits.msbPutD(tlv.valDat, 0, met << 8);
                break;
            case 4:
                bits.msbPutD(tlv.valDat, 0, met);
                break;
        }
        if (siz > 0) {
            tlv.putBytes(pck, 1095, siz, tlv.valDat); // metric
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

    private static boolean findTlv(encTlv tlv, packHolder pck, int num) {
        for (;;) {
            if (tlv.getBytes(pck)) {
                return true;
            }
            if (tlv.valTyp == num) {
                return false;
            }
        }
    }

    private static addrIPv4 findNode(encTlv tlv, packHolder pck, packHolder hlp, int num) {
        if (findTlv(tlv, pck, num)) {
            return null;
        }
        hlp.clear();
        hlp.putCopy(tlv.valDat, 0, 0, tlv.valSiz);
        hlp.putSkip(tlv.valSiz);
        hlp.merge2beg();
        if (findTlv(tlv, hlp, 516)) {
            return null;
        }
        addrIPv4 adr = new addrIPv4();
        adr.fromBuf(tlv.valDat, 0);
        return adr;
    }

    private static int findMet(encTlv tlv, packHolder pck, int num) {
        if (findTlv(tlv, pck, num)) {
            return 0;
        }
        return bits.msbGetD(tlv.valDat, 0);
    }

    /**
     * read spf node
     *
     * @param spf spf to use
     * @param tlv tlv to use
     * @param pck packet to use
     * @param hlp helper to use
     */
    public static void readSpfNode(spfCalc<addrIPv4> spf, encTlv tlv, packHolder pck, packHolder hlp) {
        addrIPv4 loc = findNode(tlv, pck, hlp, 256);
        if (loc == null) {
            return;
        }
        spf.addIdent(loc, null);
    }

    /**
     * read spf link
     *
     * @param spf spf to use
     * @param tlv tlv to use
     * @param pck packet to use
     * @param hlp helper to use
     */
    public static void readSpfLink(spfCalc<addrIPv4> spf, encTlv tlv, packHolder pck, packHolder hlp) {
        addrIPv4 loc = findNode(tlv, pck, hlp, 256);
        if (loc == null) {
            return;
        }
        addrIPv4 rem = findNode(tlv, pck, hlp, 257);
        if (rem == null) {
            return;
        }
        int met = findMet(tlv, pck, 1095);
        if (met < 1) {
            return;
        }
        spf.addConn(loc, rem, met, true, false, null);
    }

    /**
     * read spf prefix
     *
     * @param spf spf to use
     * @param tlv tlv to use
     * @param pck packet to use
     * @param hlp helper to use
     * @param safi safi to use
     * @param dist distance to use
     */
    public static void readSpfPref(spfCalc<addrIPv4> spf, encTlv tlv, packHolder pck, packHolder hlp, int safi, int dist) {
        addrIPv4 loc = findNode(tlv, pck, hlp, 256);
        if (loc == null) {
            return;
        }
        if (findTlv(tlv, pck, 265)) {
            return;
        }
        hlp.clear();
        hlp.putCopy(tlv.valDat, 0, 0, tlv.valSiz);
        hlp.putSkip(tlv.valSiz);
        hlp.merge2beg();
        tabRouteEntry<addrIP> ntry = rtrBgpUtil.readPrefix(safi, true, hlp);
        if (ntry == null) {
            return;
        }
        int i = pck.dataSize();
        ntry.best.metric = findMet(tlv, pck, 1155);
        pck.setBytesLeft(i);
        ntry.best.tag = findMet(tlv, pck, 1153);
        ntry.best.distance = dist;
        spf.addPref(loc, ntry, false);
    }

}
