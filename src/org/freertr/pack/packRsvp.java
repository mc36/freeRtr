package org.freertr.pack;

import java.util.List;
import org.freertr.addr.addrIP;
import org.freertr.addr.addrIPv4;
import org.freertr.addr.addrIPv6;
import org.freertr.ip.ipIfc4;
import org.freertr.ip.ipIfc6;
import org.freertr.tab.tabHop;
import org.freertr.util.bits;
import org.freertr.enc.encTlv;

/**
 * resource reservation protocol (rfc2205) packet
 *
 * @author matecsaba
 */
public class packRsvp {

    /**
     * create instance
     */
    public packRsvp() {
    }

    /**
     * protocol number
     */
    public final static int proto = 46;

    /**
     * header size
     */
    public final static int size = 8;

    /**
     * true=ipv4, false=ipv6
     */
    public boolean isIP4;

    /**
     * true=p2mp, false=p2p
     */
    public boolean isP2MP;

    /**
     * message type
     */
    public int typ;

    /**
     * original ttl
     */
    public int ttl;

    /**
     * hop id
     */
    public int hopId;

    /**
     * hop address
     */
    public addrIP hopAdr;

    /**
     * session id
     */
    public long sessId;

    /**
     * session address
     */
    public addrIP sessAdr;

    /**
     * association type
     */
    public int assocTyp;

    /**
     * association id
     */
    public int assocId;

    /**
     * association global id
     */
    public int assocGlb;

    /**
     * association address
     */
    public addrIP assocAdr;

    /**
     * refresh time in ms
     */
    public int timeVal;

    /**
     * sender id
     */
    public int sndrId;

    /**
     * sender address
     */
    public addrIP sndrAdr;

    /**
     * subgroup id
     */
    public int sbgrpId;

    /**
     * subgroup originator
     */
    public addrIP sbgrpOrg;

    /**
     * label request
     */
    public int labReq;

    /**
     * token bucket rate
     */
    public float flwSpcRate;

    /**
     * token bucket size
     */
    public float flwSpcSize;

    /**
     * peak data rate
     */
    public float flwSpcPeak;

    /**
     * minimum policed unit
     */
    public int flwSpcPlcd;

    /**
     * maximum packet size
     */
    public int flwSpcPcks;

    /**
     * setup priority
     */
    public int sessStp;

    /**
     * hold priority
     */
    public int sessHld;

    /**
     * exclude affinity
     */
    public int sessExc;

    /**
     * include affinity
     */
    public int sessInc;

    /**
     * must affinity
     */
    public int sessMst;

    /**
     * flags
     */
    public int sessFlg;

    /**
     * name
     */
    public String sessNam;

    /**
     * s2l sub lsp destination
     */
    public addrIP subAddr;

    /**
     * is hop count
     */
    public int adsHops;

    /**
     * path bandwidth estimate
     */
    public float adsBndwdt;

    /**
     * minimum path latency
     */
    public int adsLtncy;

    /**
     * composed mtu
     */
    public int adsCmtu;

    /**
     * explicit route
     */
    public List<tabHop> expRout;

    /**
     * record route
     */
    public List<tabHop> recRout;

    /**
     * error node
     */
    public addrIP errAdr;

    /**
     * error code
     */
    public int errCod;

    /**
     * style value
     */
    public int styleVal;

    /**
     * label value
     */
    public int labelVal;

    /**
     * path request
     */
    public final static int typPathReq = 1;

    /**
     * resv request
     */
    public final static int typResvReq = 2;

    /**
     * path error
     */
    public final static int typPathErr = 3;

    /**
     * resv error
     */
    public final static int typResvErr = 4;

    /**
     * path tear
     */
    public final static int typPathTear = 5;

    /**
     * resv tear
     */
    public final static int typResvTear = 6;

    /**
     * resv confirmation
     */
    public final static int typResvConf = 7;

    private encTlv tlv = new encTlv(16, 16, 0, 16, 1, 4, 4, 1, 0, 512, true);

    public String toString() {
        return "ip4=" + isIP4 + " p2mp=" + isP2MP + " typ=" + type2string(typ) + " ttl=" + ttl + " hop=" + hopAdr + "/" + hopId + " sess=" + sessAdr + "/" + sessId + " assoc=" + assocTyp + "/" + assocAdr + "/" + assocId + "/" + assocGlb + " time=" + timeVal + " send=" + sndrAdr + "/" + sndrId + " subgrp=" + sbgrpOrg + "/" + sbgrpId + " subdst=" + subAddr + " req=" + labReq + " flow=" + flwSpcRate + "/" + flwSpcSize + "/" + flwSpcPeak + "/" + flwSpcPlcd + "/" + flwSpcPcks + " prio=" + sessStp + "/" + sessHld + " affi=" + sessExc + "/" + sessInc + "/" + sessMst + " flg=" + sessFlg + " nam=" + sessNam + " hops=" + adsHops + " bndwdth=" + adsBndwdt + " latency=" + adsLtncy + " mtu=" + adsCmtu + " expRou=" + tabHop.dumpList(expRout) + " recRou=" + tabHop.dumpList(recRout) + " err=" + errAdr + "/" + errCod + " style=" + styleVal + " label=" + labelVal;
    }

    /**
     * get traget address
     *
     * @return target address
     */
    public addrIP getTrg() {
        if (isP2MP) {
            return subAddr;
        } else {
            return sessAdr;
        }
    }

    /**
     * convert type to string
     *
     * @param i message type
     * @return string
     */
    public static String type2string(int i) {
        switch (i) {
            case typPathReq:
                return "pathReq";
            case typResvReq:
                return "resvReq";
            case typPathErr:
                return "pathErr";
            case typResvErr:
                return "resvErr";
            case typPathTear:
                return "pathTear";
            case typResvTear:
                return "resvTear";
            case typResvConf:
                return "resvConf";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * parse header
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseHeader(packHolder pck) {
        isIP4 = pck.IPtrg.isIPv4();
        if (pck.dataSize() < size) {
            return true;
        }
        int ver = pck.getByte(0); // version + flags
        if ((ver & 0xf0) != 0x10) {
            return true;
        }
        typ = pck.getByte(1); // message type
        ttl = pck.getByte(4); // sending ttl
        int len = pck.msbGetW(6); // message length
        if (len < size) {
            return true;
        }
        if (len > pck.dataSize()) {
            return true;
        }
        pck.setDataSize(len);
        if (pck.getIPsum(0, len, 0) != 0xffff) {
            return true;
        }
        pck.getSkip(size);
        return false;
    }

    /**
     * create header
     *
     * @param pck packet to use
     */
    public void createHeader(packHolder pck) {
        pck.merge2beg();
        int len = pck.dataSize();
        pck.putByte(0, 0x10); // version
        pck.putByte(1, typ); // type
        pck.lsbPutW(2, 0); // checksum
        pck.putByte(4, ttl); // ttl
        pck.putByte(5, 0); // reserved
        pck.msbPutW(6, size + len);
        int i = pck.putIPsum(0, size, 0);
        i = pck.getIPsum(0, len, i);
        pck.lsbPutW(2, 0xffff - i); // checksum
        pck.putSkip(size);
        pck.merge2beg();
    }

    /**
     * create holder
     *
     * @param pck packet to use
     */
    public void createHolder(packHolder pck) {
        pck.IPtrg.setAddr(getTrg());
        pck.IPsrc.setAddr(sndrAdr);
        pck.IPdf = false;
        pck.IPfrg = 0;
        pck.IPttl = ttl;
        pck.IPtos = 0;
        pck.IPid = 0;
        pck.IPprt = proto;
        pck.IPalrt = 1;
        isIP4 = pck.IPtrg.isIPv4();
        isP2MP = (sbgrpId != 0) || (!sbgrpOrg.isEmpty());
    }

    private boolean findTlv(packHolder pck, int typ) {
        int len = pck.dataSize();
        for (;;) {
            if (tlv.getBytes(pck)) {
                pck.setBytesLeft(len);
                return true;
            }
            if (tlv.valTyp != typ) {
                continue;
            }
            pck.setBytesLeft(len);
            return false;
        }
    }

    private void padUpTlv() {
        for (; (tlv.valSiz & 3) != 0;) {
            tlv.valDat[tlv.valSiz] = 0;
            tlv.valSiz++;
        }
    }

    private int getTypHop() {
        if (isIP4) {
            return 0x0301;
        } else {
            return 0x0302;
        }
    }

    /**
     * parse hop option
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseHop(packHolder pck) {
        if (findTlv(pck, getTypHop())) {
            return true;
        }
        hopAdr = new addrIP();
        if (isIP4) {
            addrIPv4 adr = new addrIPv4();
            adr.fromBuf(tlv.valDat, 0);
            hopAdr.fromIPv4addr(adr);
            hopId = bits.msbGetD(tlv.valDat, addrIPv4.size);
        } else {
            addrIPv6 adr = new addrIPv6();
            adr.fromBuf(tlv.valDat, 0);
            hopAdr.fromIPv6addr(adr);
            hopId = bits.msbGetD(tlv.valDat, addrIPv6.size);
        }
        return false;
    }

    /**
     * create hop option
     *
     * @param pck packet to use
     */
    public void createHop(packHolder pck) {
        if (isIP4) {
            hopAdr.toIPv4().toBuffer(tlv.valDat, 0);
            tlv.valSiz = addrIPv4.size;
        } else {
            hopAdr.toIPv6().toBuffer(tlv.valDat, 0);
            tlv.valSiz = addrIPv6.size;
        }
        bits.msbPutD(tlv.valDat, tlv.valSiz, hopId);
        tlv.valSiz += 4;
        tlv.valTyp = getTypHop();
        tlv.putThis(pck);
    }

    private int getTypSess() {
        int i;
        if (isIP4) {
            i = 0x0107;
        } else {
            i = 0x0108;
        }
        if (isP2MP) {
            i += 6;
        }
        return i;
    }

    /**
     * parse session option
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseSess(packHolder pck) {
        isP2MP = false;
        if (findTlv(pck, getTypSess())) {
            isP2MP = true;
            if (findTlv(pck, getTypSess())) {
                return true;
            }
        }
        sessAdr = new addrIP();
        if (isIP4 || isP2MP) {
            addrIPv4 adr = new addrIPv4();
            adr.fromBuf(tlv.valDat, 0);
            sessAdr.fromIPv4addr(adr);
            sessId = bits.msbGetQ(tlv.valDat, addrIPv4.size);
        } else {
            addrIPv6 adr = new addrIPv6();
            adr.fromBuf(tlv.valDat, 0);
            sessAdr.fromIPv6addr(adr);
            sessId = bits.msbGetQ(tlv.valDat, addrIPv6.size);
        }
        return false;
    }

    /**
     * create session option
     *
     * @param pck packet to use
     */
    public void createSess(packHolder pck) {
        if (isIP4 || isP2MP) {
            sessAdr.toIPv4().toBuffer(tlv.valDat, 0);
            tlv.valSiz = addrIPv4.size;
        } else {
            sessAdr.toIPv6().toBuffer(tlv.valDat, 0);
            tlv.valSiz = addrIPv6.size;
        }
        bits.msbPutQ(tlv.valDat, tlv.valSiz, sessId);
        tlv.valSiz += 8;
        tlv.valTyp = getTypSess();
        tlv.putThis(pck);
    }

    /**
     * parse time option
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseTime(packHolder pck) {
        if (findTlv(pck, 0x0501)) {
            return true;
        }
        timeVal = bits.msbGetD(tlv.valDat, 0);
        return false;
    }

    /**
     * create time option
     *
     * @param pck packet to use
     */
    public void createTime(packHolder pck) {
        bits.msbPutD(tlv.valDat, 0, timeVal);
        tlv.valSiz = 4;
        tlv.valTyp = 0x0501;
        tlv.putThis(pck);
    }

    private int getTypSndrTmp(boolean filtrSpec) {
        int i;
        if (isIP4) {
            i = 0x07;
        } else {
            i = 0x08;
        }
        if (isP2MP) {
            i += 5;
        }
        if (filtrSpec) {
            i |= 0x0a00;
        } else {
            i |= 0x0b00;
        }
        return i;
    }

    /**
     * parse sender template option
     *
     * @param pck packet to use
     * @param filtrSpec true=filterSpec, false=senderTemplate
     * @return false on success, true on error
     */
    public boolean parseSndrTmp(packHolder pck, boolean filtrSpec) {
        if (findTlv(pck, getTypSndrTmp(filtrSpec))) {
            return true;
        }
        sndrAdr = new addrIP();
        sbgrpOrg = new addrIP();
        if (isIP4) {
            addrIPv4 adr = new addrIPv4();
            adr.fromBuf(tlv.valDat, 0);
            sndrAdr.fromIPv4addr(adr);
            sndrId = bits.msbGetD(tlv.valDat, addrIPv4.size);
            if (isP2MP) {
                adr.fromBuf(tlv.valDat, addrIPv4.size + 4);
                sbgrpOrg.fromIPv4addr(adr);
                sbgrpId = bits.msbGetD(tlv.valDat, addrIPv4.size + addrIPv4.size + 4);
            }
        } else {
            addrIPv6 adr = new addrIPv6();
            adr.fromBuf(tlv.valDat, 0);
            sndrAdr.fromIPv6addr(adr);
            sndrId = bits.msbGetD(tlv.valDat, addrIPv6.size);
            if (isP2MP) {
                adr.fromBuf(tlv.valDat, addrIPv6.size + 4);
                sbgrpOrg.fromIPv6addr(adr);
                sbgrpId = bits.msbGetD(tlv.valDat, addrIPv6.size + addrIPv6.size + 4);
            }
        }
        return false;
    }

    /**
     * create sender template option
     *
     * @param pck packet to use
     * @param filtrSpec true=filterSpec, false=senderTemplate
     */
    public void createSndrTmp(packHolder pck, boolean filtrSpec) {
        if (isIP4) {
            sndrAdr.toIPv4().toBuffer(tlv.valDat, 0);
            tlv.valSiz = addrIPv4.size;
        } else {
            sndrAdr.toIPv6().toBuffer(tlv.valDat, 0);
            tlv.valSiz = addrIPv6.size;
        }
        bits.msbPutD(tlv.valDat, tlv.valSiz, sndrId);
        tlv.valSiz += 4;
        if (isP2MP) {
            if (isIP4) {
                sbgrpOrg.toIPv4().toBuffer(tlv.valDat, tlv.valSiz);
                tlv.valSiz += addrIPv4.size;
            } else {
                sbgrpOrg.toIPv6().toBuffer(tlv.valDat, tlv.valSiz);
                tlv.valSiz += addrIPv6.size;
            }
            bits.msbPutD(tlv.valDat, tlv.valSiz, sbgrpId);
            tlv.valSiz += 4;
        }
        tlv.valTyp = getTypSndrTmp(filtrSpec);
        tlv.putThis(pck);
    }

    /**
     * parse label request option
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseLabReq(packHolder pck) {
        if (findTlv(pck, 0x1301)) {
            return true;
        }
        labReq = bits.msbGetD(tlv.valDat, 0);
        return false;
    }

    /**
     * create label request option
     *
     * @param pck packet to use
     */
    public void createLabReq(packHolder pck) {
        bits.msbPutD(tlv.valDat, 0, labReq);
        tlv.valSiz = 4;
        tlv.valTyp = 0x1301;
        tlv.putThis(pck);
    }

    /**
     * fill up label request
     */
    public void fillLabReq() {
        if (isIP4) {
            labReq = ipIfc4.type;
        } else {
            labReq = ipIfc6.type;
        }
    }

    /**
     * parse senderTspec/flowSpec option
     *
     * @param pck packet to use
     * @param flowSpec true=flowSpec, false=senderTspec
     * @return false on success, true on error
     */
    public boolean parseFlwSpc(packHolder pck, boolean flowSpec) {
        int i;
        if (flowSpec) {
            if (findTlv(pck, 0x0902)) {
                return true;
            }
            i = 0x0500;
        } else {
            if (findTlv(pck, 0x0c02)) {
                return true;
            }
            i = 0x0100;
        }
        if (bits.msbGetW(tlv.valDat, 0) != 0) { // version
            return true;
        }
        if (bits.msbGetW(tlv.valDat, 2) < 7) { // size
            return true;
        }
        if (bits.msbGetW(tlv.valDat, 4) != i) { // header type
            return true;
        }
        if (bits.msbGetW(tlv.valDat, 6) < 6) { // size
            return true;
        }
        if (bits.msbGetW(tlv.valDat, 8) != 0x7f00) { // parameter id
            return true;
        }
        if (bits.msbGetW(tlv.valDat, 10) < 5) { // size
            return true;
        }
        flwSpcRate = Float.intBitsToFloat(bits.msbGetD(tlv.valDat, 12));
        flwSpcSize = Float.intBitsToFloat(bits.msbGetD(tlv.valDat, 16));
        flwSpcPeak = Float.intBitsToFloat(bits.msbGetD(tlv.valDat, 20));
        flwSpcPlcd = bits.msbGetD(tlv.valDat, 24);
        flwSpcPcks = bits.msbGetD(tlv.valDat, 28);
        return false;
    }

    /**
     * create senderTspec/flowSpec option
     *
     * @param pck packet to use
     * @param flowSpec true=flowSpec, false=senderTspec
     */
    public void createFlwSpc(packHolder pck, boolean flowSpec) {
        int i;
        if (flowSpec) {
            tlv.valTyp = 0x0902;
            i = 0x0500;
        } else {
            tlv.valTyp = 0x0c02;
            i = 0x0100;
        }
        bits.msbPutW(tlv.valDat, 0, 0); // version
        bits.msbPutW(tlv.valDat, 2, 7); // size
        bits.msbPutW(tlv.valDat, 4, i); // header type
        bits.msbPutW(tlv.valDat, 6, 6); // size
        bits.msbPutW(tlv.valDat, 8, 0x7f00); // parameter id
        bits.msbPutW(tlv.valDat, 10, 5); // size
        bits.msbPutD(tlv.valDat, 12, Float.floatToIntBits(flwSpcRate));
        bits.msbPutD(tlv.valDat, 16, Float.floatToIntBits(flwSpcSize));
        bits.msbPutD(tlv.valDat, 20, Float.floatToIntBits(flwSpcPeak));
        bits.msbPutD(tlv.valDat, 24, flwSpcPlcd);
        bits.msbPutD(tlv.valDat, 28, flwSpcPcks);
        tlv.valSiz = 32;
        tlv.putThis(pck);
    }

    /**
     * parse session attribute option
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseSesAtr(packHolder pck) {
        if (!findTlv(pck, 0xcf07)) {
            sessExc = 0;
            sessInc = 0;
            sessMst = 0;
            sessStp = bits.getByte(tlv.valDat, 0); // setup priority
            sessHld = bits.getByte(tlv.valDat, 1); // hold priority
            sessFlg = bits.getByte(tlv.valDat, 2); // flags
            int len = bits.getByte(tlv.valDat, 3); // name length
            sessNam = new String(tlv.valDat, 4, len);
            return false;
        }
        if (!findTlv(pck, 0xcf01)) {
            sessExc = bits.msbGetD(tlv.valDat, 0); // exclude affinity
            sessInc = bits.msbGetD(tlv.valDat, 4); // include affinity
            sessMst = bits.msbGetD(tlv.valDat, 8); // must affinity
            sessStp = bits.getByte(tlv.valDat, 12); // setup priority
            sessHld = bits.getByte(tlv.valDat, 13); // hold priority
            sessFlg = bits.getByte(tlv.valDat, 14); // flags
            int len = bits.getByte(tlv.valDat, 15); // name length
            sessNam = new String(tlv.valDat, 16, len);
            return false;
        }
        return true;
    }

    /**
     * create session attribute option
     *
     * @param pck packet to use
     */
    public void createSesAtr(packHolder pck) {
        int len = sessNam.length();
        if ((sessExc == 0) && (sessInc == 0) && (sessMst == 0)) {
            bits.putByte(tlv.valDat, 0, sessStp); // setup priority
            bits.putByte(tlv.valDat, 1, sessHld); // hold priority
            bits.putByte(tlv.valDat, 2, sessFlg); // hold priority
            bits.putByte(tlv.valDat, 3, len); // name length
            tlv.valSiz = 4;
            tlv.valTyp = 0xcf07;
        } else {
            bits.msbPutD(tlv.valDat, 0, sessExc); // exclude affinity
            bits.msbPutD(tlv.valDat, 4, sessInc); // include affinity
            bits.msbPutD(tlv.valDat, 8, sessMst); // must affinity
            bits.putByte(tlv.valDat, 12, sessStp); // setup priority
            bits.putByte(tlv.valDat, 13, sessHld); // hold priority
            bits.putByte(tlv.valDat, 14, sessFlg); // hold priority
            bits.putByte(tlv.valDat, 15, len); // name length
            tlv.valSiz = 16;
            tlv.valTyp = 0xcf01;
        }
        bits.byteCopy(sessNam.getBytes(), 0, tlv.valDat, tlv.valSiz, len); // name
        tlv.valSiz += len;
        tlv.valDat[tlv.valSiz] = 0; // termination
        tlv.valSiz++;
        padUpTlv();
        tlv.putThis(pck);
    }

    /**
     * parse adspec option
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseAdSpec(packHolder pck) {
        if (findTlv(pck, 0x0d02)) {
            return true;
        }
        if (bits.msbGetW(tlv.valDat, 0) != 0) { // version
            return true;
        }
        if (bits.msbGetW(tlv.valDat, 2) < 9) { // size
            return true;
        }
        if (bits.msbGetW(tlv.valDat, 4) != 0x0100) { // header type
            return true;
        }
        if (bits.msbGetW(tlv.valDat, 6) < 8) { // size
            return true;
        }
        if (bits.msbGetW(tlv.valDat, 8) != 0x0400) { // header type
            return true;
        }
        if (bits.msbGetW(tlv.valDat, 10) < 1) { // size
            return true;
        }
        adsHops = bits.msbGetD(tlv.valDat, 12); // hop count
        if (bits.msbGetW(tlv.valDat, 16) != 0x0600) { // header type
            return true;
        }
        if (bits.msbGetW(tlv.valDat, 18) < 1) { // size
            return true;
        }
        adsBndwdt = Float.intBitsToFloat(bits.msbGetD(tlv.valDat, 20));
        if (bits.msbGetW(tlv.valDat, 24) != 0x0800) { // header type
            return true;
        }
        if (bits.msbGetW(tlv.valDat, 26) < 1) { // size
            return true;
        }
        adsLtncy = bits.msbGetD(tlv.valDat, 28); // minimum path latency
        if (bits.msbGetW(tlv.valDat, 32) != 0x0a00) { // header type
            return true;
        }
        if (bits.msbGetW(tlv.valDat, 34) < 1) { // size
            return true;
        }
        adsCmtu = bits.msbGetD(tlv.valDat, 36); // composed mtu
        return false;
    }

    /**
     * create adspec option
     *
     * @param pck packet to use
     */
    public void createAdSpec(packHolder pck) {
        bits.msbPutW(tlv.valDat, 0, 0); // version
        bits.msbPutW(tlv.valDat, 2, 10); // size
        bits.msbPutW(tlv.valDat, 4, 0x100); // header type
        bits.msbPutW(tlv.valDat, 6, 8); // size
        bits.msbPutW(tlv.valDat, 8, 0x400); // header type
        bits.msbPutW(tlv.valDat, 10, 1); // size
        bits.msbPutD(tlv.valDat, 12, adsHops); // hop count
        bits.msbPutW(tlv.valDat, 16, 0x600); // header type
        bits.msbPutW(tlv.valDat, 18, 1); // size
        bits.msbPutD(tlv.valDat, 20, Float.floatToIntBits(adsBndwdt)); // bandwidth
        bits.msbPutW(tlv.valDat, 24, 0x800); // header type
        bits.msbPutW(tlv.valDat, 26, 1); // size
        bits.msbPutD(tlv.valDat, 28, adsLtncy); // minimum path latency
        bits.msbPutW(tlv.valDat, 32, 0xa00); // header type
        bits.msbPutW(tlv.valDat, 34, 1); // size
        bits.msbPutD(tlv.valDat, 36, adsCmtu); // composed mtu
        bits.msbPutW(tlv.valDat, 40, 0x500); // header type
        bits.msbPutW(tlv.valDat, 42, 0); // size
        tlv.valSiz = 44;
        tlv.valTyp = 0x0d02;
        tlv.putThis(pck);
    }

    private List<tabHop> parseHops() {
        packHolder pck = new packHolder(true, true);
        pck.putCopy(tlv.valDat, 0, 0, tlv.valSiz);
        pck.putSkip(tlv.valSiz);
        pck.merge2beg();
        return tabHop.parseHops(pck);
    }

    private byte[] createHops(List<tabHop> lst) {
        packHolder pck = new packHolder(true, true);
        tabHop.createHops(pck, lst);
        return pck.getCopy();
    }

    /**
     * parse explicit route option
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseExpRout(packHolder pck) {
        if (findTlv(pck, 0x1401)) {
            return true;
        }
        expRout = parseHops();
        if (expRout == null) {
            return true;
        }
        return false;
    }

    /**
     * create explicit route option
     *
     * @param pck packet to use
     */
    public void createExpRout(packHolder pck) {
        byte[] buf = createHops(expRout);
        tlv.putBytes(pck, 0x1401, buf.length, buf);
    }

    /**
     * parse record route option
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseRecRout(packHolder pck) {
        if (findTlv(pck, 0x1501)) {
            return true;
        }
        recRout = parseHops();
        if (recRout == null) {
            return true;
        }
        return false;
    }

    /**
     * create record route option
     *
     * @param pck packet to use
     */
    public void createRecRout(packHolder pck) {
        if (recRout == null) {
            return;
        }
        byte[] buf = createHops(recRout);
        tlv.putBytes(pck, 0x1501, buf.length, buf);
    }

    /**
     * update record route
     *
     * @param adr address to add
     * @param strict true for strict, false for loose
     */
    public void updateRecRout(addrIP adr, boolean strict) {
        if (recRout == null) {
            return;
        }
        tabHop hop = new tabHop();
        hop.adr = adr.copyBytes();
        hop.strict = strict;
        recRout.add(0, hop);
    }

    private int getTypErr() {
        if (isIP4) {
            return 0x0601;
        } else {
            return 0x0602;
        }
    }

    /**
     * parse error option
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseErr(packHolder pck) {
        if (findTlv(pck, getTypErr())) {
            return true;
        }
        errAdr = new addrIP();
        if (isIP4) {
            addrIPv4 adr = new addrIPv4();
            adr.fromBuf(tlv.valDat, 0);
            errAdr.fromIPv4addr(adr);
            errCod = bits.msbGetD(tlv.valDat, addrIPv4.size);
        } else {
            addrIPv6 adr = new addrIPv6();
            adr.fromBuf(tlv.valDat, 0);
            errAdr.fromIPv6addr(adr);
            errCod = bits.msbGetD(tlv.valDat, addrIPv6.size);
        }
        return false;
    }

    /**
     * create error option
     *
     * @param pck packet to use
     */
    public void createErr(packHolder pck) {
        if (isIP4) {
            errAdr.toIPv4().toBuffer(tlv.valDat, 0);
            tlv.valSiz = addrIPv4.size;
        } else {
            errAdr.toIPv6().toBuffer(tlv.valDat, 0);
            tlv.valSiz = addrIPv6.size;
        }
        bits.msbPutD(tlv.valDat, tlv.valSiz, errCod);
        tlv.valSiz += 4;
        tlv.valTyp = getTypErr();
        tlv.putThis(pck);
    }

    /**
     * parse style option
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseStyle(packHolder pck) {
        if (findTlv(pck, 0x0801)) {
            return true;
        }
        styleVal = bits.msbGetD(tlv.valDat, 0);
        return false;
    }

    /**
     * create style option
     *
     * @param pck packet to use
     */
    public void createStyle(packHolder pck) {
        bits.msbPutD(tlv.valDat, 0, styleVal);
        tlv.valSiz = 4;
        tlv.valTyp = 0x0801;
        tlv.putThis(pck);
    }

    /**
     * parse label option
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseLabel(packHolder pck) {
        if (findTlv(pck, 0x1001)) {
            return true;
        }
        labelVal = bits.msbGetD(tlv.valDat, 0);
        return false;
    }

    /**
     * create label option
     *
     * @param pck packet to use
     */
    public void createLabel(packHolder pck) {
        bits.msbPutD(tlv.valDat, 0, labelVal);
        tlv.valSiz = 4;
        tlv.valTyp = 0x1001;
        tlv.putThis(pck);
    }

    private int getTypS2LsubLsp() {
        if (isIP4) {
            return 0x3201;
        } else {
            return 0x3202;
        }
    }

    /**
     * parse s2l sub option
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseS2Lsub(packHolder pck) {
        subAddr = new addrIP();
        if (!isP2MP) {
            return false;
        }
        if (findTlv(pck, getTypS2LsubLsp())) {
            return true;
        }
        if (isIP4) {
            addrIPv4 adr = new addrIPv4();
            adr.fromBuf(tlv.valDat, 0);
            subAddr.fromIPv4addr(adr);
        } else {
            addrIPv6 adr = new addrIPv6();
            adr.fromBuf(tlv.valDat, 0);
            subAddr.fromIPv6addr(adr);
        }
        return false;
    }

    /**
     * create s2l sub option
     *
     * @param pck packet to use
     */
    public void createS2Lsub(packHolder pck) {
        if (!isP2MP) {
            return;
        }
        if (isIP4) {
            subAddr.toIPv4().toBuffer(tlv.valDat, 0);
            tlv.valSiz = addrIPv4.size;
        } else {
            subAddr.toIPv6().toBuffer(tlv.valDat, 0);
            tlv.valSiz = addrIPv6.size;
        }
        tlv.valTyp = getTypS2LsubLsp();
        tlv.putThis(pck);
    }

    private int getTypAssoc(boolean ext) {
        int i;
        if (isIP4) {
            i = 0xc701;
        } else {
            i = 0xc702;
        }
        if (ext) {
            i += 2;
        }
        return i;
    }

    /**
     * parse association option
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseAssoc(packHolder pck) {
        boolean ext = false;
        if (findTlv(pck, getTypAssoc(false))) {
            ext = true;
            if (findTlv(pck, getTypAssoc(true))) {
                return false;
            }
        }
        assocTyp = bits.msbGetW(tlv.valDat, 0);
        assocId = bits.msbGetW(tlv.valDat, 2);
        assocAdr = new addrIP();
        int pos = 4;
        if (isIP4) {
            addrIPv4 adr = new addrIPv4();
            adr.fromBuf(tlv.valDat, pos);
            assocAdr.fromIPv4addr(adr);
            pos += addrIPv4.size;
        } else {
            addrIPv6 adr = new addrIPv6();
            adr.fromBuf(tlv.valDat, pos);
            assocAdr.fromIPv6addr(adr);
            pos += addrIPv6.size;
        }
        if (!ext) {
            return false;
        }
        assocGlb = bits.msbGetD(tlv.valDat, pos);
        return false;
    }

    /**
     * create association option
     *
     * @param pck packet to use
     */
    public void createAssoc(packHolder pck) {
        if (assocAdr == null) {
            return;
        }
        bits.msbPutW(tlv.valDat, 0, assocTyp);
        bits.msbPutW(tlv.valDat, 2, assocId);
        tlv.valSiz = 4;
        if (isIP4) {
            assocAdr.toIPv4().toBuffer(tlv.valDat, tlv.valSiz);
            tlv.valSiz += addrIPv4.size;
        } else {
            assocAdr.toIPv6().toBuffer(tlv.valDat, tlv.valSiz);
            tlv.valSiz += addrIPv6.size;
        }
        boolean ext = assocGlb != 0;
        if (ext) {
            bits.msbPutD(tlv.valDat, tlv.valSiz, assocGlb);
            tlv.valSiz += 4;
        }
        tlv.valTyp = getTypAssoc(ext);
        tlv.putThis(pck);
    }

    /**
     * parse path request
     *
     * @param pck packet to read
     * @return false on success, true on error
     */
    public boolean parseDatPatReq(packHolder pck) {
        if (typ != typPathReq) {
            return true;
        }
        if (parseSess(pck)) {
            return true;
        }
        if (parseHop(pck)) {
            return true;
        }
        if (parseTime(pck)) {
            return true;
        }
        if (parseExpRout(pck)) {
            return true;
        }
        if (parseLabReq(pck)) {
            return true;
        }
        if (parseSesAtr(pck)) {
            return true;
        }
        if (parseSndrTmp(pck, false)) {
            return true;
        }
        if (parseFlwSpc(pck, false)) {
            return true;
        }
        if (parseAdSpec(pck)) {
            return true;
        }
        if (parseS2Lsub(pck)) {
            return true;
        }
        if (parseAssoc(pck)) {
            return true;
        }
        parseRecRout(pck);
        return false;
    }

    /**
     * create path request
     *
     * @param pck packet to use
     */
    public void createDatPatReq(packHolder pck) {
        typ = typPathReq;
        createSess(pck);
        createHop(pck);
        createTime(pck);
        createExpRout(pck);
        createLabReq(pck);
        createSesAtr(pck);
        createAssoc(pck);
        createSndrTmp(pck, false);
        createFlwSpc(pck, false);
        createAdSpec(pck);
        createS2Lsub(pck);
        createRecRout(pck);
    }

    /**
     * parse path tear
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseDatPatTer(packHolder pck) {
        if (typ != typPathTear) {
            return true;
        }
        if (parseSess(pck)) {
            return true;
        }
        if (parseHop(pck)) {
            return true;
        }
        if (parseSndrTmp(pck, false)) {
            return true;
        }
        if (parseFlwSpc(pck, false)) {
            return true;
        }
        if (parseAdSpec(pck)) {
            return true;
        }
        if (parseS2Lsub(pck)) {
            return true;
        }
        return false;
    }

    /**
     * create path tear
     *
     * @param pck packet to use
     */
    public void createDatPatTer(packHolder pck) {
        typ = typPathTear;
        createSess(pck);
        createHop(pck);
        createSndrTmp(pck, false);
        createFlwSpc(pck, false);
        createAdSpec(pck);
        createS2Lsub(pck);
    }

    /**
     * parse path error
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseDatPatErr(packHolder pck) {
        if (typ != typPathErr) {
            return true;
        }
        if (parseSess(pck)) {
            return true;
        }
        if (parseErr(pck)) {
            return true;
        }
        if (parseSndrTmp(pck, false)) {
            return true;
        }
        if (parseFlwSpc(pck, false)) {
            return true;
        }
        if (parseAdSpec(pck)) {
            return true;
        }
        if (parseS2Lsub(pck)) {
            return true;
        }
        return false;
    }

    /**
     * create path error
     *
     * @param pck packet to use
     */
    public void createDatPatErr(packHolder pck) {
        typ = typPathErr;
        createSess(pck);
        createErr(pck);
        createSndrTmp(pck, false);
        createFlwSpc(pck, false);
        createAdSpec(pck);
        createS2Lsub(pck);
    }

    /**
     * parse reservation request
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseDatResReq(packHolder pck) {
        if (typ != typResvReq) {
            return true;
        }
        if (parseSess(pck)) {
            return true;
        }
        if (parseHop(pck)) {
            return true;
        }
        if (parseTime(pck)) {
            return true;
        }
        if (parseStyle(pck)) {
            return true;
        }
        if (parseFlwSpc(pck, true)) {
            return true;
        }
        if (parseSndrTmp(pck, true)) {
            return true;
        }
        if (parseLabel(pck)) {
            return true;
        }
        if (parseS2Lsub(pck)) {
            return true;
        }
        parseRecRout(pck);
        return false;
    }

    /**
     * create reservation request
     *
     * @param pck packet to use
     */
    public void createDatResReq(packHolder pck) {
        typ = typResvReq;
        createSess(pck);
        createHop(pck);
        createTime(pck);
        createStyle(pck);
        createFlwSpc(pck, true);
        createSndrTmp(pck, true);
        createLabel(pck);
        createS2Lsub(pck);
        createRecRout(pck);
    }

    /**
     * parse reservation tear
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseDatResTer(packHolder pck) {
        if (typ != typResvTear) {
            return true;
        }
        if (parseSess(pck)) {
            return true;
        }
        if (parseHop(pck)) {
            return true;
        }
        if (parseStyle(pck)) {
            return true;
        }
        if (parseFlwSpc(pck, true)) {
            return true;
        }
        if (parseSndrTmp(pck, true)) {
            return true;
        }
        if (parseS2Lsub(pck)) {
            return true;
        }
        return false;
    }

    /**
     * create reservation tear
     *
     * @param pck packet to use
     */
    public void createDatResTer(packHolder pck) {
        typ = typResvTear;
        createSess(pck);
        createHop(pck);
        createStyle(pck);
        createFlwSpc(pck, true);
        createSndrTmp(pck, true);
        createS2Lsub(pck);
    }

    /**
     * parse reservation error
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseDatResErr(packHolder pck) {
        if (typ != typResvErr) {
            return true;
        }
        if (parseSess(pck)) {
            return true;
        }
        if (parseHop(pck)) {
            return true;
        }
        if (parseErr(pck)) {
            return true;
        }
        if (parseStyle(pck)) {
            return true;
        }
        if (parseFlwSpc(pck, true)) {
            return true;
        }
        if (parseSndrTmp(pck, true)) {
            return true;
        }
        if (parseS2Lsub(pck)) {
            return true;
        }
        return false;
    }

    /**
     * create reservation error
     *
     * @param pck packet to use
     */
    public void createDatResErr(packHolder pck) {
        typ = typResvErr;
        createSess(pck);
        createHop(pck);
        createErr(pck);
        createStyle(pck);
        createFlwSpc(pck, true);
        createSndrTmp(pck, true);
        createS2Lsub(pck);
    }

    /**
     * parse path request
     *
     * @param pck packet to read
     */
    public void parseDatAll(packHolder pck) {
        parseSess(pck);
        parseAdSpec(pck);
        parseErr(pck);
        parseExpRout(pck);
        parseFlwSpc(pck, true);
        parseFlwSpc(pck, false);
        parseHop(pck);
        parseLabReq(pck);
        parseLabel(pck);
        parseSesAtr(pck);
        parseAssoc(pck);
        parseStyle(pck);
        parseSndrTmp(pck, true);
        parseSndrTmp(pck, false);
        parseS2Lsub(pck);
        parseTime(pck);
    }

}
