package pack;

import tab.tabHop;
import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import ip.ipIfc4;
import ip.ipIfc6;
import java.util.List;
import java.util.ArrayList;
import util.bits;
import util.typLenVal;

/**
 * resource reservation protocol (rfc2205) packet
 *
 * @author matecsaba
 */
public class packRsvp {

    /**
     * protocol number
     */
    public static final int proto = 46;

    /**
     * header size
     */
    public static final int size = 8;

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
     * path request h->-m---t
     */
    public static final int typPathReq = 1;

    /**
     * resv request h---m-<-t
     */
    public static final int typResvReq = 2;

    /**
     * path error h-<-m---t
     */
    public static final int typPathErr = 3;

    /**
     * resv error h---m->-t
     */
    public static final int typResvErr = 4;

    /**
     * path tear h->-m---t
     */
    public static final int typPathTear = 5;

    /**
     * resv tear h---m-<-t
     */
    public static final int typResvTear = 6;

    /**
     * resv confirmation
     */
    public static final int typResvConf = 7;

    private typLenVal tlv1 = new typLenVal(16, 16, 0, 16, 1, 4, 4, 1, 0, 512, true);

    private typLenVal tlv2 = new typLenVal(0, 8, 8, 8, 1, 2, 2, 1, 0, 512, true);

    public String toString() {
        return "ip4=" + isIP4 + " p2mp=" + isP2MP + " typ=" + type2string(typ) + " ttl=" + ttl + " hop=" + hopAdr + "/" + hopId + " sess=" + sessAdr + "/" + sessId + " time=" + timeVal + " send=" + sndrAdr + "/" + sndrId + " subgrp=" + sbgrpOrg + "/" + sbgrpId + " subdst=" + subAddr + " req=" + labReq + " flow=" + flwSpcRate + "/" + flwSpcSize + "/" + flwSpcPeak + "/" + flwSpcPlcd + "/" + flwSpcPcks + " prio=" + sessStp + "/" + sessHld + " flg=" + sessFlg + " nam=" + sessNam + " hops=" + adsHops + " bndwdth=" + adsBndwdt + " latency=" + adsLtncy + " mtu=" + adsCmtu + " expRou=" + dumpHopList(expRout) + " recRou=" + dumpHopList(recRout) + " err=" + errAdr + "/" + errCod + " style=" + styleVal + " label=" + labelVal;
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

    private static String dumpHopList(List<tabHop> lst) {
        if (lst == null) {
            return "null";
        }
        String s = "";
        for (int i = 0; i < lst.size(); i++) {
            tabHop ntry = lst.get(i);
            s += " " + ntry;
        }
        return s;
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
        pck.IPttl = ttl + 1;
        pck.IPtos = 0;
        pck.IPprt = proto;
        pck.IPalrt = 1;
        isIP4 = pck.IPtrg.isIPv4();
        isP2MP = (sbgrpId != 0) || (!sbgrpOrg.isFilled(0));
    }

    private boolean findTlv1(packHolder pck, int typ) {
        int len = pck.dataSize();
        for (;;) {
            if (tlv1.getBytes(pck)) {
                pck.setBytesLeft(len);
                return true;
            }
            if (tlv1.valTyp != typ) {
                continue;
            }
            pck.setBytesLeft(len);
            return false;
        }
    }

    private void padUpTlv1() {
        for (; (tlv1.valSiz & 3) != 0;) {
            tlv1.valDat[tlv1.valSiz] = 0;
            tlv1.valSiz++;
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
        if (findTlv1(pck, getTypHop())) {
            return true;
        }
        hopAdr = new addrIP();
        if (isIP4) {
            addrIPv4 adr = new addrIPv4();
            adr.fromBuf(tlv1.valDat, 0);
            hopAdr.fromIPv4addr(adr);
            hopId = bits.msbGetD(tlv1.valDat, addrIPv4.size);
        } else {
            addrIPv6 adr = new addrIPv6();
            adr.fromBuf(tlv1.valDat, 0);
            hopAdr.fromIPv6addr(adr);
            hopId = bits.msbGetD(tlv1.valDat, addrIPv6.size);
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
            hopAdr.toIPv4().toBuffer(tlv1.valDat, 0);
            tlv1.valSiz = addrIPv4.size;
        } else {
            hopAdr.toIPv6().toBuffer(tlv1.valDat, 0);
            tlv1.valSiz = addrIPv6.size;
        }
        bits.msbPutD(tlv1.valDat, tlv1.valSiz, hopId);
        tlv1.valSiz += 4;
        tlv1.valTyp = getTypHop();
        tlv1.putThis(pck);
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
        if (findTlv1(pck, getTypSess())) {
            isP2MP = true;
            if (findTlv1(pck, getTypSess())) {
                return true;
            }
        }
        sessAdr = new addrIP();
        if (isIP4 || isP2MP) {
            addrIPv4 adr = new addrIPv4();
            adr.fromBuf(tlv1.valDat, 0);
            sessAdr.fromIPv4addr(adr);
            sessId = bits.msbGetQ(tlv1.valDat, addrIPv4.size);
        } else {
            addrIPv6 adr = new addrIPv6();
            adr.fromBuf(tlv1.valDat, 0);
            sessAdr.fromIPv6addr(adr);
            sessId = bits.msbGetQ(tlv1.valDat, addrIPv6.size);
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
            sessAdr.toIPv4().toBuffer(tlv1.valDat, 0);
            tlv1.valSiz = addrIPv4.size;
        } else {
            sessAdr.toIPv6().toBuffer(tlv1.valDat, 0);
            tlv1.valSiz = addrIPv6.size;
        }
        bits.msbPutQ(tlv1.valDat, tlv1.valSiz, sessId);
        tlv1.valSiz += 8;
        tlv1.valTyp = getTypSess();
        tlv1.putThis(pck);
    }

    /**
     * parse time option
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseTime(packHolder pck) {
        if (findTlv1(pck, 0x0501)) {
            return true;
        }
        timeVal = bits.msbGetD(tlv1.valDat, 0);
        return false;
    }

    /**
     * create time option
     *
     * @param pck packet to use
     */
    public void createTime(packHolder pck) {
        bits.msbPutD(tlv1.valDat, 0, timeVal);
        tlv1.valSiz = 4;
        tlv1.valTyp = 0x0501;
        tlv1.putThis(pck);
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
        if (findTlv1(pck, getTypSndrTmp(filtrSpec))) {
            return true;
        }
        sndrAdr = new addrIP();
        sbgrpOrg = new addrIP();
        if (isIP4) {
            addrIPv4 adr = new addrIPv4();
            adr.fromBuf(tlv1.valDat, 0);
            sndrAdr.fromIPv4addr(adr);
            sndrId = bits.msbGetD(tlv1.valDat, addrIPv4.size);
            if (isP2MP) {
                adr.fromBuf(tlv1.valDat, addrIPv4.size + 4);
                sbgrpOrg.fromIPv4addr(adr);
                sbgrpId = bits.msbGetD(tlv1.valDat, addrIPv4.size + addrIPv4.size + 4);
            }
        } else {
            addrIPv6 adr = new addrIPv6();
            adr.fromBuf(tlv1.valDat, 0);
            sndrAdr.fromIPv6addr(adr);
            sndrId = bits.msbGetD(tlv1.valDat, addrIPv6.size);
            if (isP2MP) {
                adr.fromBuf(tlv1.valDat, addrIPv6.size + 4);
                sbgrpOrg.fromIPv6addr(adr);
                sbgrpId = bits.msbGetD(tlv1.valDat, addrIPv6.size + addrIPv6.size + 4);
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
            sndrAdr.toIPv4().toBuffer(tlv1.valDat, 0);
            tlv1.valSiz = addrIPv4.size;
        } else {
            sndrAdr.toIPv6().toBuffer(tlv1.valDat, 0);
            tlv1.valSiz = addrIPv6.size;
        }
        bits.msbPutD(tlv1.valDat, tlv1.valSiz, sndrId);
        tlv1.valSiz += 4;
        if (isP2MP) {
            if (isIP4) {
                sbgrpOrg.toIPv4().toBuffer(tlv1.valDat, tlv1.valSiz);
                tlv1.valSiz += addrIPv4.size;
            } else {
                sbgrpOrg.toIPv6().toBuffer(tlv1.valDat, tlv1.valSiz);
                tlv1.valSiz += addrIPv6.size;
            }
            bits.msbPutD(tlv1.valDat, tlv1.valSiz, sbgrpId);
            tlv1.valSiz += 4;
        }
        tlv1.valTyp = getTypSndrTmp(filtrSpec);
        tlv1.putThis(pck);
    }

    /**
     * parse label request option
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseLabReq(packHolder pck) {
        if (findTlv1(pck, 0x1301)) {
            return true;
        }
        labReq = bits.msbGetD(tlv1.valDat, 0);
        return false;
    }

    /**
     * create label request option
     *
     * @param pck packet to use
     */
    public void createLabReq(packHolder pck) {
        bits.msbPutD(tlv1.valDat, 0, labReq);
        tlv1.valSiz = 4;
        tlv1.valTyp = 0x1301;
        tlv1.putThis(pck);
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
            if (findTlv1(pck, 0x0902)) {
                return true;
            }
            i = 0x0500;
        } else {
            if (findTlv1(pck, 0x0c02)) {
                return true;
            }
            i = 0x0100;
        }
        if (bits.msbGetW(tlv1.valDat, 0) != 0) { // version
            return true;
        }
        if (bits.msbGetW(tlv1.valDat, 2) < 7) { // size
            return true;
        }
        if (bits.msbGetW(tlv1.valDat, 4) != i) { // header type
            return true;
        }
        if (bits.msbGetW(tlv1.valDat, 6) < 6) { // size
            return true;
        }
        if (bits.msbGetW(tlv1.valDat, 8) != 0x7f00) { // parameter id
            return true;
        }
        if (bits.msbGetW(tlv1.valDat, 10) < 5) { // size
            return true;
        }
        flwSpcRate = Float.intBitsToFloat(bits.msbGetD(tlv1.valDat, 12));
        flwSpcSize = Float.intBitsToFloat(bits.msbGetD(tlv1.valDat, 16));
        flwSpcPeak = Float.intBitsToFloat(bits.msbGetD(tlv1.valDat, 20));
        flwSpcPlcd = bits.msbGetD(tlv1.valDat, 24);
        flwSpcPcks = bits.msbGetD(tlv1.valDat, 28);
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
            tlv1.valTyp = 0x0902;
            i = 0x0500;
        } else {
            tlv1.valTyp = 0x0c02;
            i = 0x0100;
        }
        bits.msbPutW(tlv1.valDat, 0, 0); // version
        bits.msbPutW(tlv1.valDat, 2, 7); // size
        bits.msbPutW(tlv1.valDat, 4, i); // header type
        bits.msbPutW(tlv1.valDat, 6, 6); // size
        bits.msbPutW(tlv1.valDat, 8, 0x7f00); // parameter id
        bits.msbPutW(tlv1.valDat, 10, 5); // size
        bits.msbPutD(tlv1.valDat, 12, Float.floatToIntBits(flwSpcRate));
        bits.msbPutD(tlv1.valDat, 16, Float.floatToIntBits(flwSpcSize));
        bits.msbPutD(tlv1.valDat, 20, Float.floatToIntBits(flwSpcPeak));
        bits.msbPutD(tlv1.valDat, 24, flwSpcPlcd);
        bits.msbPutD(tlv1.valDat, 28, flwSpcPcks);
        tlv1.valSiz = 32;
        tlv1.putThis(pck);
    }

    /**
     * parse session attribute option
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseSesAtr(packHolder pck) {
        if (findTlv1(pck, 0xcf07)) {
            return true;
        }
        sessStp = bits.getByte(tlv1.valDat, 0); // setup priority
        sessHld = bits.getByte(tlv1.valDat, 1); // hold priority
        sessFlg = bits.getByte(tlv1.valDat, 2); // flags
        int len = bits.getByte(tlv1.valDat, 3); // name length
        sessNam = new String(tlv1.valDat, 4, len);
        return false;
    }

    /**
     * create session attribute option
     *
     * @param pck packet to use
     */
    public void createSesAtr(packHolder pck) {
        int len = sessNam.length();
        bits.putByte(tlv1.valDat, 0, sessStp); // setup priority
        bits.putByte(tlv1.valDat, 1, sessHld); // hold priority
        bits.putByte(tlv1.valDat, 2, sessFlg); // hold priority
        bits.putByte(tlv1.valDat, 3, len); // name length
        bits.byteFill(tlv1.valDat, len, 16, 0); // padding
        bits.byteCopy(sessNam.getBytes(), 0, tlv1.valDat, 4, len); // name
        len += 4 - (len % 3);
        tlv1.valSiz = 4 + len;
        tlv1.valTyp = 0xcf07;
        padUpTlv1();
        tlv1.putThis(pck);
    }

    /**
     * parse adspec option
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseAdSpec(packHolder pck) {
        if (findTlv1(pck, 0x0d02)) {
            return true;
        }
        if (bits.msbGetW(tlv1.valDat, 0) != 0) { // version
            return true;
        }
        if (bits.msbGetW(tlv1.valDat, 2) < 9) { // size
            return true;
        }
        if (bits.msbGetW(tlv1.valDat, 4) != 0x0100) { // header type
            return true;
        }
        if (bits.msbGetW(tlv1.valDat, 6) < 8) { // size
            return true;
        }
        if (bits.msbGetW(tlv1.valDat, 8) != 0x0400) { // header type
            return true;
        }
        if (bits.msbGetW(tlv1.valDat, 10) < 1) { // size
            return true;
        }
        adsHops = bits.msbGetD(tlv1.valDat, 12); // hop count
        if (bits.msbGetW(tlv1.valDat, 16) != 0x0600) { // header type
            return true;
        }
        if (bits.msbGetW(tlv1.valDat, 18) < 1) { // size
            return true;
        }
        adsBndwdt = Float.intBitsToFloat(bits.msbGetD(tlv1.valDat, 20));
        if (bits.msbGetW(tlv1.valDat, 24) != 0x0800) { // header type
            return true;
        }
        if (bits.msbGetW(tlv1.valDat, 26) < 1) { // size
            return true;
        }
        adsLtncy = bits.msbGetD(tlv1.valDat, 28); // minimum path latency
        if (bits.msbGetW(tlv1.valDat, 32) != 0x0a00) { // header type
            return true;
        }
        if (bits.msbGetW(tlv1.valDat, 34) < 1) { // size
            return true;
        }
        adsCmtu = bits.msbGetD(tlv1.valDat, 36); // composed mtu
        return false;
    }

    /**
     * create adspec option
     *
     * @param pck packet to use
     */
    public void createAdSpec(packHolder pck) {
        bits.msbPutW(tlv1.valDat, 0, 0); // version
        bits.msbPutW(tlv1.valDat, 2, 10); // size
        bits.msbPutW(tlv1.valDat, 4, 0x100); // header type
        bits.msbPutW(tlv1.valDat, 6, 8); // size
        bits.msbPutW(tlv1.valDat, 8, 0x400); // header type
        bits.msbPutW(tlv1.valDat, 10, 1); // size
        bits.msbPutD(tlv1.valDat, 12, adsHops); // hop count
        bits.msbPutW(tlv1.valDat, 16, 0x600); // header type
        bits.msbPutW(tlv1.valDat, 18, 1); // size
        bits.msbPutD(tlv1.valDat, 20, Float.floatToIntBits(adsBndwdt)); // bandwidth
        bits.msbPutW(tlv1.valDat, 24, 0x800); // header type
        bits.msbPutW(tlv1.valDat, 26, 1); // size
        bits.msbPutD(tlv1.valDat, 28, adsLtncy); // minimum path latency
        bits.msbPutW(tlv1.valDat, 32, 0xa00); // header type
        bits.msbPutW(tlv1.valDat, 34, 1); // size
        bits.msbPutD(tlv1.valDat, 36, adsCmtu); // composed mtu
        bits.msbPutW(tlv1.valDat, 40, 0x500); // header type
        bits.msbPutW(tlv1.valDat, 42, 0); // size
        tlv1.valSiz = 44;
        tlv1.valTyp = 0x0d02;
        tlv1.putThis(pck);
    }

    private tabHop parseHop() {
        tabHop hop = new tabHop();
        hop.strict = (tlv2.valTyp & 0x80) == 0;
        switch (tlv2.valTyp & 0x7f) {
            case 1: // ipv4
                addrIPv4 adr4 = new addrIPv4();
                adr4.fromBuf(tlv2.valDat, 0);
                hop.adr.fromIPv4addr(adr4);
                break;
            case 2: // ipv6
                addrIPv6 adr6 = new addrIPv6();
                adr6.fromBuf(tlv2.valDat, 0);
                hop.adr.fromIPv6addr(adr6);
                break;
            default:
                return null;
        }
        return hop;
    }

    private void createHop(tabHop hop) {
        if (hop.adr.isIPv4()) {
            hop.adr.toIPv4().toBuffer(tlv2.valDat, 0);
            tlv2.valSiz = addrIPv4.size;
            tlv2.valTyp = 1;
        } else {
            hop.adr.toIPv6().toBuffer(tlv2.valDat, 0);
            tlv2.valSiz = addrIPv6.size;
            tlv2.valTyp = 2;
        }
        bits.putByte(tlv2.valDat, tlv2.valSiz + 0, tlv2.valSiz * 8); // prefix length
        bits.putByte(tlv2.valDat, tlv2.valSiz + 1, 0); // reserved
        tlv2.valSiz += 2;
        if (!hop.strict) {
            tlv2.valTyp |= 0x80;
        }
    }

    private List<tabHop> parseHops() {
        List<tabHop> lst = new ArrayList<tabHop>();
        packHolder pck = new packHolder(true, true);
        pck.putCopy(tlv1.valDat, 0, 0, tlv1.valSiz);
        pck.putSkip(tlv1.valSiz);
        pck.merge2beg();
        for (;;) {
            if (tlv2.getBytes(pck)) {
                break;
            }
            tabHop hop = parseHop();
            if (hop == null) {
                return null;
            }
            lst.add(hop);
        }
        return lst;
    }

    private byte[] createHops(List<tabHop> lst) {
        packHolder pck = new packHolder(true, true);
        for (int i = 0; i < lst.size(); i++) {
            tabHop ntry = lst.get(i);
            if (ntry == null) {
                continue;
            }
            createHop(ntry);
            tlv2.putThis(pck);
            pck.merge2end();
        }
        return pck.getCopy();
    }

    /**
     * parse explicit route option
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseExpRout(packHolder pck) {
        if (findTlv1(pck, 0x1401)) {
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
        tlv1.putBytes(pck, 0x1401, buf.length, buf);
    }

    /**
     * parse record route option
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseRecRout(packHolder pck) {
        if (findTlv1(pck, 0x1501)) {
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
        tlv1.putBytes(pck, 0x1501, buf.length, buf);
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
        if (findTlv1(pck, getTypErr())) {
            return true;
        }
        errAdr = new addrIP();
        if (isIP4) {
            addrIPv4 adr = new addrIPv4();
            adr.fromBuf(tlv1.valDat, 0);
            errAdr.fromIPv4addr(adr);
            errCod = bits.msbGetD(tlv1.valDat, addrIPv4.size);
        } else {
            addrIPv6 adr = new addrIPv6();
            adr.fromBuf(tlv1.valDat, 0);
            errAdr.fromIPv6addr(adr);
            errCod = bits.msbGetD(tlv1.valDat, addrIPv6.size);
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
            errAdr.toIPv4().toBuffer(tlv1.valDat, 0);
            tlv1.valSiz = addrIPv4.size;
        } else {
            errAdr.toIPv6().toBuffer(tlv1.valDat, 0);
            tlv1.valSiz = addrIPv6.size;
        }
        bits.msbPutD(tlv1.valDat, tlv1.valSiz, errCod);
        tlv1.valSiz += 4;
        tlv1.valTyp = getTypErr();
        tlv1.putThis(pck);
    }

    /**
     * parse style option
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseStyle(packHolder pck) {
        if (findTlv1(pck, 0x0801)) {
            return true;
        }
        styleVal = bits.msbGetD(tlv1.valDat, 0);
        return false;
    }

    /**
     * create style option
     *
     * @param pck packet to use
     */
    public void createStyle(packHolder pck) {
        bits.msbPutD(tlv1.valDat, 0, styleVal);
        tlv1.valSiz = 4;
        tlv1.valTyp = 0x0801;
        tlv1.putThis(pck);
    }

    /**
     * parse label option
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parseLabel(packHolder pck) {
        if (findTlv1(pck, 0x1001)) {
            return true;
        }
        labelVal = bits.msbGetD(tlv1.valDat, 0);
        return false;
    }

    /**
     * create label option
     *
     * @param pck packet to use
     */
    public void createLabel(packHolder pck) {
        bits.msbPutD(tlv1.valDat, 0, labelVal);
        tlv1.valSiz = 4;
        tlv1.valTyp = 0x1001;
        tlv1.putThis(pck);
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
        if (findTlv1(pck, getTypS2LsubLsp())) {
            return true;
        }
        if (isIP4) {
            addrIPv4 adr = new addrIPv4();
            adr.fromBuf(tlv1.valDat, 0);
            subAddr.fromIPv4addr(adr);
        } else {
            addrIPv6 adr = new addrIPv6();
            adr.fromBuf(tlv1.valDat, 0);
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
            subAddr.toIPv4().toBuffer(tlv1.valDat, 0);
            tlv1.valSiz = addrIPv4.size;
        } else {
            subAddr.toIPv6().toBuffer(tlv1.valDat, 0);
            tlv1.valSiz = addrIPv6.size;
        }
        tlv1.valTyp = getTypS2LsubLsp();
        tlv1.putThis(pck);
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
        parseRecRout(pck);
        if (parseS2Lsub(pck)) {
            return true;
        }
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
        createRecRout(pck);
        createS2Lsub(pck);
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
        parseStyle(pck);
        parseSndrTmp(pck, true);
        parseSndrTmp(pck, false);
        parseS2Lsub(pck);
        parseTime(pck);
    }

}
