package pack;

import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import addr.addrPrefix;
import java.util.ArrayList;
import java.util.List;
import pipe.pipeSide;
import rtr.rtrBgpUtil;
import tab.tabRouteEntry;
import util.bits;
import util.counter;
import util.typLenVal;

/**
 * label distribution protocol (rfc5036) packet
 *
 * @author matecsaba
 */
public class packLdp {

    /**
     * port number
     */
    public static final int port = 646;

    /**
     * packet data
     */
    public packHolder pack = new packHolder(true, true);

    /**
     * connection
     */
    public pipeSide conn;

    /**
     * counter
     */
    public counter cntr;

    private typLenVal tlv = new typLenVal(0, 16, 16, 16, 1, 0, 4, 1, 0, 1024, true);

    /**
     * label switch id
     */
    public addrIPv4 lsrID = new addrIPv4();

    /**
     * label space id
     */
    public int labSpc;

    /**
     * size of pdu
     */
    public int pduSiz;

    /**
     * message type
     */
    public int msgTyp;

    /**
     * message id
     */
    public int msgID;

    /**
     * message size
     */
    public int msgSiz;

    /**
     * hold time
     */
    public int holdTime;

    /**
     * targeted hello
     */
    public boolean targeted;

    /**
     * transport address
     */
    public addrIP transAddr = new addrIP();

    /**
     * status
     */
    public int stat;

    /**
     * generic label
     */
    public int label;

    /**
     * list of prefixes
     */
    public List<addrPrefix<addrIP>> prfLst;

    /**
     * list of pseudowires
     */
    public List<packLdpPwe> pweLst;

    /**
     * list of (multi)point to multipoints
     */
    public List<packLdpMp> pmpLst;

    /**
     * version number
     */
    public static final int verNum = 1;

    /**
     * notification
     */
    public static final int msgTnotify = 0x0001;

    /**
     * hello
     */
    public static final int msgThello = 0x0100;

    /**
     * initialization
     */
    public static final int msgTinit = 0x0200;

    /**
     * keep alive
     */
    public static final int msgTkepAlv = 0x0201;

    /**
     * capability
     */
    public static final int msgTcapa = 0x0202;

    /**
     * address advertisement
     */
    public static final int msgTadrAdv = 0x0300;

    /**
     * address withdraw
     */
    public static final int msgTadrWdr = 0x0301;

    /**
     * label mapping
     */
    public static final int msgTlabMap = 0x0400;

    /**
     * label request
     */
    public static final int msgTlabReq = 0x0401;

    /**
     * label withdraw
     */
    public static final int msgTlabWdr = 0x0402;

    /**
     * label release
     */
    public static final int msgTlabRel = 0x0403;

    /**
     * label abort request
     */
    public static final int msgTlabAbr = 0x0404;

    /**
     * call setup
     */
    public static final int msgTcalStp = 0x0500;

    /**
     * call release
     */
    public static final int msgTcalRel = 0x0501;

    /**
     * forwarding equivalent class
     */
    public static final int tlvTfec = 0x0100;

    /**
     * address list
     */
    public static final int tlvTadrLst = 0x0101;

    /**
     * hop count
     */
    public static final int tlvThopCnt = 0x0103;

    /**
     * path vector
     */
    public static final int tlvTpatVec = 0x0104;

    /**
     * generic label
     */
    public static final int tlvTgenLab = 0x0200;

    /**
     * atm label
     */
    public static final int tlvTatmLab = 0x0201;

    /**
     * frame relay label
     */
    public static final int tlvTfrLab = 0x0202;

    /**
     * ft protection
     */
    public static final int tlvTftProt = 0x0202;

    /**
     * status
     */
    public static final int tlvTstatus = 0x0300;

    /**
     * extended status
     */
    public static final int tlvTextSta = 0x0301;

    /**
     * returned pdu
     */
    public static final int tlvTretPdu = 0x0302;

    /**
     * returned message
     */
    public static final int tlvTretMsg = 0x0303;

    /**
     * returned tlv
     */
    public static final int tlvTretTlv = 0x0304;

    /**
     * common hello parameters
     */
    public static final int tlvThello = 0x0400;

    /**
     * ipv4 transport address
     */
    public static final int tlvTip4adr = 0x0401;

    /**
     * configuration sequence number
     */
    public static final int tlvTcfgSeq = 0x0402;

    /**
     * ipv6 transport address
     */
    public static final int tlvTip6adr = 0x0403;

    /**
     * mac
     */
    public static final int tlvTmac = 0x0404;

    /**
     * common session parameters
     */
    public static final int tlvTsess = 0x0500;

    /**
     * atm session parameters
     */
    public static final int tlvTatmSes = 0x0501;

    /**
     * frame relay session parameters
     */
    public static final int tlvTfrSes = 0x0502;

    /**
     * ft session parameters
     */
    public static final int tlvTftSes = 0x0503;

    /**
     * ft ack parameters
     */
    public static final int tlvTftAck = 0x0504;

    /**
     * ft cork parameters
     */
    public static final int tlvTftCrk = 0x0505;

    /**
     * dynamic capability announcement
     */
    public static final int tlvTdynCap = 0x0506;

    /**
     * upstream label assignment
     */
    public static final int tlvTupstrm = 0x0507;

    /**
     * point-to-multipoint
     */
    public static final int tlvTp2mp = 0x8508;

    /**
     * multipoint-to-multipoint
     */
    public static final int tlvTmp2mp = 0x0509;

    /**
     * typed wildcard fec capability
     */
    public static final int tlvTtypFec = 0x050b;

    /**
     * returned message id
     */
    public static final int tlvTmsgID = 0x0600;

    /**
     * mtu
     */
    public static final int tlvTmtu = 0x0601;

    /**
     * unrecognized notification capability
     */
    public static final int tlvTbadNot = 0x0603;

    /**
     * pw status
     */
    public static final int tlvTpwStat = 0x096a;

    /**
     * pw parameters
     */
    public static final int tlvTpwParam = 0x096b;

    /**
     * pw group id
     */
    public static final int tlvTpwGroup = 0x096c;

    /**
     * prefix
     */
    public static final int fecTpref = 0x02;

    /**
     * p2mp
     */
    public static final int fecTp2mp = 0x06;

    /**
     * mp2mp up
     */
    public static final int fecTmp2mpUp = 0x07;

    /**
     * mp2mp down
     */
    public static final int fecTmp2mpDn = 0x08;

    /**
     * pseudowire
     */
    public static final int fecTpwe = 0x80;

    /**
     * generalized pseudowire
     */
    public static final int fecTgpw = 0x81;

    /**
     * convert type to string
     *
     * @param i message type
     * @return decoded string
     */
    public final String type2string(int i) {
        switch (i) {
            case msgTnotify:
                return "notification";
            case msgThello:
                return "hello";
            case msgTinit:
                return "initialization";
            case msgTkepAlv:
                return "keepAlive";
            case msgTcapa:
                return "capability";
            case msgTadrAdv:
                return "addressAdvertisement";
            case msgTadrWdr:
                return "addressWithdraw";
            case msgTlabMap:
                return "labelMapping";
            case msgTlabReq:
                return "labelRequest";
            case msgTlabWdr:
                return "labelWithdraw";
            case msgTlabRel:
                return "labelRelease";
            case msgTlabAbr:
                return "labelAbort";
            case msgTcalStp:
                return "callSetup";
            case msgTcalRel:
                return "callRelease";
            default:
                return "unknown=" + i;
        }
    }

    public String toString() {
        return "lsr=" + lsrID + ":" + labSpc + " typ=" + type2string(msgTyp) + " id=" + msgID;
    }

    /**
     * send current packet
     *
     * @return false on success, true on error
     */
    public boolean sendPack() {
        int i = pack.dataSize();
        cntr.tx(pack);
        return pack.pipeSend(conn, 0, i, 2) != i;
    }

    /**
     * receive one packet
     *
     * @return false on success, true on error
     */
    public boolean recvPack() {
        final int hdrSiz = 4;
        if (pack.pipeRecv(conn, 0, hdrSiz, 144) != hdrSiz) {
            return true;
        }
        int i = pack.msbGetW(2);
        if (pack.pipeRecv(conn, hdrSiz, i, 144) != i) {
            return true;
        }
        cntr.rx(pack);
        return false;
    }

    /**
     * parse ldp header
     *
     * @return false on success, true on error
     */
    public boolean parseLDPheader() {
        if (pack.dataSize() < 4) {
            return true;
        }
        int i = pack.msbGetW(0); // version
        if (i != verNum) {
            return true;
        }
        i = pack.msbGetW(2); // size
        pack.getSkip(4);
        if (i > pack.dataSize()) {
            return true;
        }
        pack.setDataSize(i);
        pack.getAddr(lsrID, 0); // lsr id
        labSpc = pack.msbGetW(addrIPv4.size); // label space
        pack.getSkip(addrIPv4.size + 2);
        pduSiz = pack.dataSize();
        pack.setDataSize(0);
        msgSiz = 0;
        return false;
    }

    /**
     * parse message header
     *
     * @return false on success, true on error
     */
    public boolean parseMSGheader() {
        pack.setBytesLeft(0);
        pack.setDataSize(pduSiz);
        if (pduSiz < 8) {
            return true;
        }
        msgTyp = pack.msbGetW(0); // message type
        int i = pack.msbGetW(2); // message size
        pack.getSkip(4);
        if (i > pack.dataSize()) {
            return true;
        }
        pack.setDataSize(i);
        msgID = pack.msbGetD(0);
        pack.getSkip(4);
        msgSiz = pack.dataSize();
        pduSiz -= msgSiz + 8;
        return false;
    }

    /**
     * create ldp header
     */
    public void createLDPheader() {
        pack.merge2beg();
        msgSiz = pack.dataSize();
        pack.msbPutW(0, verNum); // version
        pack.msbPutW(2, msgSiz + 14); // message size
        pack.putAddr(4, lsrID); // lsr id
        pack.msbPutW(8, labSpc); // label space
        pack.putSkip(10);
        pack.msbPutW(0, msgTyp); // message type
        pack.msbPutW(2, msgSiz + 4); // message size
        pack.msbPutD(4, msgID); // message id
        pack.putSkip(8);
        pack.merge2beg();
    }

    private boolean findTLV(int typ) {
        pack.setBytesLeft(msgSiz);
        for (;;) {
            if (tlv.getBytes(pack)) {
                return true;
            }
            if (tlv.valTyp != typ) {
                continue;
            }
            return false;
        }
    }

    /**
     *
     * put hello parameters
     */
    public void putHelloParam() {
        bits.msbPutW(tlv.valDat, 0, holdTime);
        int i = 0;
        if (targeted) {
            i = 0xc000;
        }
        bits.msbPutW(tlv.valDat, 2, i);
        tlv.valSiz = 4;
        tlv.putBytes(pack, tlvThello);
    }

    /**
     * read hello parameters
     *
     * @return false on success, true on error
     */
    public boolean getHelloParam() {
        if (findTLV(tlvThello)) {
            return true;
        }
        int i = bits.msbGetW(tlv.valDat, 2);
        targeted = (i & 0x8000) != 0;
        holdTime = bits.msbGetW(tlv.valDat, 2);
        return false;
    }

    /**
     * put transport address
     */
    public void putTransAddr() {
        if (transAddr.isIPv4()) {
            addrIPv4 adr = transAddr.toIPv4();
            adr.toBuffer(tlv.valDat, 0);
            tlv.valSiz = addrIPv4.size;
            tlv.putBytes(pack, tlvTip4adr);
        } else {
            addrIPv6 adr = transAddr.toIPv6();
            adr.toBuffer(tlv.valDat, 0);
            tlv.valSiz = addrIPv6.size;
            tlv.putBytes(pack, tlvTip6adr);
        }
    }

    /**
     * read transport address
     *
     * @return false on success, true on error
     */
    public boolean getTransAddr() {
        if (!findTLV(tlvTip4adr)) {
            addrIPv4 adr = new addrIPv4();
            adr.fromBuf(tlv.valDat, 0);
            transAddr.fromIPv4addr(adr);
            return false;
        }
        if (!findTLV(tlvTip6adr)) {
            addrIPv6 adr = new addrIPv6();
            adr.fromBuf(tlv.valDat, 0);
            transAddr.fromIPv6addr(adr);
            return false;
        }
        return true;
    }

    /**
     * put session parameters
     */
    public void putSessParam() {
        bits.msbPutW(tlv.valDat, 0, verNum); // version
        bits.msbPutW(tlv.valDat, 2, holdTime); // hold time
        bits.putByte(tlv.valDat, 4, 0); // bitmap
        bits.putByte(tlv.valDat, 5, 0); // path limit
        bits.msbPutW(tlv.valDat, 6, 0); // max pdu length
        lsrID.toBuffer(tlv.valDat, 8); // lsr id
        bits.msbPutW(tlv.valDat, 12, labSpc); // label space
        tlv.valSiz = 14;
        tlv.putBytes(pack, tlvTsess);
    }

    /**
     * put (multi)point2multipoint
     */
    public void putMP2MPparam() {
        tlv.valDat[0] = (byte) 0x80;
        tlv.valSiz = 1;
        tlv.putBytes(pack, 0x8000 | tlvTp2mp);
        tlv.putBytes(pack, 0x8000 | tlvTmp2mp);
    }

    /**
     * read session parameters
     *
     * @return false on success, true on error
     */
    public boolean getSessParam() {
        if (findTLV(tlvTsess)) {
            return true;
        }
        int i = bits.msbGetW(tlv.valDat, 0); // version
        if (i != verNum) {
            return true;
        }
        holdTime = bits.msbGetW(tlv.valDat, 2); // hold time
        i = bits.getByte(tlv.valDat, 4); // bitmap
        if ((i & 0x80) != 0) {
            return true;
        }
        i = bits.getByte(tlv.valDat, 5); // max path limit
        i = bits.msbGetW(tlv.valDat, 6); // max pdu length
        lsrID.fromBuf(tlv.valDat, 8); // lsr id
        labSpc = bits.msbGetW(tlv.valDat, 12); // label space
        return false;
    }

    /**
     * put address mapping message
     */
    public void putAddrMapping() {
        if (transAddr.isIPv4()) {
            bits.msbPutW(tlv.valDat, 0, rtrBgpUtil.safiIp4uni >>> 16);
            addrIPv4 adr = transAddr.toIPv4();
            adr.toBuffer(tlv.valDat, 2);
            tlv.valSiz = addrIPv4.size + 2;
        } else {
            bits.msbPutW(tlv.valDat, 0, rtrBgpUtil.safiIp6uni >>> 16);
            addrIPv6 adr = transAddr.toIPv6();
            adr.toBuffer(tlv.valDat, 2);
            tlv.valSiz = addrIPv6.size + 2;
        }
        tlv.putBytes(pack, tlvTadrLst);
    }

    /**
     * read session parameters
     *
     * @return false on success, true on error
     */
    public boolean getAddrMapping() {
        if (findTLV(tlvTadrLst)) {
            return true;
        }
        return false;
    }

    /**
     * put generic label
     */
    public void putGenLabel() {
        bits.msbPutD(tlv.valDat, 0, label & 0xfffff);
        tlv.valSiz = 4;
        tlv.putBytes(pack, tlvTgenLab);
    }

    /**
     * get generic label
     *
     * @return false on success, true on error
     */
    public boolean getGenLabel() {
        if (findTLV(tlvTgenLab)) {
            return true;
        }
        label = bits.msbGetD(tlv.valDat, 0) & 0xfffff;
        return false;
    }

    /**
     * put fec address
     *
     * @param prf entry to add
     */
    public void putFECaddr(addrPrefix<addrIP> prf) {
        tabRouteEntry<addrIP> pref = new tabRouteEntry<addrIP>();
        pref.prefix = prf;
        packHolder p = new packHolder(true, true);
        p.putByte(0, fecTpref); // fec type
        if (prf.network.isIPv4()) {
            p.msbPutW(1, rtrBgpUtil.afiIpv4 >>> 16);
            p.putSkip(3);
            rtrBgpUtil.writePrefix(rtrBgpUtil.safiIp4uni, p, pref);
        } else {
            p.msbPutW(1, rtrBgpUtil.afiIpv6 >>> 16);
            p.putSkip(3);
            rtrBgpUtil.writePrefix(rtrBgpUtil.safiIp6uni, p, pref);
        }
        p.merge2beg();
        byte[] buf = p.getCopy();
        tlv.putBytes(pack, tlvTfec, buf);
    }

    /**
     * put fec pseudowire
     *
     * @param pwe entry to add
     * @param justCore just core components
     */
    public void putFECpwe(packLdpPwe pwe, boolean justCore) {
        packHolder p = new packHolder(true, true);
        if (!pwe.general) {
            p.putByte(0, fecTpwe); // fec type
            p.putSkip(1);
            pwe.createFec1(p, justCore);
            p.merge2beg();
            byte[] buf = p.getCopy();
            tlv.putBytes(pack, tlvTfec, buf);
            return;
        }
        p.putByte(0, fecTgpw); // fec type
        p.putSkip(1);
        pwe.createFec2(p);
        p.merge2beg();
        byte[] buf = p.getCopy();
        tlv.putBytes(pack, tlvTfec, buf);
        if (justCore) {
            return;
        }
        p.clear();
        buf = pwe.createParams();
        tlv.putBytes(pack, tlvTpwParam, buf);
    }

    /**
     * put fec multipoint
     *
     * @param pmp entry to add
     */
    public void putFECpmp(packLdpMp pmp) {
        packHolder p = new packHolder(true, true);
        p.putByte(0, pmp.typ); // fec type
        p.putSkip(1);
        pmp.createFEC(p);
        p.merge2beg();
        byte[] buf = p.getCopy();
        tlv.putBytes(pack, tlvTfec, buf);
    }

    private addrPrefix<addrIP> getFECaddr(packHolder pck) {
        int i = pck.msbGetW(0) << 16; // afi
        pck.getSkip(2);
        tabRouteEntry<addrIP> res = rtrBgpUtil.readPrefix(i, false, pck);
        return res.prefix;
    }

    /**
     * get fec addresses
     *
     * @return list of prefixes, null on error
     */
    public boolean getFEClist() {
        if (findTLV(tlvTfec)) {
            return true;
        }
        packHolder pck = new packHolder(true, true);
        pck.putCopy(tlv.valDat, 0, 0, tlv.valSiz);
        pck.putSkip(tlv.valSiz);
        pck.merge2beg();
        prfLst = new ArrayList<addrPrefix<addrIP>>();
        pweLst = new ArrayList<packLdpPwe>();
        pmpLst = new ArrayList<packLdpMp>();
        for (;;) {
            if (pck.dataSize() < 1) {
                break;
            }
            int i = pck.getByte(0); // fec type
            pck.getSkip(1);
            switch (i) {
                case fecTpref:
                    addrPrefix<addrIP> pref = getFECaddr(pck);
                    prfLst.add(pref);
                    break;
                case fecTpwe:
                    packLdpPwe pwe = new packLdpPwe();
                    pwe.parseFec1(pck);
                    pweLst.add(pwe);
                    break;
                case fecTgpw:
                    pwe = new packLdpPwe();
                    pwe.parseFec2(pck);
                    if (!findTLV(tlvTpwParam)) {
                        packHolder par = new packHolder(true, true);
                        par.putCopy(tlv.valDat, 0, 0, tlv.valSiz);
                        par.putSkip(tlv.valSiz);
                        par.merge2beg();
                        pwe.parseParams(par);
                    }
                    pweLst.add(pwe);
                    break;
                case fecTp2mp:
                case fecTmp2mpUp:
                case fecTmp2mpDn:
                    packLdpMp pmp = new packLdpMp();
                    pmp.typ = i;
                    pmp.parseFEC(pck);
                    pmpLst.add(pmp);
                    break;
                default:
                    return true;
            }
        }
        return false;
    }

    /**
     * put returned message id
     */
    public void putRetMsgId() {
        bits.msbPutD(tlv.valDat, 0, msgID);
        tlv.valSiz = 4;
        tlv.putBytes(pack, tlvTmsgID);
    }

    /**
     * get returned message id
     *
     * @return false on success, true on error
     */
    public boolean getRetMsgId() {
        if (findTLV(tlvTmsgID)) {
            return true;
        }
        msgID = bits.msbGetD(tlv.valDat, 0);
        return false;
    }

    /**
     * put status
     */
    public void putStatus() {
        bits.msbPutD(tlv.valDat, 0, stat);
        bits.msbPutD(tlv.valDat, 4, msgID);
        bits.msbPutW(tlv.valDat, 8, msgTyp);
        tlv.valSiz = 10;
        tlv.putBytes(pack, tlvTstatus);
    }

    /**
     * get status
     *
     * @return false on success, true on error
     */
    public boolean getStatus() {
        if (findTLV(tlvTstatus)) {
            return true;
        }
        stat = bits.msbGetD(tlv.valDat, 0);
        msgID = bits.msbGetD(tlv.valDat, 4);
        msgTyp = bits.msbGetW(tlv.valDat, 8);
        return false;
    }

    /**
     * put pw status
     */
    public void putPwStatus() {
        bits.msbPutD(tlv.valDat, 0, stat);
        tlv.valSiz = 4;
        tlv.putBytes(pack, tlvTpwStat);
    }

    /**
     * get pw status
     *
     * @return false on success, true on error
     */
    public boolean getPwStatus() {
        if (findTLV(tlvTpwStat)) {
            return true;
        }
        stat = bits.msbGetD(tlv.valDat, 0);
        return false;
    }

}
