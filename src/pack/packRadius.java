package pack;

import util.bits;
import util.typLenVal;
import addr.addrIP;
import addr.addrIPv4;
import addr.addrIPv6;
import cry.cryHashMd5;
import tab.tabGen;

/**
 * remote authentication dialin user (rfc2865) packet
 *
 * @author matecsaba
 */
public class packRadius {

    /**
     * port number
     */
    public final static int port = 1812;

    /**
     * size of header
     */
    public final static int size = 20;

    /**
     * shared secret
     */
    public String secret;

    /**
     * type of packet
     */
    public int code;

    /**
     * id number
     */
    public int idnt;

    /**
     * authenticator value
     */
    public byte[] auther;

    /**
     * access request
     */
    public static final int typeAccReq = 1;

    /**
     * access accept
     */
    public static final int typeAccAcc = 2;

    /**
     * access reject
     */
    public static final int typeAccRej = 3;

    /**
     * accounting request
     */
    public static final int typeAcoReq = 4;

    /**
     * accounting response
     */
    public static final int typeAcoRep = 5;

    /**
     * access challenge
     */
    public static final int typeAccChl = 11;

    /**
     * status server
     */
    public static final int typeSttSrv = 12;

    /**
     * status client
     */
    public static final int typeSttCln = 13;

    /**
     * user name
     */
    public static final int tlvUsrNam = 1;

    /**
     * user password
     */
    public static final int tlvUsrPwd = 2;

    /**
     * chap password
     */
    public static final int tlvChpPwd = 3;

    /**
     * nas ip address
     */
    public static final int tlvNasAdr = 4;

    /**
     * nas port
     */
    public static final int tlvNasPrt = 5;

    /**
     * service type
     */
    public static final int tlvSrvTyp = 6;

    /**
     * framed protocol
     */
    public static final int tlvFrmPrt = 7;

    /**
     * framed ip addresss
     */
    public static final int tlvFrmAdr = 8;

    /**
     * framed ip netmask
     */
    public static final int tlvFrmMsk = 9;

    /**
     * framed routing
     */
    public static final int tlvFrmRtn = 10;

    /**
     * filter id
     */
    public static final int tlvFilter = 11;

    /**
     * framed mtu
     */
    public static final int tlvFrmMtu = 12;

    /**
     * framed compression
     */
    public static final int tlvFrmCmp = 13;

    /**
     * login ip host
     */
    public static final int tlvLgnHst = 14;

    /**
     * login service
     */
    public static final int tlvLgnSvc = 15;

    /**
     * login tcp port
     */
    public static final int tlvLgnTcp = 16;

    /**
     * reply message
     */
    public static final int tlvReply = 18;

    /**
     * callback number
     */
    public static final int tlvCllNum = 19;

    /**
     * callback id
     */
    public static final int tlvCllId = 20;

    /**
     * framed route
     */
    public static final int tlvFrmRou = 22;

    /**
     * framed ipx network
     */
    public static final int tlvFrmIpx = 23;

    /**
     * state
     */
    public static final int tlvState = 24;

    /**
     * class
     */
    public static final int tlvClass = 25;

    /**
     * vendor specific
     */
    public static final int tlvVendor = 26;

    /**
     * session timeout
     */
    public static final int tlvSesTim = 27;

    /**
     * idle timeout
     */
    public static final int tlvIdlTim = 28;

    /**
     * termination action
     */
    public static final int tlvTrmAct = 29;

    /**
     * called station id
     */
    public static final int tlvCalled = 30;

    /**
     * calling station id
     */
    public static final int tlvCalling = 31;

    /**
     * nas identifier
     */
    public static final int tlvNasId = 32;

    /**
     * proxy state
     */
    public static final int tlvPrxSta = 33;

    /**
     * login lat service
     */
    public static final int tlvLatSrv = 34;

    /**
     * login lat mode
     */
    public static final int tlvLatMod = 35;

    /**
     * login lat group
     */
    public static final int tlvLatGrp = 36;

    /**
     * framed appletalk link
     */
    public static final int tlvFrmLnk = 37;

    /**
     * framed appletalk network
     */
    public static final int tlvFrmNet = 38;

    /**
     * framed appletalk zone
     */
    public static final int tlvFrmZon = 39;

    /**
     * chap challenge
     */
    public static final int tlvChpChl = 60;

    /**
     * nas port type
     */
    public static final int tlvPrtTyp = 61;

    /**
     * port limit
     */
    public static final int tlvPrtLim = 62;

    /**
     * login lat port
     */
    public static final int tlvLatPrt = 63;

    /**
     * user name
     */
    public String valUsrNam = null;

    /**
     * user password
     */
    public String valUsrPwd = null;

    /**
     * chap id
     */
    public int valChpIdn = -1;

    /**
     * chap password
     */
    public byte[] valChpPwd = null;

    /**
     * nas ip address
     */
    public addrIP valNasAdr = null;

    /**
     * nas port
     */
    public int valNasPrt = -1;

    /**
     * service type
     */
    public int valSrvTyp = -1;

    /**
     * framed protocol
     */
    public int valFrmPrt = -1;

    /**
     * framed ip addresss
     */
    public addrIP valFrmAdr = null;

    /**
     * framed ip netmask
     */
    public addrIP valFrmMsk = null;

    /**
     * framed routing
     */
    public int valFrmRtn = -1;

    /**
     * filter id
     */
    public String valFilter = null;

    /**
     * framed mtu
     */
    public int valFrmMtu = -1;

    /**
     * framed compression
     */
    public int valFrmCmp = -1;

    /**
     * login ip host
     */
    public addrIP valLgnHst = null;

    /**
     * login service
     */
    public int valLgnSvc = -1;

    /**
     * login tcp port
     */
    public int valLgnTcp = -1;

    /**
     * reply message
     */
    public String valReply = null;

    /**
     * callback number
     */
    public String valCllNum = null;

    /**
     * callback id
     */
    public String valCllId = null;

    /**
     * framed route
     */
    public String valFrmRou = null;

    /**
     * framed ipx network
     */
    public int valFrmIpx = -1;

    /**
     * state
     */
    public String valState = null;

    /**
     * class
     */
    public String valClass = null;

    /**
     * session timeout
     */
    public int valSesTim = -1;

    /**
     * idle timeout
     */
    public int valIdlTim = -1;

    /**
     * termination action
     */
    public int valTrmAct = -1;

    /**
     * called station id
     */
    public String valCalled = null;

    /**
     * calling station id
     */
    public String valCalling = null;

    /**
     * nas identifier
     */
    public String valNasId = null;

    /**
     * proxy state
     */
    public String valPrxSta = null;

    /**
     * login lat service
     */
    public String valLatSrv = null;

    /**
     * login lat mode
     */
    public String valLatMod = null;

    /**
     * login lat group
     */
    public String valLatGrp = null;

    /**
     * framed appletalk link
     */
    public int valFrmLnk = -1;

    /**
     * framed appletalk network
     */
    public int valFrmNet = -1;

    /**
     * framed appletalk zone
     */
    public String valFrmZon = null;

    /**
     * chap challenge
     */
    public byte[] valChpChl = null;

    /**
     * nas port type
     */
    public int valPrtTyp = -1;

    /**
     * port limit
     */
    public int valPrtLim = -1;

    /**
     * login lat port
     */
    public String valLatPrt = null;

    private typLenVal tlv = new typLenVal(0, 8, 8, 8, 1, 2, 2, 1, 0, 512, true);

    /**
     * convert code to string
     *
     * @param i code to convert
     * @return string
     */
    public String code2string(int i) {
        switch (i) {
            case typeAccReq:
                return "access request";
            case typeAccAcc:
                return "access accept";
            case typeAccRej:
                return "access reject";
            case typeAcoReq:
                return "accounting request";
            case typeAcoRep:
                return "accounting response";
            case typeAccChl:
                return "access challenge";
            case typeSttSrv:
                return "status server";
            case typeSttCln:
                return "status client";
            default:
                return "unknown=" + i;
        }
    }

    private addrIP getAddr(boolean mask) {
        if (tlv.valSiz == addrIPv4.size) {
            addrIPv4 adr4 = new addrIPv4();
            adr4.fromBuf(tlv.valDat, 0);
            addrIP adr = new addrIP();
            if (mask) {
                adr.fromIPv4mask(adr4);
            } else {
                adr.fromIPv4addr(adr4);
            }
            return adr;
        }
        if (tlv.valSiz == addrIPv6.size) {
            addrIPv6 adr6 = new addrIPv6();
            adr6.fromBuf(tlv.valDat, 0);
            addrIP adr = new addrIP();
            if (mask) {
                adr.fromIPv6mask(adr6);
            } else {
                adr.fromIPv6addr(adr6);
            }
            return adr;
        }
        return null;
    }

    private int getInt() {
        return bits.msbGetD(tlv.valDat, 0);
    }

    private String getStr() {
        return tlv.getStr();
    }

    /**
     * parse packet
     *
     * @param pck packet to use
     * @return false on success, true on error
     */
    public boolean parsePacket(packHolder pck) {
        code = pck.getByte(0);
        idnt = pck.getByte(1);
        int i = pck.msbGetW(2);
        if (i > pck.dataSize()) {
            return true;
        }
        pck.setBytesLeft(i);
        auther = new byte[16];
        pck.getCopy(auther, 0, 4, auther.length);
        pck.getSkip(size);
        int siz = pck.dataSize();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            switch (tlv.valTyp) {
                case tlvUsrNam:
                    valUsrNam = tlv.getStr();
                    break;
                case tlvUsrPwd:
                    valUsrPwd = passwdDecrypt(secret, auther, tlv.copyBytes());
                    break;
                case tlvChpPwd:
                    valChpIdn = tlv.valDat[0];
                    valChpPwd = new byte[tlv.valSiz - 1];
                    bits.byteCopy(tlv.valDat, 1, valChpPwd, 0, valChpPwd.length);
                    break;
                case tlvNasAdr:
                    valNasAdr = getAddr(false);
                    break;
                case tlvNasPrt:
                    valNasPrt = getInt();
                    break;
                case tlvSrvTyp:
                    valSrvTyp = getInt();
                    break;
                case tlvFrmPrt:
                    valFrmPrt = getInt();
                    break;
                case tlvFrmAdr:
                    valFrmAdr = getAddr(false);
                    break;
                case tlvFrmMsk:
                    valFrmMsk = getAddr(true);
                    break;
                case tlvFrmRtn:
                    valFrmRtn = getInt();
                    break;
                case tlvFilter:
                    valFilter = getStr();
                    break;
                case tlvFrmMtu:
                    valFrmMtu = getInt();
                    break;
                case tlvFrmCmp:
                    valFrmCmp = getInt();
                    break;
                case tlvLgnHst:
                    valLgnHst = getAddr(false);
                    break;
                case tlvLgnSvc:
                    valLgnSvc = getInt();
                    break;
                case tlvLgnTcp:
                    valLgnTcp = getInt();
                    break;
                case tlvReply:
                    valReply = getStr();
                    break;
                case tlvCllNum:
                    valCllNum = getStr();
                    break;
                case tlvCllId:
                    valCllId = getStr();
                    break;
                case tlvFrmRou:
                    valFrmRou = getStr();
                    break;
                case tlvFrmIpx:
                    valFrmIpx = getInt();
                    break;
                case tlvState:
                    valState = getStr();
                    break;
                case tlvClass:
                    valClass = getStr();
                    break;
                case tlvSesTim:
                    valSesTim = getInt();
                    break;
                case tlvIdlTim:
                    valIdlTim = getInt();
                    break;
                case tlvTrmAct:
                    valTrmAct = getInt();
                    break;
                case tlvCalled:
                    valCalled = getStr();
                    break;
                case tlvCalling:
                    valCalling = getStr();
                    break;
                case tlvNasId:
                    valNasId = getStr();
                    break;
                case tlvPrxSta:
                    valPrxSta = getStr();
                    break;
                case tlvLatSrv:
                    valLatSrv = getStr();
                    break;
                case tlvLatMod:
                    valLatMod = getStr();
                    break;
                case tlvLatGrp:
                    valLatGrp = getStr();
                    break;
                case tlvFrmLnk:
                    valFrmLnk = getInt();
                    break;
                case tlvFrmNet:
                    valFrmNet = getInt();
                    break;
                case tlvFrmZon:
                    valFrmZon = getStr();
                    break;
                case tlvChpChl:
                    valChpChl = tlv.copyBytes();
                    break;
                case tlvPrtTyp:
                    valPrtTyp = getInt();
                    break;
                case tlvPrtLim:
                    valPrtLim = getInt();
                    break;
                case tlvLatPrt:
                    valLatPrt = getStr();
                    break;
            }
        }
        pck.getSkip(-siz);
        return false;
    }

    private void putStr(packHolder pck, int typ, String str) {
        if (str == null) {
            return;
        }
        tlv.putStr(pck, typ, str);
    }

    private void putAddr(packHolder pck, int typ, addrIP adr) {
        if (adr == null) {
            return;
        }
        if (adr.isIPv4()) {
            addrIPv4 adr4 = adr.toIPv4();
            adr4.toBuffer(tlv.valDat, 0);
            tlv.valSiz = addrIPv4.size;
        } else {
            addrIPv6 adr6 = adr.toIPv6();
            adr6.toBuffer(tlv.valDat, 0);
            tlv.valSiz = addrIPv6.size;
        }
        tlv.putBytes(pck, typ);
    }

    private void putInt(packHolder pck, int typ, int val) {
        if (val == -1) {
            return;
        }
        bits.msbPutD(tlv.valDat, 0, val);
        tlv.valSiz = 4;
        tlv.putBytes(pck, typ);
    }

    /**
     * parse packet
     *
     * @param pck packet to use
     * @param auth update auther
     * @param vend vendors to pass, null if nothing
     */
    public void createPacket(packHolder pck, boolean auth, tabGen<packRadiusOption> vend) {
        pck.putByte(0, code);
        pck.putByte(1, idnt);
        pck.msbPutW(2, 0);
        pck.putCopy(auther, 0, 4, auther.length);
        pck.putSkip(size);
        putStr(pck, tlvUsrNam, valUsrNam);
        if (valUsrPwd != null) {
            tlv.putBytes(pck, tlvUsrPwd, passwdEncrypt(secret, auther, valUsrPwd));
        }
        if (valChpPwd != null) {
            tlv.valDat[0] = (byte) valChpIdn;
            bits.byteCopy(valChpPwd, 0, tlv.valDat, 1, valChpPwd.length);
            tlv.valSiz = valChpPwd.length + 1;
            tlv.putBytes(pck, tlvChpPwd);
        }
        if (valChpChl != null) {
            tlv.putBytes(pck, tlvChpChl, valChpChl);
        }
        putAddr(pck, tlvNasAdr, valNasAdr);
        putInt(pck, tlvNasPrt, valNasPrt);
        putInt(pck, tlvSrvTyp, valSrvTyp);
        putInt(pck, tlvFrmPrt, valFrmPrt);
        putAddr(pck, tlvFrmAdr, valFrmAdr);
        putAddr(pck, tlvFrmMsk, valFrmMsk);
        putInt(pck, tlvFrmRtn, valFrmRtn);
        putStr(pck, tlvFilter, valFilter);
        putInt(pck, tlvFrmMtu, valFrmMtu);
        putInt(pck, tlvFrmCmp, valFrmCmp);
        putAddr(pck, tlvLgnHst, valLgnHst);
        putInt(pck, tlvLgnSvc, valLgnSvc);
        putInt(pck, tlvLgnTcp, valLgnTcp);
        putStr(pck, tlvReply, valReply);
        putStr(pck, tlvCllNum, valCllNum);
        putStr(pck, tlvCllId, valCllId);
        putStr(pck, tlvFrmRou, valFrmRou);
        putInt(pck, tlvFrmIpx, valFrmIpx);
        putStr(pck, tlvState, valState);
        putStr(pck, tlvClass, valClass);
        putInt(pck, tlvSesTim, valSesTim);
        putInt(pck, tlvIdlTim, valIdlTim);
        putInt(pck, tlvTrmAct, valTrmAct);
        putStr(pck, tlvCalled, valCalled);
        putStr(pck, tlvCalling, valCalling);
        putStr(pck, tlvNasId, valNasId);
        putStr(pck, tlvPrxSta, valPrxSta);
        putStr(pck, tlvLatSrv, valLatSrv);
        putStr(pck, tlvLatMod, valLatMod);
        putStr(pck, tlvLatGrp, valLatGrp);
        putInt(pck, tlvFrmLnk, valFrmLnk);
        putInt(pck, tlvFrmNet, valFrmNet);
        putStr(pck, tlvFrmZon, valFrmZon);
        putInt(pck, tlvPrtTyp, valPrtTyp);
        putInt(pck, tlvPrtLim, valPrtLim);
        putStr(pck, tlvLatPrt, valLatPrt);
        if (vend != null) {
            for (int i = 0; i < vend.size(); i++) {
                packRadiusOption vnd = vend.get(i);
                tlv.putBytes(pck, tlvVendor, vnd.doEncode());
            }
        }
        pck.mergeHeader(0, size);
        pck.msbPutW(2 - size, pck.dataSize() + size);
        if (auth) {
            byte[] buf = calcReplyAuthen(code, idnt, secret, auther, pck.getCopy(), 0, pck.dataSize());
            pck.putCopy(buf, 0, 4 - size, buf.length);
        }
        pck.merge2beg();
    }

    /**
     * decrypt user password
     *
     * @param sec shared secret
     * @param auth authenticator
     * @param pwd ciphertext password
     * @return cleartext password
     */
    public static String passwdDecrypt(String sec, byte[] auth, byte[] pwd) {
        byte[] res = new byte[pwd.length];
        for (int o = 0; o < pwd.length;) {
            cryHashMd5 mdC = new cryHashMd5();
            mdC.init();
            mdC.update(sec.getBytes());
            mdC.update(auth);
            byte[] mdR = mdC.finish();
            auth = new byte[mdR.length];
            for (int i = 0; i < mdR.length; i++) {
                if ((o + i) >= pwd.length) {
                    break;
                }
                auth[i] = pwd[o + i];
                res[o + i] = (byte) (mdR[i] ^ pwd[o + i]);
            }
            o += mdR.length;
        }
        int o = res.length;
        for (int i = res.length - 1; i >= 0; i--) {
            if (res[i] == 0) {
                o = i;
            }
        }
        return new String(res, 0, o);
    }

    /**
     * encrypt user password
     *
     * @param sec shared secret
     * @param auth authenticator
     * @param pwd1 cleartext password
     * @return ciphertext password
     */
    public static byte[] passwdEncrypt(String sec, byte[] auth, String pwd1) {
        byte[] pwd2 = pwd1.getBytes();
        byte[] pwd3;
        if ((pwd2.length % 0xf) == 0) {
            pwd3 = pwd2;
        } else {
            pwd3 = new byte[pwd2.length + (0x10 - (pwd2.length & 0xf))];
            bits.byteFill(pwd3, 0, pwd3.length, 0);
            bits.byteCopy(pwd2, 0, pwd3, 0, pwd2.length);
        }
        byte[] res = new byte[pwd3.length];
        for (int p = 0; p < pwd3.length;) {
            cryHashMd5 mdC = new cryHashMd5();
            mdC.init();
            mdC.update(sec.getBytes());
            mdC.update(auth);
            auth = mdC.finish();
            for (int i = 0; i < auth.length; i++) {
                auth[i] ^= pwd3[p + i];
                bits.byteCopy(auth, 0, res, p, auth.length);
            }
            p += auth.length;
        }
        return res;
    }

    /**
     * calculate response authenticator
     *
     * @param code response code
     * @param id request id
     * @param sec secret
     * @param authen request authenticator
     * @param attrD response attributes
     * @param attrO offset of attributes
     * @param attrS size of attributes
     * @return calculated
     */
    public static byte[] calcReplyAuthen(int code, int id, String sec, byte[] authen, byte[] attrD, int attrO, int attrS) {
        byte[] buf = new byte[2];
        bits.msbPutW(buf, 0, attrS + size);
        cryHashMd5 mdC = new cryHashMd5();
        mdC.init();
        mdC.update(code);
        mdC.update(id);
        mdC.update(buf);
        mdC.update(authen);
        mdC.update(attrD, attrO, attrS);
        mdC.update(sec.getBytes());
        return mdC.finish();
    }

    /**
     * dump current packet
     *
     * @return packet dump
     */
    public String dump() {
        return code2string(code) + " id=" + idnt + " auth=" + bits.byteDump(auther, 0, -1) + " user=" + valUsrNam + " chap="
                + valChpIdn + " nasip=" + valNasAdr + " nasprt=" + valNasPrt + " service=" + valSrvTyp + " frnprt=" + valFrmPrt
                + " frmadr=" + valFrmAdr + " frnnsk=" + valFrmMsk + " frmrtr=" + valFrmRtn + " filter=" + valFilter + " mtu="
                + valFrmMtu + " comp=" + valFrmCmp + " logip=" + valLgnHst + " logserv=" + valLgnSvc + " logtcp=" + valLgnTcp
                + " reply=" + valReply + " cllbck=" + valCllNum + " callid=" + valCllId + " route=" + valFrmRou + " ipx="
                + valFrmIpx + " state=" + valState + " class=" + valClass + " sestim=" + valSesTim + " idltim=" + valIdlTim
                + " term=" + valTrmAct + " called=" + valCalled + " calling=" + valCalling + " nasid=" + valNasId + " latserv="
                + valLatSrv + " latmod=" + valLatMod + " latgrp=" + valLatGrp + " link=" + valFrmLnk + " net=" + valFrmNet
                + " zone=" + valFrmZon + " portyp=" + valPrtTyp + " prtlim=" + valPrtLim + " latprt=" + valLatPrt;
    }

}
