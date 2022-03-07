package net.freertr.pack;

import java.util.ArrayList;
import java.util.List;
import net.freertr.cry.cryCertificate;
import net.freertr.cry.cryECcurve;
import net.freertr.cry.cryECpoint;
import net.freertr.cry.cryEncrCBCaes;
import net.freertr.cry.cryEncrCBCdes;
import net.freertr.cry.cryEncrCBCdes3;
import net.freertr.cry.cryEncrChacha20poly1305;
import net.freertr.cry.cryEncrGCMaes;
import net.freertr.cry.cryEncrGeneric;
import net.freertr.cry.cryHashGeneric;
import net.freertr.cry.cryHashHmac;
import net.freertr.cry.cryHashMd5;
import net.freertr.cry.cryHashSha1;
import net.freertr.cry.cryHashSha2224;
import net.freertr.cry.cryHashSha2256;
import net.freertr.cry.cryHashSha2384;
import net.freertr.cry.cryHashSha2512;
import net.freertr.cry.cryHashSslMac;
import net.freertr.cry.cryKeyDH;
import net.freertr.cry.cryKeyDSA;
import net.freertr.cry.cryKeyECDH;
import net.freertr.cry.cryKeyRSA;
import net.freertr.cry.cryUtils;
import net.freertr.util.bits;
import net.freertr.util.debugger;
import net.freertr.util.logger;
import net.freertr.util.typLenVal;

/**
 * transport layer security (rfc5246) handshake packet
 *
 * @author matecsaba
 */
public class packTlsHndshk {

    /**
     * type of packet
     */
    public int pckTyp;

    /**
     * maximum version
     */
    public int maxVer;

    /**
     * mimumum version
     */
    public int minVer;

    /**
     * server name
     */
    public String servNam;

    /**
     * server random
     */
    public byte[] servRand;

    /**
     * client random
     */
    public byte[] clntRand;

    /**
     * received session id
     */
    public byte[] rcvdSess;

    /**
     * sending session id
     */
    public byte[] sendSess;

    /**
     * verify random
     */
    public byte[] vrfyRand;

    /**
     * list of ciphers
     */
    public List<Integer> cipherList;

    /**
     * list of compressors
     */
    public byte[] cmprssList;

    /**
     * list of extensions
     */
    public byte[] extnsnList;

    /**
     * selected cipher
     */
    public int cipherWant;

    /**
     * selected cipher
     */
    public int cipherDecoded;

    /**
     * selected compressor
     */
    public int cmprssWant;

    /**
     * list of certificates
     */
    public List<byte[]> certificates;

    /**
     * diffie hellman parameters
     */
    public cryKeyDH diffHell;

    /**
     * ec diffie hellman
     */
    public cryKeyECDH ecDiffHell;

    /**
     * signature hash
     */
    public int signHsh = -1;

    /**
     * signature data
     */
    public byte[] signDat;

    /**
     * pre master secret
     */
    public byte[] preMaster;

    /**
     * master secret
     */
    public byte[] masterSec;

    /**
     * key block data
     */
    public byte[] keyBlockD;

    /**
     * key block position
     */
    public int keyBlockP;

    /**
     * handshake secret client2server
     */
    public byte[] hndshkSC;

    /**
     * handshake secret server2client
     */
    public byte[] hndshkCS;

    /**
     * init vector client2server
     */
    public byte[] ivCS;

    /**
     * init vector server2client
     */
    public byte[] ivSC;

    /**
     * encryption client2server
     */
    public byte[] encCS;

    /**
     * encryption server2client
     */
    public byte[] encSC;

    /**
     * integrity client2server
     */
    public byte[] macCS;

    /**
     * integrity server2client
     */
    public byte[] macSC;

    /**
     * finished payload
     */
    public byte[] finished;

    /**
     * bytes of exchange
     */
    public List<Byte> xchgByte;

    /**
     * packets of exchange
     */
    public List<Integer> xchgPack;

    /**
     * rsa key
     */
    public cryKeyRSA keyrsa;

    /**
     * dss key
     */
    public cryKeyDSA keydsa;

    /**
     * rsa certificate
     */
    public cryCertificate certrsa;

    /**
     * dss certificate
     */
    public cryCertificate certdsa;

    /**
     * datagram mode
     */
    public boolean datagram;

    /**
     * server exchange hash
     */
    public byte[] paramHash;

    /**
     * server exchange oid
     */
    public cryHashGeneric paramHsh;

    /**
     * hello request
     */
    public final static int typeHeloReq = 0;

    /**
     * client hello
     */
    public final static int typeHeloClnt = 1;

    /**
     * server hello
     */
    public final static int typeHeloServ = 2;

    /**
     * verify hello
     */
    public final static int typeHeloVrfy = 3;

    /**
     * new session ticket
     */
    public final static int typeNewSesTck = 4;

    /**
     * encrypted extension
     */
    public final static int typeEncrExt = 8;

    /**
     * certificate data
     */
    public final static int typeCertDat = 11;

    /**
     * server key exchange
     */
    public final static int typeKexServ = 12;

    /**
     * certificate request
     */
    public final static int typeCertReq = 13;

    /**
     * server hello done
     */
    public final static int typeHeloDone = 14;

    /**
     * certificate verify
     */
    public final static int typeCertVrf = 15;

    /**
     * client key exchange
     */
    public final static int typeKexClnt = 16;

    /**
     * finished
     */
    public final static int typeFinish = 20;

    /**
     * key update
     */
    public final static int typeKeyUpd = 24;

    /**
     * message hash
     */
    public final static int typeMsgHsh = 254;

    private final packTls lower;

    /**
     * convert type to string
     *
     * @param i type of message
     * @return string
     */
    public static String type2string(int i) {
        switch (i) {
            case typeHeloReq:
                return "helloReq";
            case typeHeloClnt:
                return "clientHello";
            case typeHeloServ:
                return "serverHello";
            case typeHeloVrfy:
                return "verifyHello";
            case typeCertDat:
                return "certDat";
            case typeKexServ:
                return "serverKex";
            case typeCertReq:
                return "certReq";
            case typeHeloDone:
                return "helloDone";
            case typeCertVrf:
                return "certVer";
            case typeKexClnt:
                return "clientKex";
            case typeFinish:
                return "finish";
            case typeNewSesTck:
                return "newSess";
            case typeEncrExt:
                return "encrExt";
            case typeKeyUpd:
                return "keyUpd";
            case typeMsgHsh:
                return "msgHsh";
            default:
                return "unknown=" + i;
        }
    }

    /**
     * decode cipher suite code
     *
     * @param i cipher suite
     * @return decoded format, -1 if unknown format: 0x0000|key|sign|cipher|hash
     * kex: 1=rsa, 2=dhe sign: 1=rsa, 2=dss cipher: 1=des, 2=3des, 3=aes,
     * 4=chacha, 5=aesgcm hash: 1=md5, 2=sha1, 3=sha256, 4=sha384, 5=sha512
     */
    public static int decodeCipherCode(int i) {
        switch (i) {
            case 0x1303: // TLS_CHACHA20_POLY1305_SHA256
                return 0x0143;
            case 0x1302: // TLS_AES_256_GCM_SHA384
                return 0x0154;
            case 0x0009: // TLS_RSA_WITH_DES_CBC_SHA
                return 0x1112;
            case 0x000A: // TLS_RSA_WITH_3DES_EDE_CBC_SHA
                return 0x1122;
            case 0x0035: // TLS_RSA_WITH_AES_256_CBC_SHA
                return 0x1132;
            case 0x003D: // TLS_RSA_WITH_AES_256_CBC_SHA256
                return 0x1133;
            case 0x0012: // TLS_DHE_DSS_WITH_DES_CBC_SHA
                return 0x2212;
            case 0x0015: // TLS_DHE_RSA_WITH_DES_CBC_SHA
                return 0x2112;
            case 0x0013: // TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA
                return 0x2222;
            case 0x0016: // TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA
                return 0x2122;
            case 0x0038: // TLS_DHE_DSS_WITH_AES_256_CBC_SHA
                return 0x2232;
            case 0x0039: // TLS_DHE_RSA_WITH_AES_256_CBC_SHA
                return 0x2132;
            case 0x006A: // TLS_DHE_DSS_WITH_AES_256_CBC_SHA256
                return 0x2233;
            case 0x006B: // TLS_DHE_RSA_WITH_AES_256_CBC_SHA256
                return 0x2133;
            default:
                return -1;
        }
    }

    /**
     * convert cipher suite to string
     *
     * @param i decoded cipher suite
     * @return string
     */
    public static String cipher2string(int i) {
        String s = "";
        switch (i & 0xf000) {
            case 0x0000:
                s += "none";
                break;
            case 0x1000:
                s += "rsa";
                break;
            case 0x2000:
                s += "dhe";
                break;
            default:
                s += "?";
                break;
        }
        s += "-";
        switch (i & 0xf00) {
            case 0x000:
                s += "none";
                break;
            case 0x100:
                s += "rsa";
                break;
            case 0x200:
                s += "dss";
                break;
            default:
                s += "?";
                break;
        }
        s += "-";
        switch (i & 0xf0) {
            case 0x00:
                s += "none";
                break;
            case 0x10:
                s += "des";
                break;
            case 0x20:
                s += "3des";
                break;
            case 0x30:
                s += "aes";
                break;
            case 0x40:
                s += "chacha";
                break;
            case 0x50:
                s += "aes";
                break;
            default:
                s += "?";
                break;
        }
        s += "-";
        switch (i & 0xf) {
            case 0x0:
                s += "none";
                break;
            case 0x1:
                s += "md5";
                break;
            case 0x2:
                s += "sha1";
                break;
            case 0x3:
                s += "sha256";
                break;
            case 0x4:
                s += "sha384";
                break;
            default:
                s += "?";
                break;
        }
        return s;
    }

    /**
     * create packet handler
     *
     * @param pack packet
     * @param dtls datagram mode
     */
    public packTlsHndshk(packTls pack, boolean dtls) {
        lower = pack;
        datagram = dtls;
        clearXchgHash();
        if (lower.verMax < 0x304) {
            return;
        }
        ecDiffHell = new cryKeyECDH();
    }

    /**
     * clear exchange hash
     */
    public void clearXchgHash() {
        xchgByte = new ArrayList<Byte>();
        xchgPack = new ArrayList<Integer>();
    }

    /**
     * get header size
     *
     * @return bytes
     */
    public int getHeadSize() {
        if (datagram) {
            return 12;
        } else {
            return 4;
        }
    }

    /**
     * parse header
     *
     * @return false on success, true on error
     */
    public boolean headerParse() {
        pckTyp = -1;
        if (lower.pckTyp != packTls.typeHandshk) {
            return true;
        }
        for (int i = 0; i < lower.pckDat.dataSize(); i++) {
            xchgByte.add((byte) lower.pckDat.getByte(i));
        }
        xchgPack.add(xchgByte.size());
        int len = lower.pckDat.msbGetD(1) >>> 8;
        if (len > (lower.pckDat.dataSize() - getHeadSize())) {
            return true;
        }
        pckTyp = lower.pckDat.getByte(0);
        if (datagram) {
            if (lower.pckDat.msbGetW(6) != 0) {
                return true;
            }
            if (lower.pckDat.msbGetD(8) != len) {
                return true;
            }
        }
        lower.pckDat.getSkip(getHeadSize());
        lower.pckDat.setDataSize(len);
        if (debugger.secTlsTraf) {
            logger.debug("rx type=" + type2string(pckTyp));
        }
        return false;
    }

    /**
     * create header
     */
    public void headerCreate() {
        lower.pckTyp = packTls.typeHandshk;
        lower.pckDat.merge2beg();
        if (debugger.secTlsTraf) {
            logger.debug("tx type=" + type2string(pckTyp));
        }
        lower.pckDat.msbPutD(0, lower.pckDat.dataSize());
        lower.pckDat.putByte(0, pckTyp);
        if (datagram) {
            lower.pckDat.msbPutW(4, (int) lower.seqTx);
            lower.pckDat.msbPutD(6, 0); // fragment offset
            lower.pckDat.msbPutD(8, lower.pckDat.dataSize());
            lower.seqTx++;
        }
        lower.pckDat.putSkip(getHeadSize());
        lower.pckDat.merge2beg();
        for (int i = 0; i < lower.pckDat.dataSize(); i++) {
            xchgByte.add((byte) lower.pckDat.getByte(i));
        }
        xchgPack.add(xchgByte.size());
    }

    private List<Integer> makeCipherList() {
        List<Integer> l = new ArrayList<Integer>();
        if (maxVer >= 0x304) {
            for (int i = 0x1400; i >= 0x1300; i--) {
                if (decodeCipherCode(i) > 0) {
                    l.add(i);
                }
            }
        }
        if (minVer < 0x304) {
            for (int i = 0x100; i >= 0; i--) {
                if (decodeCipherCode(i) > 0) {
                    l.add(i);
                }
            }
        }
        return l;
    }

    private List<Integer> makeECcurveList() {
        List<Integer> lst = new ArrayList<Integer>();
        for (int i = 0; i < 0x100; i++) {
            if (cryECcurve.getByTls(i) == null) {
                continue;
            }
            lst.add(i);
        }
        return lst;
    }

    private List<Integer> makeSignatureList() {
        List<Integer> lst = new ArrayList<Integer>();
        for (int i = 0; i < 8; i++) {
            int o = 0x800 | i; // pss
            if (getSignerHash(o) == null) {
                continue;
            }
            lst.add(o);
        }
        for (int i = 0; i < 8; i++) {
            int o = (i << 8) | 0x1; // pkcs1
            if (getSignerHash(o) == null) {
                continue;
            }
            lst.add(o);
        }
        return lst;
    }

    private byte[] makeCompressList() {
        byte[] buf = new byte[1];
        buf[0] = 0;
        return buf;
    }

    private byte[] makeRandomCookie() {
        byte[] buf = new byte[32];
        for (int i = 0; i < buf.length; i++) {
            buf[i] = (byte) bits.randomB();
        }
        return buf;
    }

    private List<Integer> decodeCipherList(byte[] src) {
        List<Integer> trg = new ArrayList<Integer>();
        for (int i = 0; i < src.length; i += 2) {
            trg.add(bits.msbGetW(src, i));
        }
        return trg;
    }

    private byte[] encodeCipherList(List<Integer> src) {
        byte[] trg = new byte[src.size() * 2];
        for (int i = 0; i < src.size(); i++) {
            bits.msbPutW(trg, i * 2, src.get(i));
        }
        return trg;
    }

    private void parseExtensionList(byte[] buf, boolean client) {
        if (buf == null) {
            return;
        }
        typLenVal tlv = getTlv();
        packHolder pck = new packHolder(true, true);
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        pck.merge2beg();
        for (;;) {
            if (tlv.getBytes(pck)) {
                break;
            }
            if (debugger.secTlsTraf) {
                logger.debug("extension " + tlv.dump());
            }
            switch (tlv.valTyp) {
                case 0: // server name
                    int i = bits.msbGetW(tlv.valDat, 3);
                    if (i >= tlv.valSiz) {
                        break;
                    }
                    buf = new byte[i];
                    bits.byteCopy(tlv.valDat, 5, buf, 0, buf.length);
                    servNam = new String(buf);
                    break;
                case 43: // supported version
                    if (client) {
                        minVer = bits.msbGetW(tlv.valDat, 0);
                        maxVer = minVer;
                        break;
                    }
                    if (tlv.valSiz < 3) {
                        break;
                    }
                    minVer = 0x304;
                    maxVer = 0x300;
                    for (i = 1; i < tlv.valSiz; i += 2) {
                        int o = bits.msbGetW(tlv.valDat, i);
                        if (o < minVer) {
                            minVer = o;
                        }
                        if (o > maxVer) {
                            maxVer = o;
                        }
                    }
                    if (minVer < lower.verMin) {
                        minVer = lower.verMin;
                    }
                    if (maxVer > lower.verMax) {
                        maxVer = lower.verMax;
                    }
                    break;
                case 10: // supported groups
                    if (lower.verMax < 0x304) {
                        break;
                    }
                    for (i = 2; i < tlv.valSiz; i += 2) {
                        int o = bits.msbGetW(tlv.valDat, i);
                        ecDiffHell.curve = cryECcurve.getByTls(o);
                        if (ecDiffHell.curve != null) {
                            break;
                        }
                    }
                    break;
                case 13: // signature algorithm
                    if (lower.verMax < 0x304) {
                        break;
                    }
                    for (i = 2; i < tlv.valSiz; i += 2) {
                        int o = bits.msbGetW(tlv.valDat, i);
                        if (getSignerHash(o) == null) {
                            continue;
                        }
                        signHsh = o;
                        break;
                    }
                    break;
                case 51: // key exchange
                    if (lower.verMax < 0x304) {
                        break;
                    }
                    if (client) {
                        ecDiffHell.curve = cryECcurve.getByTls(bits.msbGetW(tlv.valDat, 0));
                        if (ecDiffHell.curve == null) {
                            break;
                        }
                        ecDiffHell.servPub = cryECpoint.fromBytes1(ecDiffHell.curve, tlv.valDat, 4);
                        break;
                    }
                    if (ecDiffHell.curve == null) {
                        break;
                    }
                    cryECcurve tmp = cryECcurve.getByTls(bits.msbGetW(tlv.valDat, 2));
                    if (tmp == null) {
                        break;
                    }
                    if (tmp.tls != ecDiffHell.curve.tls) {
                        break;
                    }
                    ecDiffHell.clntPub = cryECpoint.fromBytes1(ecDiffHell.curve, tlv.valDat, 6);
                    break;
            }
        }
    }

    private typLenVal getTlv() {
        return new typLenVal(0, 16, 16, 16, 1, 0, 4, 1, 0, 1024, true);
    }

    private byte[] extenList2bytes(List<Integer> lst) {
        byte[] buf = new byte[(lst.size() * 2) + 2];
        bits.msbPutW(buf, 0, buf.length - 2); // length
        for (int i = 0; i < lst.size(); i++) {
            bits.msbPutW(buf, (i * 2) + 2, lst.get(i));
        }
        return buf;
    }

    private byte[] makeExtensionList(boolean client) {
        typLenVal tlv = getTlv();
        packHolder pck = new packHolder(true, true);
        byte[] buf = new byte[2];
        bits.msbPutW(buf, 0, 8192);
        tlv.putBytes(pck, 28, buf); // record size limit
        if (client && (servNam != null)) {
            int len = servNam.length();
            buf = new byte[len + 5];
            bits.msbPutW(buf, 0, len + 3);
            buf[2] = 0; // name
            bits.msbPutW(buf, 3, len);
            bits.byteCopy(servNam.getBytes(), 0, buf, 5, len);
            tlv.putBytes(pck, 0, buf); // server name
        }
        if (lower.verMax < 0x304) {
            pck.merge2end();
            return pck.getCopy();
        }
        if (client) {
            buf = new byte[(2 * (maxVer - minVer)) + 3];
            buf[0] = (byte) (buf.length - 1);
            for (int i = minVer; i <= maxVer; i++) {
                bits.msbPutW(buf, 1 + (2 * (i - minVer)), i);
            }
        } else {
            buf = new byte[2];
            bits.msbPutW(buf, 0, maxVer);
        }
        tlv.putBytes(pck, 43, buf); // supported versions
        if (client) {
            buf = extenList2bytes(makeSignatureList());
            tlv.putBytes(pck, 13, buf); // signature algorithms
        } else {
            buf = new byte[2];
            bits.msbPutW(buf, 0, signHsh);
        }
        if (client) {
            buf = extenList2bytes(makeECcurveList());
            tlv.putBytes(pck, 10, buf); // supported groups
        }
        byte[] res;
        if (client) {
            if (ecDiffHell.curve != null) {
                res = ecDiffHell.clntPub.toBytes1();
                buf = new byte[6];
                bits.msbPutW(buf, 0, res.length + 4);
                bits.msbPutW(buf, 2, ecDiffHell.curve.tls);
                bits.msbPutW(buf, 4, res.length);
            } else {
                buf = new byte[2];
                res = new byte[0];
            }
        } else {
            if (ecDiffHell.servPub != null) {
                res = ecDiffHell.servPub.toBytes1();
                buf = new byte[4];
                bits.msbPutW(buf, 2, res.length);
            } else {
                res = new byte[0];
                buf = new byte[2];
            }
            if (ecDiffHell.curve == null) {
                buf = null;
            } else {
                bits.msbPutW(buf, 0, ecDiffHell.curve.tls);
            }
        }
        if (buf != null) {
            tlv.putBytes(pck, 51, bits.byteConcat(buf, res)); // key share
        }
        if (!client && (ecDiffHell.servPub == null)) {
            buf = new byte[6];
            bits.msbPutD(buf, 2, bits.randomD());
            bits.msbPutW(buf, 0, buf.length - 2); // length
            tlv.putBytes(pck, 44, buf); // cookie
        }
        pck.merge2end();
        return pck.getCopy();
    }

    /**
     * fill up client hello
     */
    public void clntHelloFill() {
        minVer = lower.verMin;
        maxVer = lower.verMax;
        clntRand = makeRandomCookie();
        cipherList = makeCipherList();
        cmprssList = makeCompressList();
        extnsnList = makeExtensionList(true);
        sendSess = null;
    }

    /**
     * fill up client hello
     *
     * @return false on success, true on error
     */
    public boolean clntHelloFillEc() {
        if (ecDiffHell.curve == null) {
            return true;
        }
        ecDiffHell.clntXchg();
        return false;
    }

    /**
     * parse client hello
     *
     * @return false on success, true on error
     */
    public boolean clntHelloParse() {
        if (pckTyp != typeHeloClnt) {
            return true;
        }
        minVer = lower.verCurr;
        maxVer = lower.pckDat.msbGetW(0);
        if (datagram) {
            maxVer = packTls.version2dtls(maxVer);
        }
        lower.pckDat.getSkip(2);
        clntRand = lower.getBytes(-32);
        rcvdSess = lower.getBytes(1); // session id
        if (datagram) {
            vrfyRand = lower.getBytes(1);
        }
        cipherList = decodeCipherList(lower.getBytes(2));
        cmprssList = lower.getBytes(1);
        extnsnList = lower.getBytes(2);
        parseExtensionList(extnsnList, false);
        clntHelloDump("rx");
        return false;
    }

    /**
     * create client hello
     */
    public void clntHelloCreate() {
        pckTyp = typeHeloClnt;
        lower.verCurr = packTls.version2wire(minVer);
        lower.pckDat.clear();
        int i = packTls.version2wire(maxVer);
        if (datagram) {
            i = packTls.version2dtls(i);
        }
        lower.pckDat.msbPutW(0, i);
        lower.pckDat.putSkip(2);
        lower.putBytes(clntRand, 0);
        lower.putBytes(sendSess, 1); // session id
        if (datagram) {
            lower.putBytes(vrfyRand, 1);
        }
        lower.putBytes(encodeCipherList(cipherList), 2);
        lower.putBytes(cmprssList, 1);
        if (extnsnList != null) {
            lower.putBytes(extnsnList, 2);
        }
        clntHelloDump("tx");
    }

    private void clntHelloDump(String dir) {
        if (!debugger.secTlsTraf) {
            return;
        }
        String s = "";
        for (int i = 0; i < cipherList.size(); i++) {
            s += "," + cipher2string(decodeCipherCode(cipherList.get(i)));
        }
        s = s.substring(1, s.length());
        logger.debug(dir + " ver=" + packTls.version2string(datagram, minVer) + "-" + packTls.version2string(datagram, maxVer) + " random=" + bits.byteDump(clntRand, 0, -1) + " ciphers=" + s + " extensions=" + bits.byteDump(extnsnList, 0, -1));
    }

    /**
     * create verify hello
     */
    public void vrfyHelloFill() {
        vrfyRand = new byte[20];
        for (int i = 0; i < vrfyRand.length; i++) {
            vrfyRand[i] = (byte) bits.randomB();
        }
        versionFill();
    }

    /**
     * parse verify hello
     *
     * @return false on success, true on error
     */
    public boolean vrfyHelloParse() {
        if (pckTyp != typeHeloVrfy) {
            return true;
        }
        minVer = lower.verCurr;
        maxVer = lower.pckDat.msbGetW(0);
        if (datagram) {
            maxVer = packTls.version2dtls(maxVer);
        }
        lower.pckDat.getSkip(2);
        vrfyRand = lower.getBytes(1);
        vrfyHelloDump("rx");
        return false;
    }

    /**
     * create verify hello
     */
    public void vrfyHelloCreate() {
        pckTyp = typeHeloVrfy;
        lower.verCurr = packTls.version2wire(minVer);
        lower.pckDat.clear();
        int i = packTls.version2wire(maxVer);
        if (datagram) {
            i = packTls.version2dtls(i);
        }
        lower.pckDat.msbPutW(0, i);
        lower.pckDat.putSkip(2);
        lower.putBytes(vrfyRand, 1);
        vrfyHelloDump("tx");
    }

    private void vrfyHelloDump(String dir) {
        if (!debugger.secTlsTraf) {
            return;
        }
        logger.debug(dir + " ver=" + packTls.version2string(datagram, minVer) + "-"
                + packTls.version2string(datagram, maxVer) + " cookie=" + bits.byteDump(vrfyRand, 0, -1));
    }

    private void versionFill() {
        int i = maxVer;
        if (i < lower.verMin) {
            i = lower.verMin;
        }
        if (i > lower.verMax) {
            i = lower.verMax;
        }
        maxVer = i;
        minVer = i;
    }

    /**
     * fill up server hello
     */
    public void servHelloFillRetry() {
        servRand = new byte[]{
            (byte) 0xCF, (byte) 0x21, (byte) 0xAD, (byte) 0x74, (byte) 0xE5, (byte) 0x9A, (byte) 0x61, (byte) 0x11,
            (byte) 0xBE, (byte) 0x1D, (byte) 0x8C, (byte) 0x02, (byte) 0x1E, (byte) 0x65, (byte) 0xB8, (byte) 0x91,
            (byte) 0xC2, (byte) 0xA2, (byte) 0x11, (byte) 0x16, (byte) 0x7A, (byte) 0xBB, (byte) 0x8C, (byte) 0x5E,
            (byte) 0x07, (byte) 0x9E, (byte) 0x09, (byte) 0xE2, (byte) 0xC8, (byte) 0xA8, (byte) 0x33, (byte) 0x9C
        };
    }

    /**
     * fill up server hello
     *
     * @return false on successful, true on error
     */
    public boolean servHelloFillEc() {
        if (ecDiffHell.curve == null) {
            return true;
        }
        ecDiffHell.servXchg();
        return false;
    }

    /**
     * fill up server hello
     *
     * @return false on successful, true on error
     */
    public boolean servHelloFill() {
        int o = -1;
        for (int i = 0; i < cmprssList.length; i++) {
            if (cmprssList[i] == 0) {
                o = i;
            }
        }
        if (o < 0) {
            return true;
        }
        cmprssWant = 0;
        o = -1;
        List<Integer> avail = makeCipherList();
        for (int i = 0; i < cipherList.size(); i++) {
            int val = cipherList.get(i);
            int dec = decodeCipherCode(val);
            if (dec < 0) {
                continue;
            }
            if (val < o) {
                continue;
            }
            int q = -1;
            for (int p = 0; p < avail.size(); p++) {
                if (avail.get(p) == val) {
                    q = p;
                }
            }
            if (q < 0) {
                continue;
            }
            o = val;
        }
        if (o < 0) {
            return true;
        }
        cipherWant = o;
        cipherDecoded = decodeCipherCode(cipherWant);
        servRand = makeRandomCookie();
        extnsnList = makeExtensionList(false);
        if (maxVer >= 0x304) {
            sendSess = rcvdSess;
        } else {
            sendSess = null;
        }
        versionFill();
        return false;
    }

    /**
     * create server hello
     */
    public void servHelloCreate() {
        pckTyp = typeHeloServ;
        lower.verCurr = packTls.version2wire(minVer);
        lower.pckDat.clear();
        int i = packTls.version2wire(maxVer);
        if (datagram) {
            i = packTls.version2dtls(i);
        }
        lower.pckDat.msbPutW(0, i);
        lower.pckDat.putSkip(2);
        lower.putBytes(servRand, 0);
        lower.putBytes(sendSess, 1); // session id
        lower.pckDat.msbPutW(0, cipherWant);
        lower.pckDat.putByte(2, cmprssWant);
        lower.pckDat.putSkip(3);
        if (extnsnList != null) {
            lower.putBytes(extnsnList, 2);
        }
        servHelloDump("tx");
    }

    /**
     * parse server hello
     *
     * @return false on success, true on error
     */
    public boolean servHelloParse() {
        if (pckTyp != typeHeloServ) {
            return true;
        }
        minVer = lower.verCurr;
        maxVer = lower.pckDat.msbGetW(0);
        if (datagram) {
            maxVer = packTls.version2dtls(maxVer);
        }
        lower.pckDat.getSkip(2);
        servRand = lower.getBytes(-32);
        rcvdSess = lower.getBytes(1); // session id
        cipherWant = lower.pckDat.msbGetW(0);
        cmprssWant = lower.pckDat.getByte(2);
        lower.pckDat.getSkip(3);
        extnsnList = lower.getBytes(2);
        cipherDecoded = decodeCipherCode(cipherWant);
        parseExtensionList(extnsnList, true);
        if (cmprssWant != 0) {
            return true;
        }
        if (cipherDecoded < 0) {
            return true;
        }
        servHelloDump("rx");
        return false;
    }

    private void servHelloDump(String dir) {
        if (!debugger.secTlsTraf) {
            return;
        }
        logger.debug(dir + " ver=" + packTls.version2string(datagram, minVer) + "-" + packTls.version2string(datagram, maxVer) + " cipher=" + cipher2string(decodeCipherCode(cipherWant)) + " random=" + bits.byteDump(servRand, 0, -1) + " extensions=" + bits.byteDump(extnsnList, 0, -1));
    }

    /**
     * fill certificates
     */
    public void certDatFill() {
        certificates = new ArrayList<byte[]>();
        switch (cipherDecoded & 0xf00) {
            case 0x100:
                certificates.add(certrsa.asn1WriteBuf());
                break;
            case 0x200:
                certificates.add(certdsa.asn1WriteBuf());
                break;
            default:
                break;
        }
    }

    /**
     * create certificates
     */
    public void certLstCreate() {
        pckTyp = typeCertDat;
        lower.pckDat.clear();
        for (int i = 0; i < certificates.size(); i++) {
            byte[] buf = certificates.get(i);
            lower.putBytes(buf, 3);
            lower.pckDat.merge2end();
        }
        lower.pckDat.msbPutD(0, lower.pckDat.dataSize() << 8);
        lower.pckDat.putSkip(3);
        certDatDump("tx");
    }

    /**
     * parse certificates
     *
     * @return false on success, true on error
     */
    public boolean certLstParse() {
        if (pckTyp != typeCertDat) {
            return true;
        }
        lower.pckDat.getSkip(3);
        certificates = new ArrayList<byte[]>();
        for (; lower.pckDat.dataSize() > 0;) {
            byte[] cert = lower.getBytes(3);
            if (cert == null) {
                break;
            }
            certificates.add(cert);
        }
        certDatDump("rx");
        return false;
    }

    /**
     * parse certificates
     *
     * @return false on success, true on error
     */
    public boolean certDatParse() {
        if (pckTyp != typeCertDat) {
            return true;
        }
        if (lower.pckDat.getByte(0) != 0) {
            return true;
        }
        lower.pckDat.getSkip(4);
        certificates = new ArrayList<byte[]>();
        for (; lower.pckDat.dataSize() > 0;) {
            byte[] cert = lower.getBytes(3);
            if (cert == null) {
                break;
            }
            certificates.add(cert);
        }
        certDatDump("rx");
        return false;
    }

    /**
     * parse certificates
     */
    public void certDatCreate() {
        pckTyp = typeCertDat;
        lower.pckDat.clear();
        for (int i = 0; i < certificates.size(); i++) {
            byte[] buf = certificates.get(i);
            lower.putBytes(buf, 3);
            lower.pckDat.merge2end();
        }
        lower.pckDat.msbPutW(0, 0);
        lower.pckDat.putSkip(2);
        lower.pckDat.merge2end();
        lower.pckDat.msbPutD(0, lower.pckDat.dataSize());
        lower.pckDat.putSkip(4);
        certDatDump("tx");
    }

    private void certDatDump(String dir) {
        if (!debugger.secTlsTraf) {
            return;
        }
        String s = "";
        for (int i = 0; i < certificates.size(); i++) {
            byte[] buf = certificates.get(i);
            s += " cert=" + bits.byteDump(buf, 0, -1);
        }
        logger.debug(dir + s);
    }

    /**
     * parse certificate verify
     *
     * @return false on success, true on error
     */
    public boolean certVrfParse() {
        if (pckTyp != typeCertVrf) {
            return true;
        }
        signHsh = lower.pckDat.msbGetW(0);
        lower.pckDat.getSkip(2);
        signDat = lower.getBytes(2);
        paramHsh = getSignerHash(signHsh);
        paramHash = cryHashGeneric.compute(paramHsh, paramHash);
        servKexDump();
        cryCertificate crt = new cryCertificate();
        if (crt.asn1ReadBuf(certificates.get(0))) {
            logger.info("cert error");
            return true;
        }
        if (debugger.secTlsTraf) {
            logger.debug("cert=" + crt);
        }
        if (crt.key.tlsVerify(signHsh, paramHsh, paramHash, signDat)) {
            logger.info("sign error on " + crt);
            return true;
        }
        return false;
    }

    /**
     * create certificate verify
     */
    public void certVrfCreate() {
        paramHsh = getSignerHash(signHsh);
        paramHash = cryHashGeneric.compute(paramHsh, paramHash);
        servKexSign();
        pckTyp = typeCertVrf;
        lower.pckDat.clear();
        lower.pckDat.msbPutW(0, signHsh);
        lower.pckDat.putSkip(2);
        lower.pckDat.merge2end();
        lower.putBytes(signDat, 2);
        lower.pckDat.merge2end();
        servKexDump();
    }

    /**
     * fill certificate verify
     */
    public void certVrfFill() {
        paramHash = new byte[64];
        bits.byteFill(paramHash, 0, paramHash.length, 32);
        paramHash = bits.byteConcat(paramHash, "TLS 1.3, server CertificateVerify".getBytes());
        paramHash = bits.byteConcat(paramHash, new byte[1]);
        cryHashGeneric h = getAlgoHasher();
        paramHash = bits.byteConcat(paramHash, calcExchangeSumV13(h));
    }

    private void servKexDump() {
        if (debugger.secTlsTraf) {
            logger.debug("paramHash=" + bits.byteDump(paramHash, 0, -1) + " hash=" + paramHsh.getName());
        }
    }

    private cryHashGeneric getSignerHash(int signHsh) {
        switch (signHsh) {
            case 0x101:
                return new cryHashMd5();
            case 0x201:
                return new cryHashSha1();
            case 0x301:
                return new cryHashSha2224();
            case 0x401:
                return new cryHashSha2256();
            case 0x501:
                return new cryHashSha2384();
            case 0x601:
                return new cryHashSha2512();
            case 0x804:
                return new cryHashSha2256();
            case 0x805:
                return new cryHashSha2384();
            case 0x806:
                return new cryHashSha2512();
            default:
                return null;
        }
    }

    private void servKexHash() {
        paramHash = null;
        paramHsh = null;
        packTls h = new packTls(datagram);
        h.putBytes(clntRand, 0);
        h.putBytes(servRand, 0);
        h.putBytes(cryUtils.bigUint2buf(diffHell.modulus), 2);
        h.putBytes(cryUtils.bigUint2buf(diffHell.group), 2);
        h.putBytes(cryUtils.bigUint2buf(diffHell.servPub), 2);
        h.pckDat.merge2beg();
        byte[] raw = h.pckDat.getCopy();
        if (signHsh < 0) {
            switch (cipherDecoded & 0xf00) {
                case 0x100:
                    paramHash = bits.byteConcat(cryHashGeneric.compute(new cryHashMd5(), raw),
                            cryHashGeneric.compute(new cryHashSha1(), raw));
                    paramHsh = new cryHashMd5();
                    break;
                case 0x200:
                    paramHash = cryHashGeneric.compute(new cryHashSha1(), raw);
                    paramHsh = new cryHashSha1();
                    break;
                default:
                    return;
            }
            return;
        }
        paramHsh = getSignerHash(signHsh);
        paramHash = cryHashGeneric.compute(paramHsh, raw);
    }

    private void servKexSign() {
        switch (cipherDecoded & 0xf00) {
            case 0x100:
                signDat = keyrsa.tlsSigning(signHsh, paramHsh, paramHash);
                break;
            case 0x200:
                signDat = keydsa.tlsSigning(signHsh, paramHsh, paramHash);
                break;
            default:
                signDat = new byte[0];
                break;
        }
    }

    /**
     * check if server key exchange needed
     *
     * @return false if no, true if yes
     */
    public boolean servKexNeeded() {
        switch (cipherDecoded & 0xf000) {
            case 0x1000:
                return false;
            case 0x2000:
                return true;
            default:
                return false;
        }
    }

    /**
     * fill up server key exchange
     */
    public void servKexFill() {
        int i = keyrsa.keySize();
        int o = keydsa.keySize();
        if (i < o) {
            i = o;
        }
        diffHell = cryKeyDH.findGroup(i);
        diffHell.servXchg();
        if (minVer >= 0x303) {
            signHsh = 0x401; // rsa pkcs1 sha256
        }
        servKexHash();
        servKexDump();
        servKexSign();
    }

    /**
     * create server key exchange
     */
    public void servKexCreate() {
        pckTyp = typeKexServ;
        lower.pckDat.clear();
        lower.putBytes(cryUtils.bigUint2buf(diffHell.modulus), 2);
        lower.putBytes(cryUtils.bigUint2buf(diffHell.group), 2);
        lower.putBytes(cryUtils.bigUint2buf(diffHell.servPub), 2);
        if (minVer >= 0x303) {
            lower.pckDat.msbPutW(0, signHsh);
            lower.pckDat.putSkip(2);
        }
        lower.putBytes(signDat, 2);
        servKexDump("tx");
    }

    /**
     * parse server key exchange
     *
     * @return false on success, true on error
     */
    public boolean servKexParse() {
        if (pckTyp != typeKexServ) {
            return true;
        }
        diffHell = new cryKeyDH();
        diffHell.modulus = cryUtils.buf2bigUint(lower.getBytes(2));
        diffHell.group = cryUtils.buf2bigUint(lower.getBytes(2));
        diffHell.servPub = cryUtils.buf2bigUint(lower.getBytes(2));
        if (minVer >= 0x303) {
            signHsh = lower.pckDat.msbGetW(0);
            lower.pckDat.getSkip(2);
        }
        signDat = lower.getBytes(2);
        servKexDump("rx");
        cryCertificate crt = new cryCertificate();
        if (crt.asn1ReadBuf(certificates.get(0))) {
            logger.info("cert error");
            return true;
        }
        if (debugger.secTlsTraf) {
            logger.debug("cert=" + crt);
        }
        servKexHash();
        if (paramHash == null) {
            return true;
        }
        servKexDump();
        if (crt.key.tlsVerify(signHsh, paramHsh, paramHash, signDat)) {
            logger.info("sign error on " + crt);
            return true;
        }
        return false;
    }

    private void servKexDump(String dir) {
        if (!debugger.secTlsTraf) {
            return;
        }
        logger.debug(dir + " prime=" + diffHell.modulus + " group=" + diffHell.group + " sign="
                + bits.byteDump(signDat, 0, -1));
    }

    /**
     * create server hello done
     */
    public void servDoneCreate() {
        pckTyp = typeHeloDone;
        lower.pckDat.clear();
    }

    /**
     * parse server hello done
     *
     * @return false on success, true on error
     */
    public boolean servDoneParse() {
        if (pckTyp != typeHeloDone) {
            return true;
        }
        return false;
    }

    /**
     * fill client key exchange
     */
    public void clntKexFill() {
        switch (cipherDecoded & 0xf000) {
            case 0x1000:
                preMaster = new byte[48];
                for (int i = 2; i < preMaster.length; i++) {
                    preMaster[i] = (byte) bits.randomB();
                }
                int i = lower.verMax;
                if (datagram) {
                    i = packTls.version2dtls(i);
                }
                bits.msbPutW(preMaster, 0, i);
                break;
            case 0x2000:
                diffHell.clntXchg();
                diffHell.clntKey();
                break;
        }
    }

    /**
     * create client key exchange
     */
    public void clntKexCreate() {
        pckTyp = typeKexClnt;
        lower.pckDat.clear();
        switch (cipherDecoded & 0xf000) {
            case 0x1000:
                cryCertificate crt = new cryCertificate();
                if (crt.asn1ReadBuf(certificates.get(0))) {
                    return;
                }
                if (debugger.secTlsTraf) {
                    logger.debug("cert=" + crt);
                }
                cryKeyRSA key = (cryKeyRSA) crt.key;
                lower.putBytes(key.doEncrypt(key.PKCS1t15pad(preMaster)), 2);
                break;
            case 0x2000:
                lower.putBytes(cryUtils.bigUint2buf(diffHell.clntPub), 2);
                break;
        }
    }

    /**
     * parse client key exchange
     *
     * @return false on success, true on error
     */
    public boolean clntKexParse() {
        if (pckTyp != typeKexClnt) {
            return true;
        }
        switch (cipherDecoded & 0xf000) {
            case 0x1000:
                preMaster = keyrsa.PKCS1t15unpad(keyrsa.doDecrypt(lower.getBytes(2)));
                break;
            case 0x2000:
                diffHell.clntPub = cryUtils.buf2bigUint(lower.getBytes(2));
                diffHell.servKey();
                break;
        }
        return false;
    }

    /**
     * create change cipher spec
     */
    public void chgCipherCreate() {
        lower.pckTyp = packTls.typeChgCipher;
        lower.pckDat.clear();
        lower.pckDat.putByte(0, 1);
        lower.pckDat.putSkip(1);
        if (datagram) {
            lower.seqTx++;
        }
    }

    /**
     * parse change cipher spec
     *
     * @return false on success, true on error
     */
    public boolean chgCipherParse() {
        if (lower.pckTyp != packTls.typeChgCipher) {
            return true;
        }
        return false;
    }

    /**
     * fill finished
     *
     * @param client true=client, false=server
     */
    public void finishedFill(boolean client) {
        finished = calcFinished(client);
    }

    /**
     * create finished
     */
    public void finishedCreate() {
        pckTyp = typeFinish;
        lower.pckDat.clear();
        lower.putBytes(finished, 0);
        if (datagram) {
            lower.seqTx = 0x0000ffffffffffffL;
        }
        finishedDump("tx");
    }

    /**
     * parse finished
     *
     * @return false on success, true on error
     */
    public boolean finishedParse() {
        if (pckTyp != typeFinish) {
            return true;
        }
        byte[] buf = lower.getBytes(-finished.length);
        for (int i = 0; i < buf.length; i++) {
            if (buf[i] != finished[i]) {
                return true;
            }
        }
        finishedDump("rx");
        return false;
    }

    private void finishedDump(String dir) {
        if (!debugger.secTlsTraf) {
            return;
        }
        logger.debug(dir + " hash=" + bits.byteDump(finished, 0, -1));
    }

    /**
     * parse encrypted extensions
     *
     * @return false on success, true on error
     */
    public boolean encrExtParse() {
        if (pckTyp != typeEncrExt) {
            return true;
        }
        extnsnList = lower.getBytes(2);
        parseExtensionList(extnsnList, true);
        encrExtDump("rx");
        return false;
    }

    /**
     * create encrypted extensions
     */
    public void encrExtCreate() {
        pckTyp = typeEncrExt;
        lower.pckDat.clear();
        lower.putBytes(extnsnList, 2);
        encrExtDump("tx");
    }

    /**
     * fill encrypted extensions
     */
    public void encrExtFill() {
        extnsnList = new byte[0];
    }

    private void encrExtDump(String dir) {
        if (!debugger.secTlsTraf) {
            return;
        }
        logger.debug(dir + " encrext=" + bits.byteDump(extnsnList, 0, -1));
    }

    private byte[] calcExchangeSum(cryHashGeneric h, int skip, int max, byte[] pre, byte[] post) {
        h.init();
        if (pre != null) {
            for (int i = 0; i < pre.length; i++) {
                h.update(pre[i]);
            }
        }
        if (max < 1) {
            max = xchgByte.size();
        }
        for (int i = skip; i < max; i++) {
            h.update(xchgByte.get(i));
        }
        if (post != null) {
            for (int i = 0; i < post.length; i++) {
                h.update(post[i]);
            }
        }
        return h.finish();
    }

    private byte[] calcExchangeSumV13(cryHashGeneric h) {
        int chl = xchgPack.get(0);
        byte[] trf = new byte[4];
        trf[0] = (byte) typeMsgHsh;
        trf[3] = (byte) h.getHashSize();
        byte[] buf = calcExchangeSum(h, 0, chl, null, null);
        buf = bits.byteConcat(trf, buf);
        buf = calcExchangeSum(h, chl, 0, buf, null);
        return buf;
    }

    private byte[] calcFinished(boolean client) {
        String s = "";
        if (client) {
            s = "client finished";
        } else {
            s = "server finished";
        }
        switch (minVer) {
            case 0x300:
                if (client) {
                    s = "CLNT";
                } else {
                    s = "SRVR";
                }
                byte[] h1 = calcExchangeSum(new cryHashSslMac(new cryHashMd5(), masterSec, false), 0, 0, null, s.getBytes());
                byte[] h2 = calcExchangeSum(new cryHashSslMac(new cryHashSha1(), masterSec, false), 0, 0, null, s.getBytes());
                return bits.byteConcat(h1, h2);
            case 0x301:
            case 0x302:
                h1 = calcExchangeSum(new cryHashMd5(), 0, 0, null, null);
                h2 = calcExchangeSum(new cryHashSha1(), 0, 0, null, null);
                return genHashV10(masterSec, s, h1, h2, 12);
            case 0x303:
                h1 = calcExchangeSum(new cryHashSha2256(), 0, 0, null, null);
                h2 = new byte[0];
                return genHashV12(masterSec, s, h1, h2, 12);
            case 0x304:
                cryHashGeneric h = getAlgoHasher();
                if (client) {
                    h1 = genHashV13l(h, hndshkSC, "finished".getBytes(), new byte[0], h.getHashSize());
                } else {
                    h1 = genHashV13l(h, hndshkCS, "finished".getBytes(), new byte[0], h.getHashSize());
                }
                h2 = calcExchangeSumV13(h);
                h = new cryHashHmac(h, h1);
                h.init();
                h.update(h2);
                return h.finish();
            default:
                return null;
        }
    }

    private static byte[] genHashV09(byte[] sec, byte[] sd1, byte[] sd2, int siz) {
        byte[] res = new byte[siz];
        siz = siz / new cryHashMd5().getHashSize();
        for (int r = 0; r < siz; r++) {
            byte[] chr = new byte[r + 1];
            bits.byteFill(chr, 0, chr.length, 0x40 + chr.length);
            cryHashGeneric hsh = new cryHashSha1();
            hsh.init();
            hsh.update(chr);
            hsh.update(sec);
            hsh.update(sd1);
            hsh.update(sd2);
            chr = hsh.finish();
            hsh = new cryHashMd5();
            hsh.init();
            hsh.update(sec);
            hsh.update(chr);
            chr = hsh.finish();
            bits.byteCopy(chr, 0, res, r * chr.length, chr.length);
        }
        return res;
    }

    private static byte[] genHashV10h(cryHashGeneric h, byte[] sec, byte[] sed, int siz) {
        siz = (siz / h.getHashSize()) + 1;
        h = new cryHashHmac(h, sec);
        byte[] a = sed;
        byte[] res = new byte[siz * h.getHashSize()];
        for (int r = 0; r < siz; r++) {
            h.init();
            h.update(a);
            a = h.finish();
            h.init();
            h.update(a);
            h.update(sed);
            byte[] buf = h.finish();
            bits.byteCopy(buf, 0, res, r * buf.length, buf.length);
        }
        return res;
    }

    private static byte[] getHashV10c(byte[] lab, byte[] sd1, byte[] sd2) {
        return bits.byteConcat(bits.byteConcat(lab, sd1), sd2);
    }

    private static byte[] genHashV10(byte[] sec, String lab, byte[] sd1, byte[] sd2, int siz) {
        byte[] sc1 = new byte[(sec.length + 1) / 2];
        byte[] sc2 = new byte[sc1.length];
        bits.byteCopy(sec, 0, sc1, 0, sc1.length);
        bits.byteCopy(sec, sec.length - sc2.length, sc2, 0, sc2.length);
        sec = getHashV10c(lab.getBytes(), sd1, sd2);
        sd1 = genHashV10h(new cryHashMd5(), sc1, sec, siz);
        sd2 = genHashV10h(new cryHashSha1(), sc2, sec, siz);
        sec = new byte[siz];
        for (int i = 0; i < sec.length; i++) {
            sec[i] = (byte) (sd1[i] ^ sd2[i]);
        }
        return sec;
    }

    private static byte[] genHashV12(byte[] sec, String lab, byte[] sd1, byte[] sd2, int siz) {
        sd1 = genHashV10h(new cryHashSha2256(), sec, getHashV10c(lab.getBytes(), sd1, sd2), siz);
        sd2 = new byte[siz];
        bits.byteCopy(sd1, 0, sd2, 0, siz);
        return sd2;
    }

    private static byte[] genHashV13t(cryHashGeneric hash, byte[] slt, byte[] ikm) {
        cryHashGeneric h = new cryHashHmac(hash, slt);
        h.init();
        h.update(ikm);
        return h.finish();
    }

    private static byte[] genHashV13p(cryHashGeneric hash, byte[] prk, byte[] info, int siz) {
        cryHashHmac h = new cryHashHmac(hash, prk);
        byte[] prev = new byte[0];
        byte[] res = new byte[0];
        for (int seq = 1; res.length < siz; seq++) {
            h.init();
            h.update(prev);
            h.update(info);
            h.update(seq);
            byte[] cur = h.finish();
            prev = cur;
            res = bits.byteConcat(res, cur);
        }
        if (res.length == siz) {
            return res;
        }
        prev = new byte[siz];
        bits.byteCopy(res, 0, prev, 0, prev.length);
        return prev;
    }

    private static byte[] genHashV13l(cryHashGeneric hash, byte[] sec, byte[] lab, byte[] ctx, int siz) {
        byte[] res = new byte[9];
        bits.msbPutW(res, 0, siz);
        res[2] = (byte) (lab.length + 6);
        res[3] = 0x74;
        res[4] = 0x6C;
        res[5] = 0x73;
        res[6] = 0x31;
        res[7] = 0x33;
        res[8] = 0x20;
        res = bits.byteConcat(res, lab);
        byte[] buf = new byte[1];
        buf[0] = (byte) ctx.length;
        res = bits.byteConcat(res, buf);
        res = bits.byteConcat(res, ctx);
        return genHashV13p(hash, sec, res, siz);
    }

    private static byte[] genHashV13d(cryHashGeneric hash, byte[] sec, String lab, String msg) {
        hash.init();
        hash.update(msg.getBytes());
        return genHashV13l(hash, sec, lab.getBytes(), hash.finish(), hash.getHashSize());
    }

    private static byte[] genHashV13d(cryHashGeneric hash, byte[] sec, String lab, byte[] msgh) {
        return genHashV13l(hash, sec, lab.getBytes(), msgh, hash.getHashSize());
    }

    private byte[] getKeyMaterial(int siz) {
        byte[] buf = new byte[siz];
        bits.byteCopy(keyBlockD, keyBlockP, buf, 0, siz);
        keyBlockP += siz;
        return buf;
    }

    private cryEncrGeneric getAlgoCipher() {
        switch (cipherDecoded & 0xf0) {
            case 0x10:
                return new cryEncrCBCdes();
            case 0x20:
                return new cryEncrCBCdes3();
            case 0x30:
                return new cryEncrCBCaes();
            case 0x40:
                return new cryEncrChacha20poly1305();
            case 0x50:
                return new cryEncrGCMaes();
            default:
                return null;
        }
    }

    private cryHashGeneric getAlgoHasher() {
        switch (cipherDecoded & 0xf) {
            case 0x1:
                return new cryHashMd5();
            case 0x2:
                return new cryHashSha1();
            case 0x3:
                return new cryHashSha2256();
            case 0x4:
                return new cryHashSha2384();
            case 0x5:
                return new cryHashSha2512();
            default:
                return null;
        }
    }

    private cryHashGeneric initAlgoHasher(byte[] key) {
        cryHashGeneric alg = getAlgoHasher();
        if (alg == null) {
            return null;
        }
        if (minVer == 0x300) {
            return new cryHashSslMac(alg, key, true);
        }
        return new cryHashHmac(alg, key);
    }

    private cryEncrGeneric initAlgoCipher(byte[] key, byte[] iv, boolean enc) {
        cryEncrGeneric alg = getAlgoCipher();
        if (alg == null) {
            return null;
        }
        alg.init(key, iv, enc);
        return alg;
    }

    /**
     * initialize keys
     *
     * @param client true=client, false=server
     * @return false on success, true on error
     */
    public boolean calcKeys(boolean client) {
        switch (cipherDecoded & 0xf000) {
            case 0x1000:
                break;
            case 0x2000:
                preMaster = cryUtils.bigUint2buf(diffHell.common);
                break;
        }
        switch (minVer) {
            case 0x300:
                masterSec = genHashV09(preMaster, clntRand, servRand, 48);
                keyBlockD = genHashV09(masterSec, servRand, clntRand, 256);
                break;
            case 0x301:
            case 0x302:
                masterSec = genHashV10(preMaster, "master secret", clntRand, servRand, 48);
                keyBlockD = genHashV10(masterSec, "key expansion", servRand, clntRand, 256);
                break;
            case 0x303:
                masterSec = genHashV12(preMaster, "master secret", clntRand, servRand, 48);
                keyBlockD = genHashV12(masterSec, "key expansion", servRand, clntRand, 256);
                break;
            default:
                return true;
        }
        macCS = getKeyMaterial(getAlgoHasher().getHashSize());
        macSC = getKeyMaterial(getAlgoHasher().getHashSize());
        encCS = getKeyMaterial(getAlgoCipher().getKeySize());
        encSC = getKeyMaterial(getAlgoCipher().getKeySize());
        ivCS = getKeyMaterial(getAlgoCipher().getBlockSize());
        ivSC = getKeyMaterial(getAlgoCipher().getBlockSize());
        applyKeys(client, false);
        return false;
    }

    /**
     * apply keys
     *
     * @param client true if client, false if server
     * @param aead true if aead mode needed
     */
    public void applyKeys(boolean client, boolean aead) {
        if (debugger.secTlsTraf) {
            logger.debug("premaster=" + bits.byteDump(preMaster, 0, -1) + " master=" + bits.byteDump(masterSec, 0, -1)
                    + " ivCS=" + bits.byteDump(ivCS, 0, -1) + " ivSC=" + bits.byteDump(ivSC, 0, -1) + " encCS="
                    + bits.byteDump(encCS, 0, -1) + " encSC=" + bits.byteDump(encSC, 0, -1) + " macCS="
                    + bits.byteDump(macCS, 0, -1) + " macSC=" + bits.byteDump(macSC, 0, -1));
        }
        if (client) {
            if (!aead) {
                lower.macTx = initAlgoHasher(macCS);
            }
            if (!aead) {
                lower.macRx = initAlgoHasher(macSC);
            }
            lower.encTx = initAlgoCipher(encCS, ivCS, true);
            lower.encRx = initAlgoCipher(encSC, ivSC, false);
            lower.keyTx = encCS;
            lower.keyRx = encSC;
            lower.ivTx = ivCS;
            lower.ivRx = ivSC;
        } else {
            if (!aead) {
                lower.macTx = initAlgoHasher(macSC);
            }
            if (!aead) {
                lower.macRx = initAlgoHasher(macCS);
            }
            lower.encTx = initAlgoCipher(encSC, ivSC, true);
            lower.encRx = initAlgoCipher(encCS, ivCS, false);
            lower.keyTx = encSC;
            lower.keyRx = encCS;
            lower.ivTx = ivSC;
            lower.ivRx = ivCS;
        }
        lower.aeadMode = aead;
        if (!aead) {
            return;
        }
        lower.aeadTx = 0;
        lower.aeadRx = 0;
        lower.padModulo = lower.encTx.getTagSize();
    }

    /**
     * initialize handshake keys
     *
     * @param client true=client, false=server
     * @return false on success, true on error
     */
    public boolean calcKeysHs(boolean client) {
        if (client) {
            if (ecDiffHell.servPub == null) {
                return true;
            }
            ecDiffHell.clntKey();
        } else {
            if (ecDiffHell.clntPub == null) {
                return true;
            }
            ecDiffHell.servKey();
        }
        byte[] buf = ecDiffHell.common.getBytesX();
        if (debugger.secTlsTraf) {
            logger.debug("clnt=" + bits.byteDump(ecDiffHell.clntPub.toBytes1(), 0, -1) + " serv=" + bits.byteDump(ecDiffHell.servPub.toBytes1(), 0, -1) + " common=" + bits.byteDump(buf, 0, -1));
        }
        cryHashGeneric h = getAlgoHasher();
        preMaster = genHashV13t(h, new byte[h.getHashSize()], new byte[h.getHashSize()]);
        preMaster = genHashV13d(h, preMaster, "derived", "");
        preMaster = genHashV13t(h, preMaster, ecDiffHell.common.getBytesX());
        masterSec = genHashV13d(h, preMaster, "derived", "");
        masterSec = genHashV13t(h, masterSec, new byte[h.getHashSize()]);
        byte[] xch = calcExchangeSumV13(h);
        buf = genHashV13d(h, preMaster, "s hs traffic", xch);
        hndshkSC = buf;
        encSC = genHashV13l(h, buf, "key".getBytes(), new byte[0], getAlgoCipher().getKeySize());
        ivSC = genHashV13l(h, buf, "iv".getBytes(), new byte[0], getAlgoCipher().getIVsize());
        buf = genHashV13d(h, preMaster, "c hs traffic", xch);
        hndshkCS = buf;
        encCS = genHashV13l(h, buf, "key".getBytes(), new byte[0], getAlgoCipher().getKeySize());
        ivCS = genHashV13l(h, buf, "iv".getBytes(), new byte[0], getAlgoCipher().getIVsize());
        applyKeys(client, true);
        return false;
    }

    /**
     * initialize application keys
     */
    public void calcKeysAp() {
        cryHashGeneric h = getAlgoHasher();
        byte[] xch = calcExchangeSumV13(h);
        byte[] buf = genHashV13d(h, masterSec, "s ap traffic", xch);
        encSC = genHashV13l(h, buf, "key".getBytes(), new byte[0], getAlgoCipher().getKeySize());
        ivSC = genHashV13l(h, buf, "iv".getBytes(), new byte[0], getAlgoCipher().getIVsize());
        buf = genHashV13d(h, masterSec, "c ap traffic", xch);
        encCS = genHashV13l(h, buf, "key".getBytes(), new byte[0], getAlgoCipher().getKeySize());
        ivCS = genHashV13l(h, buf, "iv".getBytes(), new byte[0], getAlgoCipher().getIVsize());
    }

}
