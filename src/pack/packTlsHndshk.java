package pack;

import java.util.ArrayList;
import java.util.List;

import util.bits;
import util.debugger;
import util.logger;
import cry.cryCertificate;
import cry.cryEncrCBCaes;
import cry.cryEncrCBCdes;
import cry.cryEncrCBCdes3;
import cry.cryEncrGeneric;
import cry.cryHashGeneric;
import cry.cryHashHmac;
import cry.cryHashMd5;
import cry.cryHashSha1;
import cry.cryHashSha2256;
import cry.cryHashSha2512;
import cry.cryHashSslMac;
import cry.cryKeyDH;
import cry.cryKeyDSA;
import cry.cryKeyRSA;
import cry.cryUtils;

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
     * server random
     */
    public byte[] servRand;

    /**
     * client random
     */
    public byte[] clntRand;

    /**
     * verify random
     */
    public byte[] vrfyRand;

    /**
     * list of ciphers
     */
    public int[] cipherList;

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
     * signature hash
     */
    public int signHsh = -1;

    /**
     * signature algorithm
     */
    public int signAlg = -1;

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
     * init vector client->server
     */
    public byte[] ivCS;

    /**
     * init vector server->client
     */
    public byte[] ivSC;

    /**
     * encryption client->server
     */
    public byte[] encCS;

    /**
     * encryption server->client
     */
    public byte[] encSC;

    /**
     * integrity client->server
     */
    public byte[] macCS;

    /**
     * integrity server->client
     */
    public byte[] macSC;

    /**
     * finished payload
     */
    public byte[] finished;

    /**
     * hash of exchange
     */
    public List<Integer> xchgHash;

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
            default:
                return "unknown=" + i;
        }
    }

    /**
     * decode cipher suite code
     *
     * @param i cipher suite
     * @return decoded format, -1 if unknown format: 0x0000|key|sign|cipher|hash
     * kex: 1=rsa, 2=dhe sign: 1=rsa, 2=dss cipher: 1=des, 2=3des, 3=aes hash:
     * 1=md5, 2=sha1, 3=sha256
     */
    public static int decodeCipherCode(int i) {
        switch (i) {
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
            case 0x10:
                s += "des";
                break;
            case 0x20:
                s += "3des";
                break;
            case 0x30:
                s += "aes";
                break;
            default:
                s += "?";
                break;
        }
        s += "-";
        switch (i & 0xf) {
            case 0x1:
                s += "md5";
                break;
            case 0x2:
                s += "sha1";
                break;
            case 0x3:
                s += "sha256";
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
     * @param dtls datagram mode
     *
     * @param pack
     */
    public packTlsHndshk(packTls pack, boolean dtls) {
        lower = pack;
        datagram = dtls;
        clearXchgHash();
    }

    /**
     * clear exchange hash
     */
    public void clearXchgHash() {
        xchgHash = new ArrayList<Integer>();
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
            xchgHash.add(lower.pckDat.getByte(i));
        }
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
            xchgHash.add(lower.pckDat.getByte(i));
        }
    }

    private static int[] makeCipherList() {
        List<Integer> l1 = new ArrayList<Integer>();
        for (int i = 0x100; i >= 0; i--) {
            if (decodeCipherCode(i) > 0) {
                l1.add(i);
            }
        }
        int[] l2 = new int[l1.size()];
        for (int i = 0; i < l2.length; i++) {
            l2[i] = l1.get(i);
        }
        return l2;
    }

    private static byte[] makeCompressList() {
        byte[] buf = new byte[1];
        buf[0] = 0;
        return buf;
    }

    private static byte[] makeRandomCookie() {
        byte[] buf = new byte[32];
        for (int i = 0; i < buf.length; i++) {
            buf[i] = (byte) bits.randomB();
        }
        return buf;
    }

    private static int[] decodeCipherList(byte[] src) {
        int[] trg = new int[src.length / 2];
        for (int i = 0; i < trg.length; i++) {
            trg[i] = bits.msbGetW(src, i * 2);
        }
        return trg;
    }

    private static byte[] encodeCipherList(int[] src) {
        byte[] trg = new byte[src.length * 2];
        for (int i = 0; i < src.length; i++) {
            bits.msbPutW(trg, i * 2, src[i]);
        }
        return trg;
    }

    private static byte[] makeExtensionList() {
        byte[] buf = new byte[5];
        bits.msbPutW(buf, 0, 0xff01); // type
        bits.msbPutW(buf, 2, 1); // length
        buf[4] = 0; // value
        return buf;
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
        extnsnList = makeExtensionList();
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
        lower.getBytes(1); // session id
        if (datagram) {
            vrfyRand = lower.getBytes(1);
        }
        cipherList = decodeCipherList(lower.getBytes(2));
        cmprssList = lower.getBytes(1);
        extnsnList = lower.getBytes(2);
        clntHelloDump("rx");
        return false;
    }

    /**
     * create client hello
     */
    public void clntHelloCreate() {
        pckTyp = typeHeloClnt;
        lower.verCurr = minVer;
        lower.pckDat.clear();
        int i = maxVer;
        if (datagram) {
            i = packTls.version2dtls(i);
        }
        lower.pckDat.msbPutW(0, i);
        lower.pckDat.putSkip(2);
        lower.putBytes(clntRand, 0);
        lower.putBytes(null, 1); // session id
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
        for (int i = 0; i < cipherList.length; i++) {
            s += "," + cipher2string(decodeCipherCode(cipherList[i]));
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
        lower.verCurr = minVer;
        lower.pckDat.clear();
        int i = maxVer;
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
        int p = -1;
        for (int i = 0; i < cipherList.length; i++) {
            int q = decodeCipherCode(cipherList[i]);
            if (q < p) {
                continue;
            }
            o = i;
            p = q;
        }
        if (p < 0) {
            return true;
        }
        cipherWant = cipherList[o];
        cipherDecoded = decodeCipherCode(cipherWant);
        servRand = makeRandomCookie();
        extnsnList = makeExtensionList();
        versionFill();
        return false;
    }

    /**
     * create server hello
     */
    public void servHelloCreate() {
        pckTyp = typeHeloServ;
        lower.verCurr = minVer;
        lower.pckDat.clear();
        int i = maxVer;
        if (datagram) {
            i = packTls.version2dtls(i);
        }
        lower.pckDat.msbPutW(0, i);
        lower.pckDat.putSkip(2);
        lower.putBytes(servRand, 0);
        lower.putBytes(null, 1); // session id
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
        lower.getBytes(1); // session id
        cipherWant = lower.pckDat.msbGetW(0);
        cmprssWant = lower.pckDat.getByte(2);
        lower.pckDat.getSkip(3);
        extnsnList = lower.getBytes(2);
        cipherDecoded = decodeCipherCode(cipherWant);
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
    public void certDatCreate() {
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
    public boolean certDatParse() {
        if (pckTyp != typeCertDat) {
            return true;
        }
        lower.pckDat.getSkip(3);
        certificates = new ArrayList<byte[]>();
        for (; lower.pckDat.dataSize() > 0;) {
            byte[] cert = lower.getBytes(3);
            certificates.add(cert);
        }
        certDatDump("rx");
        return false;
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

    private byte[] servKexSign() {
        packTls h = new packTls(datagram);
        h.putBytes(clntRand, 0);
        h.putBytes(servRand, 0);
        h.putBytes(cryUtils.bigUint2buf(diffHell.modulus), 2);
        h.putBytes(cryUtils.bigUint2buf(diffHell.group), 2);
        h.putBytes(cryUtils.bigUint2buf(diffHell.servPub), 2);
        h.pckDat.merge2beg();
        byte[] raw = h.pckDat.getCopy();
        byte[] hsh = null;
        if (signHsh < 0) {
            switch (cipherDecoded & 0xf00) {
                case 0x100:
                    hsh = bits.byteConcat(cryHashGeneric.compute(new cryHashMd5(), raw),
                            cryHashGeneric.compute(new cryHashSha1(), raw));
                    break;
                case 0x200:
                    hsh = cryHashGeneric.compute(new cryHashSha1(), raw);
                    break;
                default:
                    return null;
            }
        } else {
            cryHashGeneric alg = null;
            switch (signHsh) {
                case 1:
                    alg = new cryHashMd5();
                    break;
                case 2:
                    alg = new cryHashSha1();
                    break;
                case 4:
                    alg = new cryHashSha2256();
                    break;
                case 6:
                    alg = new cryHashSha2512();
                    break;
                default:
                    return null;
            }
            hsh = cryHashGeneric.compute(alg, raw);
        }
        if (debugger.secTlsTraf) {
            logger.debug("paramHash=" + bits.byteDump(hsh, 0, -1));
        }
        return hsh;
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
        if (lower.verCurr >= 0x303) {
            signHsh = 2;
        }
        byte[] hash = servKexSign();
        switch (cipherDecoded & 0xf00) {
            case 0x100:
                signDat = keyrsa.tlsSigning(lower.verCurr, hash);
                signAlg = 1;
                break;
            case 0x200:
                signDat = keydsa.tlsSigning(lower.verCurr, hash);
                signAlg = 2;
                break;
            default:
                signDat = new byte[0];
                break;
        }
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
        if (lower.verCurr >= 0x303) {
            lower.pckDat.putByte(0, signHsh);
            lower.pckDat.putByte(1, signAlg);
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
        if (lower.verCurr >= 0x303) {
            signHsh = lower.pckDat.getByte(0);
            signAlg = lower.pckDat.getByte(1);
            lower.pckDat.getSkip(2);
        }
        signDat = lower.getBytes(2);
        servKexDump("rx");
        cryCertificate crt = new cryCertificate();
        if (crt.asn1ReadBuf(certificates.get(0))) {
            return true;
        }
        if (debugger.secTlsTraf) {
            logger.debug("cert=" + crt);
        }
        byte[] hash = servKexSign();
        if (crt.key.tlsVerify(lower.verCurr, hash, signDat)) {
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

    private byte[] calcExchangeSum(cryHashGeneric h, byte[] pre, byte[] post) {
        h.init();
        if (pre != null) {
            for (int i = 0; i < pre.length; i++) {
                h.update(pre[i]);
            }
        }
        for (int i = 0; i < xchgHash.size(); i++) {
            h.update(xchgHash.get(i));
        }
        if (post != null) {
            for (int i = 0; i < post.length; i++) {
                h.update(post[i]);
            }
        }
        return h.finish();
    }

    private byte[] calcFinished(boolean client) {
        String s = "";
        if (client) {
            s = "client finished";
        } else {
            s = "server finished";
        }
        switch (lower.verCurr) {
            case 0x300:
                if (client) {
                    s = "CLNT";
                } else {
                    s = "SRVR";
                }
                byte[] h1 = calcExchangeSum(new cryHashSslMac(new cryHashMd5(), masterSec, false), null, s.getBytes());
                byte[] h2 = calcExchangeSum(new cryHashSslMac(new cryHashSha1(), masterSec, false), null, s.getBytes());
                return bits.byteConcat(h1, h2);
            case 0x301:
            case 0x302:
                h1 = calcExchangeSum(new cryHashMd5(), null, null);
                h2 = calcExchangeSum(new cryHashSha1(), null, null);
                return genHashV10(masterSec, s, h1, h2, 12);
            case 0x303:
                h1 = calcExchangeSum(new cryHashSha2256(), null, null);
                h2 = new byte[0];
                return genHashV12(masterSec, s, h1, h2, 12);
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
            default:
                return null;
        }
    }

    private cryHashGeneric initAlgoHasher(byte[] key) {
        cryHashGeneric alg = getAlgoHasher();
        if (alg == null) {
            return null;
        }
        if (lower.verCurr == 0x300) {
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
        switch (lower.verCurr) {
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
        if (debugger.secTlsTraf) {
            logger.debug("premaster=" + bits.byteDump(preMaster, 0, -1) + " master=" + bits.byteDump(masterSec, 0, -1)
                    + " ivCS=" + bits.byteDump(ivCS, 0, -1) + " ivSC=" + bits.byteDump(ivSC, 0, -1) + " encCS="
                    + bits.byteDump(encCS, 0, -1) + " encSC=" + bits.byteDump(encSC, 0, -1) + " macCS="
                    + bits.byteDump(macCS, 0, -1) + " macSC=" + bits.byteDump(macSC, 0, -1));
        }
        if (client) {
            lower.macTx = initAlgoHasher(macCS);
            lower.macRx = initAlgoHasher(macSC);
            lower.encTx = initAlgoCipher(encCS, ivCS, true);
            lower.encRx = initAlgoCipher(encSC, ivSC, false);
        } else {
            lower.macTx = initAlgoHasher(macSC);
            lower.macRx = initAlgoHasher(macCS);
            lower.encTx = initAlgoCipher(encSC, ivSC, true);
            lower.encRx = initAlgoCipher(encCS, ivCS, false);
        }
        return false;
    }

}
