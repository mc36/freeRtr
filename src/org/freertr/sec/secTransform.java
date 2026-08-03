package org.freertr.sec;

import java.util.List;
import org.freertr.cry.cryEncrCBCaes;
import org.freertr.cry.cryEncrCBCblowfish;
import org.freertr.cry.cryEncrCBCdes;
import org.freertr.cry.cryEncrCBCdes3;
import org.freertr.cry.cryEncrCBCrc2;
import org.freertr.cry.cryEncrCFBaes;
import org.freertr.cry.cryEncrCTRaes;
import org.freertr.cry.cryEncrECBaes;
import org.freertr.cry.cryEncrGCMaes;
import org.freertr.cry.cryEncrGeneric;
import org.freertr.cry.cryEncrNone;
import org.freertr.cry.cryEncrOFBaes;
import org.freertr.cry.cryEncrPCBCaes;
import org.freertr.cry.cryHashGeneric;
import org.freertr.cry.cryHashHmac;
import org.freertr.cry.cryHashMd5;
import org.freertr.cry.cryHashNone;
import org.freertr.cry.cryHashSha1;
import org.freertr.cry.cryHashSha2224;
import org.freertr.cry.cryHashSha2256;
import org.freertr.cry.cryHashSha2384;
import org.freertr.cry.cryHashSha2512;
import org.freertr.cry.cryHashSha3224;
import org.freertr.cry.cryHashSha3256;
import org.freertr.cry.cryHashSha3384;
import org.freertr.cry.cryHashSha3512;
import org.freertr.cry.cryKeyCurve25519;
import org.freertr.cry.cryKeyDH;
import org.freertr.cry.cryKeyECDH;
import org.freertr.cry.cryKeyGeneric;
import org.freertr.cry.cryKeyMLKEM;
import org.freertr.cry.cryKeyPQhybrid;
import org.freertr.pack.packHolder;
import org.freertr.user.userHelp;
import org.freertr.util.bits;
import org.freertr.util.cmds;

/**
 * one isakmp (rfc2408) transform
 *
 * @author matecsaba
 */
public class secTransform {

    /**
     * create instance
     */
    public secTransform() {
    }

    /**
     * transform number
     */
    public int number;

    /**
     * transform id
     */
    public int transId;

    /**
     * encryption algorithm 1=des, 2=blowfish, 3=3des, 4=aescbc, 5=aescfb,
     * 6=aesecb, 7=none, 8=aesgcm, 9=rc2, 10=aesctr, 11=aesofb, 12=aespcbc
     */
    public int encrAlg;

    /**
     * encryption key length
     */
    public int encrKey;

    /**
     * hash algorithm 1=md5, 2=sha1, 3=sha256, 4=sha512, 5=sha224, 6=sha384
     * 7=sha3224, 8=sha3256, 9=sha3384, 10=sha3512, 11=none
     */
    public int hashAlg;

    /**
     * pseudo random function
     */
    public int prfAlg;

    /**
     * authentication algorithm 1=preshared key
     */
    public int authAlg;

    /**
     * encapsulation method
     */
    public int encapMet;

    /**
     * group number
     */
    public int groupNum;

    /**
     * lifetime in seconds
     */
    public int lifeSec;

    /**
     * random lifetime
     */
    public int lifeRnd;

    /**
     * lifetime in kilobytes
     */
    public long lifeByt;

    private int propTyp;

    private int propVal;

    /**
     * copy bytes of this transform
     *
     * @return copied version
     */
    public secTransform copyBytes() {
        secTransform n = new secTransform();
        n.encapMet = encapMet;
        n.authAlg = authAlg;
        n.encrAlg = encrAlg;
        n.encrKey = encrKey;
        n.prfAlg = prfAlg;
        n.groupNum = groupNum;
        n.hashAlg = hashAlg;
        n.lifeByt = lifeByt;
        n.lifeSec = lifeSec;
        n.lifeRnd = lifeRnd;
        n.number = number;
        n.transId = transId;
        return n;
    }

    public String toString() {
        return "auth=" + authAlg + " grp=" + groupNum + " encr=" + encr2str() + "/" + encrKey + " hash=" + hash2str();
    }

    /**
     * decode encryption algorithm
     *
     * @return string
     */
    public String encr2str() {
        switch (encrAlg) {
            case 1:
                return "des";
            case 2:
                return "blowfish";
            case 3:
                return "3des";
            case 4:
                return "aes" + encrKey + "cbc";
            case 5:
                return "aes" + encrKey + "cfb";
            case 6:
                return "aes" + encrKey + "ecb";
            case 7:
                return "none";
            case 8:
                return "aes" + encrKey + "gcm";
            case 9:
                return "rc2";
            case 10:
                return "aes" + encrKey + "ctr";
            case 11:
                return "aes" + encrKey + "ofb";
            case 12:
                return "aes" + encrKey + "pcbc";
            default:
                return "unknown";
        }
    }

    /**
     * encode encryption algorithm
     *
     * @param s string
     */
    public void str2encr(String s) {
        encrAlg = 0;
        encrKey = 0;
        if (s.equals("des")) {
            encrAlg = 1;
        }
        if (s.equals("blowfish")) {
            encrAlg = 2;
        }
        if (s.equals("3des")) {
            encrAlg = 3;
        }
        if (s.equals("aes128cbc")) {
            encrAlg = 4;
            encrKey = 128;
        }
        if (s.equals("aes192cbc")) {
            encrAlg = 4;
            encrKey = 192;
        }
        if (s.equals("aes256cbc")) {
            encrAlg = 4;
            encrKey = 256;
        }
        if (s.equals("aes128cfb")) {
            encrAlg = 5;
            encrKey = 128;
        }
        if (s.equals("aes192cfb")) {
            encrAlg = 5;
            encrKey = 192;
        }
        if (s.equals("aes256cfb")) {
            encrAlg = 5;
            encrKey = 256;
        }
        if (s.equals("aes128ecb")) {
            encrAlg = 6;
            encrKey = 128;
        }
        if (s.equals("aes192ecb")) {
            encrAlg = 6;
            encrKey = 192;
        }
        if (s.equals("aes256ecb")) {
            encrAlg = 6;
            encrKey = 256;
        }
        if (s.equals("none")) {
            encrAlg = 7;
        }
        if (s.equals("aes128gcm")) {
            encrAlg = 8;
            encrKey = 128;
        }
        if (s.equals("aes192gcm")) {
            encrAlg = 8;
            encrKey = 192;
        }
        if (s.equals("aes256gcm")) {
            encrAlg = 8;
            encrKey = 256;
        }
        if (s.equals("rc2")) {
            encrAlg = 9;
        }
        if (s.equals("aes128ctr")) {
            encrAlg = 10;
            encrKey = 128;
        }
        if (s.equals("aes192ctr")) {
            encrAlg = 10;
            encrKey = 192;
        }
        if (s.equals("aes256ctr")) {
            encrAlg = 10;
            encrKey = 256;
        }
        if (s.equals("aes128ofb")) {
            encrAlg = 11;
            encrKey = 128;
        }
        if (s.equals("aes192ofb")) {
            encrAlg = 11;
            encrKey = 192;
        }
        if (s.equals("aes256ofb")) {
            encrAlg = 11;
            encrKey = 256;
        }
        if (s.equals("aes128pcbc")) {
            encrAlg = 12;
            encrKey = 128;
        }
        if (s.equals("aes192pcbc")) {
            encrAlg = 12;
            encrKey = 192;
        }
        if (s.equals("aes256pcbc")) {
            encrAlg = 12;
            encrKey = 256;
        }
    }

    /**
     * check if aead mode
     *
     * @return true if yes, false if no
     */
    public boolean isAead() {
        return encrAlg == 8;
    }

    /**
     * encode hash algorithm
     *
     * @param s string
     * @return value
     */
    public static int str2hash(String s) {
        if (s.equals("md5")) {
            return 1;
        }
        if (s.equals("sha1")) {
            return 2;
        }
        if (s.equals("sha256")) {
            return 3;
        }
        if (s.equals("sha512")) {
            return 4;
        }
        if (s.equals("sha224")) {
            return 5;
        }
        if (s.equals("sha384")) {
            return 6;
        }
        if (s.equals("sha3224")) {
            return 7;
        }
        if (s.equals("sha3256")) {
            return 8;
        }
        if (s.equals("sha3384")) {
            return 9;
        }
        if (s.equals("sha3512")) {
            return 10;
        }
        if (s.equals("none")) {
            return 11;
        }
        return 0;
    }

    /**
     * decode hash algorithm
     *
     * @param i value
     * @return string
     */
    public static String hash2str(int i) {
        switch (i) {
            case 1:
                return "md5";
            case 2:
                return "sha1";
            case 3:
                return "sha256";
            case 4:
                return "sha512";
            case 5:
                return "sha224";
            case 6:
                return "sha384";
            case 7:
                return "sha3224";
            case 8:
                return "sha3256";
            case 9:
                return "sha3384";
            case 10:
                return "sha3512";
            case 11:
                return "none";
            default:
                return "unknown";
        }
    }

    /**
     * decode hash algorithm
     *
     * @return string
     */
    public String hash2str() {
        return hash2str(hashAlg);
    }

    /**
     * decode hash algorithm
     *
     * @return string
     */
    public String prf2str() {
        return hash2str(prfAlg);
    }

    /**
     * decode main mode encryption algorithm
     *
     * @param i encoded value
     * @return decoded value
     */
    public static int decodeMMencr(int i) {
        switch (i) {
            case 1:
                return 1;
            case 3:
                return 2;
            case 4:
                return 9;
            case 5:
                return 3;
            case 7:
                return 4;
            case 13:
                return 10;
            case 20:
                return 8;
            default:
                return 0;
        }
    }

    /**
     * encode main mode encryption algorithm
     *
     * @param i encoded value
     * @return decoded value
     */
    public static int encodeMMencr(int i) {
        switch (i) {
            case 1:
                return 1;
            case 2:
                return 3;
            case 3:
                return 5;
            case 4:
                return 7;
            case 8:
                return 20;
            case 9:
                return 4;
            case 10:
                return 13;
            default:
                return 0;
        }
    }

    /**
     * decode quick mode encryption algorithm
     *
     * @param i encoded value
     * @return decoded value
     */
    public static int decodeQMencr(int i) {
        switch (i) {
            case 2:
                return 1;
            case 7:
                return 2;
            case 3:
                return 3;
            case 4:
                return 9;
            case 12:
                return 4;
            case 13:
                return 10;
            case 20:
                return 8;
            default:
                return 0;
        }
    }

    /**
     * encode quick mode encryption algorithm
     *
     * @param i encoded value
     * @return decoded value
     */
    public static int encodeQMencr(int i) {
        switch (i) {
            case 1:
                return 2;
            case 2:
                return 7;
            case 3:
                return 3;
            case 4:
                return 12;
            case 8:
                return 20;
            case 9:
                return 4;
            case 10:
                return 13;
            default:
                return 0;
        }
    }

    /**
     * decode main mode hash algorithm
     *
     * @param i encoded value
     * @return decoded value
     */
    public static int decodeMMhash(int i) {
        switch (i) {
            case 1:
                return 1;
            case 2:
                return 2;
            case 4:
                return 3;
            case 5:
                return 6;
            case 6:
                return 4;
            default:
                return 0;
        }
    }

    /**
     * encode main mode hash algorithm
     *
     * @param i encoded value
     * @return decoded value
     */
    public static int encodeMMhash(int i) {
        switch (i) {
            case 1:
                return 1;
            case 2:
                return 2;
            case 3:
                return 4;
            case 4:
                return 6;
            case 6:
                return 5;
            default:
                return 0;
        }
    }

    /**
     * decode quick mode hash algorithm
     *
     * @param i encoded value
     * @return decoded value
     */
    public static int decodeQMhash(int i) {
        switch (i) {
            case 1:
                return 1;
            case 2:
                return 2;
            case 5:
                return 3;
            case 7:
                return 4;
            case 6:
                return 6;
            default:
                return 0;
        }
    }

    /**
     * encode quick mode hash algorithm
     *
     * @param i encoded value
     * @return decoded value
     */
    public static int encodeQMhash(int i) {
        switch (i) {
            case 1:
                return 1;
            case 2:
                return 2;
            case 3:
                return 5;
            case 4:
                return 7;
            case 6:
                return 6;
            default:
                return 0;
        }
    }

    /**
     * decode ike2 hash algorithm
     *
     * @param i encoded value
     * @return decoded value
     */
    public static int decode2hash(int i) {
        switch (i) {
            case 1:
                return 1;
            case 2:
                return 2;
            case 12:
                return 3;
            case 13:
                return 6;
            case 14:
                return 4;
            default:
                return 0;
        }
    }

    /**
     * encode ike2 hash algorithm
     *
     * @param i encoded value
     * @return decoded value
     */
    public static int encode2hash(int i) {
        switch (i) {
            case 1:
                return 1;
            case 2:
                return 2;
            case 3:
                return 12;
            case 4:
                return 14;
            case 6:
                return 13;
            default:
                return 0;
        }
    }

    /**
     * parse main mode transform from packet
     *
     * @param pck packet to read from
     * @return false on success, true on error
     */
    public boolean parseMMtransform(packHolder pck) {
        boolean res = true;
        int end = pck.dataSize() - pck.msbGetW(2);
        number = pck.getByte(4);
        transId = pck.getByte(5);
        pck.getSkip(8);
        int lifeType = 0;
        for (;;) {
            if (pck.dataSize() <= end) {
                res = false;
                break;
            }
            get1property(pck);
            switch (propTyp) {
                case 1:
                    encrAlg = decodeMMencr(propVal);
                    break;
                case 2:
                    hashAlg = decodeMMhash(propVal);
                    break;
                case 3:
                    authAlg = propVal;
                    break;
                case 4:
                    groupNum = propVal;
                    break;
                case 11:
                    lifeType = propVal;
                    break;
                case 14:
                    encrKey = propVal;
                    break;
                case 12:
                    switch (lifeType) {
                        case 1:
                            lifeSec = propVal;
                            break;
                        case 2:
                            lifeByt = propVal;
                            break;
                    }
                    lifeType = 0;
                    break;
                default:
                    break;
            }
        }
        pck.setBytesLeft(end);
        return res;
    }

    /**
     * create main mode transform
     *
     * @param pck packet to write to
     * @param isNext next transform will follow
     */
    public void createMMtransform(packHolder pck, boolean isNext) {
        int siz = pck.headSize();
        pck.msbPutW(0, 0);
        if (isNext) {
            pck.putByte(0, 3);
        }
        pck.putByte(4, number);
        pck.putByte(5, transId);
        pck.msbPutW(6, 0);
        pck.putSkip(8);
        add1property(pck, 1, encodeMMencr(encrAlg));
        add1property(pck, 14, encrKey);
        add1property(pck, 2, encodeMMhash(hashAlg));
        add1property(pck, 3, authAlg);
        add1property(pck, 4, groupNum);
        if (lifeSec > 0) {
            add1property(pck, 11, 1);
            add1property(pck, 12, lifeSec);
        }
        if (lifeByt > 0) {
            add1property(pck, 11, 2);
            add1property(pck, 12, (int) lifeByt);
        }
        siz = pck.headSize() - siz;
        pck.msbPutW(2 - siz, siz);
    }

    /**
     * parse quick mode transform from packet
     *
     * @param pck packet to read from
     * @return false on success, true on error
     */
    public boolean parseQMtransform(packHolder pck) {
        int end = pck.dataSize() - pck.msbGetW(2);
        number = pck.getByte(4);
        encrAlg = decodeQMencr(pck.getByte(5));
        pck.getSkip(8);
        int lifeType = 0;
        for (;;) {
            if (pck.dataSize() <= end) {
                break;
            }
            get1property(pck);
            switch (propTyp) {
                case 4:
                    encapMet = propVal;
                    break;
                case 5:
                    hashAlg = decodeQMhash(propVal);
                    break;
                case 6:
                    encrKey = propVal;
                    break;
                case 1:
                    lifeType = propVal;
                    break;
                case 2:
                    switch (lifeType) {
                        case 1:
                            lifeSec = propVal;
                            break;
                        case 2:
                            lifeByt = propVal;
                            break;
                    }
                    lifeType = 0;
                    break;
                default:
                    break;
            }
        }
        return false;
    }

    /**
     * create quick mode transform
     *
     * @param pck packet to write to
     * @param isNext next transform will follow
     */
    public void createQMtransform(packHolder pck, boolean isNext) {
        int siz = pck.headSize();
        pck.msbPutW(0, 0);
        if (isNext) {
            pck.putByte(0, 3);
        }
        pck.putByte(4, number);
        pck.putByte(5, encodeQMencr(encrAlg));
        pck.msbPutW(6, 0);
        pck.putSkip(8);
        add1property(pck, 4, encapMet);
        add1property(pck, 6, encrKey);
        add1property(pck, 5, encodeQMhash(hashAlg));
        if (lifeSec > 0) {
            add1property(pck, 1, 1);
            add1property(pck, 2, lifeSec);
        }
        if (lifeByt > 0) {
            add1property(pck, 1, 2);
            add1property(pck, 2, (int) lifeByt);
        }
        siz = pck.headSize() - siz;
        pck.msbPutW(2 - siz, siz);
    }

    /**
     * parse ike2 transform from packet
     *
     * @param pck packet to read from
     * @return false on success, true on error
     */
    public boolean parse2transform(packHolder pck) {
        int typ = pck.getByte(0);
        int len = pck.msbGetW(2) - 4;
        pck.getSkip(4);
        if (len < 0) {
            return true;
        }
        propTyp = pck.getByte(0);
        propVal = pck.msbGetW(2);
        pck.getSkip(4);
        len -= 4;
        switch (propTyp) {
            case 1:
                encrAlg = decodeQMencr(propVal);
                break;
            case 2:
                prfAlg = decodeQMhash(propVal);
                break;
            case 3:
                hashAlg = decode2hash(propVal);
                break;
            case 4:
                groupNum = propVal;
                break;
            case 5:
                encapMet = propVal + 1;
                break;
        }
        for (;;) {
            if (len < 1) {
                break;
            }
            typ = pck.dataSize();
            get1property(pck);
            len -= typ - pck.dataSize();
            switch (propTyp) {
                case 14:
                    encrKey = propVal;
                    break;
            }
        }
        return false;
    }

    /**
     * create ike2 transform
     *
     * @param pck packet to write to
     * @return number of transforms
     */
    public int create2transform(packHolder pck) {
        int trnC = 0;
        int trnP = -1;
        if (encrAlg > 0) {
            byte[] buf;
            if (encrKey > 0) {
                buf = new byte[8];
            } else {
                buf = new byte[4];
            }
            bits.msbPutW(buf, 0, 0x100);
            bits.msbPutW(buf, 2, encodeQMencr(encrAlg));
            if (encrKey > 0) {
                bits.msbPutW(buf, 4, 0x800e);
                bits.msbPutW(buf, 6, encrKey);
            }
            trnP = add2transform(pck, buf);
            trnC++;
        }
        if (prfAlg > 0) {
            byte[] buf = new byte[4];
            bits.msbPutW(buf, 0, 0x200);
            bits.msbPutW(buf, 2, encodeQMhash(prfAlg));
            trnP = add2transform(pck, buf);
            trnC++;
        }
        if (hashAlg > 0) {
            byte[] buf = new byte[4];
            bits.msbPutW(buf, 0, 0x300);
            bits.msbPutW(buf, 2, encode2hash(hashAlg));
            trnP = add2transform(pck, buf);
            trnC++;
        }
        if (groupNum > 0) {
            byte[] buf = new byte[4];
            bits.msbPutW(buf, 0, 0x400);
            bits.msbPutW(buf, 2, groupNum);
            trnP = add2transform(pck, buf);
            trnC++;
        }
        if (encapMet > 0) {
            byte[] buf = new byte[4];
            bits.msbPutW(buf, 0, 0x500);
            bits.msbPutW(buf, 2, encapMet - 1);
            trnP = add2transform(pck, buf);
            trnC++;
        }
        if (trnP >= 0) {
            pck.msbPutW(trnP - pck.headSize(), 0);
        }
        return trnC;
    }

    private int add2transform(packHolder pck, byte[] buf) {
        int ps = pck.headSize();
        pck.msbPutW(0, 0x300);
        pck.msbPutW(2, buf.length + 4);
        pck.putSkip(4);
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        return ps;
    }

    private void get1property(packHolder pck) {
        propTyp = pck.msbGetW(0);
        propVal = pck.msbGetW(2);
        pck.getSkip(4);
        if ((propTyp & 0x8000) != 0) {
            propTyp &= 0x7fff;
            return;
        }
        int siz = propVal;
        switch (siz) {
            case 0:
                propVal = 0;
                break;
            case 1:
                propVal = pck.getByte(0);
                break;
            case 2:
                propVal = pck.msbGetW(0);
                break;
            case 3:
                propVal = pck.msbGetD(0) >>> 8;
                break;
            default:
                propVal = pck.msbGetD(siz - 4);
                break;
        }
        pck.getSkip(siz);
    }

    private void add1property(packHolder pck, int typ, int val) {
        if (val < 1) {
            return;
        }
        if ((val & 0xffff) == val) {
            pck.msbPutW(0, typ | 0x8000);
            pck.msbPutW(2, val);
            pck.putSkip(4);
            return;
        }
        pck.msbPutW(0, typ);
        pck.msbPutW(2, 4);
        pck.msbPutD(4, val);
        pck.putSkip(8);
    }

    /**
     * test if transforms match
     *
     * @param t1 transform 1
     * @param t2 transform 2
     * @return true if match, false if not match
     */
    public static boolean testMatching(secTransform t1, secTransform t2) {
        if (t1.authAlg != t2.authAlg) {
            return false;
        }
        if (t1.encrAlg != t2.encrAlg) {
            return false;
        }
        if (t1.encrKey != t2.encrKey) {
            return false;
        }
        if (t1.prfAlg != t2.prfAlg) {
            return false;
        }
        if (t1.groupNum != t2.groupNum) {
            return false;
        }
        if (t1.hashAlg != t2.hashAlg) {
            return false;
        }
        return true;
    }

    /**
     * find matching transform
     *
     * @param lst list to find in
     * @param need needed transform
     * @return number of transform, -1 on error
     */
    public static int findMatching(List<secTransform> lst, secTransform need) {
        for (int i = 0; i < lst.size(); i++) {
            if (testMatching(need, lst.get(i))) {
                return i;
            }
        }
        return -1;
    }

    /**
     * get key size
     *
     * @return size in bytes
     */
    public int getKeyS() {
        int i = encrKey / 8;
        if (i > 0) {
            return i;
        }
        return getEncr().getKeySize();
    }

    /**
     * get hash size
     *
     * @return size in bytes
     */
    public int getHashS() {
        switch (hashAlg) {
            case 1:
                return 12;
            case 2:
                return 12;
            case 3:
                return 16;
            case 4:
                return 32;
            case 6:
                return 24;
            default:
                return getHash().getHashSize();
        }
    }

    /**
     * get encryption algorithm
     *
     * @return encryption, null on error
     */
    public cryEncrGeneric getEncr() {
        switch (encrAlg) {
            case 1:
                return new cryEncrCBCdes();
            case 2:
                return new cryEncrCBCblowfish();
            case 3:
                return new cryEncrCBCdes3();
            case 4:
                return new cryEncrCBCaes();
            case 5:
                return new cryEncrCFBaes();
            case 6:
                return new cryEncrECBaes();
            case 7:
                return new cryEncrNone();
            case 8:
                return new cryEncrGCMaes();
            case 9:
                return new cryEncrCBCrc2();
            case 10:
                return new cryEncrCTRaes();
            case 11:
                return new cryEncrOFBaes();
            case 12:
                return new cryEncrPCBCaes();
            default:
                return null;
        }
    }

    /**
     * get hash algorithm
     *
     * @param i value
     * @return hash, null on error
     */
    public static cryHashGeneric getHash(int i) {
        cryHashGeneric h = null;
        switch (i) {
            case 1:
                h = new cryHashMd5();
                break;
            case 2:
                h = new cryHashSha1();
                break;
            case 3:
                h = new cryHashSha2256();
                break;
            case 4:
                h = new cryHashSha2512();
                break;
            case 5:
                h = new cryHashSha2224();
                break;
            case 6:
                h = new cryHashSha2384();
                break;
            case 7:
                h = new cryHashSha3224();
                break;
            case 8:
                h = new cryHashSha3256();
                break;
            case 9:
                h = new cryHashSha3384();
                break;
            case 10:
                h = new cryHashSha3512();
                break;
            case 11:
                h = new cryHashNone();
                break;
            default:
                return null;
        }
        h.init();
        return h;
    }

    /**
     * get hash algorithm
     *
     * @return hash, null on error
     */
    public cryHashGeneric getHash() {
        return getHash(hashAlg);
    }

    /**
     * get prf algorithm
     *
     * @return hash, null on error
     */
    public cryHashGeneric getPrf() {
        return getHash(prfAlg);
    }

    /**
     * get hash algorithm in hmac mode
     *
     * @param key key to use
     * @return hash, null on error
     */
    public cryHashGeneric getHmac(byte[] key) {
        cryHashGeneric h = getHash();
        if (h == null) {
            return null;
        }
        if (hashAlg == 11) {
            return h;
        }
        h = new cryHashHmac(h, key);
        h.init();
        return h;
    }

    /**
     * get prf algorithm in hmac mode
     *
     * @param key key to use
     * @return hash, null on error
     */
    public cryHashGeneric getHprf(byte[] key) {
        cryHashGeneric h = getPrf();
        if (h == null) {
            return null;
        }
        if (hashAlg == 11) {
            return h;
        }
        h = new cryHashHmac(h, key);
        h.init();
        return h;
    }

    /**
     * get current group
     *
     * @return group, null if nothing
     */
    public cryKeyGeneric getGroup() {
        switch (groupNum) {
            case 25:
                cryKeyECDH ec = new cryKeyECDH();
                ec.keyMakeTls(19);
                return ec;
            case 26:
                ec = new cryKeyECDH();
                ec.keyMakeTls(21);
                return ec;
            case 19:
                ec = new cryKeyECDH();
                ec.keyMakeTls(23);
                return ec;
            case 20:
                ec = new cryKeyECDH();
                ec.keyMakeTls(24);
                return ec;
            case 21:
                ec = new cryKeyECDH();
                ec.keyMakeTls(25);
                return ec;
            case 28:
                ec = new cryKeyECDH();
                ec.keyMakeTls(26);
                return ec;
            case 29:
                ec = new cryKeyECDH();
                ec.keyMakeTls(27);
                return ec;
            case 30:
                ec = new cryKeyECDH();
                ec.keyMakeTls(28);
                return ec;
            case 31:
                return new cryKeyCurve25519();
            case 35:
                cryKeyMLKEM ml = new cryKeyMLKEM();
                ml.keyMakeSize(512);
                return ml;
            case 36:
                ml = new cryKeyMLKEM();
                ml.keyMakeSize(768);
                return ml;
            case 37:
                ml = new cryKeyMLKEM();
                ml.keyMakeSize(1024);
                return ml;
            case 99:
                return new cryKeyPQhybrid();
            default:
                break;
        }
        cryKeyDH dh = new cryKeyDH();
        if (dh.keyMakeIke(groupNum)) {
            return null;
        }
        return dh;
    }

    /**
     * get running configuration
     *
     * @param beg beginning of lines
     * @param lst list to append
     */
    public void getShRun(String beg, List<String> lst) {
        cmds.cfgLine(lst, groupNum == 0, beg, "group", bits.padBeg("" + groupNum, 2, "0"));
        cmds.cfgLine(lst, encrAlg == 0, beg, "cipher", encr2str());
        cmds.cfgLine(lst, hashAlg == 0, beg, "hash", hash2str());
        cmds.cfgLine(lst, prfAlg == 0, beg, "prf", prf2str());
        cmds.cfgLine(lst, lifeSec == 0, beg, "seconds", "" + lifeSec);
        cmds.cfgLine(lst, lifeByt == 0, beg, "bytes", "" + lifeByt);
        cmds.cfgLine(lst, lifeRnd == 0, beg, "random", "" + lifeRnd);
    }

    /**
     * get help text
     *
     * @param l list to append
     */
    public void getHelp(userHelp l) {
        l.add(null, false, 1, new int[]{2}, "group", "select diffie-hellman group");
        l.add(null, false, 2, new int[]{-1}, "01", "768 bit modp");
        l.add(null, false, 2, new int[]{-1}, "02", "1024 bit modp");
        l.add(null, false, 2, new int[]{-1}, "05", "1536 bit modp");
        l.add(null, false, 2, new int[]{-1}, "14", "2048 bit modp");
        l.add(null, false, 2, new int[]{-1}, "15", "3072 bit modp");
        l.add(null, false, 2, new int[]{-1}, "16", "4096 bit modp");
        l.add(null, false, 2, new int[]{-1}, "17", "6144 bit modp");
        l.add(null, false, 2, new int[]{-1}, "18", "8192 bit modp");
        l.add(null, false, 2, new int[]{-1}, "19", "256 bit ecp");
        l.add(null, false, 2, new int[]{-1}, "20", "384 bit ecp");
        l.add(null, false, 2, new int[]{-1}, "21", "521 bit ecp");
        l.add(null, false, 2, new int[]{-1}, "22", "1024 bit modp");
        l.add(null, false, 2, new int[]{-1}, "23", "2048 bit modp");
        l.add(null, false, 2, new int[]{-1}, "24", "2048 bit modp");
        l.add(null, false, 2, new int[]{-1}, "25", "192 bit ecp");
        l.add(null, false, 2, new int[]{-1}, "26", "224 bit ecp");
        l.add(null, false, 2, new int[]{-1}, "28", "256 bit ecp");
        l.add(null, false, 2, new int[]{-1}, "29", "384 bit ecp");
        l.add(null, false, 2, new int[]{-1}, "30", "512 bit ecp");
        l.add(null, false, 2, new int[]{-1}, "31", "curve 25519");
        l.add(null, false, 2, new int[]{-1}, "35", "512 bit mlkem");
        l.add(null, false, 2, new int[]{-1}, "36", "768 bit mlkem");
        l.add(null, false, 2, new int[]{-1}, "37", "1024 bit mlkem");
        l.add(null, false, 2, new int[]{-1}, "99", "x25519mlkem738");
        l.add(null, false, 1, new int[]{2}, "seconds", "sa lifetime in time");
        l.add(null, false, 2, new int[]{-1}, "<num>", "number of seconds");
        l.add(null, false, 1, new int[]{2}, "random", "randomize time");
        l.add(null, false, 2, new int[]{-1}, "<num>", "number of milliseconds");
        l.add(null, false, 1, new int[]{2}, "bytes", "sa lifetime in traffic amount");
        l.add(null, false, 2, new int[]{-1}, "<num>", "number of bytes");
        l.add(null, false, 1, new int[]{2}, "hash", "select hash algorithm");
        l.add(null, false, 2, new int[]{-1}, "md5", "message digest 5 algorithm");
        l.add(null, false, 2, new int[]{-1}, "sha1", "secure hash algorithm 1");
        l.add(null, false, 2, new int[]{-1}, "sha224", "secure hash algorithm 2 224");
        l.add(null, false, 2, new int[]{-1}, "sha256", "secure hash algorithm 2 256");
        l.add(null, false, 2, new int[]{-1}, "sha384", "secure hash algorithm 2 384");
        l.add(null, false, 2, new int[]{-1}, "sha512", "secure hash algorithm 2 512");
        l.add(null, false, 2, new int[]{-1}, "sha3224", "secure hash algorithm 3 224");
        l.add(null, false, 2, new int[]{-1}, "sha3256", "secure hash algorithm 3 256");
        l.add(null, false, 2, new int[]{-1}, "sha3384", "secure hash algorithm 3 384");
        l.add(null, false, 2, new int[]{-1}, "sha3512", "secure hash algorithm 3 512");
        l.add(null, false, 2, new int[]{-1}, "none", "null hash");
        l.add(null, false, 1, new int[]{2}, "prf", "select prf algorithm");
        l.add(null, false, 2, new int[]{-1}, "md5", "message digest 5 algorithm");
        l.add(null, false, 2, new int[]{-1}, "sha1", "secure hash algorithm 1");
        l.add(null, false, 2, new int[]{-1}, "sha224", "secure hash algorithm 2 224");
        l.add(null, false, 2, new int[]{-1}, "sha256", "secure hash algorithm 2 256");
        l.add(null, false, 2, new int[]{-1}, "sha384", "secure hash algorithm 2 384");
        l.add(null, false, 2, new int[]{-1}, "sha512", "secure hash algorithm 2 512");
        l.add(null, false, 2, new int[]{-1}, "sha3224", "secure hash algorithm 3 224");
        l.add(null, false, 2, new int[]{-1}, "sha3256", "secure hash algorithm 3 256");
        l.add(null, false, 2, new int[]{-1}, "sha3384", "secure hash algorithm 3 384");
        l.add(null, false, 2, new int[]{-1}, "sha3512", "secure hash algorithm 3 512");
        l.add(null, false, 2, new int[]{-1}, "none", "null hash");
        l.add(null, false, 1, new int[]{2}, "cipher", "select cipher algorithm");
        l.add(null, false, 2, new int[]{-1}, "des", "des algorithm");
        l.add(null, false, 2, new int[]{-1}, "blowfish", "blowfish algorithm");
        l.add(null, false, 2, new int[]{-1}, "rc2", "rc2 algorithm");
        l.add(null, false, 2, new int[]{-1}, "3des", "triple des algorithm");
        l.add(null, false, 2, new int[]{-1}, "aes128cbc", "128bit aes cbc algorithm");
        l.add(null, false, 2, new int[]{-1}, "aes192cbc", "192bit aes cbc algorithm");
        l.add(null, false, 2, new int[]{-1}, "aes256cbc", "256bit aes cbc algorithm");
        l.add(null, false, 2, new int[]{-1}, "aes128cfb", "128bit aes cfb algorithm");
        l.add(null, false, 2, new int[]{-1}, "aes192cfb", "192bit aes cfb algorithm");
        l.add(null, false, 2, new int[]{-1}, "aes256cfb", "256bit aes cfb algorithm");
        l.add(null, false, 2, new int[]{-1}, "aes128ecb", "128bit aes ecb algorithm");
        l.add(null, false, 2, new int[]{-1}, "aes192ecb", "192bit aes ecb algorithm");
        l.add(null, false, 2, new int[]{-1}, "aes256ecb", "256bit aes ecb algorithm");
        l.add(null, false, 2, new int[]{-1}, "none", "null encryption");
        l.add(null, false, 2, new int[]{-1}, "aes128gcm", "128bit aes gcm algorithm");
        l.add(null, false, 2, new int[]{-1}, "aes192gcm", "192bit aes gcm algorithm");
        l.add(null, false, 2, new int[]{-1}, "aes256gcm", "256bit aes gcm algorithm");
        l.add(null, false, 2, new int[]{-1}, "aes128ctr", "128bit aes ctr algorithm");
        l.add(null, false, 2, new int[]{-1}, "aes192ctr", "192bit aes ctr algorithm");
        l.add(null, false, 2, new int[]{-1}, "aes256ctr", "256bit aes ctr algorithm");
        l.add(null, false, 2, new int[]{-1}, "aes128ofb", "128bit aes ofb algorithm");
        l.add(null, false, 2, new int[]{-1}, "aes192ofb", "192bit aes ofb algorithm");
        l.add(null, false, 2, new int[]{-1}, "aes256ofb", "256bit aes ofb algorithm");
        l.add(null, false, 2, new int[]{-1}, "aes128pcbc", "128bit aes pcbc algorithm");
        l.add(null, false, 2, new int[]{-1}, "aes192pcbc", "192bit aes pcbc algorithm");
        l.add(null, false, 2, new int[]{-1}, "aes256pcbc", "256bit aes pcbc algorithm");
    }

    /**
     * process command string
     *
     * @param cmd command to read
     * @return false on success, true on error
     */
    public boolean doCfgStr(cmds cmd) {
        String s = cmd.word();
        if (s.equals("group")) {
            groupNum = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("cipher")) {
            s = cmd.word();
            str2encr(s);
            return false;
        }
        if (s.equals("hash")) {
            hashAlg = str2hash(cmd.word());
            return false;
        }
        if (s.equals("prf")) {
            prfAlg = str2hash(cmd.word());
            return false;
        }
        if (s.equals("seconds")) {
            lifeSec = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("random")) {
            lifeRnd = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("bytes")) {
            lifeByt = bits.str2long(cmd.word());
            return false;
        }
        if (!s.equals(cmds.negated)) {
            return true;
        }
        s = cmd.word();
        if (s.equals("group")) {
            groupNum = 0;
            return false;
        }
        if (s.equals("cipher")) {
            encrAlg = 0;
            encrKey = 0;
            return false;
        }
        if (s.equals("hash")) {
            hashAlg = 0;
            return false;
        }
        if (s.equals("prf")) {
            prfAlg = 0;
            return false;
        }
        if (s.equals("seconds")) {
            lifeSec = 0;
            return false;
        }
        if (s.equals("random")) {
            lifeRnd = 0;
            return false;
        }
        if (s.equals("bytes")) {
            lifeByt = 0;
            return false;
        }
        return true;
    }

}
