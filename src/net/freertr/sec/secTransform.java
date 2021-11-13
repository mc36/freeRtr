package net.freertr.sec;

import java.util.List;
import net.freertr.cry.cryEncrCBCaes;
import net.freertr.cry.cryEncrCBCblowfish;
import net.freertr.cry.cryEncrCBCdes;
import net.freertr.cry.cryEncrCBCdes3;
import net.freertr.cry.cryEncrGeneric;
import net.freertr.cry.cryHashGeneric;
import net.freertr.cry.cryHashHmac;
import net.freertr.cry.cryHashMd5;
import net.freertr.cry.cryHashSha1;
import net.freertr.cry.cryHashSha2256;
import net.freertr.cry.cryHashSha2512;
import net.freertr.cry.cryKeyDH;
import net.freertr.pack.packHolder;
import net.freertr.user.userHelping;
import net.freertr.util.bits;
import net.freertr.util.cmds;

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
     * encryption algorithm 1=des, 2=blowfish, 3=3des, 4=aes
     */
    public int encrAlg;

    /**
     * encryption key length
     */
    public int encrKey;

    /**
     * hash algorithm 1=md5, 2=sha1, 3=sha256, 4=sha512
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
                return "aes" + encrKey;
            default:
                return "none";
        }
    }

    /**
     * decode hash algorithm
     *
     * @return string
     */
    public String hash2str() {
        switch (hashAlg) {
            case 1:
                return "md5";
            case 2:
                return "sha1";
            case 3:
                return "sha256";
            case 4:
                return "sha512";
            default:
                return "none";
        }
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
            case 5:
                return 3;
            case 7:
                return 4;
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
            case 12:
                return 4;
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
            default:
                return 0;
        }
    }

    /**
     * decode ike2 prf algorithm
     *
     * @param i encoded value
     * @return decoded value
     */
    public static int decode2prf(int i) {
        return decodeQMhash(i);
    }

    /**
     * encode ike2 prf algorithm
     *
     * @param i encoded value
     * @return decoded value
     */
    public static int encode2prf(int i) {
        return encodeQMhash(i);
    }

    /**
     * encode ike2 encryption algorithm
     *
     * @param i encoded value
     * @return decoded value
     */
    public static int encode2encr(int i) {
        return encodeQMencr(i);
    }

    /**
     * decode ike2 encryption algorithm
     *
     * @param i encoded value
     * @return decoded value
     */
    public static int decode2encr(int i) {
        return decodeQMencr(i);
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
            add1property(pck, 12, (int)lifeByt);
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
            add1property(pck, 2, (int)lifeByt);
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
                encrAlg = decode2encr(propVal);
                break;
            case 2:
                prfAlg = decode2prf(propVal);
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
            bits.msbPutW(buf, 2, encode2encr(encrAlg));
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
            bits.msbPutW(buf, 2, encode2prf(prfAlg));
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
            default:
                return null;
        }
    }

    /**
     * get hash algorithm
     *
     * @return hash, null on error
     */
    public cryHashGeneric getHash() {
        cryHashGeneric h = null;
        switch (hashAlg) {
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
            default:
                return null;
        }
        h.init();
        return h;
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
        h = new cryHashHmac(h, key);
        h.init();
        return h;
    }

    /**
     * get current group
     *
     * @return group, null if nothing
     */
    public cryKeyDH getGroup() {
        return cryKeyDH.getGroup(groupNum);
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
        cmds.cfgLine(lst, lifeSec == 0, beg, "seconds", "" + lifeSec);
        cmds.cfgLine(lst, lifeByt == 0, beg, "bytes", "" + lifeByt);
    }

    /**
     * get help text
     *
     * @param l list to append
     */
    public void getHelp(userHelping l) {
        l.add(null, "1 2  group               select diffie-hellman group");
        l.add(null, "2 .    01                768 bit group");
        l.add(null, "2 .    02                1024 bit group");
        l.add(null, "2 .    05                1536 bit group");
        l.add(null, "2 .    14                2048 bit group");
        l.add(null, "2 .    15                3072 bit group");
        l.add(null, "2 .    16                4096 bit group");
        l.add(null, "2 .    17                6144 bit group");
        l.add(null, "2 .    18                8192 bit group");
        l.add(null, "2 .    22                1024 bit group");
        l.add(null, "2 .    23                2048 bit group");
        l.add(null, "2 .    24                2048 bit group");
        l.add(null, "1 2  seconds             sa lifetime in time");
        l.add(null, "2 .    <num>             number of seconds");
        l.add(null, "1 2  bytes               sa lifetime in traffic amount");
        l.add(null, "2 .    <num>             number of bytes");
        l.add(null, "1 2  hash                select hash algorithm");
        l.add(null, "2 .    md5               message digest 5 algorithm");
        l.add(null, "2 .    sha1              secure hash algorithm 1");
        l.add(null, "2 .    sha256            secure hash algorithm 256");
        l.add(null, "2 .    sha512            secure hash algorithm 512");
        l.add(null, "1 2  cipher              select cipher algorithm");
        l.add(null, "2 .    des               des algorithm");
        l.add(null, "2 .    blowfish          blowfish algorithm");
        l.add(null, "2 .    3des              triple des algorithm");
        l.add(null, "2 .    aes128            128bit aes algorithm");
        l.add(null, "2 .    aes192            192bit aes algorithm");
        l.add(null, "2 .    aes256            256bit aes algorithm");
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
            if (s.equals("aes128")) {
                encrAlg = 4;
                encrKey = 128;
            }
            if (s.equals("aes192")) {
                encrAlg = 4;
                encrKey = 192;
            }
            if (s.equals("aes256")) {
                encrAlg = 4;
                encrKey = 256;
            }
            return false;
        }
        if (s.equals("hash")) {
            s = cmd.word();
            hashAlg = 0;
            if (s.equals("md5")) {
                hashAlg = 1;
            }
            if (s.equals("sha1")) {
                hashAlg = 2;
            }
            if (s.equals("sha256")) {
                hashAlg = 3;
            }
            if (s.equals("sha512")) {
                hashAlg = 4;
            }
            return false;
        }
        if (s.equals("seconds")) {
            lifeSec = bits.str2num(cmd.word());
            return false;
        }
        if (s.equals("bytes")) {
            lifeByt = bits.str2long(cmd.word());
            return false;
        }
        if (!s.equals("no")) {
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
        if (s.equals("seconds")) {
            lifeSec = 0;
            return false;
        }
        if (s.equals("bytes")) {
            lifeByt = 0;
            return false;
        }
        return true;
    }

}
