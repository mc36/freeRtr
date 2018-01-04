package cry;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import pack.packHolder;
import util.bits;

/**
 * certificate (rfc5280) storage
 *
 * @author matecsaba
 */
public class cryCertificate {

    /**
     * name of this certificate
     */
    public String crtName;

    /**
     * decoded algorithm
     */
    public int decAlgo;

    /**
     * issuer of certificate
     */
    public cryCertEntity issuer;

    /**
     * subject of certificate
     */
    public cryCertEntity subject;

    /**
     * serial number
     */
    public BigInteger serNum;

    /**
     * not valid before
     */
    public long validBeg;

    /**
     * not valid after
     */
    public long validEnd;

    /**
     * key algorithm
     */
    public cryKeyGeneric key;

    /**
     * binary content
     */
    public packHolder binCont;

    /**
     * binary algorithm
     */
    public packHolder binAlgo;

    /**
     * binary signature
     */
    public packHolder binSign;

    /**
     * rsa encryption
     */
    public final static int[] oidRsaEncrypt = {0x2A, 0x86, 0x48, 0x86, 0xF7, 0x0D, 0x01, 0x01, 0x01};

    /**
     * rsa encryption with md2
     */
    public final static int[] oidRsaMd2 = {0x2A, 0x86, 0x48, 0x86, 0xF7, 0x0D, 0x01, 0x01, 0x02};

    /**
     * rsa encryption with md4
     */
    public final static int[] oidRsaMd4 = {0x2A, 0x86, 0x48, 0x86, 0xF7, 0x0D, 0x01, 0x01, 0x03};

    /**
     * rsa encryption with md5
     */
    public final static int[] oidRsaMd5 = {0x2A, 0x86, 0x48, 0x86, 0xF7, 0x0D, 0x01, 0x01, 0x04};

    /**
     * rsa encryption with sha1
     */
    public final static int[] oidRsaSha1 = {0x2A, 0x86, 0x48, 0x86, 0xF7, 0x0D, 0x01, 0x01, 0x05};

    /**
     * rsa encryption with sha224
     */
    public final static int[] oidRsaSha224 = {0x2A, 0x86, 0x48, 0x86, 0xF7, 0x0D, 0x01, 0x01, 0x0E};

    /**
     * rsa encryption with sha256
     */
    public final static int[] oidRsaSha256 = {0x2A, 0x86, 0x48, 0x86, 0xF7, 0x0D, 0x01, 0x01, 0x0B};

    /**
     * rsa encryption with sha384
     */
    public final static int[] oidRsaSha384 = {0x2A, 0x86, 0x48, 0x86, 0xF7, 0x0D, 0x01, 0x01, 0x0C};

    /**
     * rsa encryption with sha512
     */
    public final static int[] oidRsaSha512 = {0x2A, 0x86, 0x48, 0x86, 0xF7, 0x0D, 0x01, 0x01, 0x0D};

    /**
     * dss encryption
     */
    public final static int[] oidDssEncrypt = {0x2A, 0x86, 0x48, 0xCE, 0x38, 0x04, 0x01};

    /**
     * dss encryption with sha1
     */
    public final static int[] oidDssSha1 = {0x2A, 0x86, 0x48, 0xCE, 0x38, 0x04, 0x03};

    /**
     * dss encryption with sha224
     */
    public final static int[] oidDssSha224 = {0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x03, 0x01};

    /**
     * dss encryption with sha256
     */
    public final static int[] oidDssSha256 = {0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x03, 0x02};

    /**
     * ecdss encryption
     */
    public final static int[] oidEcDssEncrypt = {0x2a, 0x86, 0x48, 0xce, 0x3d, 0x02, 0x01};

    /**
     * ecdss encryption with sha1
     */
    public final static int[] oidEcDssSha1 = {0x2a, 0x86, 0x48, 0xce, 0x3d, 0x04, 0x01};

    /**
     * ecdss encryption with sha224
     */
    public final static int[] oidEcDssSha224 = {0x2A, 0x86, 0x48, 0xCE, 0x3D, 0x04, 0x03, 0x01};

    /**
     * ecdss encryption with sha256
     */
    public final static int[] oidEcDssSha256 = {0x2A, 0x86, 0x48, 0xCE, 0x3D, 0x04, 0x03, 0x02};

    /**
     * ecdss encryption with sha384
     */
    public final static int[] oidEcDssSha384 = {0x2A, 0x86, 0x48, 0xCE, 0x3D, 0x04, 0x03, 0x03};

    /**
     * ecdss encryption with sha512
     */
    public final static int[] oidEcDssSha512 = {0x2A, 0x86, 0x48, 0xCE, 0x3D, 0x04, 0x03, 0x04};

    /**
     * rsa encryption
     */
    public final static int typRsaEncrypt = 0x1001;

    /**
     * rsa encryption with md2
     */
    public final static int typRsaMd2 = 0x1002;

    /**
     * rsa encryption with md4
     */
    public final static int typRsaMd4 = 0x1003;

    /**
     * rsa encryption with md5
     */
    public final static int typRsaMd5 = 0x1004;

    /**
     * rsa encryption with sha1
     */
    public final static int typRsaSha1 = 0x1005;

    /**
     * rsa encryption with sha224
     */
    public final static int typRsaSha224 = 0x1006;

    /**
     * rsa encryption with sha256
     */
    public final static int typRsaSha256 = 0x1007;

    /**
     * rsa encryption with sha384
     */
    public final static int typRsaSha384 = 0x1008;

    /**
     * rsa encryption with sha512
     */
    public final static int typRsaSha512 = 0x1009;

    /**
     * dss encryption
     */
    public final static int typDssEncrypt = 0x2001;

    /**
     * dss encryption with sha1
     */
    public final static int typDssSha1 = 0x2002;

    /**
     * dss encryption with sha224
     */
    public final static int typDssSha224 = 0x2003;

    /**
     * dss encryption with sha256
     */
    public final static int typDssSha256 = 0x2004;

    /**
     * ecdss encryption
     */
    public final static int typEcDssEncrypt = 0x3001;

    /**
     * ecdss encryption with sha1
     */
    public final static int typEcDssSha1 = 0x3002;

    /**
     * ecdss encryption with sha224
     */
    public final static int typEcDssSha224 = 0x3003;

    /**
     * ecdss encryption with sha256
     */
    public final static int typEcDssSha256 = 0x3004;

    /**
     * ecdss encryption with sha384
     */
    public final static int typEcDssSha384 = 0x3005;

    /**
     * ecdss encryption with sha512
     */
    public final static int typEcDssSha512 = 0x3006;

    private static boolean compareOid(cryAsn1 a, final int[] oid) {
        if (a.buf.length != oid.length) {
            return false;
        }
        for (int i = 0; i < oid.length; i++) {
            if (oid[i] != (a.buf[i] & 0xff)) {
                return false;
            }
        }
        return true;
    }

    /**
     * convert oid to integer
     *
     * @param a oid to decode
     * @return type
     */
    public static int objid2int(cryAsn1 a) {
        if ((a.cnst) || (a.tag != cryAsn1.tagObjectID)) {
            return 0;
        }
        if (compareOid(a, oidRsaEncrypt)) {
            return typRsaEncrypt;
        }
        if (compareOid(a, oidRsaMd2)) {
            return typRsaMd2;
        }
        if (compareOid(a, oidRsaMd4)) {
            return typRsaMd4;
        }
        if (compareOid(a, oidRsaMd5)) {
            return typRsaMd5;
        }
        if (compareOid(a, oidRsaSha1)) {
            return typRsaSha1;
        }
        if (compareOid(a, oidRsaSha224)) {
            return typRsaSha224;
        }
        if (compareOid(a, oidRsaSha256)) {
            return typRsaSha256;
        }
        if (compareOid(a, oidRsaSha384)) {
            return typRsaSha384;
        }
        if (compareOid(a, oidRsaSha512)) {
            return typRsaSha512;
        }
        if (compareOid(a, oidDssEncrypt)) {
            return typDssEncrypt;
        }
        if (compareOid(a, oidDssSha1)) {
            return typDssSha1;
        }
        if (compareOid(a, oidDssSha224)) {
            return typDssSha224;
        }
        if (compareOid(a, oidDssSha256)) {
            return typDssSha256;
        }
        if (compareOid(a, oidEcDssEncrypt)) {
            return typEcDssEncrypt;
        }
        if (compareOid(a, oidEcDssSha1)) {
            return typEcDssSha1;
        }
        if (compareOid(a, oidEcDssSha224)) {
            return typEcDssSha224;
        }
        if (compareOid(a, oidEcDssSha256)) {
            return typEcDssSha256;
        }
        if (compareOid(a, oidEcDssSha384)) {
            return typEcDssSha384;
        }
        if (compareOid(a, oidEcDssSha512)) {
            return typEcDssSha512;
        }
        return 0;
    }

    /**
     * convert type to object id
     *
     * @param i type of object
     * @return object id, null if unknown
     */
    public static int[] int2objId(int i) {
        switch (i) {
            case typRsaEncrypt:
                return oidRsaEncrypt;
            case typRsaMd2:
                return oidRsaMd2;
            case typRsaMd4:
                return oidRsaMd4;
            case typRsaMd5:
                return oidRsaMd5;
            case typRsaSha1:
                return oidRsaSha1;
            case typRsaSha224:
                return oidRsaSha224;
            case typRsaSha256:
                return oidRsaSha256;
            case typRsaSha384:
                return oidRsaSha384;
            case typRsaSha512:
                return oidRsaSha512;
            case typDssEncrypt:
                return oidDssEncrypt;
            case typDssSha1:
                return oidDssSha1;
            case typDssSha224:
                return oidDssSha224;
            case typDssSha256:
                return oidDssSha256;
            case typEcDssEncrypt:
                return oidEcDssEncrypt;
            case typEcDssSha1:
                return oidEcDssSha1;
            case typEcDssSha224:
                return oidEcDssSha224;
            case typEcDssSha256:
                return oidEcDssSha256;
            case typEcDssSha384:
                return oidEcDssSha384;
            case typEcDssSha512:
                return oidEcDssSha512;
            default:
                return null;
        }
    }

    /**
     * convert type to string
     *
     * @param i type to convert
     * @return string
     */
    public static String type2string(int i) {
        switch (i) {
            case typRsaEncrypt:
                return "rsaEncrypt";
            case typRsaMd2:
                return "rsaMd2";
            case typRsaMd4:
                return "rsaMd4";
            case typRsaMd5:
                return "rsaMd5";
            case typRsaSha1:
                return "rsaSha1";
            case typRsaSha224:
                return "rsaSha224";
            case typRsaSha256:
                return "rsaSha256";
            case typRsaSha384:
                return "rsaSha384";
            case typRsaSha512:
                return "rsaSha512";
            case typDssEncrypt:
                return "dssEncrypt";
            case typDssSha1:
                return "dssSha1";
            case typDssSha224:
                return "dssSha224";
            case typDssSha256:
                return "dssSha256";
            case typEcDssEncrypt:
                return "ecdssEncrypt";
            case typEcDssSha1:
                return "ecdssSha1";
            case typEcDssSha224:
                return "ecdssSha224";
            case typEcDssSha256:
                return "ecdssSha256";
            case typEcDssSha384:
                return "ecdssSha384";
            case typEcDssSha512:
                return "ecdssSha512";
            default:
                return "unknown=" + i;
        }
    }

    public String toString() {
        return "key=" + type2string(decAlgo) + " issuer=" + issuer + " subject=" + subject + " valid=" + bits.time2str(null, validBeg, 3) + "-" + bits.time2str(null, validEnd, 3);
    }

    /**
     * get hasher of certificate
     *
     * @return hasher algorithm, null=error
     */
    public cryHashGeneric getHasher() {
        switch (decAlgo) {
            case typRsaMd2:
                return new cryHashMd2();
            case typRsaMd5:
                return new cryHashMd5();
            case typRsaSha1:
                return new cryHashSha1();
            case typRsaSha224:
                return new cryHashSha2224();
            case typRsaSha256:
                return new cryHashSha2256();
            case typRsaSha384:
                return new cryHashSha2384();
            case typRsaSha512:
                return new cryHashSha2512();
            case typDssSha1:
                return new cryHashSha1();
            case typDssSha224:
                return new cryHashSha2224();
            case typDssSha256:
                return new cryHashSha2256();
            case typEcDssSha1:
                return new cryHashSha1();
            case typEcDssSha224:
                return new cryHashSha2224();
            case typEcDssSha256:
                return new cryHashSha2256();
            case typEcDssSha384:
                return new cryHashSha2384();
            case typEcDssSha512:
                return new cryHashSha2512();
            default:
                return null;
        }
    }

    /**
     * read asn1 formatted certificate
     *
     * @param buf buffer to read
     * @return false on success, true on error
     */
    public boolean asn1ReadBuf(byte[] buf) {
        packHolder pck = new packHolder(true, true);
        pck.putCopy(buf, 0, 0, buf.length);
        pck.putSkip(buf.length);
        pck.merge2beg();
        return asn1reader(pck);
    }

    /**
     * read pem formatted certificate
     *
     * @param s concatenated lines
     * @return false on success, true on error
     */
    public boolean pemReadStr(String s) {
        byte[] buf = cryBase64.decodeBytes(s);
        if (buf == null) {
            return true;
        }
        return asn1ReadBuf(buf);
    }

    /**
     * read pem formatted key
     *
     * @param sl list of lines
     * @return false on success, true on error
     */
    public boolean pemReadArr(String[] sl) {
        String s = "";
        for (int i = 0; i < sl.length; i++) {
            s = s + sl[i];
        }
        return pemReadStr(s);
    }

    /**
     * read pem formatted key
     *
     * @param sl list of lines
     * @return false on success, true on error
     */
    public boolean pemReadLst(List<String> sl) {
        String s = "";
        for (int i = 0; i < sl.size(); i++) {
            s = s + sl.get(i);
        }
        return pemReadStr(s);
    }

    /**
     * write to buffer
     *
     * @return asn1 buffer
     */
    public byte[] asn1WriteBuf() {
        packHolder p = new packHolder(true, true);
        asn1writer(p);
        return p.getCopy();
    }

    /**
     * write to pem format string
     *
     * @return pem string
     */
    public String pemWriteStr() {
        byte[] buf = asn1WriteBuf();
        return cryBase64.encodeBytes(buf, 0, buf.length);
    }

    /**
     * write to pem format strings
     *
     * @return list of string
     */
    public List<String> pemWriteLst() {
        final int max = 64;
        String s = pemWriteStr();
        List<String> l = new ArrayList<String>();
        for (;;) {
            int i = s.length();
            if (i > max) {
                i = max;
            }
            String a = s.substring(0, i);
            if (a.length() < 1) {
                break;
            }
            s = s.substring(i, s.length());
            l.add(a);
        }
        return l;
    }

    /**
     * write to pem format strings
     *
     * @return array of string
     */
    public String[] pemWriteArr() {
        List<String> l = pemWriteLst();
        String[] a = new String[l.size()];
        for (int i = 0; i < a.length; i++) {
            a[i] = l.get(i);
        }
        return a;
    }

    /**
     * write certificate to asn1 format
     *
     * @param pck packet to write to
     */
    public void asn1writer(packHolder pck) {
        packHolder p = new packHolder(true, true);
        cryAsn1.writeSequence(p, binCont);
        cryAsn1.writeSequence(p, binAlgo);
        cryAsn1.writeBitString(p, binSign);
        cryAsn1.writeSequence(pck, p);
    }

    /**
     * read certificate from asn1 format
     *
     * @param pck packet to read from
     * @return false on success, true on error
     */
    public boolean asn1reader(packHolder pck) {
        cryAsn1 a = new cryAsn1();
        if (a.tagRead(pck)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != cryAsn1.tagSequence)) {
            return true;
        }
        pck = a.getPack();
        if (a.tagRead(pck)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != cryAsn1.tagSequence)) {
            return true;
        }
        binCont = a.getPack();
        if (a.tagRead(pck)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != cryAsn1.tagSequence)) {
            return true;
        }
        binAlgo = a.getPack();
        if (a.tagRead(pck)) {
            return true;
        }
        if ((a.cnst) || (a.tag != cryAsn1.tagBitString)) {
            return true;
        }
        binSign = a.getPack();
        if (pck.dataSize() != 0) {
            return true;
        }
        binCont.mergeHeader(0, 0);
        binAlgo.mergeHeader(0, 0);
        binSign.mergeHeader(0, 0);
        pck = binAlgo.copyBytes(true, true);
        if (a.tagRead(pck)) {
            return true;
        }
        decAlgo = objid2int(a);
        if (decAlgo < 1) {
            return true;
        }
        pck = binCont.copyBytes(true, true);
        if (a.tagRead(pck)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != cryAsn1.tagEoc)) {
            return true;
        }
        packHolder p = a.getPack();
        if (a.tagRead(p)) {
            return true;
        }
        if ((a.cnst) || (a.tag != cryAsn1.tagInteger)) {
            return true;
        }
        if (a.getBigInt().intValue() != 2) {
            return true;
        }
        if (a.tagRead(pck)) {
            return true;
        }
        if ((a.cnst) || (a.tag != cryAsn1.tagInteger)) {
            return true;
        }
        serNum = a.getBigInt();
        if (a.tagRead(pck)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != cryAsn1.tagSequence)) {
            return true;
        }
        p = a.getPack();
        if (a.tagRead(p)) {
            return true;
        }
        if (decAlgo != objid2int(a)) {
            return true;
        }
        if (a.tagRead(pck)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != cryAsn1.tagSequence)) {
            return true;
        }
        issuer = new cryCertEntity();
        if (issuer.asn1reader(a.getPack())) {
            return true;
        }
        if (a.tagRead(pck)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != cryAsn1.tagSequence)) {
            return true;
        }
        p = a.getPack();
        if (a.tagRead(p)) {
            return true;
        }
        validBeg = bits.str2time(null, "20" + new String(a.buf));
        if (a.tagRead(p)) {
            return true;
        }
        validEnd = bits.str2time(null, "20" + new String(a.buf));
        if (a.tagRead(pck)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != cryAsn1.tagSequence)) {
            return true;
        }
        subject = new cryCertEntity();
        if (subject.asn1reader(a.getPack())) {
            return true;
        }
        switch (decAlgo & 0xf000) {
            case 0x1000:
                key = new cryKeyRSA();
                break;
            case 0x2000:
                key = new cryKeyDSA();
                break;
            case 0x3000:
                key = new cryKeyECDSA();
                break;
            default:
                return true;
        }
        if (key.certReader(pck)) {
            return true;
        }
        return false;
    }

    /**
     * sign data with this certificate
     *
     * @param data data to sign
     * @return signature generated
     */
    public byte[] signData(byte[] data) {
        cryHashGeneric h = getHasher();
        if (h == null) {
            return null;
        }
        data = cryHashGeneric.compute(h, data);
        return key.certSigning(data);
    }

    /**
     * verify signature on data
     *
     * @param data data to verify
     * @param sign signature to check
     * @return false on success, true on error
     */
    public boolean verifyData(byte[] data, byte[] sign) {
        cryHashGeneric h = getHasher();
        if (h == null) {
            return true;
        }
        data = cryHashGeneric.compute(h, data);
        return key.certVerify(data, sign);
    }

    private void addDate(packHolder pck, long tim) {
        cryAsn1 a = new cryAsn1();
        a.putUTCtime(tim);
        a.tagWrite(pck);
        pck.merge2end();
    }

    /**
     * create content
     */
    public void createCont() {
        binCont = new packHolder(true, true);
        packHolder p1 = new packHolder(true, true);
        cryAsn1.writeBigInt(p1, new BigInteger("2"));
        cryAsn1.writeEoc(binCont, p1);
        cryAsn1.writeBigInt(binCont, serNum);
        p1.clear();
        cryAsn1.writeObjectId(p1, int2objId(decAlgo));
        cryAsn1.writeSequence(binCont, p1);
        p1.clear();
        issuer.asn1writer(p1);
        cryAsn1.writeSequence(binCont, p1);
        p1.clear();
        addDate(p1, validBeg);
        addDate(p1, validEnd);
        cryAsn1.writeSequence(binCont, p1);
        p1.clear();
        subject.asn1writer(p1);
        cryAsn1.writeSequence(binCont, p1);
        key.certWriter(binCont);
    }

    /**
     * create signature algorithm
     */
    public void setSignAlgo() {
        String s = key.algName();
        decAlgo = 0;
        if (s.equals(new cryKeyRSA().algName())) {
            decAlgo = typRsaSha1;
        }
        if (s.equals(new cryKeyDSA().algName())) {
            decAlgo = typDssSha1;
        }
        if (s.equals(new cryKeyECDSA().algName())) {
            decAlgo = typEcDssSha1;
        }
        binAlgo = new packHolder(true, true);
        cryAsn1.writeObjectId(binAlgo, int2objId(decAlgo));
    }

    /**
     * self sign this certificate
     */
    public void selfSign() {
        binSign = new packHolder(true, true);
        cryAsn1.writeSequence(binSign, binCont);
        byte[] buf = signData(binSign.getCopy());
        binSign.clear();
        binSign.putCopy(buf, 0, 0, buf.length);
        binSign.putSkip(buf.length);
        binSign.merge2beg();
    }

    /**
     * test self signed certificate
     *
     * @return false on success, true on error
     */
    public boolean testSelf() {
        return testClientCert(this, this);
    }

    /**
     * test if client certificate is signed by ca certificate
     *
     * @param clnt client certificate
     * @param ca ca certificate
     * @return false on success, true on error
     */
    public static boolean testClientCert(cryCertificate clnt, cryCertificate ca) {
        packHolder p = new packHolder(true, true);
        cryAsn1.writeSequence(p, clnt.binCont);
        return ca.verifyData(p.getCopy(), clnt.binSign.getCopy());
    }

    /**
     * create self signed certificate
     *
     * @param key key to use
     * @param name name of owner
     * @param days days to certify
     * @return created certificate
     */
    public static cryCertificate createSelfSigned(cryKeyGeneric key, String name, int days) {
        cryCertificate crt = new cryCertificate();
        cryCertEntity ent = new cryCertEntity();
        ent.commonName = name;
        crt.serNum = new BigInteger("" + bits.randomD());
        crt.issuer = ent;
        crt.subject = ent;
        crt.key = key;
        crt.validBeg = bits.getTime();
        crt.validEnd = crt.validBeg + (((long) days) * 1000 * 60 * 60 * 24);
        crt.setSignAlgo();
        crt.createCont();
        crt.selfSign();
        return crt;
    }

}
