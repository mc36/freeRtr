package org.freertr.cry;

import org.freertr.enc.encAsn1;
import java.math.BigInteger;
import org.freertr.pack.packHolder;
import org.freertr.pack.packSsh;
import org.freertr.util.bits;

/**
 * rivest shamir adleman
 *
 * @author matecsaba
 */
public class cryKeyRSA extends cryKeyGeneric {

    /**
     * create instance
     */
    public cryKeyRSA() {
    }

    /**
     * ssh name
     */
    public final static String sshName = "ssh-rsa";

    /**
     * n modulus
     */
    protected BigInteger modulus;

    /**
     * e public exponent
     */
    protected BigInteger pubExp;

    /**
     * d private exponent
     */
    protected BigInteger privExp;

    /**
     * p prime1 exponent
     */
    protected BigInteger prime1;

    /**
     * q prime2 exponent
     */
    protected BigInteger prime2;

    /**
     * dmp exponent1
     */
    protected BigInteger expon1;

    /**
     * dmq exponent2
     */
    protected BigInteger expon2;

    /**
     * iqp coefficient
     */
    protected BigInteger coeff;

    public String toString() {
        return "mod=" + modulus + " pubExp=" + pubExp + " privExp=" + privExp + " p1=" + prime1 + " p2=" + prime2 + " e1="
                + expon1 + " e2=" + expon2 + " coeff=" + coeff;
    }

    /**
     * max byte size
     *
     * @return size
     */
    public int byteSize() {
        return (modulus.bitLength() + 7) / 8;
    }

    /**
     * get ssh name
     *
     * @return name
     */
    public String algName() {
        return "rsa";
    }

    /**
     * get ssh hash
     *
     * @return hasher
     */
    public String sshName() {
        return sshName;
    }

    public cryHashGeneric sshHash() {
        return new cryHashSha2256();
    }

    public boolean certReader(packHolder pck) {
        encAsn1 a = new encAsn1();
        if (a.tagRead(pck)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != encAsn1.tagSequence)) {
            return true;
        }
        pck = a.getPack();
        if (a.tagRead(pck)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != encAsn1.tagSequence)) {
            return true;
        }
        packHolder p = a.getPack();
        if (a.tagRead(p)) {
            return true;
        }
        if (cryCertificate.objid2int(a) != cryCertificate.typRsaEncrypt) {
            return true;
        }
        if (a.tagRead(pck)) {
            return true;
        }
        if ((a.cnst) || (a.tag != encAsn1.tagBitString)) {
            return true;
        }
        pck = a.getPack();
        pck.getSkip(1);
        if (a.tagRead(pck)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != encAsn1.tagSequence)) {
            return true;
        }
        pck = a.getPack();
        modulus = encAsn1.readBigInt(pck);
        pubExp = encAsn1.readBigInt(pck);
        return false;
    }

    public void certWriter(packHolder pck) {
        packHolder p1 = new packHolder(true, true);
        packHolder p2 = new packHolder(true, true);
        packHolder p3 = new packHolder(true, true);
        encAsn1.writeObjectId(p1, cryCertificate.oidRsaEncrypt);
        encAsn1.writeNull(p1);
        encAsn1.writeSequence(p2, p1);
        encAsn1.writeBigInt(p3, modulus);
        encAsn1.writeBigInt(p3, pubExp);
        p1.clear();
        encAsn1.writeSequence(p1, p3);
        p1.putByte(0, 0);
        p1.putSkip(1);
        p1.merge2beg();
        encAsn1.writeBitString(p2, p1);
        encAsn1.writeSequence(pck, p2);
    }

    public boolean privReader(packHolder pck) {
        encAsn1 a = new encAsn1();
        if (a.tagRead(pck)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != encAsn1.tagSequence)) {
            return true;
        }
        pck = a.getPack();
        BigInteger ver = encAsn1.readBigInt(pck);
        if (ver == null) {
            return true;
        }
        modulus = encAsn1.readBigInt(pck);
        if (modulus == null) {
            return true;
        }
        pubExp = encAsn1.readBigInt(pck);
        if (pubExp == null) {
            return true;
        }
        privExp = encAsn1.readBigInt(pck);
        if (privExp == null) {
            return true;
        }
        prime1 = encAsn1.readBigInt(pck);
        if (prime1 == null) {
            return true;
        }
        prime2 = encAsn1.readBigInt(pck);
        if (prime2 == null) {
            return true;
        }
        expon1 = encAsn1.readBigInt(pck);
        if (expon1 == null) {
            return true;
        }
        expon2 = encAsn1.readBigInt(pck);
        if (expon2 == null) {
            return true;
        }
        coeff = encAsn1.readBigInt(pck);
        if (coeff == null) {
            return true;
        }
        if (pck.dataSize() != 0) {
            return true;
        }
        return false;
    }

    public void privWriter(packHolder pck) {
        packHolder p = new packHolder(true, true);
        encAsn1.writeBigInt(p, BigInteger.ZERO);
        encAsn1.writeBigInt(p, modulus);
        encAsn1.writeBigInt(p, pubExp);
        encAsn1.writeBigInt(p, privExp);
        encAsn1.writeBigInt(p, prime1);
        encAsn1.writeBigInt(p, prime2);
        encAsn1.writeBigInt(p, expon1);
        encAsn1.writeBigInt(p, expon2);
        encAsn1.writeBigInt(p, coeff);
        encAsn1.writeSequence(pck, p);
    }

    public boolean keyMakeName(String nam) {
        return false;
    }

    public boolean keyMakeTls(int id) {
        return false;
    }

    public boolean keyMakeIke(int id) {
        return false;
    }

    public int keyMakeVal() {
        return -1;
    }

    public boolean keyMakeSize(int len) {
        pubExp = new BigInteger("65537", 10);
        prime1 = cryUtils.randomPrime(len / 2);
        prime2 = cryUtils.randomPrime(len / 2);
        if (prime1.compareTo(prime2) < 0) {
            BigInteger i = prime1;
            prime1 = prime2;
            prime2 = i;
        }
        modulus = prime1.multiply(prime2);
        BigInteger i1 = prime1.subtract(BigInteger.ONE);
        BigInteger i2 = prime2.subtract(BigInteger.ONE);
        BigInteger i0 = i1.multiply(i2);
        try {
            privExp = pubExp.modInverse(i0);
        } catch (Exception e) {
            return true;
        }
        expon1 = privExp.mod(i1);
        expon2 = privExp.mod(i2);
        try {
            coeff = prime2.modInverse(prime1);
        } catch (Exception e) {
            return true;
        }
        return false;
    }

    public boolean keyVerify() {
        if (!cryUtils.testPrime(pubExp)) {
            return true;
        }
        if (!cryUtils.testPrime(prime1)) {
            return true;
        }
        if (!cryUtils.testPrime(prime2)) {
            return true;
        }
        if (prime1.compareTo(prime2) < 0) {
            return true;
        }
        if (modulus.compareTo(prime1.multiply(prime2)) != 0) {
            return true;
        }
        BigInteger i1 = prime1.subtract(BigInteger.ONE);
        BigInteger i2 = prime2.subtract(BigInteger.ONE);
        BigInteger i0 = i1.multiply(i2);
        if (privExp.compareTo(pubExp.modInverse(i0)) != 0) {
            return true;
        }
        if (expon1.compareTo(privExp.mod(i1)) != 0) {
            return true;
        }
        if (expon2.compareTo(privExp.mod(i2)) != 0) {
            return true;
        }
        if (coeff.compareTo(prime2.modInverse(prime1)) != 0) {
            return true;
        }
        return false;
    }

    public int keySize() {
        return modulus.bitLength();
    }

    public String keyDump() {
        return null;
    }

    /**
     * client exchange
     */
    public void keyClntInit() {
    }

    /**
     * server exchange
     */
    public void keyServInit() {
    }

    /**
     * client common secret computation
     */
    public void keyClntCalc() {
    }

    /**
     * server common secret computation
     */
    public void keyServCalc() {
    }

    public byte[] keyCommonTls() {
        return null;
    }

    public byte[] keyCommonSsh() {
        return null;
    }

    public byte[] keyCommonIke() {
        return null;
    }

    public byte[] keyClntTls() {
        return null;
    }

    public byte[] keyServTls() {
        return null;
    }

    public boolean keyClntTls(byte[] buf, int ofs) {
        return false;
    }

    public boolean keyServTls(byte[] buf, int ofs) {
        return false;
    }

    public byte[] keyClntSsh() {
        return null;
    }

    public byte[] keyServSsh() {
        return null;
    }

    public boolean keyClntSsh(byte[] buf, int ofs) {
        return false;
    }

    public boolean keyServSsh(byte[] buf, int ofs) {
        return false;
    }

    public byte[] keyClntIke() {
        return null;
    }

    public byte[] keyServIke() {
        return null;
    }

    public boolean keyClntIke(byte[] buf, int ofs) {
        return false;
    }

    public boolean keyServIke(byte[] buf, int ofs) {
        return false;
    }

    public byte[][] keyParamTls() {
        return null;
    }

    public byte[][] keyParamSsh() {
        return null;
    }

    public boolean keyParamTls(byte[][] buf) {
        return false;
    }

    public boolean keyParamSsh(byte[][] buf) {
        return false;
    }

    /**
     * do encryption
     *
     * @param src cleartext
     * @return ciphertext
     */
    public byte[] doEncrypt(byte[] src) {
        BigInteger i = cryUtils.buffer2bigInt(src, 0, src.length);
        i = i.modPow(pubExp, modulus);
        return i.toByteArray();
    }

    /**
     * do decryption
     *
     * @param src ciphertext
     * @return cleartext
     */
    public byte[] doDecrypt(byte[] src) {
        BigInteger i = cryUtils.buffer2bigInt(src, 0, src.length);
        i = i.modPow(privExp, modulus);
        return i.toByteArray();
    }

    public boolean sshReader(byte[] key) {
        packHolder p = new packHolder(true, true);
        p.putCopy(key, 0, 0, key.length);
        p.putSkip(key.length);
        p.merge2beg();
        if (!packSsh.stringRead(p).equals(sshName)) {
            return true;
        }
        pubExp = packSsh.bigIntRead(p);
        modulus = packSsh.bigIntRead(p);
        return false;
    }

    public byte[] sshWriter() {
        packHolder p = new packHolder(true, true);
        packSsh.stringWrite(p, sshName);
        packSsh.bigIntWrite(p, pubExp);
        packSsh.bigIntWrite(p, modulus);
        p.merge2beg();
        return p.getCopy();
    }

    /**
     * mask generator function 1
     *
     * @param hsh hash
     * @param src source
     * @param ofs offset
     * @param len length
     * @param need needed
     * @return mask
     */
    public byte[] pssMgf1(cryHashGeneric hsh, byte[] src, int ofs, int len, int need) {
        int hl = hsh.getHashSize();
        byte[] res = new byte[need];
        byte[] buf = new byte[4];
        int cnt = 0;
        for (; cnt < (need / hl); cnt++) {
            bits.msbPutD(buf, 0, cnt);
            hsh.init();
            hsh.update(src, ofs, len);
            hsh.update(buf, 0, buf.length);
            bits.byteCopy(hsh.finish(), 0, res, cnt * hl, hl);
        }
        if ((cnt * hl) >= need) {
            return res;
        }
        bits.msbPutD(buf, 0, cnt);
        hsh.init();
        hsh.update(src, ofs, len);
        hsh.update(buf, 0, buf.length);
        bits.byteCopy(hsh.finish(), 0, res, cnt * hl, res.length - (cnt * hl));
        return res;
    }

    /**
     * pad for tls12
     *
     * @param oid oid bytes
     * @param src buffer to pad
     * @return padded integer
     */
    public BigInteger PKCS1t2pad(byte[] oid, byte[] src) {
        byte[] buf = new byte[byteSize()];
        for (int i = 0; i < buf.length; i++) {
            buf[i] = (byte) 0xff;
        }
        buf[0] = 0;
        buf[1] = 1;
        bits.byteCopy(src, 0, buf, buf.length - src.length, src.length);
        bits.byteCopy(oid, 0, buf, buf.length - src.length - oid.length, oid.length);
        return new BigInteger(buf);
    }

    /**
     * pad for tls11
     *
     * @param src buffer to pad
     * @return padded integer
     */
    public BigInteger PKCS1t1pad(byte[] src) {
        byte[] buf = new byte[byteSize()];
        for (int i = 0; i < buf.length; i++) {
            buf[i] = (byte) 0xff;
        }
        buf[0] = 0;
        buf[1] = 1;
        bits.byteCopy(src, 0, buf, buf.length - src.length, src.length);
        buf[buf.length - src.length - 1] = 0;
        return new BigInteger(buf);
    }

    /**
     * pad for pkcs15
     *
     * @param src buffer to pad
     * @return padded buffer
     */
    public byte[] PKCS1t15pad(byte[] src) {
        byte[] buf = new byte[byteSize()];
        buf[1] = 2;
        for (int i = 2; i < buf.length - src.length; i++) {
            int o;
            for (;;) {
                o = bits.randomB();
                if (o != 0) {
                    break;
                }
            }
            buf[i] = (byte) o;
        }
        buf[buf.length - src.length - 1] = 0;
        bits.byteCopy(src, 0, buf, buf.length - src.length, src.length);
        return buf;
    }

    /**
     * unpad for pkcs15
     *
     * @param src buffer to unpad
     * @return unpadded buffer
     */
    public byte[] PKCS1t15unpad(byte[] src) {
        int o = -1;
        for (int i = src.length - 1; i >= 0; i--) {
            if (src[i] == 0) {
                o = i;
            }
        }
        if (o < 0) {
            return src;
        }
        byte[] buf = new byte[src.length - o - 1];
        for (int i = 0; i < buf.length; i++) {
            buf[i] = src[src.length - buf.length + i];
        }
        return buf;
    }

    public boolean sshVerify(cryHashGeneric algo, String algn, byte[] hash, byte[] sign) {
        hash = cryHashGeneric.compute(algo, hash);
        packHolder p = new packHolder(true, true);
        p.putCopy(sign, 0, 0, sign.length);
        p.putSkip(sign.length);
        p.merge2beg();
        if (!packSsh.stringRead(p).equals(algn)) {
            return true;
        }
        BigInteger s = packSsh.bigUIntRead(p);
        s = s.modPow(pubExp, modulus);
        return PKCS1t2pad(algo.getPkcs(), hash).compareTo(s) != 0;
    }

    public byte[] sshSigning(cryHashGeneric algo, String algn, byte[] hash) {
        hash = cryHashGeneric.compute(algo, hash);
        packHolder p = new packHolder(true, true);
        BigInteger s = PKCS1t2pad(algo.getPkcs(), hash);
        s = s.modPow(privExp, modulus);
        packSsh.stringWrite(p, algn);
        packSsh.bigUIntWrite(p, s);
        p.merge2beg();
        return p.getCopy();
    }

    public boolean certVerify(cryHashGeneric pkcs, byte[] hash, byte[] sign) {
        hash = cryHashGeneric.compute(pkcs, hash);
        BigInteger s = cryUtils.buffer2bigInt(sign, 0, sign.length);
        s = s.modPow(pubExp, modulus);
        return PKCS1t2pad(pkcs.getPkcs(), hash).compareTo(s) != 0;
    }

    public byte[] certSigning(cryHashGeneric pkcs, byte[] hash) {
        hash = cryHashGeneric.compute(pkcs, hash);
        BigInteger s = PKCS1t2pad(pkcs.getPkcs(), hash);
        s = s.modPow(privExp, modulus);
        return s.toByteArray();
    }

    /**
     * do padding
     *
     * @param ver version
     * @param pkcs hash algorithm
     * @param hash hash hash to pad
     * @return padded, null if pss
     */
    public BigInteger doPadding(int ver, cryHashGeneric pkcs, byte[] hash) {
        if (ver < 0) {
            return PKCS1t1pad(hash);
        }
        if (ver < 0x100) {
            hash = cryHashGeneric.compute(pkcs, hash);
            return PKCS1t1pad(hash);
        }
        if (ver < 0x800) {
            hash = cryHashGeneric.compute(pkcs, hash);
            return PKCS1t2pad(pkcs.getPkcs(), hash);
        }
        return null;
    }

    public boolean tlsVerify(int ver, cryHashGeneric pkcs, byte[] hash, byte[] sign) {
        BigInteger s = cryUtils.buffer2bigInt(sign, 0, sign.length);
        s = s.modPow(pubExp, modulus);
        BigInteger h = doPadding(ver, pkcs, hash);
        if (h != null) {
            return h.compareTo(s) != 0;
        }
        hash = cryHashGeneric.compute(pkcs, hash);
        byte[] sgn = s.toByteArray();
        if (sgn.length < (hash.length + hash.length + 2)) {
            return true;
        }
        byte[] buf = new byte[hash.length + hash.length + 8];
        bits.byteCopy(hash, 0, buf, 8, hash.length);
        int embt = modulus.bitLength() - 1;
        byte[] blk = new byte[(embt + 7) / 8];
        bits.byteCopy(sgn, 0, blk, blk.length - sgn.length, sgn.length);
        int fbm = 0xff >>> ((blk.length * 8) - embt);
        if ((blk[0] & 0xff) != (blk[0] & fbm)) {
            return true;
        }
        if ((blk[blk.length - 1] & 0xff) != 0xbc) {
            return true;
        }
        byte[] dbm = pssMgf1(pkcs, blk, blk.length - hash.length - 1, hash.length, blk.length - hash.length - 1);
        for (int i = 0; i != dbm.length; i++) {
            blk[i] ^= dbm[i];
        }
        blk[0] &= (byte) fbm;
        for (int i = 0; i != blk.length - hash.length - hash.length - 2; i++) {
            if (blk[i] != 0) {
                return true;
            }
        }
        if (blk[blk.length - hash.length - hash.length - 2] != 0x01) {
            return true;
        }
        bits.byteCopy(blk, blk.length - hash.length - hash.length - 1, buf, buf.length - hash.length, hash.length);
        pkcs.init();
        pkcs.update(buf);
        bits.byteCopy(pkcs.finish(), 0, buf, buf.length - hash.length, hash.length);
        return bits.byteComp(blk, blk.length - hash.length - 1, buf, buf.length - hash.length, hash.length) != 0;
    }

    public byte[] tlsSigning(int ver, cryHashGeneric pkcs, byte[] hash) {
        BigInteger s = doPadding(ver, pkcs, hash);
        if (s != null) {
            s = s.modPow(privExp, modulus);
            return cryUtils.bigUint2buf(s);
        }
        hash = cryHashGeneric.compute(pkcs, hash);
        byte[] salt = new byte[hash.length];
        for (int i = 0; i < salt.length; i++) {
            salt[i] = (byte) bits.randomB();
        }
        byte[] buf = new byte[hash.length + hash.length + 8];
        bits.byteCopy(hash, 0, buf, 8, hash.length);
        bits.byteCopy(salt, 0, buf, buf.length - hash.length, hash.length);
        pkcs.init();
        pkcs.update(buf);
        byte[] h = pkcs.finish();
        int embt = modulus.bitLength() - 1;
        byte[] blk = new byte[(embt + 7) / 8];
        blk[blk.length - hash.length - hash.length - 2] = 0x01;
        bits.byteCopy(salt, 0, blk, blk.length - hash.length - hash.length - 1, hash.length);
        byte[] dbm = pssMgf1(pkcs, h, 0, h.length, blk.length - hash.length - 1);
        for (int i = 0; i != dbm.length; i++) {
            blk[i] ^= dbm[i];
        }
        bits.byteCopy(h, 0, blk, blk.length - hash.length - 1, hash.length);
        blk[0] &= (byte) (0xff >>> ((blk.length * 8) - embt));
        blk[blk.length - 1] = (byte) 0xbc;
        s = new BigInteger(blk);
        s = s.modPow(privExp, modulus);
        return cryUtils.bigUint2buf(s);
    }

}
