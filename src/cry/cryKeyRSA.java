package cry;

import java.math.BigInteger;

import pack.packHolder;
import pack.packSsh;
import util.bits;

/**
 * rivest shamir adleman
 *
 * @author matecsaba
 */
public class cryKeyRSA extends cryKeyGeneric {

    /**
     * ssh name
     */
    public final static String sshName = "ssh-rsa";

    /**
     * n modulus
     */
    public BigInteger modulus;

    /**
     * e public exponent
     */
    public BigInteger pubExp;

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

    public String algName() {
        return "rsa";
    }

    public boolean certReader(packHolder pck) {
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
        if ((a.cnst) || (a.tag != cryAsn1.tagBitString)) {
            return true;
        }
        pck = a.getPack();
        pck.getSkip(1);
        if (a.tagRead(pck)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != cryAsn1.tagSequence)) {
            return true;
        }
        pck = a.getPack();
        modulus = cryAsn1.readBigInt(pck);
        pubExp = cryAsn1.readBigInt(pck);
        return false;
    }

    public void certWriter(packHolder pck) {
        packHolder p1 = new packHolder(true, true);
        packHolder p2 = new packHolder(true, true);
        packHolder p3 = new packHolder(true, true);
        cryAsn1.writeObjectId(p1, cryCertificate.oidRsaEncrypt);
        cryAsn1.writeNull(p1);
        cryAsn1.writeSequence(p2, p1);
        cryAsn1.writeBigInt(p3, modulus);
        cryAsn1.writeBigInt(p3, pubExp);
        p1.clear();
        cryAsn1.writeSequence(p1, p3);
        p1.putByte(0, 0);
        p1.putSkip(1);
        p1.merge2beg();
        cryAsn1.writeBitString(p2, p1);
        cryAsn1.writeSequence(pck, p2);
    }

    public boolean privReader(packHolder pck) {
        cryAsn1 a = new cryAsn1();
        if (a.tagRead(pck)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != cryAsn1.tagSequence)) {
            return true;
        }
        pck = a.getPack();
        BigInteger ver = cryAsn1.readBigInt(pck);
        if (ver == null) {
            return true;
        }
        modulus = cryAsn1.readBigInt(pck);
        if (modulus == null) {
            return true;
        }
        pubExp = cryAsn1.readBigInt(pck);
        if (pubExp == null) {
            return true;
        }
        privExp = cryAsn1.readBigInt(pck);
        if (privExp == null) {
            return true;
        }
        prime1 = cryAsn1.readBigInt(pck);
        if (prime1 == null) {
            return true;
        }
        prime2 = cryAsn1.readBigInt(pck);
        if (prime2 == null) {
            return true;
        }
        expon1 = cryAsn1.readBigInt(pck);
        if (expon1 == null) {
            return true;
        }
        expon2 = cryAsn1.readBigInt(pck);
        if (expon2 == null) {
            return true;
        }
        coeff = cryAsn1.readBigInt(pck);
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
        cryAsn1.writeBigInt(p, BigInteger.ZERO);
        cryAsn1.writeBigInt(p, modulus);
        cryAsn1.writeBigInt(p, pubExp);
        cryAsn1.writeBigInt(p, privExp);
        cryAsn1.writeBigInt(p, prime1);
        cryAsn1.writeBigInt(p, prime2);
        cryAsn1.writeBigInt(p, expon1);
        cryAsn1.writeBigInt(p, expon2);
        cryAsn1.writeBigInt(p, coeff);
        cryAsn1.writeSequence(pck, p);
    }

    public void keyMake(int len) {
        pubExp = new BigInteger("65537", 10);
        prime1 = randomPrime(len / 2);
        prime2 = randomPrime(len / 2);
        if (prime1.compareTo(prime2) < 0) {
            BigInteger i = prime1;
            prime1 = prime2;
            prime2 = i;
        }
        modulus = prime1.multiply(prime2);
        BigInteger i1 = prime1.subtract(BigInteger.ONE);
        BigInteger i2 = prime2.subtract(BigInteger.ONE);
        BigInteger i0 = i1.multiply(i2);
        privExp = pubExp.modInverse(i0);
        expon1 = privExp.mod(i1);
        expon2 = privExp.mod(i2);
        coeff = prime2.modInverse(prime1);
    }

    public boolean keyVerify() {
        if (!testPrime(pubExp)) {
            return true;
        }
        if (!testPrime(prime1)) {
            return true;
        }
        if (!testPrime(prime2)) {
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

    /**
     * do encryption
     *
     * @param src cleartext
     * @return ciphertext
     */
    public byte[] doEncrypt(byte[] src) {
        BigInteger i = buffer2bigInt(src, 0, src.length);
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
        BigInteger i = buffer2bigInt(src, 0, src.length);
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
     * pad for ssh
     *
     * @param src buffer to pad
     * @return padded integer
     */
    public BigInteger PKCS1t0pad(byte[] src) {
        final byte[] pad = {0x00, 0x30, 0x21, 0x30, 0x09, 0x06, 0x05, 0x2b, 0x0e, 0x03, 0x02, 0x1a, 0x05, 0x00, 0x04, 0x14};
        byte[] buf = new byte[(modulus.bitLength() + 7) / 8];
        for (int i = 0; i < buf.length; i++) {
            buf[i] = (byte) 0xff;
        }
        buf[0] = 0;
        buf[1] = 1;
        bits.byteCopy(src, 0, buf, buf.length - src.length, src.length);
        bits.byteCopy(pad, 0, buf, buf.length - src.length - pad.length, pad.length);
        return new BigInteger(buf);
    }

    /**
     * pad for ssl
     *
     * @param src buffer to pad
     * @return padded integer
     */
    public BigInteger PKCS1t1pad(byte[] src) {
        byte[] buf = new byte[(modulus.bitLength() + 7) / 8];
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
        byte[] buf = new byte[(modulus.bitLength() + 7) / 8];
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

    public boolean sshVerify(byte[] hash, byte[] sign) {
        hash = cryHashGeneric.compute(new cryHashSha1(), hash);
        packHolder p = new packHolder(true, true);
        p.putCopy(sign, 0, 0, sign.length);
        p.putSkip(sign.length);
        p.merge2beg();
        if (!packSsh.stringRead(p).equals(sshName)) {
            return true;
        }
        BigInteger s = packSsh.bigUIntRead(p);
        s = s.modPow(pubExp, modulus);
        return PKCS1t0pad(hash).compareTo(s) != 0;
    }

    public byte[] sshSigning(byte[] hash) {
        hash = cryHashGeneric.compute(new cryHashSha1(), hash);
        packHolder p = new packHolder(true, true);
        BigInteger s = PKCS1t0pad(hash);
        s = s.modPow(privExp, modulus);
        packSsh.stringWrite(p, sshName);
        packSsh.bigUIntWrite(p, s);
        p.merge2beg();
        return p.getCopy();
    }

    public boolean certVerify(byte[] hash, byte[] sign) {
        BigInteger s = cryUtils.buf2bigUint(sign);
        s = s.modPow(pubExp, modulus);
        return PKCS1t0pad(hash).compareTo(s) != 0;
    }

    public byte[] certSigning(byte[] hash) {
        BigInteger s = PKCS1t0pad(hash);
        s = s.modPow(privExp, modulus);
        return s.toByteArray();
    }

    public boolean tlsVerify(int ver, byte[] hash, byte[] sign) {
        BigInteger s = cryUtils.buf2bigUint(sign);
        s = s.modPow(pubExp, modulus);
        BigInteger h;
        if (ver >= 0x303) {
            h = PKCS1t0pad(hash);
        } else {
            h = PKCS1t1pad(hash);
        }
        return h.compareTo(s) != 0;
    }

    public byte[] tlsSigning(int ver, byte[] hash) {
        BigInteger s;
        if (ver >= 0x303) {
            s = PKCS1t0pad(hash);
        } else {
            s = PKCS1t1pad(hash);
        }
        s = s.modPow(privExp, modulus);
        return s.toByteArray();
    }

}
