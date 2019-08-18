package cry;

import java.math.BigInteger;

import pack.packHolder;
import pack.packSsh;

/**
 * digital signature algorithm
 *
 * @author matecsaba
 */
public class cryKeyDSA extends cryKeyGeneric {

    /**
     * ssh name
     */
    public final static String sshName = "ssh-dss";

    /**
     * p prime
     */
    public BigInteger prime;

    /**
     * q subprime
     */
    public BigInteger subprime;

    /**
     * g group
     */
    public BigInteger group;

    /**
     * y public key
     */
    public BigInteger pub;

    /**
     * x private key
     */
    protected BigInteger priv;

    /**
     * signature r
     */
    public BigInteger sgnR;

    /**
     * signature s
     */
    public BigInteger sgnS;

    private final static int hashBits = 160;

    private final static int hashBytes = hashBits / 8;

    /**
     * convert to string
     *
     * @return string
     */
    public String toString() {
        return "prime=" + prime + " subprime=" + subprime + " group=" + group + " pubkey=" + pub + " privkey=" + priv;
    }

    /**
     * get name
     *
     * @return name
     */
    public String algName() {
        return "dsa";
    }

    /**
     * read certificate
     *
     * @param pck packet
     * @return false on success, true on error
     */
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
        if (cryCertificate.objid2int(a) != cryCertificate.typDssEncrypt) {
            return true;
        }
        if (a.tagRead(p)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != cryAsn1.tagSequence)) {
            return true;
        }
        p = a.getPack();
        prime = cryAsn1.readBigInt(p);
        subprime = cryAsn1.readBigInt(p);
        group = cryAsn1.readBigInt(p);
        if (a.tagRead(pck)) {
            return true;
        }
        if ((a.cnst) || (a.tag != cryAsn1.tagBitString)) {
            return true;
        }
        p = a.getPack();
        p.getSkip(1);
        pub = cryAsn1.readBigInt(p);
        return false;
    }

    /**
     * write certificate
     *
     * @param pck packet
     */
    public void certWriter(packHolder pck) {
        packHolder p1 = new packHolder(true, true);
        packHolder p2 = new packHolder(true, true);
        packHolder p3 = new packHolder(true, true);
        cryAsn1.writeBigInt(p1, prime);
        cryAsn1.writeBigInt(p1, subprime);
        cryAsn1.writeBigInt(p1, group);
        cryAsn1.writeObjectId(p2, cryCertificate.oidDssEncrypt);
        cryAsn1.writeSequence(p2, p1);
        cryAsn1.writeSequence(p3, p2);
        p1 = new packHolder(true, true);
        cryAsn1.writeBigInt(p1, pub);
        p1.putByte(0, 0);
        p1.putSkip(1);
        p1.merge2beg();
        cryAsn1.writeBitString(p3, p1);
        cryAsn1.writeSequence(pck, p3);
    }

    /**
     * read private key
     *
     * @param pck packet
     * @return false on success, true on error
     */
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
        prime = cryAsn1.readBigInt(pck);
        if (prime == null) {
            return true;
        }
        subprime = cryAsn1.readBigInt(pck);
        if (subprime == null) {
            return true;
        }
        group = cryAsn1.readBigInt(pck);
        if (group == null) {
            return true;
        }
        pub = cryAsn1.readBigInt(pck);
        if (pub == null) {
            return true;
        }
        priv = cryAsn1.readBigInt(pck);
        if (priv == null) {
            return true;
        }
        if (pck.dataSize() != 0) {
            return true;
        }
        return false;
    }

    /**
     * write private key
     *
     * @param pck packet
     */
    public void privWriter(packHolder pck) {
        packHolder p = new packHolder(true, true);
        cryAsn1.writeBigInt(p, BigInteger.ZERO);
        cryAsn1.writeBigInt(p, prime);
        cryAsn1.writeBigInt(p, subprime);
        cryAsn1.writeBigInt(p, group);
        cryAsn1.writeBigInt(p, pub);
        cryAsn1.writeBigInt(p, priv);
        cryAsn1.writeSequence(pck, p);
    }

    /**
     * make key
     *
     * @param len length
     */
    public void keyMake(int len) {
        priv = randomBigInt(hashBits);
        subprime = randomPrime(hashBits);
        for (;;) {
            BigInteger i = randomBigInt(len - hashBits);
            prime = subprime.multiply(i).add(BigInteger.ONE);
            if (testPrime(prime)) {
                break;
            }
        }
        BigInteger o = prime.subtract(BigInteger.ONE).divide(subprime);
        for (;;) {
            BigInteger i = randomBigInt(prime.bitLength() - 1);
            group = i.modPow(o, prime);
            if (group.compareTo(BigInteger.ONE) > 0) {
                break;
            }
        }
        pub = group.modPow(priv, prime);
    }

    /**
     * verify key
     *
     * @return false on success, true on error
     */
    public boolean keyVerify() {
        if (!testPrime(subprime)) {
            return true;
        }
        if (!testPrime(prime)) {
            return true;
        }
        if (prime.mod(subprime).compareTo(BigInteger.ONE) != 0) {
            return true;
        }
        if (group.compareTo(BigInteger.ONE) < 1) {
            return true;
        }
        if (pub.compareTo(group.modPow(priv, prime)) != 0) {
            return true;
        }
        return false;
    }

    /**
     * get key size
     *
     * @return size
     */
    public int keySize() {
        return prime.bitLength();
    }

    /**
     * convert signature to ssh
     *
     * @return byte array of r|s
     */
    public byte[] sign2ssh() {
        byte[] b1 = bigInt2buffer(sgnR, hashBytes);
        byte[] b2 = bigInt2buffer(sgnS, hashBytes);
        byte[] b3 = new byte[hashBytes + hashBytes];
        for (int i = 0; i < hashBytes; i++) {
            b3[i] = b1[i];
            b3[hashBytes + i] = b2[i];
        }
        return b3;
    }

    /**
     * convert ssh to signature
     *
     * @param sgn r|s byte array
     * @return false on success, true on error
     */
    public boolean ssh2sign(byte[] sgn) {
        if (sgn.length != hashBytes + hashBytes) {
            return true;
        }
        sgnR = buffer2bigInt(sgn, 0, hashBytes);
        sgnS = buffer2bigInt(sgn, hashBytes, hashBytes);
        return false;
    }

    /**
     * do signature
     *
     * @param msg cleartext
     */
    public void doSigning(byte[] msg) {
        BigInteger m = buffer2bigInt(msg, 0, msg.length);
        BigInteger k = randomBigInt(hashBits).mod(subprime);
        sgnR = group.modPow(k, prime).mod(subprime);
        k = k.modInverse(subprime);
        sgnS = priv.multiply(sgnR).add(m).multiply(k).mod(subprime);
    }

    /**
     * do verification
     *
     * @param msg cleartext
     * @return false on success, true on error
     */
    public boolean doVerify(byte[] msg) {
        BigInteger m = buffer2bigInt(msg, 0, msg.length);
        BigInteger w = sgnS.modInverse(subprime);
        BigInteger u1 = m.multiply(w).mod(subprime);
        BigInteger u2 = sgnR.multiply(w).mod(subprime);
        u1 = group.modPow(u1, prime);
        u2 = pub.modPow(u2, prime);
        BigInteger v = u1.multiply(u2).mod(prime).mod(subprime);
        return v.compareTo(sgnR) != 0;
    }

    /**
     * read ssh key
     *
     * @param key key
     * @return false on success, true on error
     */
    public boolean sshReader(byte[] key) {
        packHolder p = new packHolder(true, true);
        p.putCopy(key, 0, 0, key.length);
        p.putSkip(key.length);
        p.merge2beg();
        if (!packSsh.stringRead(p).equals(sshName)) {
            return true;
        }
        prime = packSsh.bigIntRead(p);
        subprime = packSsh.bigIntRead(p);
        group = packSsh.bigIntRead(p);
        pub = packSsh.bigIntRead(p);
        return false;
    }

    /**
     * write ssh key
     *
     * @return key
     */
    public byte[] sshWriter() {
        packHolder p = new packHolder(true, true);
        packSsh.stringWrite(p, sshName);
        packSsh.bigIntWrite(p, prime);
        packSsh.bigIntWrite(p, subprime);
        packSsh.bigIntWrite(p, group);
        packSsh.bigIntWrite(p, pub);
        p.merge2beg();
        return p.getCopy();
    }

    /**
     * verify ssh key
     *
     * @param hash hashed
     * @param sign signed
     * @return false on success, true on error
     */
    public boolean sshVerify(byte[] hash, byte[] sign) {
        hash = cryHashGeneric.compute(new cryHashSha1(), hash);
        packHolder p = new packHolder(true, true);
        p.putCopy(sign, 0, 0, sign.length);
        p.putSkip(sign.length);
        p.merge2beg();
        if (!packSsh.stringRead(p).equals(sshName)) {
            return true;
        }
        sign = packSsh.bytesRead(p);
        ssh2sign(sign);
        return doVerify(hash);
    }

    /**
     * sign for ssh
     *
     * @param hash hashed
     * @return signed
     */
    public byte[] sshSigning(byte[] hash) {
        hash = cryHashGeneric.compute(new cryHashSha1(), hash);
        doSigning(hash);
        hash = sign2ssh();
        packHolder p = new packHolder(true, true);
        packSsh.stringWrite(p, sshName);
        packSsh.bytesWrite(p, hash);
        p.merge2beg();
        return p.getCopy();
    }

    /**
     * verify ceriticate
     *
     * @param hash hash
     * @param sign signature
     * @return false on success, true on error
     */
    public boolean certVerify(byte[] hash, byte[] sign) {
        packHolder p = new packHolder(true, true);
        p.putCopy(sign, 0, 0, sign.length);
        p.putSkip(sign.length);
        p.merge2beg();
        p.getSkip(1);
        cryAsn1 a = new cryAsn1();
        if (a.tagRead(p)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != cryAsn1.tagSequence)) {
            return true;
        }
        p = a.getPack();
        sgnR = cryAsn1.readBigInt(p);
        sgnS = cryAsn1.readBigInt(p);
        return doVerify(hash);
    }

    /**
     * sign for certificate
     *
     * @param hash hash
     * @return signed
     */
    public byte[] certSigning(byte[] hash) {
        doSigning(hash);
        packHolder p1 = new packHolder(true, true);
        packHolder p2 = new packHolder(true, true);
        cryAsn1.writeBigInt(p1, sgnR);
        cryAsn1.writeBigInt(p1, sgnS);
        cryAsn1.writeSequence(p2, p1);
        p2.putByte(0, 0);
        p2.putSkip(1);
        p2.merge2beg();
        return p2.getCopy();
    }

    /**
     * tls verification
     *
     * @param ver version
     * @param hash hash
     * @param sign signature
     * @return false on success, true on error
     */
    public boolean tlsVerify(int ver, byte[] hash, byte[] sign) {
        packHolder p = new packHolder(true, true);
        p.putCopy(sign, 0, 0, sign.length);
        p.putSkip(sign.length);
        p.merge2beg();
        cryAsn1 a = new cryAsn1();
        if (a.tagRead(p)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != cryAsn1.tagSequence)) {
            return true;
        }
        p = a.getPack();
        sgnR = cryAsn1.readBigInt(p);
        sgnS = cryAsn1.readBigInt(p);
        return doVerify(hash);
    }

    /**
     * sign for tls
     *
     * @param ver version
     * @param hash hash
     * @return signature
     */
    public byte[] tlsSigning(int ver, byte[] hash) {
        doSigning(hash);
        packHolder p1 = new packHolder(true, true);
        packHolder p2 = new packHolder(true, true);
        cryAsn1.writeBigInt(p1, sgnR);
        cryAsn1.writeBigInt(p1, sgnS);
        cryAsn1.writeSequence(p2, p1);
        p2.merge2beg();
        return p2.getCopy();
    }

}
