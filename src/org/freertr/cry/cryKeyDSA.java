package org.freertr.cry;

import org.freertr.enc.encAsn1;
import java.math.BigInteger;
import org.freertr.pack.packHolder;
import org.freertr.pack.packSsh;

/**
 * digital signature algorithm
 *
 * @author matecsaba
 */
public class cryKeyDSA extends cryKeyGeneric {

    /**
     * create instance
     */
    public cryKeyDSA() {
    }

    /**
     * ssh name
     */
    public final static String sshName = "ssh-dss";

    /**
     * p prime
     */
    protected BigInteger prime;

    /**
     * q subprime
     */
    protected BigInteger subprime;

    /**
     * g group
     */
    protected BigInteger group;

    /**
     * y public key
     */
    protected BigInteger pub;

    /**
     * x private key
     */
    protected BigInteger priv;

    /**
     * signature r
     */
    protected BigInteger sgnR;

    /**
     * signature s
     */
    protected BigInteger sgnS;

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
     * get ssh name
     *
     * @return name
     */
    public String sshName() {
        return sshName;
    }

    /**
     * get ssh hash
     *
     * @return hasher
     */
    public cryHashGeneric sshHash() {
        return new cryHashSha1();
    }

    /**
     * read certificate
     *
     * @param pck packet
     * @return false on success, true on error
     */
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
        if (cryCertificate.objid2int(a) != cryCertificate.typDssEncrypt) {
            return true;
        }
        if (a.tagRead(p)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != encAsn1.tagSequence)) {
            return true;
        }
        p = a.getPack();
        prime = encAsn1.readBigInt(p);
        subprime = encAsn1.readBigInt(p);
        group = encAsn1.readBigInt(p);
        if (a.tagRead(pck)) {
            return true;
        }
        if ((a.cnst) || (a.tag != encAsn1.tagBitString)) {
            return true;
        }
        p = a.getPack();
        p.getSkip(1);
        pub = encAsn1.readBigInt(p);
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
        encAsn1.writeBigInt(p1, prime);
        encAsn1.writeBigInt(p1, subprime);
        encAsn1.writeBigInt(p1, group);
        encAsn1.writeObjectId(p2, cryCertificate.oidDssEncrypt);
        encAsn1.writeSequence(p2, p1);
        encAsn1.writeSequence(p3, p2);
        p1 = new packHolder(true, true);
        encAsn1.writeBigInt(p1, pub);
        p1.putByte(0, 0);
        p1.putSkip(1);
        p1.merge2beg();
        encAsn1.writeBitString(p3, p1);
        encAsn1.writeSequence(pck, p3);
    }

    /**
     * read private key
     *
     * @param pck packet
     * @return false on success, true on error
     */
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
        prime = encAsn1.readBigInt(pck);
        if (prime == null) {
            return true;
        }
        subprime = encAsn1.readBigInt(pck);
        if (subprime == null) {
            return true;
        }
        group = encAsn1.readBigInt(pck);
        if (group == null) {
            return true;
        }
        pub = encAsn1.readBigInt(pck);
        if (pub == null) {
            return true;
        }
        priv = encAsn1.readBigInt(pck);
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
        encAsn1.writeBigInt(p, BigInteger.ZERO);
        encAsn1.writeBigInt(p, prime);
        encAsn1.writeBigInt(p, subprime);
        encAsn1.writeBigInt(p, group);
        encAsn1.writeBigInt(p, pub);
        encAsn1.writeBigInt(p, priv);
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

    /**
     * make key
     *
     * @param len length
     */
    public boolean keyMakeSize(int len) {
        priv = cryUtils.randomBigInt(hashBits);
        subprime = cryUtils.randomPrime(hashBits);
        for (;;) {
            BigInteger i = cryUtils.randomBigInt(len - hashBits);
            prime = subprime.multiply(i).add(BigInteger.ONE);
            if (cryUtils.testPrime(prime)) {
                break;
            }
        }
        BigInteger o = prime.subtract(BigInteger.ONE).divide(subprime);
        for (;;) {
            BigInteger i = cryUtils.randomBigInt(prime.bitLength() - 1);
            group = i.modPow(o, prime);
            if (group.compareTo(BigInteger.ONE) > 0) {
                break;
            }
        }
        pub = group.modPow(priv, prime);
        return false;
    }

    /**
     * verify key
     *
     * @return false on success, true on error
     */
    public boolean keyVerify() {
        if (!cryUtils.testPrime(subprime)) {
            return true;
        }
        if (!cryUtils.testPrime(prime)) {
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
     * convert signature to ssh
     *
     * @return byte array of r|s
     */
    public byte[] sign2ssh() {
        byte[] b1 = cryUtils.bigInt2buffer(sgnR, hashBytes);
        byte[] b2 = cryUtils.bigInt2buffer(sgnS, hashBytes);
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
        sgnR = cryUtils.buffer2bigInt(sgn, 0, hashBytes);
        sgnS = cryUtils.buffer2bigInt(sgn, hashBytes, hashBytes);
        return false;
    }

    /**
     * do signature
     *
     * @param msg cleartext
     */
    public void doSigning(byte[] msg) {
        BigInteger m = cryUtils.buffer2bigInt(msg, 0, msg.length);
        BigInteger k = cryUtils.randomBigInt(hashBits).mod(subprime);
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
        BigInteger m = cryUtils.buffer2bigInt(msg, 0, msg.length);
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
    public boolean sshVerify(cryHashGeneric algo, String algn, byte[] hash, byte[] sign) {
        hash = cryHashGeneric.compute(algo, hash);
        packHolder p = new packHolder(true, true);
        p.putCopy(sign, 0, 0, sign.length);
        p.putSkip(sign.length);
        p.merge2beg();
        if (!packSsh.stringRead(p).equals(algn)) {
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
    public byte[] sshSigning(cryHashGeneric algo, String algn, byte[] hash) {
        hash = cryHashGeneric.compute(algo, hash);
        doSigning(hash);
        hash = sign2ssh();
        packHolder p = new packHolder(true, true);
        packSsh.stringWrite(p, algn);
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
    public boolean certVerify(cryHashGeneric pkcs, byte[] hash, byte[] sign) {
        hash = cryHashGeneric.compute(pkcs, hash);
        packHolder p = new packHolder(true, true);
        p.putCopy(sign, 0, 0, sign.length);
        p.putSkip(sign.length);
        p.merge2beg();
        p.getSkip(1);
        encAsn1 a = new encAsn1();
        if (a.tagRead(p)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != encAsn1.tagSequence)) {
            return true;
        }
        p = a.getPack();
        sgnR = encAsn1.readBigInt(p);
        sgnS = encAsn1.readBigInt(p);
        return doVerify(hash);
    }

    /**
     * sign for certificate
     *
     * @param hash hash
     * @return signed
     */
    public byte[] certSigning(cryHashGeneric pkcs, byte[] hash) {
        hash = cryHashGeneric.compute(pkcs, hash);
        doSigning(hash);
        packHolder p1 = new packHolder(true, true);
        packHolder p2 = new packHolder(true, true);
        encAsn1.writeBigInt(p1, sgnR);
        encAsn1.writeBigInt(p1, sgnS);
        encAsn1.writeSequence(p2, p1);
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
    public boolean tlsVerify(int ver, cryHashGeneric pkcs, byte[] hash, byte[] sign) {
        hash = cryHashGeneric.compute(pkcs, hash);
        packHolder p = new packHolder(true, true);
        p.putCopy(sign, 0, 0, sign.length);
        p.putSkip(sign.length);
        p.merge2beg();
        encAsn1 a = new encAsn1();
        if (a.tagRead(p)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != encAsn1.tagSequence)) {
            return true;
        }
        p = a.getPack();
        sgnR = encAsn1.readBigInt(p);
        sgnS = encAsn1.readBigInt(p);
        return doVerify(hash);
    }

    /**
     * sign for tls
     *
     * @param ver version
     * @param hash hash
     * @return signature
     */
    public byte[] tlsSigning(int ver, cryHashGeneric pkcs, byte[] hash) {
        hash = cryHashGeneric.compute(pkcs, hash);
        doSigning(hash);
        packHolder p1 = new packHolder(true, true);
        packHolder p2 = new packHolder(true, true);
        encAsn1.writeBigInt(p1, sgnR);
        encAsn1.writeBigInt(p1, sgnS);
        encAsn1.writeSequence(p2, p1);
        p2.merge2beg();
        return p2.getCopy();
    }

}
