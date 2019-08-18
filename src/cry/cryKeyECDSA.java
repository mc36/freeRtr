package cry;

import java.math.BigInteger;
import pack.packHolder;
import pack.packSsh;

/**
 * elliptic curve digital signature algorithm
 *
 * @author matecsaba
 */
public class cryKeyECDSA extends cryKeyGeneric {

    /**
     * curve
     */
    public cryECcurve curve;

    /**
     * y public key
     */
    public cryECpoint pub;

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

    /**
     * get ssh name
     *
     * @return name
     */
    public String sshName() {
        return "ssh-ecdsa-sha2-" + curve;
    }

    /**
     * get name
     *
     * @return name
     */
    public String algName() {
        return "ecdsa";
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
        if (a.tagRead(pck)) {
            return true;
        }
        if ((a.cnst) || (a.tag != cryAsn1.tagOctetString)) {
            return true;
        }
        priv = new BigInteger(a.getPack().getCopy());
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
        if ((a.cnst) || (a.tag != cryAsn1.tagObjectID)) {
            return true;
        }
        curve = cryECcurve.getByOid(a.buf);
        if (curve == null) {
            return true;
        }
        if (a.tagRead(pck)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != cryAsn1.tagBoolean)) {
            return true;
        }
        p = a.getPack();
        if (a.tagRead(p)) {
            return true;
        }
        if ((a.cnst) || (a.tag != cryAsn1.tagBitString)) {
            return true;
        }
        pub = cryECpoint.fromBytes(curve, a.buf);
        if (pub == null) {
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
        packHolder p1 = new packHolder(true, true);
        cryAsn1.writeBigInt(p1, BigInteger.ONE);
        packHolder p2 = new packHolder(true, true);
        byte[] buf = priv.toByteArray();
        p2.putCopy(buf, 0, 0, buf.length);
        p2.putSkip(buf.length);
        p2.merge2beg();
        cryAsn1.writeOctString(p1, p2);
        p2.clear();
        cryAsn1.writeObjectId(p2, curve.oid);
        cryAsn1.writeEoc(p1, p2);
        p2.clear();
        packHolder p3 = new packHolder(true, true);
        buf = pub.toBytes();
        p3.putCopy(buf, 0, 0, buf.length);
        p3.putSkip(buf.length);
        p3.merge2beg();
        cryAsn1.writeBitString(p2, p3);
        cryAsn1.writeEoc2(p1, p2);
        cryAsn1.writeSequence(pck, p1);
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
        if (cryCertificate.objid2int(a) != cryCertificate.typEcDssEncrypt) {
            return true;
        }
        if (a.tagRead(p)) {
            return true;
        }
        if ((a.cnst) || (a.tag != cryAsn1.tagObjectID)) {
            return true;
        }
        curve = cryECcurve.getByOid(a.buf);
        if (curve == null) {
            return true;
        }
        if (a.tagRead(pck)) {
            return true;
        }
        if ((a.cnst) || (a.tag != cryAsn1.tagBitString)) {
            return true;
        }
        pub = cryECpoint.fromBytes(curve, a.buf);
        if (pub == null) {
            return true;
        }
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
        cryAsn1.writeObjectId(p2, cryCertificate.oidEcDssEncrypt);
        cryAsn1.writeObjectId(p2, curve.oid);
        cryAsn1.writeSequence(p1, p2);
        p2.clear();
        byte[] buf = pub.toBytes();
        p2.putCopy(buf, 0, 0, buf.length);
        p2.putSkip(buf.length);
        p2.merge2beg();
        cryAsn1.writeBitString(p1, p2);
        cryAsn1.writeSequence(pck, p1);
    }

    /**
     * make key
     *
     * @param len length
     */
    public void keyMake(int len) {
        curve = cryECcurve.getBySize(len);
        priv = randomBigInt(curve.n.bitLength() - 2);
        pub = curve.g.mul(priv);
    }

    /**
     * verify key
     *
     * @return false on success, true on error
     */
    public boolean keyVerify() {
        return !curve.g.check();
    }

    /**
     * get key size
     *
     * @return size
     */
    public int keySize() {
        return curve.p.bitLength();
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
        if (!packSsh.stringRead(p).equals(sshName())) {
            return true;
        }
        if (!packSsh.stringRead(p).equals(curve + "")) {
            return true;
        }
        pub = cryECpoint.fromBytes(curve, packSsh.bytesRead(p));
        if (pub == null) {
            return true;
        }
        return false;
    }

    /**
     * write ssh key
     *
     * @return key
     */
    public byte[] sshWriter() {
        packHolder p = new packHolder(true, true);
        packSsh.stringWrite(p, sshName());
        packSsh.stringWrite(p, "" + curve);
        packSsh.bytesWrite(p, pub.toBytes());
        return null;
    }

    private BigInteger calcZ(BigInteger n, byte[] msg) {
        BigInteger z = new BigInteger(msg);
        int nl = n.bitLength();
        int zl = z.bitLength();
        if (nl < zl) {
            z = z.shiftRight(zl - nl);
        }
        return z;
    }

    /**
     * do signature
     *
     * @param msg cleartext
     */
    public void doSigning(byte[] msg) {
        BigInteger z = calcZ(curve.n, msg);
        BigInteger k = randomBigInt(curve.n.bitLength() - 2);
        cryECpoint Q = curve.g.mul(k);
        sgnR = Q.x.mod(curve.n);
        sgnS = k.modInverse(curve.n).multiply(z.add(sgnR.multiply(priv)));
    }

    /**
     * do verification
     *
     * @param msg cleartext
     * @return false on success, true on error
     */
    public boolean doVerify(byte[] msg) {
        BigInteger z = calcZ(curve.n, msg);
        BigInteger w = sgnS.modInverse(curve.n);
        BigInteger u1 = z.multiply(w).mod(curve.n);
        BigInteger u2 = sgnR.multiply(w).mod(curve.n);
        cryECpoint Q = curve.g.mul(u1).add(pub.mul(u2));
        return sgnR.compareTo(Q.x.mod(curve.n)) != 0;
    }

    /**
     * convert signature to ssh
     *
     * @return byte array of r|s
     */
    public byte[] sign2ssh() {
        int hashBytes = (curve.p.bitLength() + 7) / 8;
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
        int hashBytes = (curve.p.bitLength() + 7) / 8;
        if (sgn.length != hashBytes + hashBytes) {
            return true;
        }
        sgnR = buffer2bigInt(sgn, 0, hashBytes);
        sgnS = buffer2bigInt(sgn, hashBytes, hashBytes);
        return false;
    }

    /**
     * verify ssh signature
     *
     * @param hash hash
     * @param sign signature
     * @return false on success, true on error
     */
    public boolean sshVerify(byte[] hash, byte[] sign) {
        hash = cryHashGeneric.compute(new cryHashSha1(), hash);
        packHolder p = new packHolder(true, true);
        p.putCopy(sign, 0, 0, sign.length);
        p.putSkip(sign.length);
        p.merge2beg();
        if (!packSsh.stringRead(p).equals(sshName())) {
            return true;
        }
        sign = packSsh.bytesRead(p);
        ssh2sign(sign);
        return doVerify(hash);
    }

    /**
     * sign for ssh
     *
     * @param hash hash
     * @return signature
     */
    public byte[] sshSigning(byte[] hash) {
        hash = cryHashGeneric.compute(new cryHashSha1(), hash);
        doSigning(hash);
        hash = sign2ssh();
        packHolder p = new packHolder(true, true);
        packSsh.stringWrite(p, sshName());
        packSsh.bytesWrite(p, hash);
        p.merge2beg();
        return p.getCopy();
    }

    /**
     * verify certificate
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
     * sign certificate
     *
     * @param hash hash
     * @return signature
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
     * verify tls
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
