package org.freertr.cry;

import org.freertr.enc.encAsn1;
import java.math.BigInteger;
import org.freertr.pack.packHolder;
import org.freertr.pack.packSsh;

/**
 * elliptic curve digital signature algorithm
 *
 * @author matecsaba
 */
public class cryKeyECDSA extends cryKeyGeneric {

    /**
     * create instance
     */
    public cryKeyECDSA() {
    }

    /**
     * curve
     */
    protected cryKeyECcurve curve;

    /**
     * y public key
     */
    protected cryKeyECpoint pub;

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

    /**
     * get ssh name
     *
     * @return name
     */
    public String sshName() {
        return "ecdsa-sha2-" + curve.sshName();
    }

    public cryHashGeneric sshHash() {
        int i = curve.byteSize();
        if (i <= 32) {
            return new cryHashSha2256();
        }
        if (i <= 48) {
            return new cryHashSha2384();
        }
        return new cryHashSha2512();
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
        if (a.tagRead(pck)) {
            return true;
        }
        if ((a.cnst) || (a.tag != encAsn1.tagOctetString)) {
            return true;
        }
        priv = new BigInteger(a.getPack().getCopy());
        if (a.tagRead(pck)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != encAsn1.tagEoc)) {
            return true;
        }
        packHolder p = a.getPack();
        if (a.tagRead(p)) {
            return true;
        }
        if ((a.cnst) || (a.tag != encAsn1.tagObjectID)) {
            return true;
        }
        curve = cryKeyECcurve.getByOid(a.buf);
        if (curve == null) {
            return true;
        }
        if (a.tagRead(pck)) {
            return true;
        }
        if ((!a.cnst) || (a.tag != encAsn1.tagBoolean)) {
            return true;
        }
        p = a.getPack();
        if (a.tagRead(p)) {
            return true;
        }
        if ((a.cnst) || (a.tag != encAsn1.tagBitString)) {
            return true;
        }
        pub = cryKeyECpoint.fromBytesCert(curve, a.buf, 0);
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
        encAsn1.writeBigInt(p1, BigInteger.ONE);
        packHolder p2 = new packHolder(true, true);
        byte[] buf = priv.toByteArray();
        p2.putCopy(buf, 0, 0, buf.length);
        p2.putSkip(buf.length);
        p2.merge2beg();
        encAsn1.writeOctString(p1, p2);
        p2.clear();
        encAsn1.writeObjectId(p2, curve.oid);
        encAsn1.writeEoc(p1, p2);
        p2.clear();
        packHolder p3 = new packHolder(true, true);
        buf = pub.toBytesCert();
        p3.putCopy(buf, 0, 0, buf.length);
        p3.putSkip(buf.length);
        p3.merge2beg();
        encAsn1.writeBitString(p2, p3);
        encAsn1.writeBool(p1, p2);
        encAsn1.writeSequence(pck, p1);
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
        if (cryCertificate.objid2int(a) != cryCertificate.typEcDssEncrypt) {
            return true;
        }
        if (a.tagRead(p)) {
            return true;
        }
        if ((a.cnst) || (a.tag != encAsn1.tagObjectID)) {
            return true;
        }
        curve = cryKeyECcurve.getByOid(a.buf);
        if (curve == null) {
            return true;
        }
        if (a.tagRead(pck)) {
            return true;
        }
        if ((a.cnst) || (a.tag != encAsn1.tagBitString)) {
            return true;
        }
        pub = cryKeyECpoint.fromBytesCert(curve, a.buf, 0);
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
        encAsn1.writeObjectId(p2, cryCertificate.oidEcDssEncrypt);
        encAsn1.writeObjectId(p2, curve.oid);
        encAsn1.writeSequence(p1, p2);
        p2.clear();
        byte[] buf = pub.toBytesCert();
        p2.putByte(0, 0);
        p2.putCopy(buf, 0, 0, buf.length);
        p2.putSkip(buf.length);
        p2.merge2beg();
        encAsn1.writeBitString(p1, p2);
        encAsn1.writeSequence(pck, p1);
    }

    public boolean keyMakeName(String nam) {
        curve = cryKeyECcurve.getByName(nam);
        return keyMake();
    }

    public boolean keyMakeTls(int id) {
        curve = cryKeyECcurve.getByTls(id);
        return false;
    }

    public boolean keyMakeIke(int id) {
        return false;
    }

    public int keyMakeVal() {
        if (curve == null) {
            return -1;
        }
        return curve.sgn;
    }

    private boolean keyMake() {
        priv = cryUtils.randomBigInt(curve.n.bitLength() - 2);
        pub = curve.g.mul(priv);
        return false;
    }

    /**
     * make key
     *
     * @param len length
     */
    public boolean keyMakeSize(int len) {
        curve = cryKeyECcurve.getBySize(len);
        return keyMake();
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
        if (!packSsh.stringRead(p).equals(curve.sshName())) {
            return true;
        }
        pub = cryKeyECpoint.fromBytesTls(curve, packSsh.bytesRead(p), 0);
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
        packSsh.stringWrite(p, curve.sshName());
        packSsh.bytesWrite(p, pub.toBytesTls());
        p.merge2beg();
        return p.getCopy();
    }

    private BigInteger calcZ(BigInteger n, byte[] msg) {
        BigInteger z = cryUtils.buffer2bigInt(msg, 0, msg.length);
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
        BigInteger k = cryUtils.randomBigInt(curve.n.bitLength() - 2);
        cryKeyECpoint Q = curve.g.mul(k);
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
        cryKeyECpoint Q = curve.g.mul(u1).add(pub.mul(u2));
        return sgnR.compareTo(Q.x.mod(curve.n)) != 0;
    }

    /**
     * verify ssh signature
     *
     * @param hash hash
     * @param sign signature
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
        p.clear();
        p.putCopy(sign, 0, 0, sign.length);
        p.putSkip(sign.length);
        p.merge2beg();
        sgnR = packSsh.bigIntRead(p);
        sgnS = packSsh.bigIntRead(p);
        return doVerify(hash);
    }

    /**
     * sign for ssh
     *
     * @param hash hash
     * @return signature
     */
    public byte[] sshSigning(cryHashGeneric algo, String algn, byte[] hash) {
        hash = cryHashGeneric.compute(algo, hash);
        doSigning(hash);
        packHolder p = new packHolder(true, true);
        packSsh.bigIntWrite(p, sgnR);
        packSsh.bigIntWrite(p, sgnS);
        p.merge2beg();
        hash = p.getCopy();
        p.clear();
        packSsh.stringWrite(p, algn);
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
     * sign certificate
     *
     * @param hash hash
     * @return signature
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
     * verify tls
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
