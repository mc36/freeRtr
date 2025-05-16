package org.freertr.cry;

import org.freertr.enc.encAsn1;
import java.math.BigInteger;
import org.freertr.pack.packHolder;

/**
 * elliptic curve diffie-hellman key exchange
 *
 * @author matecsaba
 */
public class cryKeyECDH extends cryKeyGeneric {

    /**
     * create instance
     */
    public cryKeyECDH() {
    }

    /**
     * common value
     */
    protected cryKeyECpoint common;

    /**
     * curve
     */
    protected cryKeyECcurve curve;

    /**
     * client private value
     */
    protected BigInteger clntPriv;

    /**
     * client public value
     */
    protected cryKeyECpoint clntPub;

    /**
     * server private value
     */
    protected BigInteger servPriv;

    /**
     * server public value
     */
    protected cryKeyECpoint servPub;

    public String algName() {
        return "ecdh";
    }

    public boolean certReader(packHolder pck) {
        return true;
    }

    public void certWriter(packHolder pck) {
    }

    public boolean privReader(packHolder pck) {
        encAsn1 a = new encAsn1();
        if (a.tagRead(pck)) {
            return true;
        }
        if ((a.cnst) || (a.tag != encAsn1.tagObjectID)) {
            return true;
        }
        curve = cryKeyECcurve.getByOid(a.buf);
        if (curve == null) {
            return true;
        }
        return false;
    }

    public void privWriter(packHolder pck) {
        pck.clear();
        encAsn1.writeObjectId(pck, curve.oid);
    }

    public boolean sshReader(byte[] key) {
        return true;
    }

    public byte[] sshWriter() {
        return null;
    }

    public boolean sshVerify(cryHashGeneric algo, String algn, byte[] hash, byte[] sign) {
        return true;
    }

    public byte[] sshSigning(cryHashGeneric algo, String algn, byte[] hash) {
        return null;
    }

    public boolean certVerify(cryHashGeneric pkcs, byte[] hash, byte[] sign) {
        return true;
    }

    public byte[] certSigning(cryHashGeneric pkcs, byte[] hash) {
        return null;
    }

    public boolean tlsVerify(int ver, cryHashGeneric pkcs, byte[] hash, byte[] sign) {
        return true;
    }

    public byte[] tlsSigning(int ver, cryHashGeneric pkcs, byte[] hash) {
        return null;
    }

    public boolean keyMakeSize(int len) {
        curve = cryKeyECcurve.getBySize(len);
        return false;
    }

    public boolean keyMakeName(String nam) {
        curve = cryKeyECcurve.getByName(nam);
        return false;
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
        return curve.tls;
    }

    public boolean keyVerify() {
        return !curve.g.check();
    }

    public int keySize() {
        return curve.p.bitLength();
    }

    public String keyDump() {
        return "cln=" + clntPub + " srv=" + servPub + " res=" + common;
    }

    public void keyClntInit() {
        clntPriv = cryUtils.randomBigInt(curve.p.bitLength() - 2);
        clntPub = curve.g.mul(clntPriv);
    }

    public void keyServInit() {
        servPriv = cryUtils.randomBigInt(curve.p.bitLength() - 2);
        servPub = curve.g.mul(servPriv);
    }

    public void keyClntCalc() {
        common = servPub.mul(clntPriv);
    }

    public void keyServCalc() {
        common = clntPub.mul(servPriv);
    }

    public byte[] keyCommonTls() {
        int siz = (curve.p.bitLength() + 7) / 8;
        return cryUtils.bigInt2buffer(common.x, siz);
    }

    public byte[] keyCommonSsh() {
        return common.x.toByteArray();
    }

    public byte[] keyCommonIke() {
        return null;
    }

    public byte[] keyClntTls() {
        if (clntPub == null) {
            return null;
        }
        return clntPub.toBytesTls();
    }

    public byte[] keyServTls() {
        if (servPub == null) {
            return null;
        }
        return servPub.toBytesTls();
    }

    public boolean keyClntTls(byte[] buf, int ofs) {
        clntPub = cryKeyECpoint.fromBytesTls(curve, buf, ofs);
        return false;
    }

    public boolean keyServTls(byte[] buf, int ofs) {
        servPub = cryKeyECpoint.fromBytesTls(curve, buf, ofs);
        return false;
    }

}
