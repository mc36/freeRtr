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
    public cryECpoint common;

    /**
     * curve
     */
    public cryECcurve curve;

    /**
     * client private value
     */
    protected BigInteger clntPriv;

    /**
     * client public value
     */
    public cryECpoint clntPub;

    /**
     * server private value
     */
    protected BigInteger servPriv;

    /**
     * server public value
     */
    public cryECpoint servPub;

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
        curve = cryECcurve.getByOid(a.buf);
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

    public boolean keyMake(int len) {
        curve = cryECcurve.getBySize(len);
        return false;
    }

    public boolean keyMake(String nam) {
        curve = cryECcurve.getByName(nam);
        return false;
    }

    public boolean keyVerify() {
        return !curve.g.check();
    }

    public int keySize() {
        return curve.p.bitLength();
    }

    /**
     * client exchange
     */
    public void clntXchg() {
        clntPriv = randomBigInt(curve.p.bitLength() - 2);
        clntPub = curve.g.mul(clntPriv);
    }

    /**
     * server exchange
     */
    public void servXchg() {
        servPriv = randomBigInt(curve.p.bitLength() - 2);
        servPub = curve.g.mul(servPriv);
    }

    /**
     * client common secret computation
     */
    public void clntKey() {
        common = servPub.mul(clntPriv);
    }

    /**
     * server common secret computation
     */
    public void servKey() {
        common = clntPub.mul(servPriv);
    }

}
