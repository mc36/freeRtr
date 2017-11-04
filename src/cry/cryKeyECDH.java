package cry;

import java.math.BigInteger;
import pack.packHolder;

/**
 * elliptic curve diffie-hellman key exchange
 *
 * @author matecsaba
 */
public class cryKeyECDH extends cryKeyGeneric {

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
        cryAsn1 a = new cryAsn1();
        if (a.tagRead(pck)) {
            return true;
        }
        if ((a.cnst) || (a.tag != cryAsn1.tagObjectID)) {
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
        cryAsn1.writeObjectId(pck, curve.oid);
    }

    public boolean sshReader(byte[] key) {
        return true;
    }

    public byte[] sshWriter() {
        return null;
    }

    public boolean sshVerify(byte[] hash, byte[] sign) {
        return true;
    }

    public byte[] sshSigning(byte[] hash) {
        return null;
    }

    public boolean certVerify(byte[] hash, byte[] sign) {
        return true;
    }

    public byte[] certSigning(byte[] hash) {
        return null;
    }

    public boolean tlsVerify(int ver, byte[] hash, byte[] sign) {
        return true;
    }

    public byte[] tlsSigning(int ver, byte[] hash) {
        return null;
    }

    public void keyMake(int len) {
        curve = cryECcurve.getBySize(len);
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
