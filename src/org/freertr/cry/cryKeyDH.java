package org.freertr.cry;

import org.freertr.enc.encAsn1;
import java.math.BigInteger;
import org.freertr.pack.packHolder;

/**
 * diffie-hellman key exchange
 *
 * @author matecsaba
 */
public class cryKeyDH extends cryKeyGeneric {

    /**
     * create instance
     */
    public cryKeyDH() {
    }

    /**
     * common value
     */
    protected BigInteger common;

    /**
     * generator value
     */
    public BigInteger group;

    /**
     * modulus value
     */
    public BigInteger modulus;

    /**
     * client private value
     */
    protected BigInteger clntPriv;

    /**
     * client public value
     */
    public BigInteger clntPub;

    /**
     * server private value
     */
    protected BigInteger servPriv;

    /**
     * server public value
     */
    public BigInteger servPub;

    public String toString() {
        return "group=" + group + " prime=" + modulus;
    }

    public String algName() {
        return "dh";
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
        if ((!a.cnst) || (a.tag != encAsn1.tagSequence)) {
            return true;
        }
        pck = a.getPack();
        modulus = encAsn1.readBigInt(pck);
        if (modulus == null) {
            return true;
        }
        group = encAsn1.readBigInt(pck);
        if (group == null) {
            return true;
        }
        return false;
    }

    public void privWriter(packHolder pck) {
        packHolder p = new packHolder(true, true);
        encAsn1.writeBigInt(p, modulus);
        encAsn1.writeBigInt(p, group);
        encAsn1.writeSequence(pck, p);
    }

    public boolean keyMakeName(String nam) {
        cryKeyDHgroup g = cryKeyDHgroup.getByName(nam);
        if (g == null) {
            return true;
        }
        group = g.grp;
        modulus = g.mod;
        return false;
    }

    public boolean keyMakeTls(int id) {
        return false;
    }

    public boolean keyMakeIke(int id) {
        cryKeyDHgroup g = cryKeyDHgroup.getByIke(id);
        if (g == null) {
            return true;
        }
        group = g.grp;
        modulus = g.mod;
        return false;
    }

    public boolean keyMakeSize(int len) {
        cryKeyDHgroup g = cryKeyDHgroup.getBySize(len);
        if (g == null) {
            return true;
        }
        group = g.grp;
        modulus = g.mod;
        return false;
    }

    /**
     * create random group
     *
     * @param len size
     */
    public void keyMakeRandom(int len) {
        for (;;) {
            modulus = cryUtils.randomPrime(len);
            BigInteger i = modulus.shiftRight(1);
            if (cryUtils.testPrime(i)) {
                break;
            }
        }
        group = new BigInteger("2", 16);
    }

    public int keyMakeVal() {
        return -1;
    }

    public boolean keyVerify() {
        if (!cryUtils.testPrime(modulus)) {
            return true;
        }
        if (!cryUtils.testPrime(modulus.shiftRight(1))) {
            return true;
        }
        if (!cryUtils.testPrime(group)) {
            return true;
        }
        return false;
    }

    public int keySize() {
        return modulus.bitLength();
    }

    public String keyDump() {
        return "cln=" + clntPub + " srv=" + servPub + " res=" + common;
    }

    public void keyClntInit() {
        clntPriv = cryUtils.randomBigInt(modulus.bitLength() - 2);
        clntPub = group.modPow(clntPriv, modulus);
    }

    public void keyServInit() {
        servPriv = cryUtils.randomBigInt(modulus.bitLength() - 2);
        servPub = group.modPow(servPriv, modulus);
    }

    public void keyClntCalc() {
        common = servPub.modPow(clntPriv, modulus);
    }

    public void keyServCalc() {
        common = clntPub.modPow(servPriv, modulus);
    }

    public byte[] keyCommonTls() {
        return cryUtils.bigUint2buf(common);
    }

    public byte[] keyCommonSsh() {
        return common.toByteArray();
    }

    public byte[] keyCommonIke() {
        return cryUtils.bigUint2buf(common);
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

}
