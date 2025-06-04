package org.freertr.cry;

import org.freertr.enc.encAsn1;
import java.math.BigInteger;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;

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
    protected BigInteger group;

    /**
     * modulus value
     */
    protected BigInteger modulus;

    /**
     * id number
     */
    protected int idNum;

    /**
     * client private value
     */
    protected BigInteger clntPriv;

    /**
     * client public value
     */
    protected BigInteger clntPub;

    /**
     * server private value
     */
    protected BigInteger servPriv;

    /**
     * server public value
     */
    protected BigInteger servPub;

    public String toString() {
        return "group=" + group + " prime=" + modulus;
    }

    /**
     * max byte size
     *
     * @return size
     */
    public int byteSize() {
        return (modulus.bitLength() + 7) / 8;
    }

    public String algName() {
        return "dh";
    }

    public String sshName() {
        return "diffe-hellman-group" + idNum + "-sha256";
    }

    public cryHashGeneric sshHash() {
        return new cryHashSha2256();
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
        idNum = g.ike;
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
        idNum = g.ike;
        return false;
    }

    public boolean keyMakeSize(int len) {
        cryKeyDHgroup g = cryKeyDHgroup.getBySize(len);
        if (g == null) {
            return true;
        }
        group = g.grp;
        modulus = g.mod;
        idNum = g.ike;
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
        if (modulus == null) {
            return -1;
        }
        return idNum;
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
        return "grp=" + idNum + " cln=" + clntPub + " srv=" + servPub + " res=" + common;
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
        if (common == null) {
            return null;
        }
        return cryUtils.bigUint2buf(common);
    }

    public byte[] keyCommonSsh() {
        if (common == null) {
            return null;
        }
        return common.toByteArray();
    }

    public byte[] keyCommonIke() {
        return keyCommonTls();
    }

    public byte[] keyClntTls() {
        if (clntPub == null) {
            return null;
        }
        return cryUtils.bigUint2buf(clntPub);
    }

    public byte[] keyServTls() {
        if (servPub == null) {
            return null;
        }
        return cryUtils.bigUint2buf(servPub);
    }

    public boolean keyClntTls(byte[] buf, int ofs) {
        clntPub = cryUtils.buffer2bigInt(buf, ofs, buf.length - ofs);
        return false;
    }

    public boolean keyServTls(byte[] buf, int ofs) {
        servPub = cryUtils.buffer2bigInt(buf, ofs, buf.length - ofs);
        return false;
    }

    public byte[] keyClntSsh() {
        if (clntPub == null) {
            return null;
        }
        return clntPub.toByteArray();
    }

    public byte[] keyServSsh() {
        if (servPub == null) {
            return null;
        }
        return servPub.toByteArray();
    }

    public boolean keyClntSsh(byte[] buf, int ofs) {
        byte[] tmp = new byte[buf.length - ofs];
        bits.byteCopy(buf, ofs, tmp, 0, tmp.length);
        clntPub = new BigInteger(tmp);
        return false;
    }

    public boolean keyServSsh(byte[] buf, int ofs) {
        byte[] tmp = new byte[buf.length - ofs];
        bits.byteCopy(buf, ofs, tmp, 0, tmp.length);
        servPub = new BigInteger(tmp);
        return false;
    }

    public byte[] keyClntIke() {
        return keyClntTls();
    }

    public byte[] keyServIke() {
        return keyServTls();
    }

    public boolean keyClntIke(byte[] buf, int ofs) {
        clntPub = cryUtils.buffer2bigInt(buf, ofs, buf.length - ofs);
        return false;
    }

    public boolean keyServIke(byte[] buf, int ofs) {
        servPub = cryUtils.buffer2bigInt(buf, ofs, buf.length - ofs);
        return false;
    }

    public byte[][] keyParamTls() {
        byte[][] res = new byte[2][];
        res[0] = cryUtils.bigUint2buf(modulus);
        res[1] = cryUtils.bigUint2buf(group);
        return res;
    }

    public byte[][] keyParamSsh() {
        byte[][] res = new byte[2][];
        res[0] = modulus.toByteArray();
        res[1] = group.toByteArray();
        return res;
    }

    public boolean keyParamTls(byte[][] buf) {
        modulus = cryUtils.buffer2bigInt(buf[0], 0, buf[0].length);
        group = cryUtils.buffer2bigInt(buf[1], 0, buf[1].length);
        return false;
    }

    public boolean keyParamSsh(byte[][] buf) {
        modulus = new BigInteger(buf[0]);
        group = new BigInteger(buf[0]);
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
