package org.freertr.cry;

import org.freertr.pack.packHolder;
import org.freertr.util.bits;

/**
 * post quantum hybrid
 *
 * @author matecsaba
 */
public class cryKeyPQhybrid extends cryKeyGeneric {

    /**
     * create instance
     */
    public cryKeyPQhybrid() {
        ec = new cryKeyCurve25519();
        ml = new cryKeyMLKEM();
        ml.keyMakeSize(768);
    }

    /**
     * tls value
     */
    public final static int tlsVal = 0x11ec;

    private cryKeyCurve25519 ec;

    private cryKeyMLKEM ml;

    /**
     * client public value
     */
    protected byte[] clntPub;

    /**
     * server public value
     */
    protected byte[] servPub;

    public String algName() {
        return "x25519mlkem768";
    }

    public String sshName() {
        return "mlkem768x25519-sha256";
    }

    public cryHashGeneric sshHash() {
        return new cryHashSha2256();
    }

    public boolean privReader(packHolder pck) {
        return true;
    }

    public void privWriter(packHolder pck) {
    }

    public boolean certReader(packHolder pck) {
        return true;
    }

    public void certWriter(packHolder pck) {
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
        return true;
    }

    public boolean keyMakeName(String nam) {
        return false;
    }

    public boolean keyMakeTls(int id) {
        return id != tlsVal;
    }

    public boolean keyMakeIke(int id) {
        return id != 65535;
    }

    public int keyMakeVal() {
        return tlsVal;
    }

    public boolean keyVerify() {
        return true;
    }

    public int keySize() {
        return 256;
    }

    public String keyDump() {
        return ml.keyDump() + " " + ec.keyDump();
    }

    public void keyClntInit() {
        ml.keyClntInit();
        ec.keyClntInit();
    }

    public void keyServInit() {
        ml.keyServInit();
        ec.keyServInit();
    }

    public void keyClntCalc() {
        ml.keyClntCalc();
        ec.keyClntCalc();
    }

    public void keyServCalc() {
        ml.keyServCalc();
        ec.keyServCalc();
    }

    public byte[] keyCommonTls() {
        return mergeParts(ml.keyCommonTls(), ec.keyCommonTls());
    }

    public byte[] keyCommonSsh() {
        byte[] buf = keyCommonTls();
        if (buf == null) {
            return null;
        }
        cryHashSha2256 hsh = new cryHashSha2256();
        hsh.init();
        hsh.update(buf);
        buf = hsh.finish();
        return buf;
    }

    public byte[] keyCommonIke() {
        return keyCommonTls();
    }

    private byte[] mergeParts(byte[] ml, byte[] ec) {
        if (ml == null) {
            return null;
        }
        if (ec == null) {
            return null;
        }
        return bits.byteConcat(ml, ec);
    }

    private byte[][] splitParts(byte[] buf, int ofs) {
        byte[] buf2 = new byte[32];
        byte[] buf1 = new byte[buf.length - ofs - buf2.length];
        bits.byteCopy(buf, ofs, buf1, 0, buf1.length);
        bits.byteCopy(buf, ofs + buf1.length, buf2, 0, buf2.length);
        return new byte[][]{buf1, buf2};
    }

    public byte[] keyClntTls() {
        return mergeParts(ml.keyClntTls(), ec.keyClntTls());
    }

    public byte[] keyServTls() {
        return mergeParts(ml.keyServTls(), ec.keyServTls());
    }

    public boolean keyClntTls(byte[] buf, int ofs) {
        byte[][] prt = splitParts(buf, ofs);
        ml.keyClntTls(prt[0], 0);
        ec.keyClntTls(prt[1], 0);
        return false;
    }

    public boolean keyServTls(byte[] buf, int ofs) {
        byte[][] prt = splitParts(buf, ofs);
        ml.keyServTls(prt[0], 0);
        ec.keyServTls(prt[1], 0);
        return false;
    }

    public byte[] keyClntSsh() {
        return keyClntTls();
    }

    public byte[] keyServSsh() {
        return keyServTls();
    }

    public boolean keyClntSsh(byte[] buf, int ofs) {
        return keyClntTls(buf, ofs);
    }

    public boolean keyServSsh(byte[] buf, int ofs) {
        return keyServTls(buf, ofs);
    }

    public byte[] keyClntIke() {
        return keyClntSsh();
    }

    public byte[] keyServIke() {
        return keyServSsh();
    }

    public boolean keyClntIke(byte[] buf, int ofs) {
        return keyClntSsh(buf, ofs);
    }

    public boolean keyServIke(byte[] buf, int ofs) {
        return keyServSsh(buf, ofs);
    }

    public byte[][] keyParamTls() {
        return null;
    }

    public byte[][] keyParamSsh() {
        return null;
    }

    public boolean keyParamTls(byte[][] buf) {
        return true;
    }

    public boolean keyParamSsh(byte[][] buf) {
        return true;
    }

    public boolean sshReader(byte[] key) {
        return false;
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

}
