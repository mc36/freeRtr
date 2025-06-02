package org.freertr.cry;

import org.freertr.pack.packHolder;
import org.freertr.util.bits;

/**
 * module lattice digital signature algorithm
 *
 * @author matecsaba
 */
public class cryKeyMLDSA extends cryKeyGeneric {

    private byte[] rho;

    private byte[] key;

    private byte[] tr;

    private byte[] encT0;

    private byte[] encT1;

    private byte[] encS1;

    private byte[] encS2;

    private cryHashShake256 shakeDigest = new cryHashShake256();

    /**
     * magic values
     */
    protected final static int stream128BlockBytes = 168;

    /**
     * magic values
     */
    protected final static int stream256BlockBytes = 136;

    /**
     * magic values
     */
    protected final static int DilithiumN = 256;

    /**
     * magic values
     */
    protected final static int DilithiumQ = 8380417;

    /**
     * magic values
     */
    protected final static int DilithiumQinv = 58728449;

    /**
     * magic values
     */
    protected final static int DilithiumD = 13;

    /**
     * magic values
     */
    protected final static int SeedBytes = 32;

    /**
     * magic values
     */
    protected final static int CrhBytes = 64;

    /**
     * magic values
     */
    protected final static int RndBytes = 32;

    /**
     * magic values
     */
    protected final static int TrBytes = 64;

    /**
     * magic values
     */
    protected final static int DilithiumPolyT1PackedBytes = 320;

    /**
     * magic values
     */
    protected final static int DilithiumPolyT0PackedBytes = 416;

    /**
     * magic values
     */
    protected int DilithiumPolyVecHPackedBytes;

    /**
     * magic values
     */
    protected int DilithiumPolyZPackedBytes;

    /**
     * magic values
     */
    protected int DilithiumPolyW1PackedBytes;

    /**
     * magic values
     */
    protected int DilithiumPolyEtaPackedBytes;

    /**
     * magic values
     */
    protected int DilithiumK;

    /**
     * magic values
     */
    protected int DilithiumL;

    /**
     * magic values
     */
    protected int DilithiumEta;

    /**
     * magic values
     */
    protected int DilithiumTau;

    /**
     * magic values
     */
    protected int DilithiumBeta;

    /**
     * magic values
     */
    protected int DilithiumGamma1;

    /**
     * magic values
     */
    protected int DilithiumGamma2;

    /**
     * magic values
     */
    protected int DilithiumOmega;

    /**
     * magic values
     */
    protected int DilithiumCTilde;

    /**
     * magic values
     */
    protected int PolyUniformGamma1NBlocks;

    public boolean keyMakeSize(int len) {
        switch (len) {
            case 44:
                DilithiumK = 4;
                DilithiumL = 4;
                DilithiumEta = 2;
                DilithiumTau = 39;
                DilithiumBeta = 78;
                DilithiumGamma1 = (1 << 17);
                DilithiumGamma2 = ((DilithiumQ - 1) / 88);
                DilithiumOmega = 80;
                DilithiumPolyZPackedBytes = 576;
                DilithiumPolyW1PackedBytes = 192;
                DilithiumPolyEtaPackedBytes = 96;
                DilithiumCTilde = 32;
                break;
            case 65:
                DilithiumK = 6;
                DilithiumL = 5;
                DilithiumEta = 4;
                DilithiumTau = 49;
                DilithiumBeta = 196;
                DilithiumGamma1 = (1 << 19);
                DilithiumGamma2 = ((DilithiumQ - 1) / 32);
                DilithiumOmega = 55;
                DilithiumPolyZPackedBytes = 640;
                DilithiumPolyW1PackedBytes = 128;
                DilithiumPolyEtaPackedBytes = 128;
                DilithiumCTilde = 48;
                break;
            case 87:
                DilithiumK = 8;
                DilithiumL = 7;
                DilithiumEta = 2;
                DilithiumTau = 60;
                DilithiumBeta = 120;
                DilithiumGamma1 = (1 << 19);
                DilithiumGamma2 = ((DilithiumQ - 1) / 32);
                DilithiumOmega = 75;
                DilithiumPolyZPackedBytes = 640;
                DilithiumPolyW1PackedBytes = 128;
                DilithiumPolyEtaPackedBytes = 96;
                DilithiumCTilde = 64;
                break;
            default:
                return true;
        }
        DilithiumPolyVecHPackedBytes = DilithiumOmega + DilithiumK;
        if (DilithiumGamma1 == (1 << 17)) {
            PolyUniformGamma1NBlocks = ((576 + stream256BlockBytes - 1) / stream256BlockBytes);
        } else {
            PolyUniformGamma1NBlocks = ((640 + stream256BlockBytes - 1) / stream256BlockBytes);
        }
        return false;
    }

    public void initSign() {
        shakeDigest.init();
        shakeDigest.update(tr, 0, TrBytes);
        absorbCtx(false, new byte[0]);
    }

    public void initVerify() {
        shakeDigest.init();
        byte[] mu = new byte[TrBytes];
        shakeDigest.update(rho, 0, rho.length);
        shakeDigest.update(encT1, 0, encT1.length);
        shakeDigest.fillupBuffer(mu, 0, TrBytes);
        shakeDigest.init();
        shakeDigest.update(mu, 0, TrBytes);
        absorbCtx(false, new byte[0]);
    }

    public void update(byte[] in, int off, int len) {
        shakeDigest.update(in, off, len);
    }

    public void doKeyPair() {
        shakeDigest.init();
        byte[] seedBuf = new byte[SeedBytes];
        for (int i = 0; i < seedBuf.length; i++) {
            seedBuf[i] = (byte) bits.randomB();
        }
        byte[] buf = new byte[2 * SeedBytes + CrhBytes];
        byte[] rhoPrime = new byte[CrhBytes];
        tr = new byte[TrBytes];
        rho = new byte[SeedBytes];
        key = new byte[SeedBytes];
        cryKeyMLDSAmat aMatrix = new cryKeyMLDSAmat(this);
        cryKeyMLDSAvecL s1 = new cryKeyMLDSAvecL(this);
        cryKeyMLDSAvecK s2 = new cryKeyMLDSAvecK(this);
        cryKeyMLDSAvecK t1 = new cryKeyMLDSAvecK(this);
        cryKeyMLDSAvecK t0 = new cryKeyMLDSAvecK(this);
        shakeDigest.update(seedBuf, 0, SeedBytes);
        shakeDigest.update((byte) DilithiumK);
        shakeDigest.update((byte) DilithiumL);
        shakeDigest.fillupBuffer(buf, 0, 2 * SeedBytes + CrhBytes);
        shakeDigest.init();
        bits.byteCopy(buf, 0, rho, 0, SeedBytes);
        bits.byteCopy(buf, SeedBytes, rhoPrime, 0, CrhBytes);
        bits.byteCopy(buf, SeedBytes + CrhBytes, key, 0, SeedBytes);
        aMatrix.expandMatrix(rho);
        s1.uniformEta(rhoPrime, (short) 0);
        s2.uniformEta(rhoPrime, (short) DilithiumL);
        cryKeyMLDSAvecL s1hat = new cryKeyMLDSAvecL(this);
        s1.copyTo(s1hat);
        s1hat.polyVecNtt();
        aMatrix.pointwiseMontgomery(t1, s1hat);
        t1.reduce();
        t1.invNttToMont();
        t1.addPolyVecK(s2);
        t1.conditionalAddQ();
        t1.power2Round(t0);
        encT1 = packPublicKey(t1);
        shakeDigest.update(rho, 0, rho.length);
        shakeDigest.update(encT1, 0, encT1.length);
        shakeDigest.fillupBuffer(tr, 0, TrBytes);
        shakeDigest.init();
        encS1 = new byte[DilithiumL * DilithiumPolyEtaPackedBytes];
        for (int i = 0; i < DilithiumL; i++) {
            s1.vec[i].polyEtaPack(encS1, i * DilithiumPolyEtaPackedBytes);
        }
        encS2 = new byte[DilithiumK * DilithiumPolyEtaPackedBytes];
        for (int i = 0; i < DilithiumK; i++) {
            s2.vec[i].polyEtaPack(encS2, i * DilithiumPolyEtaPackedBytes);
        }
        encT0 = new byte[DilithiumK * DilithiumPolyT0PackedBytes];
        for (int i = 0; i < DilithiumK; i++) {
            t0.vec[i].polyt0Pack(encT0, i * DilithiumPolyT0PackedBytes);
        }
    }

    public byte[] doSignature() {
        byte[] rnd = new byte[RndBytes];
        for (int i = 0; i < rnd.length; i++) {
            rnd[i] = (byte) bits.randomB();
        }
        byte[] mu = new byte[CrhBytes];
        shakeDigest.fillupBuffer(mu, 0, CrhBytes);
        shakeDigest.init();
        byte[] outSig = new byte[DilithiumCTilde + DilithiumL * DilithiumPolyZPackedBytes + DilithiumPolyVecHPackedBytes];
        byte[] rhoPrime = new byte[CrhBytes];
        short nonce = 0;
        cryKeyMLDSAvecL s1 = new cryKeyMLDSAvecL(this);
        cryKeyMLDSAvecL y = new cryKeyMLDSAvecL(this);
        cryKeyMLDSAvecL z = new cryKeyMLDSAvecL(this);
        cryKeyMLDSAvecK t0 = new cryKeyMLDSAvecK(this);
        cryKeyMLDSAvecK s2 = new cryKeyMLDSAvecK(this);
        cryKeyMLDSAvecK w1 = new cryKeyMLDSAvecK(this);
        cryKeyMLDSAvecK w0 = new cryKeyMLDSAvecK(this);
        cryKeyMLDSAvecK h = new cryKeyMLDSAvecK(this);
        cryKeyMLDSApoly cp = new cryKeyMLDSApoly(this);
        cryKeyMLDSAmat aMatrix = new cryKeyMLDSAmat(this);
        unpackSecretKey(t0, s1, s2, encT0, encS1, encS2);
        byte[] keyMu = new byte[SeedBytes + RndBytes + CrhBytes];
        bits.byteCopy(key, 0, keyMu, 0, SeedBytes);
        bits.byteCopy(rnd, 0, keyMu, SeedBytes, RndBytes);
        bits.byteCopy(mu, 0, keyMu, SeedBytes + RndBytes, CrhBytes);
        shakeDigest.update(keyMu, 0, SeedBytes + RndBytes + CrhBytes);
        shakeDigest.fillupBuffer(rhoPrime, 0, CrhBytes);
        shakeDigest.init();
        aMatrix.expandMatrix(rho);
        s1.polyVecNtt();
        s2.polyVecNtt();
        t0.polyVecNtt();
        int count = 0;
        while (count < 1000) {
            count++;
            y.uniformGamma1(rhoPrime, nonce++);
            y.copyTo(z);
            z.polyVecNtt();
            aMatrix.pointwiseMontgomery(w1, z);
            w1.reduce();
            w1.invNttToMont();
            w1.conditionalAddQ();
            w1.decompose(w0);
            w1.packW1(this, outSig, 0);
            shakeDigest.update(mu, 0, CrhBytes);
            shakeDigest.update(outSig, 0, DilithiumK * DilithiumPolyW1PackedBytes);
            shakeDigest.fillupBuffer(outSig, 0, DilithiumCTilde);
            shakeDigest.init();
            cp.challenge(outSig, 0, DilithiumCTilde);
            cp.polyNtt();
            z.pointwisePolyMontgomery(cp, s1);
            z.invNttToMont();
            z.addPolyVecL(y);
            z.reduce();
            if (z.checkNorm(DilithiumGamma1 - DilithiumBeta)) {
                continue;
            }
            h.pointwisePolyMontgomery(cp, s2);
            h.invNttToMont();
            w0.subtract(h);
            w0.reduce();
            if (w0.checkNorm(DilithiumGamma2 - DilithiumBeta)) {
                continue;
            }
            h.pointwisePolyMontgomery(cp, t0);
            h.invNttToMont();
            h.reduce();
            if (h.checkNorm(DilithiumGamma2)) {
                continue;
            }
            w0.addPolyVecK(h);
            w0.conditionalAddQ();
            int n = h.makeHint(w0, w1);
            if (n > DilithiumOmega) {
                continue;
            }
            int end = DilithiumCTilde;
            for (int i = 0; i < DilithiumL; i++) {
                z.vec[i].zPack(outSig, end);
                end += DilithiumPolyZPackedBytes;
            }
            for (int i = 0; i < DilithiumOmega + DilithiumK; i++) {
                outSig[end + i] = 0;
            }
            int pls = 0;
            for (int i = 0; i < DilithiumK; i++) {
                for (int j = 0; j < DilithiumN; j++) {
                    if (h.vec[i].coeffs[j] != 0) {
                        outSig[end + pls++] = (byte) j;
                    }
                }
                outSig[end + DilithiumOmega + i] = (byte) pls;
            }
            return outSig;
        }
        return null;
    }

    public boolean doSignature(byte[] sig) {
        byte[] buf = new byte[Math.max(CrhBytes + DilithiumK * DilithiumPolyW1PackedBytes, DilithiumCTilde)];
        shakeDigest.fillupBuffer(buf, 0, shakeDigest.getBlockSize());
        shakeDigest.init();
        cryKeyMLDSAvecK h = new cryKeyMLDSAvecK(this);
        cryKeyMLDSAvecL z = new cryKeyMLDSAvecL(this);
        int end = DilithiumCTilde;
        for (int i = 0; i < DilithiumL; i++) {
            byte[] tmp = new byte[DilithiumPolyZPackedBytes];
            bits.byteCopy(sig, end + i * DilithiumPolyZPackedBytes, tmp, 0, tmp.length);
            z.vec[i].zUnpack(tmp);
        }
        end += DilithiumL * DilithiumPolyZPackedBytes;
        int pls = 0;
        for (int i = 0; i < DilithiumK; i++) {
            for (int j = 0; j < DilithiumN; j++) {
                h.vec[i].coeffs[j] = 0;
            }
            if ((sig[end + DilithiumOmega + i] & 0xFF) < pls || (sig[end + DilithiumOmega + i] & 0xFF) > DilithiumOmega) {
                return false;
            }
            for (int j = pls; j < (sig[end + DilithiumOmega + i] & 0xFF); j++) {
                if (j > pls && (sig[end + j] & 0xFF) <= (sig[end + j - 1] & 0xFF)) {
                    return false;
                }
                h.vec[i].coeffs[(sig[end + j] & 0xFF)] = 1;
            }
            pls = (int) (sig[end + DilithiumOmega + i]);
        }
        for (int j = pls; j < DilithiumOmega; j++) {
            if ((sig[end + j] & 0xFF) != 0) {
                return false;
            }
        }
        if (z.checkNorm(DilithiumGamma1 - DilithiumBeta)) {
            return false;
        }
        cryKeyMLDSApoly cp = new cryKeyMLDSApoly(this);
        cryKeyMLDSAmat aMatrix = new cryKeyMLDSAmat(this);
        cryKeyMLDSAvecK t1 = new cryKeyMLDSAvecK(this);
        cryKeyMLDSAvecK w1 = new cryKeyMLDSAvecK(this);
        for (int i = 0; i < DilithiumK; i++) {
            byte[] tmp = new byte[DilithiumPolyT1PackedBytes];
            bits.byteCopy(encT1, i * DilithiumPolyT1PackedBytes, tmp, 0, tmp.length);
            t1.vec[i].polyt1Unpack(tmp);
        }
        cp.challenge(sig, 0, DilithiumCTilde);
        aMatrix.expandMatrix(rho);
        z.polyVecNtt();
        aMatrix.pointwiseMontgomery(w1, z);
        cp.polyNtt();
        t1.shiftLeft();
        t1.polyVecNtt();
        t1.pointwisePolyMontgomery(cp, t1);
        w1.subtract(t1);
        w1.reduce();
        w1.invNttToMont();
        w1.conditionalAddQ();
        w1.useHint(w1, h);
        w1.packW1(this, buf, CrhBytes);
        shakeDigest.init();
        shakeDigest.update(buf, 0, CrhBytes + DilithiumK * DilithiumPolyW1PackedBytes);
        shakeDigest.fillupBuffer(buf, 0, DilithiumCTilde);
        shakeDigest.init();
        return bits.byteComp(sig, 0, buf, 0, DilithiumCTilde) == 0;
    }

    private void absorbCtx(boolean isPreHash, byte[] ctx) {
        if (ctx == null) {
            return;
        }
        shakeDigest.update(isPreHash ? (byte) 1 : (byte) 0);
        shakeDigest.update((byte) ctx.length);
        shakeDigest.update(ctx, 0, ctx.length);
    }

    private void unpackSecretKey(cryKeyMLDSAvecK t0, cryKeyMLDSAvecL s1, cryKeyMLDSAvecK s2, byte[] t0Enc, byte[] s1Enc, byte[] s2Enc) {
        for (int i = 0; i < DilithiumL; i++) {
            s1.vec[i].polyEtaUnpack(s1Enc, i * DilithiumPolyEtaPackedBytes);
        }
        for (int i = 0; i < DilithiumK; i++) {
            s2.vec[i].polyEtaUnpack(s2Enc, i * DilithiumPolyEtaPackedBytes);
        }
        for (int i = 0; i < DilithiumK; i++) {
            t0.vec[i].polyt0Unpack(t0Enc, i * DilithiumPolyT0PackedBytes);
        }
    }

    private byte[] packPublicKey(cryKeyMLDSAvecK t1) {
        byte[] out = new byte[DilithiumK * DilithiumPolyT1PackedBytes];
        for (int i = 0; i < DilithiumK; i++) {
            bits.byteCopy(t1.vec[i].polyt1Pack(), 0, out, i * DilithiumPolyT1PackedBytes, DilithiumPolyT1PackedBytes);
        }
        return out;
    }

    public byte[] encodePrivateKey() {
        byte[] r = bits.byteConcat(rho, key);
        r = bits.byteConcat(r, tr);
        r = bits.byteConcat(r, encS1);
        r = bits.byteConcat(r, encS2);
        return bits.byteConcat(r, encT0);
    }

    public byte[] encodePublicKey() {
        return bits.byteConcat(rho, encT1);
    }

    public void decodePrivateKey(byte[] encoding) {
        int index = 0;
        rho = new byte[SeedBytes];
        bits.byteCopy(encoding, index, rho, 0, rho.length);
        index += SeedBytes;
        key = new byte[SeedBytes];
        bits.byteCopy(encoding, index, key, 0, key.length);
        index += SeedBytes;
        tr = new byte[TrBytes];
        bits.byteCopy(encoding, index, tr, 0, tr.length);
        index += TrBytes;
        int delta = DilithiumL * DilithiumPolyEtaPackedBytes;
        encS1 = new byte[delta];
        bits.byteCopy(encoding, index, encS1, 0, encS1.length);
        index += delta;
        delta = DilithiumK * DilithiumPolyEtaPackedBytes;
        encS2 = new byte[delta];
        bits.byteCopy(encoding, index, encS2, 0, encS2.length);
        index += delta;
        delta = DilithiumK * DilithiumPolyT0PackedBytes;
        encT0 = new byte[delta];
        bits.byteCopy(encoding, index, encT0, 0, encT0.length);
        index += delta;
        cryKeyMLDSAmat aMatrix = new cryKeyMLDSAmat(this);
        cryKeyMLDSAvecL s1 = new cryKeyMLDSAvecL(this);
        cryKeyMLDSAvecK s2 = new cryKeyMLDSAvecK(this);
        cryKeyMLDSAvecK t1 = new cryKeyMLDSAvecK(this);
        cryKeyMLDSAvecK t0 = new cryKeyMLDSAvecK(this);
        unpackSecretKey(t0, s1, s2, encT0, encS1, encS2);
        aMatrix.expandMatrix(rho);
        cryKeyMLDSAvecL s1hat = new cryKeyMLDSAvecL(this);
        s1.copyTo(s1hat);
        s1hat.polyVecNtt();
        aMatrix.pointwiseMontgomery(t1, s1hat);
        t1.reduce();
        t1.invNttToMont();
        t1.addPolyVecK(s2);
        t1.conditionalAddQ();
        t1.power2Round(t0);
        encT1 = packPublicKey(t1);
    }

    public void decodePublicKey(byte[] encoding) {
        key = null;
        tr = null;
        encS1 = null;
        encS2 = null;
        encT0 = null;
        rho = new byte[SeedBytes];
        bits.byteCopy(encoding, 0, rho, 0, rho.length);
        encT1 = new byte[encoding.length - rho.length];
        bits.byteCopy(encoding, rho.length, encT1, 0, encT1.length);
    }

    public String algName() {
        return "mldsa" + DilithiumK + "" + DilithiumL;
    }

    public boolean privReader(packHolder pck) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public void privWriter(packHolder pck) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public boolean certReader(packHolder pck) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public void certWriter(packHolder pck) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public boolean certVerify(cryHashGeneric pkcs, byte[] hash, byte[] sign) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public byte[] certSigning(cryHashGeneric pkcs, byte[] hash) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public boolean tlsVerify(int ver, cryHashGeneric pkcs, byte[] hash, byte[] sign) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public byte[] tlsSigning(int ver, cryHashGeneric pkcs, byte[] hash) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public boolean keyMakeName(String nam) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public boolean keyMakeTls(int id) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public boolean keyMakeIke(int id) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public int keyMakeVal() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public boolean keyVerify() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public int keySize() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public String keyDump() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public void keyClntInit() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public void keyServInit() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public void keyClntCalc() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public void keyServCalc() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public byte[] keyCommonTls() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public byte[] keyCommonSsh() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public byte[] keyCommonIke() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public byte[] keyClntTls() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public byte[] keyServTls() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public boolean keyClntTls(byte[] buf, int ofs) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public boolean keyServTls(byte[] buf, int ofs) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public byte[] keyClntSsh() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public byte[] keyServSsh() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public boolean keyClntSsh(byte[] buf, int ofs) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public boolean keyServSsh(byte[] buf, int ofs) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public byte[] keyClntIke() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public byte[] keyServIke() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public boolean keyClntIke(byte[] buf, int ofs) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public boolean keyServIke(byte[] buf, int ofs) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public byte[][] keyParamTls() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public byte[][] keyParamSsh() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public boolean keyParamTls(byte[][] buf) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public boolean keyParamSsh(byte[][] buf) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public boolean sshReader(byte[] key) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public byte[] sshWriter() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public boolean sshVerify(cryHashGeneric algo, String algn, byte[] hash, byte[] sign) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public byte[] sshSigning(cryHashGeneric algo, String algn, byte[] hash) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

}

class cryKeyMLDSApoly {

    public final static int[] nttZetas = {
        0, 25847, -2608894, -518909, 237124, -777960, -876248, 466468,
        1826347, 2353451, -359251, -2091905, 3119733, -2884855, 3111497, 2680103,
        2725464, 1024112, -1079900, 3585928, -549488, -1119584, 2619752, -2108549,
        -2118186, -3859737, -1399561, -3277672, 1757237, -19422, 4010497, 280005,
        2706023, 95776, 3077325, 3530437, -1661693, -3592148, -2537516, 3915439,
        -3861115, -3043716, 3574422, -2867647, 3539968, -300467, 2348700, -539299,
        -1699267, -1643818, 3505694, -3821735, 3507263, -2140649, -1600420, 3699596,
        811944, 531354, 954230, 3881043, 3900724, -2556880, 2071892, -2797779,
        -3930395, -1528703, -3677745, -3041255, -1452451, 3475950, 2176455, -1585221,
        -1257611, 1939314, -4083598, -1000202, -3190144, -3157330, -3632928, 126922,
        3412210, -983419, 2147896, 2715295, -2967645, -3693493, -411027, -2477047,
        -671102, -1228525, -22981, -1308169, -381987, 1349076, 1852771, -1430430,
        -3343383, 264944, 508951, 3097992, 44288, -1100098, 904516, 3958618,
        -3724342, -8578, 1653064, -3249728, 2389356, -210977, 759969, -1316856,
        189548, -3553272, 3159746, -1851402, -2409325, -177440, 1315589, 1341330,
        1285669, -1584928, -812732, -1439742, -3019102, -3881060, -3628969, 3839961,
        2091667, 3407706, 2316500, 3817976, -3342478, 2244091, -2446433, -3562462,
        266997, 2434439, -1235728, 3513181, -3520352, -3759364, -1197226, -3193378,
        900702, 1859098, 909542, 819034, 495491, -1613174, -43260, -522500,
        -655327, -3122442, 2031748, 3207046, -3556995, -525098, -768622, -3595838,
        342297, 286988, -2437823, 4108315, 3437287, -3342277, 1735879, 203044,
        2842341, 2691481, -2590150, 1265009, 4055324, 1247620, 2486353, 1595974,
        -3767016, 1250494, 2635921, -3548272, -2994039, 1869119, 1903435, -1050970,
        -1333058, 1237275, -3318210, -1430225, -451100, 1312455, 3306115, -1962642,
        -1279661, 1917081, -2546312, -1374803, 1500165, 777191, 2235880, 3406031,
        -542412, -2831860, -1671176, -1846953, -2584293, -3724270, 594136, -3776993,
        -2013608, 2432395, 2454455, -164721, 1957272, 3369112, 185531, -1207385,
        -3183426, 162844, 1616392, 3014001, 810149, 1652634, -3694233, -1799107,
        -3038916, 3523897, 3866901, 269760, 2213111, -975884, 1717735, 472078,
        -426683, 1723600, -1803090, 1910376, -1667432, -1104333, -260646, -3833893,
        -2939036, -2235985, -420899, -2286327, 183443, -976891, 1612842, -3545687,
        -554416, 3919660, -48306, -1362209, 3937738, 1400424, -846154, 1976782
    };

    public int[] coeffs;

    private final cryKeyMLDSA engine;

    public cryKeyMLDSApoly(cryKeyMLDSA ng) {
        coeffs = new int[cryKeyMLDSA.DilithiumN];
        engine = ng;
    }

    void copyTo(cryKeyMLDSApoly z) {
        System.arraycopy(coeffs, 0, z.coeffs, 0, cryKeyMLDSA.DilithiumN);
    }

    public void uniformBlocks(byte[] seed, short nonce) {
        int buflen = ((768 + cryKeyMLDSA.stream128BlockBytes - 1) / cryKeyMLDSA.stream128BlockBytes) * cryKeyMLDSA.stream128BlockBytes;
        byte[] buf = new byte[buflen + 2];
        cryHashShake128 digest = new cryHashShake128();
        digest.init();
        digest.update(seed, 0, seed.length);
        digest.update(nonce);
        digest.update(nonce >> 8);
        digest.fillupBuffer(buf, 0, buflen);
        int ctr = rejectUniform(this, 0, cryKeyMLDSA.DilithiumN, buf, buflen);
        while (ctr < cryKeyMLDSA.DilithiumN) {
            int off = buflen % 3;
            for (int i = 0; i < off; i++) {
                buf[i] = buf[buflen - off + i];
            }
            digest.fillupBuffer(buf, off, cryKeyMLDSA.stream128BlockBytes);
            buflen = cryKeyMLDSA.stream128BlockBytes + off;
            ctr += rejectUniform(this, ctr, cryKeyMLDSA.DilithiumN - ctr, buf, buflen);
        }

    }

    private static int rejectUniform(cryKeyMLDSApoly outputPoly, int coeffOff, int len, byte[] inpBuf, int buflen) {
        int ctr = 0;
        int pos = 0;
        while (ctr < len && pos + 3 <= buflen) {
            int t = (inpBuf[pos++] & 0xFF);
            t |= (inpBuf[pos++] & 0xFF) << 8;
            t |= (inpBuf[pos++] & 0xFF) << 16;
            t &= 0x7FFFFF;
            if (t < cryKeyMLDSA.DilithiumQ) {
                outputPoly.coeffs[coeffOff + ctr] = t;
                ctr++;
            }
        }
        return ctr;
    }

    public void uniformEta(byte[] seed, short nonce) {
        int polyUniformEtaNBlocks;
        if (engine.DilithiumEta == 2) {
            polyUniformEtaNBlocks = ((136 + cryKeyMLDSA.stream256BlockBytes - 1) / cryKeyMLDSA.stream256BlockBytes);
        } else {
            polyUniformEtaNBlocks = ((227 + cryKeyMLDSA.stream256BlockBytes - 1) / cryKeyMLDSA.stream256BlockBytes);
        }
        int buflen = polyUniformEtaNBlocks * cryKeyMLDSA.stream256BlockBytes;
        byte[] buf = new byte[buflen];
        cryHashShake256 digest = new cryHashShake256();
        digest.init();
        digest.update(seed, 0, seed.length);
        digest.update(nonce);
        digest.update(nonce >> 8);
        digest.fillupBuffer(buf, 0, buflen);
        int ctr = rejectEta(this, 0, cryKeyMLDSA.DilithiumN, buf, buflen, engine.DilithiumEta);
        while (ctr < cryKeyMLDSA.DilithiumN) {
            digest.fillupBuffer(buf, 0, cryKeyMLDSA.stream256BlockBytes);
            ctr += rejectEta(this, ctr, cryKeyMLDSA.DilithiumN - ctr, buf, cryKeyMLDSA.stream256BlockBytes, engine.DilithiumEta);
        }

    }

    private static int rejectEta(cryKeyMLDSApoly outputPoly, int coeffOff, int len, byte[] buf, int buflen, int eta) {
        int ctr = 0;
        int pos = 0;
        while (ctr < len && pos < buflen) {
            int t0 = (buf[pos] & 0xFF) & 0x0F;
            int t1 = (buf[pos++] & 0xFF) >> 4;
            if (eta == 2) {
                if (t0 < 15) {
                    t0 = t0 - (205 * t0 >> 10) * 5;
                    outputPoly.coeffs[coeffOff + ctr] = 2 - t0;
                    ctr++;
                }
                if (t1 < 15 && ctr < len) {
                    t1 = t1 - (205 * t1 >> 10) * 5;
                    outputPoly.coeffs[coeffOff + ctr] = 2 - t1;
                    ctr++;
                }
            } else {
                if (t0 < 9) {
                    outputPoly.coeffs[coeffOff + ctr] = 4 - t0;
                    ctr++;
                }
                if (t1 < 9 && ctr < len) {
                    outputPoly.coeffs[coeffOff + ctr] = 4 - t1;
                    ctr++;
                }
            }
        }
        return ctr;
    }

    public void polyNtt() {
        int[] r = new int[coeffs.length];
        System.arraycopy(coeffs, 0, r, 0, coeffs.length);
        int len, start, j, k;
        int zeta, t;
        k = 0;
        for (len = 128; len > 0; len >>>= 1) {
            for (start = 0; start < cryKeyMLDSA.DilithiumN; start = j + len) {
                zeta = nttZetas[++k];
                for (j = start; j < start + len; j++) {
                    t = montgomeryReduce(((long) zeta * (long) r[j + len]));
                    r[j + len] = r[j] - t;
                    r[j] = r[j] + t;
                }
            }
        }
        coeffs = r;
    }

    public void pointwiseMontgomery(cryKeyMLDSApoly v, cryKeyMLDSApoly w) {
        int i;
        for (i = 0; i < cryKeyMLDSA.DilithiumN; i++) {
            coeffs[i] = montgomeryReduce((long) ((long) v.coeffs[i] * (long) w.coeffs[i]));
        }
    }

    public void pointwiseAccountMontgomery(cryKeyMLDSAvecL u, cryKeyMLDSAvecL v) {
        cryKeyMLDSApoly t = new cryKeyMLDSApoly(engine);
        pointwiseMontgomery(u.vec[0], v.vec[0]);
        for (int i = 1; i < engine.DilithiumL; i++) {
            t.pointwiseMontgomery(u.vec[i], v.vec[i]);
            addPoly(t);
        }

    }

    public void addPoly(cryKeyMLDSApoly a) {
        int i;
        for (i = 0; i < cryKeyMLDSA.DilithiumN; i++) {
            coeffs[i] = coeffs[i] + a.coeffs[i];
        }
    }

    public void reduce() {
        for (int i = 0; i < cryKeyMLDSA.DilithiumN; i++) {
            coeffs[i] = reduce32(coeffs[i]);
        }
    }

    public void invNttToMont() {
        int[] out = new int[coeffs.length];
        System.arraycopy(coeffs, 0, out, 0, coeffs.length);
        int k = 256;
        int j = 0;
        for (int len = 1; len < cryKeyMLDSA.DilithiumN; len <<= 1) {
            for (int start = 0; start < cryKeyMLDSA.DilithiumN; start = j + len) {
                int zeta = (-1) * nttZetas[--k];
                for (j = start; j < start + len; j++) {
                    int t = out[j];
                    out[j] = t + out[j + len];
                    out[j + len] = t - out[j + len];
                    out[j + len] = montgomeryReduce((long) ((long) zeta * (long) out[j + len]));
                }
            }
        }
        for (j = 0; j < cryKeyMLDSA.DilithiumN; j++) {
            out[j] = montgomeryReduce((long) ((long) 41978 * (long) out[j]));
        }
        coeffs = out;
    }

    public void conditionalAddQ() {
        for (int i = 0; i < cryKeyMLDSA.DilithiumN; i++) {
            coeffs[i] = conditionalAddQ(coeffs[i]);
        }
    }

    public void power2Round(cryKeyMLDSApoly a) {
        for (int i = 0; i < cryKeyMLDSA.DilithiumN; i++) {
            int[] p2r = power2Round(coeffs[i]);
            coeffs[i] = p2r[0];
            a.coeffs[i] = p2r[1];
        }
    }

    public byte[] polyt1Pack() {
        byte[] out = new byte[cryKeyMLDSA.DilithiumPolyT1PackedBytes];
        for (int i = 0; i < cryKeyMLDSA.DilithiumN / 4; i++) {
            out[5 * i + 0] = (byte) (coeffs[4 * i + 0] >> 0);
            out[5 * i + 1] = (byte) ((coeffs[4 * i + 0] >> 8) | (coeffs[4 * i + 1] << 2));
            out[5 * i + 2] = (byte) ((coeffs[4 * i + 1] >> 6) | (coeffs[4 * i + 2] << 4));
            out[5 * i + 3] = (byte) ((coeffs[4 * i + 2] >> 4) | (coeffs[4 * i + 3] << 6));
            out[5 * i + 4] = (byte) (coeffs[4 * i + 3] >> 2);
        }
        return out;
    }

    public void polyt1Unpack(byte[] a) {
        for (int i = 0; i < cryKeyMLDSA.DilithiumN / 4; i++) {
            coeffs[4 * i + 0] = (((a[5 * i + 0] & 0xFF) >> 0) | ((int) (a[5 * i + 1] & 0xFF) << 8)) & 0x3FF;
            coeffs[4 * i + 1] = (((a[5 * i + 1] & 0xFF) >> 2) | ((int) (a[5 * i + 2] & 0xFF) << 6)) & 0x3FF;
            coeffs[4 * i + 2] = (((a[5 * i + 2] & 0xFF) >> 4) | ((int) (a[5 * i + 3] & 0xFF) << 4)) & 0x3FF;
            coeffs[4 * i + 3] = (((a[5 * i + 3] & 0xFF) >> 6) | ((int) (a[5 * i + 4] & 0xFF) << 2)) & 0x3FF;
        }
    }

    public byte[] polyEtaPack(byte[] out, int outOff) {
        byte[] t = new byte[8];
        if (engine.DilithiumEta == 2) {
            for (int i = 0; i < cryKeyMLDSA.DilithiumN / 8; i++) {
                t[0] = (byte) (engine.DilithiumEta - coeffs[8 * i + 0]);
                t[1] = (byte) (engine.DilithiumEta - coeffs[8 * i + 1]);
                t[2] = (byte) (engine.DilithiumEta - coeffs[8 * i + 2]);
                t[3] = (byte) (engine.DilithiumEta - coeffs[8 * i + 3]);
                t[4] = (byte) (engine.DilithiumEta - coeffs[8 * i + 4]);
                t[5] = (byte) (engine.DilithiumEta - coeffs[8 * i + 5]);
                t[6] = (byte) (engine.DilithiumEta - coeffs[8 * i + 6]);
                t[7] = (byte) (engine.DilithiumEta - coeffs[8 * i + 7]);
                out[outOff + 3 * i + 0] = (byte) ((t[0] >> 0) | (t[1] << 3) | (t[2] << 6));
                out[outOff + 3 * i + 1] = (byte) ((t[2] >> 2) | (t[3] << 1) | (t[4] << 4) | (t[5] << 7));
                out[outOff + 3 * i + 2] = (byte) ((t[5] >> 1) | (t[6] << 2) | (t[7] << 5));
            }
        } else {
            for (int i = 0; i < cryKeyMLDSA.DilithiumN / 2; i++) {
                t[0] = (byte) (engine.DilithiumEta - coeffs[2 * i + 0]);
                t[1] = (byte) (engine.DilithiumEta - coeffs[2 * i + 1]);
                out[outOff + i] = (byte) (t[0] | t[1] << 4);
            }
        }
        return out;
    }

    public void polyEtaUnpack(byte[] a, int aOff) {
        if (engine.DilithiumEta == 2) {
            for (int i = 0; i < cryKeyMLDSA.DilithiumN / 8; i++) {
                int base = aOff + 3 * i;
                coeffs[8 * i + 0] = (((a[base + 0] & 0xFF) >> 0)) & 7;
                coeffs[8 * i + 1] = (((a[base + 0] & 0xFF) >> 3)) & 7;
                coeffs[8 * i + 2] = ((a[base + 0] & 0xFF) >> 6) | ((a[base + 1] & 0xFF) << 2) & 7;
                coeffs[8 * i + 3] = (((a[base + 1] & 0xFF) >> 1)) & 7;
                coeffs[8 * i + 4] = (((a[base + 1] & 0xFF) >> 4)) & 7;
                coeffs[8 * i + 5] = ((a[base + 1] & 0xFF) >> 7) | ((a[base + 2] & 0xFF) << 1) & 7;
                coeffs[8 * i + 6] = (((a[base + 2] & 0xFF) >> 2)) & 7;
                coeffs[8 * i + 7] = (((a[base + 2] & 0xFF) >> 5)) & 7;
                coeffs[8 * i + 0] = engine.DilithiumEta - coeffs[8 * i + 0];
                coeffs[8 * i + 1] = engine.DilithiumEta - coeffs[8 * i + 1];
                coeffs[8 * i + 2] = engine.DilithiumEta - coeffs[8 * i + 2];
                coeffs[8 * i + 3] = engine.DilithiumEta - coeffs[8 * i + 3];
                coeffs[8 * i + 4] = engine.DilithiumEta - coeffs[8 * i + 4];
                coeffs[8 * i + 5] = engine.DilithiumEta - coeffs[8 * i + 5];
                coeffs[8 * i + 6] = engine.DilithiumEta - coeffs[8 * i + 6];
                coeffs[8 * i + 7] = engine.DilithiumEta - coeffs[8 * i + 7];
            }
        } else {
            for (int i = 0; i < cryKeyMLDSA.DilithiumN / 2; i++) {
                coeffs[2 * i + 0] = a[aOff + i] & 0x0F;
                coeffs[2 * i + 1] = (a[aOff + i] & 0xFF) >> 4;
                coeffs[2 * i + 0] = engine.DilithiumEta - coeffs[2 * i + 0];
                coeffs[2 * i + 1] = engine.DilithiumEta - coeffs[2 * i + 1];
            }
        }
    }

    public byte[] polyt0Pack(byte[] out, int outOff) {
        int[] t = new int[8];
        for (int i = 0; i < cryKeyMLDSA.DilithiumN / 8; i++) {
            t[0] = (1 << (cryKeyMLDSA.DilithiumD - 1)) - coeffs[8 * i + 0];
            t[1] = (1 << (cryKeyMLDSA.DilithiumD - 1)) - coeffs[8 * i + 1];
            t[2] = (1 << (cryKeyMLDSA.DilithiumD - 1)) - coeffs[8 * i + 2];
            t[3] = (1 << (cryKeyMLDSA.DilithiumD - 1)) - coeffs[8 * i + 3];
            t[4] = (1 << (cryKeyMLDSA.DilithiumD - 1)) - coeffs[8 * i + 4];
            t[5] = (1 << (cryKeyMLDSA.DilithiumD - 1)) - coeffs[8 * i + 5];
            t[6] = (1 << (cryKeyMLDSA.DilithiumD - 1)) - coeffs[8 * i + 6];
            t[7] = (1 << (cryKeyMLDSA.DilithiumD - 1)) - coeffs[8 * i + 7];
            int base = outOff + 13 * i;
            out[base + 0] = (byte) (t[0]);
            out[base + 1] = (byte) (t[0] >> 8);
            out[base + 1] = (byte) (out[base + 1] | (byte) (t[1] << 5));
            out[base + 2] = (byte) (t[1] >> 3);
            out[base + 3] = (byte) (t[1] >> 11);
            out[base + 3] = (byte) (out[base + 3] | (byte) (t[2] << 2));
            out[base + 4] = (byte) (t[2] >> 6);
            out[base + 4] = (byte) (out[base + 4] | (byte) (t[3] << 7));
            out[base + 5] = (byte) (t[3] >> 1);
            out[base + 6] = (byte) (t[3] >> 9);
            out[base + 6] = (byte) (out[base + 6] | (byte) (t[4] << 4));
            out[base + 7] = (byte) (t[4] >> 4);
            out[base + 8] = (byte) (t[4] >> 12);
            out[base + 8] = (byte) (out[base + 8] | (byte) (t[5] << 1));
            out[base + 9] = (byte) (t[5] >> 7);
            out[base + 9] = (byte) (out[base + 9] | (byte) (t[6] << 6));
            out[base + 10] = (byte) (t[6] >> 2);
            out[base + 11] = (byte) (t[6] >> 10);
            out[base + 11] = (byte) (out[base + 11] | (byte) (t[7] << 3));
            out[base + 12] = (byte) (t[7] >> 5);
        }
        return out;
    }

    public void polyt0Unpack(byte[] a, int aOff) {
        for (int i = 0; i < cryKeyMLDSA.DilithiumN / 8; i++) {
            int base = aOff + 13 * i;
            coeffs[8 * i + 0]
                    = ((a[base + 0] & 0xFF)
                    | ((a[base + 1] & 0xFF) << 8)) & 0x1FFF;
            coeffs[8 * i + 1]
                    = ((((a[base + 1] & 0xFF) >> 5)
                    | ((a[base + 2] & 0xFF) << 3))
                    | ((a[base + 3] & 0xFF) << 11)) & 0x1FFF;
            coeffs[8 * i + 2]
                    = ((((a[base + 3] & 0xFF) >> 2)
                    | ((a[base + 4] & 0xFF) << 6))) & 0x1FFF;
            coeffs[8 * i + 3]
                    = ((((a[base + 4] & 0xFF) >> 7)
                    | ((a[base + 5] & 0xFF) << 1))
                    | ((a[base + 6] & 0xFF) << 9)) & 0x1FFF;
            coeffs[8 * i + 4]
                    = ((((a[base + 6] & 0xFF) >> 4)
                    | ((a[base + 7] & 0xFF) << 4))
                    | ((a[base + 8] & 0xFF) << 12)) & 0x1FFF;
            coeffs[8 * i + 5]
                    = ((((a[base + 8] & 0xFF) >> 1)
                    | ((a[base + 9] & 0xFF) << 7))) & 0x1FFF;
            coeffs[8 * i + 6]
                    = ((((a[base + 9] & 0xFF) >> 6)
                    | ((a[base + 10] & 0xFF) << 2))
                    | ((a[base + 11] & 0xFF) << 10)) & 0x1FFF;
            coeffs[8 * i + 7]
                    = (((a[base + 11] & 0xFF) >> 3
                    | ((a[base + 12] & 0xFF) << 5))) & 0x1FFF;
            coeffs[8 * i + 0] = ((1 << (cryKeyMLDSA.DilithiumD - 1)) - coeffs[8 * i + 0]);
            coeffs[8 * i + 1] = ((1 << (cryKeyMLDSA.DilithiumD - 1)) - coeffs[8 * i + 1]);
            coeffs[8 * i + 2] = ((1 << (cryKeyMLDSA.DilithiumD - 1)) - coeffs[8 * i + 2]);
            coeffs[8 * i + 3] = ((1 << (cryKeyMLDSA.DilithiumD - 1)) - coeffs[8 * i + 3]);
            coeffs[8 * i + 4] = ((1 << (cryKeyMLDSA.DilithiumD - 1)) - coeffs[8 * i + 4]);
            coeffs[8 * i + 5] = ((1 << (cryKeyMLDSA.DilithiumD - 1)) - coeffs[8 * i + 5]);
            coeffs[8 * i + 6] = ((1 << (cryKeyMLDSA.DilithiumD - 1)) - coeffs[8 * i + 6]);
            coeffs[8 * i + 7] = ((1 << (cryKeyMLDSA.DilithiumD - 1)) - coeffs[8 * i + 7]);
        }
    }

    public void uniformGamma1(byte[] seed, short nonce) {
        byte[] buf = new byte[engine.PolyUniformGamma1NBlocks * cryKeyMLDSA.stream256BlockBytes];
        cryHashShake256 digest = new cryHashShake256();
        digest.init();
        digest.update(seed, 0, seed.length);
        digest.update(nonce);
        digest.update(nonce >> 8);
        digest.fillupBuffer(buf, 0, engine.PolyUniformGamma1NBlocks * cryKeyMLDSA.stream256BlockBytes);
        unpackZ(buf);
    }

    private void unpackZ(byte[] a) {
        int gamma1 = engine.DilithiumGamma1;
        if (gamma1 == (1 << 17)) {
            for (int i = 0; i < cryKeyMLDSA.DilithiumN / 4; i++) {
                coeffs[4 * i + 0]
                        = ((((a[9 * i + 0] & 0xFF))
                        | ((a[9 * i + 1] & 0xFF) << 8))
                        | ((a[9 * i + 2] & 0xFF) << 16)) & 0x3FFFF;
                coeffs[4 * i + 1]
                        = ((((a[9 * i + 2] & 0xFF) >> 2)
                        | ((a[9 * i + 3] & 0xFF) << 6))
                        | ((a[9 * i + 4] & 0xFF) << 14)) & 0x3FFFF;
                coeffs[4 * i + 2]
                        = ((((a[9 * i + 4] & 0xFF) >> 4)
                        | ((a[9 * i + 5] & 0xFF) << 4))
                        | ((a[9 * i + 6] & 0xFF) << 12)) & 0x3FFFF;
                coeffs[4 * i + 3]
                        = ((((a[9 * i + 6] & 0xFF) >> 6)
                        | ((a[9 * i + 7] & 0xFF) << 2))
                        | ((a[9 * i + 8] & 0xFF) << 10)) & 0x3FFFF;
                coeffs[4 * i + 0] = gamma1 - coeffs[4 * i + 0];
                coeffs[4 * i + 1] = gamma1 - coeffs[4 * i + 1];
                coeffs[4 * i + 2] = gamma1 - coeffs[4 * i + 2];
                coeffs[4 * i + 3] = gamma1 - coeffs[4 * i + 3];
            }
        } else {
            for (int i = 0; i < cryKeyMLDSA.DilithiumN / 2; i++) {
                coeffs[2 * i + 0]
                        = ((((a[5 * i + 0] & 0xFF))
                        | ((a[5 * i + 1] & 0xFF) << 8))
                        | ((a[5 * i + 2] & 0xFF) << 16)) & 0xFFFFF;
                coeffs[2 * i + 1]
                        = ((((a[5 * i + 2] & 0xFF) >> 4)
                        | ((a[5 * i + 3] & 0xFF) << 4))
                        | ((a[5 * i + 4] & 0xFF) << 12)) & 0xFFFFF;
                coeffs[2 * i + 0] = gamma1 - coeffs[2 * i + 0];
                coeffs[2 * i + 1] = gamma1 - coeffs[2 * i + 1];
            }
        }
    }

    public void decompose(cryKeyMLDSApoly a) {
        int gamma2 = engine.DilithiumGamma2;
        for (int i = 0; i < cryKeyMLDSA.DilithiumN; i++) {
            int[] decomp = decompose(coeffs[i], gamma2);
            coeffs[i] = decomp[1];
            a.coeffs[i] = decomp[0];
        }
    }

    void packW1(byte[] r, int rOff) {
        int gamma2 = engine.DilithiumGamma2;
        if (gamma2 == (cryKeyMLDSA.DilithiumQ - 1) / 88) {
            for (int i = 0; i < cryKeyMLDSA.DilithiumN / 4; i++) {
                r[rOff + 3 * i + 0] = (byte) (((byte) coeffs[4 * i + 0]) | (coeffs[4 * i + 1] << 6));
                r[rOff + 3 * i + 1] = (byte) ((byte) (coeffs[4 * i + 1] >> 2) | (coeffs[4 * i + 2] << 4));
                r[rOff + 3 * i + 2] = (byte) ((byte) (coeffs[4 * i + 2] >> 4) | (coeffs[4 * i + 3] << 2));
            }
        } else {
            for (int i = 0; i < cryKeyMLDSA.DilithiumN / 2; i++) {
                r[rOff + i] = (byte) (coeffs[2 * i + 0] | (coeffs[2 * i + 1] << 4));
            }
        }
    }

    public void challenge(byte[] seed, int seedOff, int seedLen) {
        long signs;
        byte[] buf = new byte[cryKeyMLDSA.stream256BlockBytes];
        cryHashShake256 shakeDigest = new cryHashShake256();
        shakeDigest.init();
        shakeDigest.update(seed, seedOff, seedLen);
        shakeDigest.fillupBuffer(buf, 0, cryKeyMLDSA.stream256BlockBytes);
        signs = (long) 0;
        for (int i = 0; i < 8; i++) {
            signs |= (long) (buf[i] & 0xFF) << 8 * i;
        }
        int pos = 8;
        for (int i = 0; i < cryKeyMLDSA.DilithiumN; i++) {
            coeffs[i] = 0;
        }
        int b = 0;
        for (int i = cryKeyMLDSA.DilithiumN - engine.DilithiumTau; i < cryKeyMLDSA.DilithiumN; i++) {
            do {
                if (pos >= cryKeyMLDSA.stream256BlockBytes) {
                    shakeDigest.fillupBuffer(buf, 0, cryKeyMLDSA.stream256BlockBytes);
                    pos = 0;
                }
                b = (buf[pos++] & 0xFF);
            } while (b > i);
            coeffs[i] = coeffs[b];
            coeffs[b] = (int) (1 - 2 * (signs & 1));
            signs = (long) (signs >> 1);
        }
    }

    public boolean checkNorm(int B) {
        if (B > (cryKeyMLDSA.DilithiumQ - 1) / 8) {
            return true;
        }
        for (int i = 0; i < cryKeyMLDSA.DilithiumN; i++) {
            int t = coeffs[i] >> 31;
            t = coeffs[i] - (t & 2 * coeffs[i]);
            if (t >= B) {
                return true;
            }
        }
        return false;
    }

    public void subtract(cryKeyMLDSApoly inpPoly) {
        for (int i = 0; i < cryKeyMLDSA.DilithiumN; i++) {
            coeffs[i] = coeffs[i] - inpPoly.coeffs[i];
        }
    }

    public int polyMakeHint(cryKeyMLDSApoly a0, cryKeyMLDSApoly a1) {
        int s = 0;
        for (int i = 0; i < cryKeyMLDSA.DilithiumN; i++) {
            coeffs[i] = makeHint(a0.coeffs[i], a1.coeffs[i], engine);
            s += coeffs[i];
        }
        return s;
    }

    public void polyUseHint(cryKeyMLDSApoly a, cryKeyMLDSApoly h) {
        for (int i = 0; i < cryKeyMLDSA.DilithiumN; i++) {
            coeffs[i] = useHint(a.coeffs[i], h.coeffs[i], engine.DilithiumGamma2);
        }
    }

    public void zPack(byte[] z, int zOff) {
        int gamma1 = engine.DilithiumGamma1;
        if (gamma1 == (1 << 17)) {
            for (int i = 0; i < cryKeyMLDSA.DilithiumN / 4; i++) {
                int t0 = gamma1 - coeffs[4 * i + 0];
                int t1 = gamma1 - coeffs[4 * i + 1];
                int t2 = gamma1 - coeffs[4 * i + 2];
                int t3 = gamma1 - coeffs[4 * i + 3];
                z[zOff + 9 * i + 0] = (byte) t0;
                z[zOff + 9 * i + 1] = (byte) (t0 >> 8);
                z[zOff + 9 * i + 2] = (byte) ((byte) (t0 >> 16) | (t1 << 2));
                z[zOff + 9 * i + 3] = (byte) (t1 >> 6);
                z[zOff + 9 * i + 4] = (byte) ((byte) (t1 >> 14) | (t2 << 4));
                z[zOff + 9 * i + 5] = (byte) (t2 >> 4);
                z[zOff + 9 * i + 6] = (byte) ((byte) (t2 >> 12) | (t3 << 6));
                z[zOff + 9 * i + 7] = (byte) (t3 >> 2);
                z[zOff + 9 * i + 8] = (byte) (t3 >> 10);
            }
        } else {
            for (int i = 0; i < cryKeyMLDSA.DilithiumN / 2; i++) {
                int t0 = gamma1 - coeffs[2 * i + 0];
                int t1 = gamma1 - coeffs[2 * i + 1];
                z[zOff + 5 * i + 0] = (byte) t0;
                z[zOff + 5 * i + 1] = (byte) (t0 >> 8);
                z[zOff + 5 * i + 2] = (byte) ((byte) (t0 >> 16) | (t1 << 4));
                z[zOff + 5 * i + 3] = (byte) (t1 >> 4);
                z[zOff + 5 * i + 4] = (byte) (t1 >> 12);
            }
        }
    }

    public void zUnpack(byte[] a) {
        if (engine.DilithiumGamma1 == (1 << 17)) {
            for (int i = 0; i < cryKeyMLDSA.DilithiumN / 4; i++) {
                coeffs[4 * i + 0]
                        = (((int) (a[9 * i + 0] & 0xFF)
                        | (int) ((a[9 * i + 1] & 0xFF) << 8))
                        | (int) ((a[9 * i + 2] & 0xFF) << 16))
                        & 0x3FFFF;
                coeffs[4 * i + 1]
                        = (((int) ((a[9 * i + 2] & 0xFF) >>> 2)
                        | (int) ((a[9 * i + 3] & 0xFF) << 6))
                        | (int) ((a[9 * i + 4] & 0xFF) << 14))
                        & 0x3FFFF;
                coeffs[4 * i + 2]
                        = (((int) ((a[9 * i + 4] & 0xFF) >>> 4)
                        | (int) ((a[9 * i + 5] & 0xFF) << 4))
                        | (int) ((a[9 * i + 6] & 0xFF) << 12))
                        & 0x3FFFF;
                coeffs[4 * i + 3]
                        = (((int) ((a[9 * i + 6] & 0xFF) >>> 6)
                        | (int) ((a[9 * i + 7] & 0xFF) << 2))
                        | (int) ((a[9 * i + 8] & 0xFF) << 10))
                        & 0x3FFFF;
                coeffs[4 * i + 0] = engine.DilithiumGamma1 - coeffs[4 * i + 0];
                coeffs[4 * i + 1] = engine.DilithiumGamma1 - coeffs[4 * i + 1];
                coeffs[4 * i + 2] = engine.DilithiumGamma1 - coeffs[4 * i + 2];
                coeffs[4 * i + 3] = engine.DilithiumGamma1 - coeffs[4 * i + 3];
            }
        } else {
            for (int i = 0; i < cryKeyMLDSA.DilithiumN / 2; i++) {
                coeffs[2 * i + 0]
                        = (int) (((((int) (a[5 * i + 0] & 0xFF))
                        | (int) ((a[5 * i + 1] & 0xFF) << 8))
                        | (int) ((a[5 * i + 2] & 0xFF) << 16))
                        & 0xFFFFF);
                coeffs[2 * i + 1]
                        = (int) (((((int) ((a[5 * i + 2] & 0xFF) >>> 4))
                        | (int) ((a[5 * i + 3] & 0xFF) << 4))
                        | (int) ((a[5 * i + 4] & 0xFF) << 12))
                        & 0xFFFFF);
                coeffs[2 * i + 0] = engine.DilithiumGamma1 - coeffs[2 * i + 0];
                coeffs[2 * i + 1] = engine.DilithiumGamma1 - coeffs[2 * i + 1];
            }
        }
    }

    public void shiftLeft() {
        for (int i = 0; i < cryKeyMLDSA.DilithiumN; i++) {
            coeffs[i] = coeffs[i] << cryKeyMLDSA.DilithiumD;
        }
    }

    public int montgomeryReduce(long a) {
        int t = (int) (a * cryKeyMLDSA.DilithiumQinv);
        t = (int) ((a - ((long) t) * cryKeyMLDSA.DilithiumQ) >>> 32);
        return t;
    }

    public int reduce32(int a) {
        int t = (a + (1 << 22)) >> 23;
        t = a - t * cryKeyMLDSA.DilithiumQ;
        return t;
    }

    public int conditionalAddQ(int a) {
        a += (a >> 31) & cryKeyMLDSA.DilithiumQ;
        return a;
    }

    public int[] power2Round(int a) {
        int[] out = new int[2];
        out[0] = (a + (1 << (cryKeyMLDSA.DilithiumD - 1)) - 1) >> cryKeyMLDSA.DilithiumD;
        out[1] = a - (out[0] << cryKeyMLDSA.DilithiumD);
        return out;
    }

    public int[] decompose(int a, int gamma2) {
        int a1 = (a + 127) >> 7;
        if (gamma2 == (cryKeyMLDSA.DilithiumQ - 1) / 32) {
            a1 = (a1 * 1025 + (1 << 21)) >> 22;
            a1 &= 15;
        } else {
            a1 = (a1 * 11275 + (1 << 23)) >> 24;
            a1 ^= ((43 - a1) >> 31) & a1;
        }
        int a0 = a - a1 * 2 * gamma2;
        a0 -= (((cryKeyMLDSA.DilithiumQ - 1) / 2 - a0) >> 31) & cryKeyMLDSA.DilithiumQ;
        return new int[]{a0, a1};
    }

    public int makeHint(int a0, int a1, cryKeyMLDSA engine) {
        int g2 = engine.DilithiumGamma2, q = cryKeyMLDSA.DilithiumQ;
        if (a0 <= g2 || a0 > q - g2 || (a0 == q - g2 && a1 == 0)) {
            return 0;
        }
        return 1;
    }

    public int useHint(int a, int hint, int gamma2) {
        int[] intArray = decompose(a, gamma2);
        int a0 = intArray[0];
        int a1 = intArray[1];
        if (hint == 0) {
            return a1;
        }
        if (gamma2 == (cryKeyMLDSA.DilithiumQ - 1) / 32) {
            if (a0 > 0) {
                return (a1 + 1) & 15;
            } else {
                return (a1 - 1) & 15;
            }
        } else {
            if (a0 > 0) {
                return (a1 == 43) ? 0 : a1 + 1;
            } else {
                return (a1 == 0) ? 43 : a1 - 1;
            }
        }
    }

}

class cryKeyMLDSAvec {

    public final cryKeyMLDSApoly[] vec;

    public cryKeyMLDSAvec(cryKeyMLDSA engine, int siz) {
        vec = new cryKeyMLDSApoly[siz];
        for (int i = 0; i < vec.length; i++) {
            vec[i] = new cryKeyMLDSApoly(engine);
        }
    }

    public void uniformBlocks(byte[] rho, int t) {
        for (int i = 0; i < vec.length; i++) {
            vec[i].uniformBlocks(rho, (short) (t + i));
        }
    }

    public void uniformEta(byte[] seed, short nonce) {
        short n = nonce;
        for (int i = 0; i < vec.length; i++) {
            vec[i].uniformEta(seed, n++);
        }
    }

    void copyTo(cryKeyMLDSAvecL z) {
        for (int i = 0; i < vec.length; i++) {
            vec[i].copyTo(z.vec[i]);
        }
    }

    public void polyVecNtt() {
        for (int i = 0; i < vec.length; i++) {
            vec[i].polyNtt();
        }
    }

    public void uniformGamma1(byte[] seed, short nonce) {
        for (int i = 0; i < vec.length; i++) {
            vec[i].uniformGamma1(seed, (short) (vec.length * nonce + i));
        }
    }

    public void pointwisePolyMontgomery(cryKeyMLDSApoly a, cryKeyMLDSAvecL v) {
        for (int i = 0; i < vec.length; i++) {
            vec[i].pointwiseMontgomery(a, v.vec[i]);
        }
    }

    public void invNttToMont() {
        for (int i = 0; i < vec.length; i++) {
            vec[i].invNttToMont();
        }
    }

    public void addPolyVecL(cryKeyMLDSAvecL v) {
        for (int i = 0; i < vec.length; i++) {
            vec[i].addPoly(v.vec[i]);
        }
    }

    public void reduce() {
        for (int i = 0; i < vec.length; i++) {
            vec[i].reduce();
        }
    }

    public boolean checkNorm(int bound) {
        for (int i = 0; i < vec.length; i++) {
            if (vec[i].checkNorm(bound)) {
                return true;
            }
        }
        return false;
    }

    public void addPolyVecK(cryKeyMLDSAvecK b) {
        for (int i = 0; i < vec.length; i++) {
            vec[i].addPoly(b.vec[i]);
        }
    }

    public void conditionalAddQ() {
        for (int i = 0; i < vec.length; i++) {
            vec[i].conditionalAddQ();
        }
    }

    public void power2Round(cryKeyMLDSAvecK pvk) {
        for (int i = 0; i < vec.length; i++) {
            vec[i].power2Round(pvk.vec[i]);
        }
    }

    public void decompose(cryKeyMLDSAvecK v) {
        for (int i = 0; i < vec.length; i++) {
            vec[i].decompose(v.vec[i]);
        }
    }

    public void packW1(cryKeyMLDSA engine, byte[] r, int rOff) {
        for (int i = 0; i < vec.length; i++) {
            vec[i].packW1(r, rOff + i * engine.DilithiumPolyW1PackedBytes);
        }
    }

    public void pointwisePolyMontgomery(cryKeyMLDSApoly a, cryKeyMLDSAvecK v) {
        for (int i = 0; i < vec.length; i++) {
            vec[i].pointwiseMontgomery(a, v.vec[i]);
        }
    }

    public void subtract(cryKeyMLDSAvecK inpVec) {
        for (int i = 0; i < vec.length; i++) {
            vec[i].subtract(inpVec.vec[i]);
        }
    }

    public int makeHint(cryKeyMLDSAvecK v0, cryKeyMLDSAvecK v1) {
        int s = 0;
        for (int i = 0; i < vec.length; i++) {
            s += vec[i].polyMakeHint(v0.vec[i], v1.vec[i]);
        }

        return s;
    }

    public void useHint(cryKeyMLDSAvecK u, cryKeyMLDSAvecK h) {
        for (int i = 0; i < vec.length; i++) {
            vec[i].polyUseHint(u.vec[i], h.vec[i]);
        }
    }

    public void shiftLeft() {
        for (int i = 0; i < vec.length; i++) {
            vec[i].shiftLeft();
        }
    }

}

class cryKeyMLDSAvecK extends cryKeyMLDSAvec {

    public cryKeyMLDSAvecK(cryKeyMLDSA engine) {
        super(engine, engine.DilithiumK);
    }

}

class cryKeyMLDSAvecL extends cryKeyMLDSAvec {

    public cryKeyMLDSAvecL(cryKeyMLDSA engine) {
        super(engine, engine.DilithiumL);
    }

}

class cryKeyMLDSAmat {

    private final cryKeyMLDSAvecL[] matrix;

    public cryKeyMLDSAmat(cryKeyMLDSA engine) {
        matrix = new cryKeyMLDSAvecL[engine.DilithiumK];
        for (int i = 0; i < matrix.length; i++) {
            matrix[i] = new cryKeyMLDSAvecL(engine);
        }
    }

    public void pointwiseMontgomery(cryKeyMLDSAvecK t, cryKeyMLDSAvecL v) {
        for (int i = 0; i < matrix.length; i++) {
            t.vec[i].pointwiseAccountMontgomery(matrix[i], v);
        }
    }

    public void expandMatrix(byte[] rho) {
        for (int i = 0; i < matrix.length; i++) {
            matrix[i].uniformBlocks(rho, i << 8);
        }
    }

}


/*
import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.Signature;
import java.security.spec.X509EncodedKeySpec;

public class aaaaaaaaa {

    public static void main(String[] args) throws Exception {
        oneRound(44);
        oneRound(65);
        oneRound(87);
    }

    private static void oneRound(int mode) throws Exception {
        byte[] message
                = ("hello " + mode + " mode").getBytes();
        System.out.println("------------- "
                + new String(message));
        KeyPair jkpg = KeyPairGenerator.getInstance("ML-DSA-"
                + mode).genKeyPair();
        PublicKey jpub = jkpg.getPublic();
        PrivateKey jpriv
                = jkpg.getPrivate();
        Signature jsgnr = Signature.getInstance("ML-DSA-" + mode);
        jsgnr.initSign(jpriv);
        jsgnr.update(message);
        byte[] osign = jsgnr.sign();
        byte[] opriv = getEnding(jpriv.getEncoded(), beginPriv);
        byte[] opub
                = getEnding(jpub.getEncoded(), beginPub);
        jsgnr.initVerify(jpub);
        jsgnr.update(message);
        System.out.println("java:" + jsgnr.verify(osign));
        System.out.println("java:" + opub.length + " " + opriv.length + " "
                + osign.length);

        cryKeyMLDSA engine = new cryKeyMLDSA();
        engine.keyMakeSize(mode);
        engine.doKeyPair();
        byte[] mpriv = engine.encodePrivateKey();
        byte[] mpub
                = engine.encodePublicKey();
        engine.decodePrivateKey(mpriv);
        engine.initSign();
        engine.update(message, 0, message.length);
        byte[] msgntr
                = engine.doSignature();
        System.out.println("self:" + mpub.length + " "
                + mpriv.length + " " + msgntr.length);

        engine.initVerify();
        engine.update(message, 0, message.length);
        System.out.println("self:" + engine.doSignature(msgntr));

        engine.decodePublicKey(opub);
        engine.initVerify();;
        engine.update(message, 0,
                message.length);
        System.out.println("self:" + engine.doSignature(osign));

        jpub = KeyFactory.getInstance("ML-DSA-" + mode).generatePublic(new X509EncodedKeySpec(bits.byteConcat(beginPub, mpub)));
        jsgnr
                = Signature.getInstance("ML-DSA-" + mode);
        jsgnr.initVerify(jpub);
        jsgnr.update(message);
        System.out.println("java:" + jsgnr.verify(msgntr));
    }

    private static byte[] getEnding(byte[] buf, byte[] beg) {
        byte[] res = new byte[buf.length - beg.length];
        bits.byteCopy(buf, beg.length, res, 0, res.length);
        bits.byteCopy(buf, 0, beg, 0, beg.length);
        return res;
    }

    final static byte[] beginPub = new byte[]{(byte) 0x30, (byte) 0x82, (byte) 0x07, (byte) 0xB2, (byte) 0x30, (byte) 0x0B, (byte) 0x06, (byte) 0x09, (byte) 0x60, (byte) 0x86, (byte) 0x48, (byte) 0x01, (byte) 0x65, (byte) 0x03, (byte) 0x04, (byte) 0x03, (byte) 0x12, (byte) 0x03, (byte) 0x82, (byte) 0x07, (byte) 0xA1, (byte) 0x00};

    final static byte[] beginPriv = new byte[]{(byte) 0x30, (byte) 0x82, (byte) 0x0F, (byte) 0xD8, (byte) 0x02, (byte) 0x01, (byte) 0x00, (byte) 0x30, (byte) 0x0B, (byte) 0x06, (byte) 0x09, (byte) 0x60, (byte) 0x86, (byte) 0x48, (byte) 0x01, (byte) 0x65, (byte) 0x03, (byte) 0x04, (byte) 0x03, (byte) 0x12, (byte) 0x04, (byte) 0x82, (byte) 0x0F, (byte) 0xC4, (byte) 0x04, (byte) 0x82, (byte) 0x0F, (byte) 0xC0};

}

///////////hereeee

 */
