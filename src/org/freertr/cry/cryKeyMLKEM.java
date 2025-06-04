package org.freertr.cry;

import org.freertr.pack.packHolder;
import org.freertr.util.bits;

/**
 * module lattice key exchange
 *
 * @author matecsaba
 */
public class cryKeyMLKEM extends cryKeyGeneric {

    /**
     * create instance
     */
    public cryKeyMLKEM() {
    }

    /**
     * tls value
     */
    public final static int tlsVal512 = 0x200;

    /**
     * tls value
     */
    public final static int tlsVal768 = 0x201;

    /**
     * tls value
     */
    public final static int tlsVal1024 = 0x202;

    /**
     * magic values
     */
    protected final static int KyberN = 256;

    /**
     * magic values
     */
    protected final static int KyberQ = 3329;

    /**
     * magic values
     */
    protected final static int KyberQinv = 62209;

    /**
     * magic values
     */
    protected final static int KyberSymBytes = 32;

    /**
     * magic values
     */
    protected final static int KyberPolyBytes = 384;

    /**
     * magic values
     */
    protected final static int KyberEta2 = 2;

    /**
     * magic values
     */
    protected final int sessionKeyLength = 32;

    /**
     * magic values
     */
    protected int KyberK;

    /**
     * magic values
     */
    protected int KyberEta1;

    /**
     * magic values
     */
    protected int KyberPolyVecBytes;

    /**
     * magic values
     */
    protected int KyberPolyCompressedBytes;

    /**
     * magic values
     */
    protected int KyberPolyVecCompressedBytes;

    /**
     * magic values
     */
    protected int KyberIndCpaPublicKeyBytes;

    /**
     * common value
     */
    protected byte[] common;

    /**
     * client private value
     */
    protected byte[] clntPriv;

    /**
     * client public value
     */
    protected byte[] clntPub;

    /**
     * server public value
     */
    protected byte[] servPub;

    private byte[][] kemEncryptInternal(byte[] publicKeyInput, byte[] randBytes) {
        byte[] buf = new byte[2 * KyberSymBytes];
        byte[] kr = new byte[2 * KyberSymBytes];
        bits.byteCopy(randBytes, 0, buf, 0, KyberSymBytes);
        symmetricHashH(buf, publicKeyInput, KyberSymBytes);
        symmetricHashG(kr, buf);
        byte[] msg = new byte[KyberSymBytes];
        bits.byteCopy(buf, 0, msg, 0, msg.length);
        byte[] coins = new byte[kr.length - 32];
        bits.byteCopy(kr, 32, coins, 0, coins.length);
        cryKeyMLKEMvec sp = new cryKeyMLKEMvec(this);
        cryKeyMLKEMvec publicKeyPolyVec = new cryKeyMLKEMvec(this);
        cryKeyMLKEMvec errorPolyVector = new cryKeyMLKEMvec(this);
        cryKeyMLKEMvec bp = new cryKeyMLKEMvec(this);
        cryKeyMLKEMvec[] aMatrixTranspose = new cryKeyMLKEMvec[KyberK];
        cryKeyMLKEMpoly errorPoly = new cryKeyMLKEMpoly(this);
        cryKeyMLKEMpoly v = new cryKeyMLKEMpoly(this);
        cryKeyMLKEMpoly k = new cryKeyMLKEMpoly(this);
        byte[] seed = unpackPublicKey(publicKeyPolyVec, publicKeyInput);
        k.fromMsg(msg);
        for (int i = 0; i < KyberK; i++) {
            aMatrixTranspose[i] = new cryKeyMLKEMvec(this);
        }
        generateMatrix(aMatrixTranspose, seed, true);
        byte nonce = (byte) 0;
        for (int i = 0; i < KyberK; i++) {
            sp.vec[i].getEta1Noise(coins, nonce);
            nonce = (byte) (nonce + (byte) 1);
        }
        for (int i = 0; i < KyberK; i++) {
            errorPolyVector.vec[i].getEta2Noise(coins, nonce);
            nonce = (byte) (nonce + (byte) 1);
        }
        errorPoly.getEta2Noise(coins, nonce);
        sp.polyVecNtt();
        for (int i = 0; i < KyberK; i++) {
            bp.vec[i].pointwiseAccountMontgomery(aMatrixTranspose[i], sp, this);
        }
        v.pointwiseAccountMontgomery(publicKeyPolyVec, sp, this);
        bp.polyVecInverseNttToMont();
        v.polyInverseNttToMont();
        bp.addPoly(errorPolyVector);
        v.addCoeffs(errorPoly);
        v.addCoeffs(k);
        bp.reducePoly();
        v.reduce();
        byte[] outputCipherText = packCipherText(bp, v);
        byte[] outputSharedSecret = new byte[sessionKeyLength];
        bits.byteCopy(kr, 0, outputSharedSecret, 0, outputSharedSecret.length);
        return new byte[][]{outputSharedSecret, outputCipherText};
    }

    private byte[] kemDecryptInternal(byte[] secretKey, byte[] cipherText) {
        byte[] buf = new byte[2 * KyberSymBytes];
        byte[] kr = new byte[2 * KyberSymBytes];
        cryKeyMLKEMvec bp = new cryKeyMLKEMvec(this);
        cryKeyMLKEMvec secretKeyPolyVec = new cryKeyMLKEMvec(this);
        cryKeyMLKEMpoly v = new cryKeyMLKEMpoly(this);
        cryKeyMLKEMpoly mp = new cryKeyMLKEMpoly(this);
        unpackCipherText(bp, v, cipherText);
        unpackSecretKey(secretKeyPolyVec, secretKey);
        bp.polyVecNtt();
        mp.pointwiseAccountMontgomery(secretKeyPolyVec, bp, this);
        mp.polyInverseNttToMont();
        mp.polySubtract(v);
        mp.reduce();
        bits.byteCopy(mp.toMsg(), 0, buf, 0, KyberSymBytes);
        bits.byteCopy(secretKey, KyberPolyVecBytes + KyberIndCpaPublicKeyBytes, buf, KyberSymBytes, KyberSymBytes);
        symmetricHashG(kr, buf);
        buf = new byte[sessionKeyLength];
        bits.byteCopy(kr, 0, buf, 0, buf.length);
        return buf;
    }

    private byte[][] kemKeygenInternal(byte[] d) {
        cryKeyMLKEMvec secretKey = new cryKeyMLKEMvec(this);
        cryKeyMLKEMvec publicKey = new cryKeyMLKEMvec(this);
        cryKeyMLKEMvec e = new cryKeyMLKEMvec(this);
        byte[] buf = new byte[64];
        symmetricHashG(buf, bits.byteConcat(d, new byte[]{(byte) KyberK}));
        byte[] publicSeed = new byte[32];
        byte[] noiseSeed = new byte[32];
        bits.byteCopy(buf, 0, publicSeed, 0, 32);
        bits.byteCopy(buf, 32, noiseSeed, 0, 32);
        byte count = (byte) 0;
        cryKeyMLKEMvec[] aMatrix = new cryKeyMLKEMvec[KyberK];
        for (int i = 0; i < KyberK; i++) {
            aMatrix[i] = new cryKeyMLKEMvec(this);
        }
        generateMatrix(aMatrix, publicSeed, false);
        for (int i = 0; i < KyberK; i++) {
            secretKey.vec[i].getEta1Noise(noiseSeed, count);
            count = (byte) (count + (byte) 1);
        }
        for (int i = 0; i < KyberK; i++) {
            e.vec[i].getEta1Noise(noiseSeed, count);
            count = (byte) (count + (byte) 1);
        }
        secretKey.polyVecNtt();
        e.polyVecNtt();
        for (int i = 0; i < KyberK; i++) {
            publicKey.vec[i].pointwiseAccountMontgomery(aMatrix[i], secretKey, this);
            publicKey.vec[i].convertToMont();
        }
        publicKey.addPoly(e);
        publicKey.reducePoly();
        byte[] publicBuf = packPublicKey(publicKey, publicSeed);
        byte[] secretBuf = packSecretKey(secretKey);
        byte[] s = new byte[KyberPolyVecBytes];
        bits.byteCopy(secretBuf, 0, s, 0, KyberPolyVecBytes);
        byte[] hashedPublicKey = new byte[32];
        symmetricHashH(hashedPublicKey, publicBuf, 0);
        buf = bits.byteConcat(secretBuf, publicBuf);
        buf = bits.byteConcat(buf, hashedPublicKey);
        return new byte[][]{publicBuf, buf};
    }

    private byte[] kemDecrypt(byte[] secretKey, byte[] cipherText) {
        return kemDecryptInternal(secretKey, cipherText);
    }

    private byte[][] kemEncrypt(byte[] publicKeyInput) {
        byte[] randBytes = new byte[32];
        for (int i = 0; i < randBytes.length; i++) {
            randBytes[i] = (byte) bits.randomB();
        }
        return kemEncryptInternal(publicKeyInput, randBytes);
    }

    private byte[][] kemKeygen() {
        byte[] d = new byte[KyberSymBytes];
        for (int i = 0; i < d.length; i++) {
            d[i] = (byte) bits.randomB();
        }
        return kemKeygenInternal(d);
    }

    private byte[] packCipherText(cryKeyMLKEMvec b, cryKeyMLKEMpoly v) {
        byte[] outBuf = new byte[KyberPolyVecCompressedBytes + KyberPolyCompressedBytes];
        bits.byteCopy(b.compressPolyVec(), 0, outBuf, 0, KyberPolyVecCompressedBytes);
        bits.byteCopy(v.compressPoly(), 0, outBuf, KyberPolyVecCompressedBytes, KyberPolyCompressedBytes);
        return outBuf;
    }

    private void unpackCipherText(cryKeyMLKEMvec b, cryKeyMLKEMpoly v, byte[] cipherText) {
        byte[] compressedPolyVecCipherText = new byte[KyberPolyVecCompressedBytes];
        bits.byteCopy(cipherText, 0, compressedPolyVecCipherText, 0, compressedPolyVecCipherText.length);
        b.decompressPolyVec(compressedPolyVecCipherText);
        byte[] compressedPolyCipherText = new byte[cipherText.length - KyberPolyVecCompressedBytes];
        bits.byteCopy(cipherText, KyberPolyVecCompressedBytes, compressedPolyCipherText, 0, compressedPolyCipherText.length);
        v.decompressPoly(compressedPolyCipherText);
    }

    private byte[] packPublicKey(cryKeyMLKEMvec publicKeyPolyVec, byte[] seed) {
        byte[] buf = new byte[KyberIndCpaPublicKeyBytes];
        bits.byteCopy(publicKeyPolyVec.toBytes(), 0, buf, 0, KyberPolyVecBytes);
        bits.byteCopy(seed, 0, buf, KyberPolyVecBytes, cryKeyMLKEM.KyberSymBytes);
        return buf;
    }

    private byte[] unpackPublicKey(cryKeyMLKEMvec publicKeyPolyVec, byte[] publicKey) {
        byte[] outputSeed = new byte[cryKeyMLKEM.KyberSymBytes];
        publicKeyPolyVec.fromBytes(publicKey);
        bits.byteCopy(publicKey, KyberPolyVecBytes, outputSeed, 0, cryKeyMLKEM.KyberSymBytes);
        return outputSeed;
    }

    private byte[] packSecretKey(cryKeyMLKEMvec secretKeyPolyVec) {
        return secretKeyPolyVec.toBytes();
    }

    private void unpackSecretKey(cryKeyMLKEMvec secretKeyPolyVec, byte[] secretKey) {
        secretKeyPolyVec.fromBytes(secretKey);
    }

    private void symmetricHashH(byte[] out, byte[] in, int outOffset) {
        cryHashSha3256 h = new cryHashSha3256();
        h.init();
        h.update(in, 0, in.length);
        byte[] buf = h.finish();
        bits.byteCopy(buf, 0, out, outOffset, buf.length);
    }

    private void symmetricHashG(byte[] out, byte[] in) {
        cryHashSha3512 h = new cryHashSha3512();
        h.init();
        h.update(in, 0, in.length);
        byte[] buf = h.finish();
        bits.byteCopy(buf, 0, out, 0, buf.length);
    }

    private void generateMatrix(cryKeyMLKEMvec[] aMatrix, byte[] seed, boolean transposed) {
        int xofBlockBytes = 168;
        int matrixNBlocks = ((12 * cryKeyMLKEM.KyberN / 8 * (1 << 12) / cryKeyMLKEM.KyberQ + xofBlockBytes) / xofBlockBytes);
        byte[] buf = new byte[matrixNBlocks * xofBlockBytes + 2];
        cryHashShake128 h = new cryHashShake128();
        for (int i = 0; i < KyberK; i++) {
            for (int j = 0; j < KyberK; j++) {
                h.init();
                h.update(seed, 0, seed.length);
                if (transposed) {
                    h.update(i);
                    h.update(j);
                } else {
                    h.update(j);
                    h.update(i);
                }
                int outLen = matrixNBlocks * xofBlockBytes;
                h.fillupBuffer(buf, 0, outLen);
                int ctr = aMatrix[i].vec[j].rejectionSampling(0, cryKeyMLKEM.KyberN, buf, outLen);
                while (ctr < KyberN) {
                    int off = outLen % 3;
                    for (int k = 0; k < off; k++) {
                        buf[k] = buf[outLen - off + k];
                    }
                    outLen = xofBlockBytes * 2;
                    h.fillupBuffer(buf, off, outLen);
                    outLen = off + xofBlockBytes;
                    ctr += aMatrix[i].vec[j].rejectionSampling(ctr, cryKeyMLKEM.KyberN - ctr, buf, outLen);
                }
            }
        }
    }

    public String algName() {
        return "mlkem";
    }

    public String sshName() {
        return "mlkem" + keySize() + "-sha" + (keySize() < 1024 ? 256 : 384);
    }

    public cryHashGeneric sshHash() {
        if (keySize() < 1024) {
            return new cryHashSha2256();
        }
        return new cryHashSha2384();
    }

    public boolean certReader(packHolder pck) {
        return true;
    }

    public void certWriter(packHolder pck) {
    }

    public boolean privReader(packHolder pck) {
        return true;
    }

    public void privWriter(packHolder pck) {
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
        switch (len) {
            case 512:
                KyberK = 2;
                KyberEta1 = 3;
                KyberPolyCompressedBytes = 128;
                KyberPolyVecCompressedBytes = KyberK * 320;
                break;
            case 768:
                KyberK = 3;
                KyberEta1 = 2;
                KyberPolyCompressedBytes = 128;
                KyberPolyVecCompressedBytes = KyberK * 320;
                break;
            case 1024:
                KyberK = 4;
                KyberEta1 = 2;
                KyberPolyCompressedBytes = 160;
                KyberPolyVecCompressedBytes = KyberK * 352;
                break;
            default:
                return true;
        }
        KyberPolyVecBytes = KyberK * KyberPolyBytes;
        KyberIndCpaPublicKeyBytes = KyberPolyVecBytes + KyberSymBytes;
        return false;
    }

    public boolean keyMakeName(String nam) {
        return true;
    }

    public boolean keyMakeTls(int id) {
        keyMakeSize(256 * (id - tlsVal512 + 2));
        return true;
    }

    public boolean keyMakeIke(int id) {
        return true;
    }

    public int keyMakeVal() {
        return tlsVal512 + KyberK - 2;
    }

    public boolean keyVerify() {
        return false;
    }

    public int keySize() {
        return 256 * KyberK;
    }

    public String keyDump() {
        return "cln=" + bits.byteDump(clntPub, 0, -1) + " srv=" + bits.byteDump(servPub, 0, -1) + " res=" + bits.byteDump(common, 0, -1);
    }

    public void keyClntInit() {
        byte[][] k = kemKeygen();
        clntPub = k[0];
        clntPriv = k[1];
    }

    public void keyServInit() {
    }

    public void keyClntCalc() {
        common = kemDecrypt(clntPriv, servPub);
    }

    public void keyServCalc() {
        byte[][] k = kemEncrypt(clntPub);
        common = k[0];
        servPub = k[1];
    }

    public byte[] keyCommonTls() {
        return common;
    }

    public byte[] keyCommonSsh() {
        return common;
    }

    public byte[] keyCommonIke() {
        return common;
    }

    public byte[] keyClntTls() {
        return clntPub;
    }

    public byte[] keyServTls() {
        return servPub;
    }

    public boolean keyClntTls(byte[] buf, int ofs) {
        clntPub = new byte[buf.length - ofs];
        bits.byteCopy(buf, ofs, clntPub, 0, clntPub.length);
        return false;
    }

    public boolean keyServTls(byte[] buf, int ofs) {
        servPub = new byte[buf.length - ofs];
        bits.byteCopy(buf, ofs, servPub, 0, servPub.length);
        return false;
    }

    public byte[] keyClntSsh() {
        return clntPub;
    }

    public byte[] keyServSsh() {
        return servPub;
    }

    public boolean keyClntSsh(byte[] buf, int ofs) {
        keyClntTls(buf, ofs);
        return false;
    }

    public boolean keyServSsh(byte[] buf, int ofs) {
        keyServTls(buf, ofs);
        return false;
    }

    public byte[] keyClntIke() {
        return clntPub;
    }

    public byte[] keyServIke() {
        return servPub;
    }

    public boolean keyClntIke(byte[] buf, int ofs) {
        keyClntTls(buf, ofs);
        return false;
    }

    public boolean keyServIke(byte[] buf, int ofs) {
        keyServTls(buf, ofs);
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

}

class cryKeyMLKEMpoly {

    public final static short[] nttZetas = new short[]{
        2285, 2571, 2970, 1812, 1493, 1422, 287, 202, 3158, 622, 1577, 182, 962,
        2127, 1855, 1468, 573, 2004, 264, 383, 2500, 1458, 1727, 3199, 2648, 1017,
        732, 608, 1787, 411, 3124, 1758, 1223, 652, 2777, 1015, 2036, 1491, 3047,
        1785, 516, 3321, 3009, 2663, 1711, 2167, 126, 1469, 2476, 3239, 3058, 830,
        107, 1908, 3082, 2378, 2931, 961, 1821, 2604, 448, 2264, 677, 2054, 2226,
        430, 555, 843, 2078, 871, 1550, 105, 422, 587, 177, 3094, 3038, 2869, 1574,
        1653, 3083, 778, 1159, 3182, 2552, 1483, 2727, 1119, 1739, 644, 2457, 349,
        418, 329, 3173, 3254, 817, 1097, 603, 610, 1322, 2044, 1864, 384, 2114, 3193,
        1218, 1994, 2455, 220, 2142, 1670, 2144, 1799, 2051, 794, 1819, 2475, 2459,
        478, 3221, 3021, 996, 991, 958, 1869, 1522, 1628
    };

    public final static short[] nttZetasInv = new short[]{
        1701, 1807, 1460, 2371, 2338, 2333, 308, 108, 2851, 870, 854, 1510, 2535,
        1278, 1530, 1185, 1659, 1187, 3109, 874, 1335, 2111, 136, 1215, 2945, 1465,
        1285, 2007, 2719, 2726, 2232, 2512, 75, 156, 3000, 2911, 2980, 872, 2685,
        1590, 2210, 602, 1846, 777, 147, 2170, 2551, 246, 1676, 1755, 460, 291, 235,
        3152, 2742, 2907, 3224, 1779, 2458, 1251, 2486, 2774, 2899, 1103, 1275, 2652,
        1065, 2881, 725, 1508, 2368, 398, 951, 247, 1421, 3222, 2499, 271, 90, 853,
        1860, 3203, 1162, 1618, 666, 320, 8, 2813, 1544, 282, 1838, 1293, 2314, 552,
        2677, 2106, 1571, 205, 2918, 1542, 2721, 2597, 2312, 681, 130, 1602, 1871,
        829, 2946, 3065, 1325, 2756, 1861, 1474, 1202, 2367, 3147, 1752, 2707, 171,
        3127, 3042, 1907, 1836, 1517, 359, 758, 1441
    };

    public short[] coeffs;

    private final cryKeyMLKEM engine;

    public cryKeyMLKEMpoly(cryKeyMLKEM ng) {
        coeffs = new short[cryKeyMLKEM.KyberN];
        engine = ng;
    }

    public void polyNtt() {
        short[] r = new short[cryKeyMLKEM.KyberN];
        System.arraycopy(coeffs, 0, r, 0, r.length);
        int j;
        short t, zeta;
        int k = 1;
        for (int i = 128; i >= 2; i >>= 1) {
            for (int l = 0; l < 256; l = j + i) {
                zeta = nttZetas[k++];
                for (j = l; j < l + i; j++) {
                    t = factorQMulMont(zeta, r[j + i]);
                    r[j + i] = (short) (r[j] - t);
                    r[j] = (short) (r[j] + t);
                }
            }
        }
        coeffs = r;
        reduce();
    }

    public void polyInverseNttToMont() {
        short[] r = new short[cryKeyMLKEM.KyberN];
        System.arraycopy(coeffs, 0, r, 0, cryKeyMLKEM.KyberN);
        int j;
        short t, zeta;
        int k = 0;
        for (int i = 2; i <= 128; i <<= 1) {
            for (int l = 0; l < 256; l = j + i) {
                zeta = nttZetasInv[k++];
                for (j = l; j < l + i; j++) {
                    t = r[j];
                    r[j] = barretReduce((short) (t + r[j + i]));
                    r[j + i] = (short) (t - r[j + i]);
                    r[j + i] = factorQMulMont(zeta, r[j + i]);
                }
            }
        }
        for (j = 0; j < 256; j++) {
            r[j] = factorQMulMont(r[j], nttZetasInv[127]);
        }
        coeffs = r;
    }

    public void reduce() {
        for (int i = 0; i < cryKeyMLKEM.KyberN; i++) {
            coeffs[i] = barretReduce(coeffs[i]);
        }
    }

    public void pointwiseAccountMontgomery(cryKeyMLKEMvec inp1, cryKeyMLKEMvec inp2, cryKeyMLKEM engine) {
        cryKeyMLKEMpoly t = new cryKeyMLKEMpoly(engine);
        baseMultMontgomery(inp1.vec[0], inp2.vec[0]);
        for (int i = 1; i < engine.KyberK; i++) {
            t.baseMultMontgomery(inp1.vec[i], inp2.vec[i]);
            addCoeffs(t);
        }
        reduce();
    }

    public void baseMultMontgomery(cryKeyMLKEMpoly a, cryKeyMLKEMpoly b) {
        for (int i = 0; i < cryKeyMLKEM.KyberN / 4; i++) {
            baseMultMont(4 * i,
                    a.coeffs[4 * i], a.coeffs[4 * i + 1],
                    b.coeffs[4 * i], b.coeffs[4 * i + 1],
                    nttZetas[64 + i]);
            baseMultMont(4 * i + 2,
                    a.coeffs[4 * i + 2], a.coeffs[4 * i + 3],
                    b.coeffs[4 * i + 2], b.coeffs[4 * i + 3],
                    (short) (-1 * nttZetas[64 + i]));
        }
    }

    public void addCoeffs(cryKeyMLKEMpoly b) {
        for (int i = 0; i < cryKeyMLKEM.KyberN; i++) {
            coeffs[i] = (short) (coeffs[i] + b.coeffs[i]);
        }
    }

    public void convertToMont() {
        final short f = (short) (((long) 1 << 32) % cryKeyMLKEM.KyberQ);
        for (int i = 0; i < cryKeyMLKEM.KyberN; i++) {
            coeffs[i] = montgomeryReduce(coeffs[i] * f);
        }
    }

    public byte[] compressPoly() {
        byte[] t = new byte[8];
        byte[] r = new byte[engine.KyberPolyCompressedBytes];
        int count = 0;
        conditionalSubQ();
        if (engine.KyberPolyCompressedBytes == 128) {
            for (int i = 0; i < cryKeyMLKEM.KyberN / 8; i++) {
                for (int j = 0; j < 8; j++) {
                    int t_j = coeffs[8 * i + j];
                    t_j <<= 4;
                    t_j += 1665;
                    t_j *= 80635;
                    t_j >>= 28;
                    t_j &= 15;
                    t[j] = (byte) t_j;
                }
                r[count + 0] = (byte) (t[0] | (t[1] << 4));
                r[count + 1] = (byte) (t[2] | (t[3] << 4));
                r[count + 2] = (byte) (t[4] | (t[5] << 4));
                r[count + 3] = (byte) (t[6] | (t[7] << 4));
                count += 4;
            }
        } else {
            for (int i = 0; i < cryKeyMLKEM.KyberN / 8; i++) {
                for (int j = 0; j < 8; j++) {
                    int t_j = coeffs[8 * i + j];
                    t_j <<= 5;
                    t_j += 1664;
                    t_j *= 40318;
                    t_j >>= 27;
                    t_j &= 31;
                    t[j] = (byte) t_j;
                }
                r[count + 0] = (byte) ((t[0] >> 0) | (t[1] << 5));
                r[count + 1] = (byte) ((t[1] >> 3) | (t[2] << 2) | (t[3] << 7));
                r[count + 2] = (byte) ((t[3] >> 1) | (t[4] << 4));
                r[count + 3] = (byte) ((t[4] >> 4) | (t[5] << 1) | (t[6] << 6));
                r[count + 4] = (byte) ((t[6] >> 2) | (t[7] << 3));
                count += 5;
            }
        }
        return r;
    }

    public void decompressPoly(byte[] compressedPolyCipherText) {
        int count = 0;
        if (engine.KyberPolyCompressedBytes == 128) {
            for (int i = 0; i < cryKeyMLKEM.KyberN / 2; i++) {
                coeffs[2 * i + 0] = (short) ((((short) ((compressedPolyCipherText[count] & 0xFF) & 15) * cryKeyMLKEM.KyberQ) + 8) >> 4);
                coeffs[2 * i + 1] = (short) ((((short) ((compressedPolyCipherText[count] & 0xFF) >> 4) * cryKeyMLKEM.KyberQ) + 8) >> 4);
                count += 1;
            }
        } else {
            byte[] t = new byte[8];
            for (int i = 0; i < cryKeyMLKEM.KyberN / 8; i++) {
                t[0] = (byte) ((compressedPolyCipherText[count + 0] & 0xFF) >> 0);
                t[1] = (byte) (((compressedPolyCipherText[count + 0] & 0xFF) >> 5) | ((compressedPolyCipherText[count + 1] & 0xFF) << 3));
                t[2] = (byte) ((compressedPolyCipherText[count + 1] & 0xFF) >> 2);
                t[3] = (byte) (((compressedPolyCipherText[count + 1] & 0xFF) >> 7) | ((compressedPolyCipherText[count + 2] & 0xFF) << 1));
                t[4] = (byte) (((compressedPolyCipherText[count + 2] & 0xFF) >> 4) | ((compressedPolyCipherText[count + 3] & 0xFF) << 4));
                t[5] = (byte) ((compressedPolyCipherText[count + 3] & 0xFF) >> 1);
                t[6] = (byte) (((compressedPolyCipherText[count + 3] & 0xFF) >> 6) | ((compressedPolyCipherText[count + 4] & 0xFF) << 2));
                t[7] = (byte) ((compressedPolyCipherText[count + 4] & 0xFF) >> 3);
                count += 5;
                for (int j = 0; j < 8; j++) {
                    coeffs[8 * i + j] = (short) (((t[j] & 31) * cryKeyMLKEM.KyberQ + 16) >> 5);
                }
            }
        }
    }

    public byte[] toBytes() {
        byte[] r = new byte[cryKeyMLKEM.KyberPolyBytes];
        conditionalSubQ();
        for (int i = 0; i < cryKeyMLKEM.KyberN / 2; i++) {
            short t0 = coeffs[2 * i];
            short t1 = coeffs[2 * i + 1];
            r[3 * i] = (byte) (t0 >> 0);
            r[3 * i + 1] = (byte) ((t0 >> 8) | (t1 << 4));
            r[3 * i + 2] = (byte) (t1 >> 4);
        }
        return r;

    }

    public void fromBytes(byte[] inpBytes) {
        for (int i = 0; i < cryKeyMLKEM.KyberN / 2; i++) {
            coeffs[2 * i + 0] = (short) ((((inpBytes[3 * i + 0] & 0xFF) >> 0) | ((inpBytes[3 * i + 1] & 0xFF) << 8)) & 0xFFF);
            coeffs[2 * i + 1] = (short) ((((inpBytes[3 * i + 1] & 0xFF) >> 4) | (long) ((inpBytes[3 * i + 2] & 0xFF) << 4)) & 0xFFF);
        }
    }

    public byte[] toMsg() {
        int LOWER = cryKeyMLKEM.KyberQ >>> 2;
        int UPPER = cryKeyMLKEM.KyberQ - LOWER;
        byte[] outMsg = new byte[cryKeyMLKEM.KyberSymBytes];
        conditionalSubQ();
        for (int i = 0; i < cryKeyMLKEM.KyberN / 8; i++) {
            outMsg[i] = 0;
            for (int j = 0; j < 8; j++) {
                int c_j = coeffs[8 * i + j];
                int t = ((LOWER - c_j) & (c_j - UPPER)) >>> 31;
                outMsg[i] |= (byte) (t << j);
            }
        }
        return outMsg;
    }

    public void fromMsg(byte[] msg) {
        for (int i = 0; i < cryKeyMLKEM.KyberN / 8; i++) {
            for (int j = 0; j < 8; j++) {
                short mask = (short) ((-1) * (short) (((msg[i] & 0xFF) >> j) & 1));
                coeffs[8 * i + j] = (short) (mask & (short) ((cryKeyMLKEM.KyberQ + 1) / 2));
            }
        }
    }

    public void conditionalSubQ() {
        for (int i = 0; i < cryKeyMLKEM.KyberN; i++) {
            coeffs[i] = conditionalSubQ(coeffs[i]);
        }
    }

    public void symmetricPrf(byte[] out, byte[] seed, byte nonce) {
        cryHashShake256 h = new cryHashShake256();
        byte[] extSeed = new byte[seed.length + 1];
        bits.byteCopy(seed, 0, extSeed, 0, seed.length);
        extSeed[seed.length] = nonce;
        h.init();
        h.update(extSeed, 0, extSeed.length);
        h.fillupBuffer(out, 0, out.length);
    }

    public void getEta1Noise(byte[] seed, byte nonce) {
        byte[] buf = new byte[cryKeyMLKEM.KyberN * engine.KyberEta1 / 4 + 1];
        symmetricPrf(buf, seed, nonce);
        mlCBD(buf, engine.KyberEta1);
    }

    public void getEta2Noise(byte[] seed, byte nonce) {
        byte[] buf = new byte[cryKeyMLKEM.KyberN * cryKeyMLKEM.KyberEta2 / 4 + 1];
        symmetricPrf(buf, seed, nonce);
        mlCBD(buf, cryKeyMLKEM.KyberEta2);
    }

    public void polySubtract(cryKeyMLKEMpoly b) {
        for (int i = 0; i < cryKeyMLKEM.KyberN; i++) {
            coeffs[i] = (short) (b.coeffs[i] - coeffs[i]);
        }
    }

    public int rejectionSampling(int coeffOff, int len, byte[] inpBuf, int inpBufLen) {
        int ctr = 0;
        for (int pos = 0; (ctr < len) && (pos + 3 <= inpBufLen);) {
            short val0 = (short) (((((short) (inpBuf[pos] & 0xFF)) >> 0) | (((short) (inpBuf[pos + 1] & 0xFF)) << 8)) & 0xFFF);
            short val1 = (short) (((((short) (inpBuf[pos + 1] & 0xFF)) >> 4) | (((short) (inpBuf[pos + 2] & 0xFF)) << 4)) & 0xFFF);
            pos = pos + 3;
            if (val0 < (short) cryKeyMLKEM.KyberQ) {
                coeffs[coeffOff + ctr] = val0;
                ctr++;
            }
            if (ctr < len && val1 < (short) cryKeyMLKEM.KyberQ) {
                coeffs[coeffOff + ctr] = val1;
                ctr++;
            }
        }
        return ctr;
    }

    public void mlCBD(byte[] bytes, int eta) {
        if (eta == 3) {
            for (int i = 0; i < cryKeyMLKEM.KyberN / 4; i++) {
                long t = bits.lsbGetD(bytes, 3 * i);
                long d = t & 0x00249249;
                d = d + ((t >> 1) & 0x00249249);
                d = d + ((t >> 2) & 0x00249249);
                for (int j = 0; j < 4; j++) {
                    int a = (short) ((d >> (6 * j + 0)) & 0x7);
                    int b = (short) ((d >> (6 * j + eta)) & 0x7);
                    coeffs[4 * i + j] = (short) (a - b);
                }
            }
        } else {
            for (int i = 0; i < cryKeyMLKEM.KyberN / 8; i++) {
                long t = bits.lsbGetD(bytes, 4 * i);
                long d = t & 0x55555555;
                d = d + ((t >> 1) & 0x55555555);
                for (int j = 0; j < 8; j++) {
                    int a = (short) ((d >> (4 * j + 0)) & 0x3);
                    int b = (short) ((d >> (4 * j + eta)) & 0x3);
                    coeffs[8 * i + j] = (short) (a - b);
                }
            }
        }
    }

    public short factorQMulMont(short a, short b) {
        return montgomeryReduce(a * b);
    }

    public void baseMultMont(int outIndex, short a0, short a1, short b0, short b1, short zeta) {
        short o = factorQMulMont(a1, b1);
        o = factorQMulMont(o, zeta);
        o += factorQMulMont(a0, b0);
        coeffs[outIndex] = o;
        o = factorQMulMont(a0, b1);
        o += factorQMulMont(a1, b0);
        coeffs[outIndex + 1] = o;
    }

    public short montgomeryReduce(int a) {
        short u = (short) (a * cryKeyMLKEM.KyberQinv);
        int t = u * cryKeyMLKEM.KyberQ;
        t = a - t;
        t >>= 16;
        return (short) t;
    }

    public short barretReduce(short a) {
        short t;
        long shift = (((long) 1) << 26);
        short v = (short) ((shift + (cryKeyMLKEM.KyberQ / 2)) / cryKeyMLKEM.KyberQ);
        t = (short) ((v * a) >> 26);
        t = (short) (t * cryKeyMLKEM.KyberQ);
        return (short) (a - t);
    }

    public short conditionalSubQ(short a) {
        a -= cryKeyMLKEM.KyberQ;
        a += (short) ((a >> 15) & cryKeyMLKEM.KyberQ);
        return a;
    }

}

class cryKeyMLKEMvec {

    public cryKeyMLKEMpoly[] vec;

    private final cryKeyMLKEM engine;

    public cryKeyMLKEMvec(cryKeyMLKEM ng) {
        engine = ng;
        vec = new cryKeyMLKEMpoly[engine.KyberK];
        for (int i = 0; i < engine.KyberK; i++) {
            vec[i] = new cryKeyMLKEMpoly(ng);
        }
    }

    public void polyVecNtt() {
        for (int i = 0; i < engine.KyberK; i++) {
            vec[i].polyNtt();
        }
    }

    public void polyVecInverseNttToMont() {
        for (int i = 0; i < engine.KyberK; i++) {
            vec[i].polyInverseNttToMont();
        }
    }

    public byte[] compressPolyVec() {
        conditionalSubQ();
        byte[] r = new byte[engine.KyberPolyVecCompressedBytes];
        int count = 0;
        if (engine.KyberPolyVecCompressedBytes == engine.KyberK * 320) {
            short[] t = new short[4];
            for (int i = 0; i < engine.KyberK; i++) {
                for (int j = 0; j < cryKeyMLKEM.KyberN / 4; j++) {
                    for (int k = 0; k < 4; k++) {
                        long t_k = vec[i].coeffs[4 * j + k];
                        t_k <<= 10;
                        t_k += 1665;
                        t_k *= 1290167;
                        t_k >>= 32;
                        t_k &= 0x3ff;
                        t[k] = (short) t_k;
                    }
                    r[count + 0] = (byte) (t[0] >> 0);
                    r[count + 1] = (byte) ((t[0] >> 8) | (t[1] << 2));
                    r[count + 2] = (byte) ((t[1] >> 6) | (t[2] << 4));
                    r[count + 3] = (byte) ((t[2] >> 4) | (t[3] << 6));
                    r[count + 4] = (byte) ((t[3] >> 2));
                    count += 5;
                }
            }
        } else {
            short[] t = new short[8];
            for (int i = 0; i < engine.KyberK; i++) {
                for (int j = 0; j < cryKeyMLKEM.KyberN / 8; j++) {
                    for (int k = 0; k < 8; k++) {
                        long t_k = vec[i].coeffs[8 * j + k];
                        t_k <<= 11;
                        t_k += 1664;
                        t_k *= 645084;
                        t_k >>= 31;
                        t_k &= 0x7ff;
                        t[k] = (short) t_k;
                    }
                    r[count + 0] = (byte) ((t[0] >> 0));
                    r[count + 1] = (byte) ((t[0] >> 8) | (t[1] << 3));
                    r[count + 2] = (byte) ((t[1] >> 5) | (t[2] << 6));
                    r[count + 3] = (byte) ((t[2] >> 2));
                    r[count + 4] = (byte) ((t[2] >> 10) | (t[3] << 1));
                    r[count + 5] = (byte) ((t[3] >> 7) | (t[4] << 4));
                    r[count + 6] = (byte) ((t[4] >> 4) | (t[5] << 7));
                    r[count + 7] = (byte) ((t[5] >> 1));
                    r[count + 8] = (byte) ((t[5] >> 9) | (t[6] << 2));
                    r[count + 9] = (byte) ((t[6] >> 6) | (t[7] << 5));
                    r[count + 10] = (byte) ((t[7] >> 3));
                    count += 11;
                }
            }
        }
        return r;
    }

    public void decompressPolyVec(byte[] compressedPolyVecCipherText) {
        int count = 0;
        if (engine.KyberPolyVecCompressedBytes == (engine.KyberK * 320)) {
            short[] t = new short[4];
            for (int i = 0; i < engine.KyberK; i++) {
                for (int j = 0; j < cryKeyMLKEM.KyberN / 4; j++) {
                    t[0] = (short) (((compressedPolyVecCipherText[count] & 0xFF) >> 0) | (short) ((compressedPolyVecCipherText[count + 1] & 0xFF) << 8));
                    t[1] = (short) (((compressedPolyVecCipherText[count + 1] & 0xFF) >> 2) | (short) ((compressedPolyVecCipherText[count + 2] & 0xFF) << 6));
                    t[2] = (short) (((compressedPolyVecCipherText[count + 2] & 0xFF) >> 4) | (short) ((compressedPolyVecCipherText[count + 3] & 0xFF) << 4));
                    t[3] = (short) (((compressedPolyVecCipherText[count + 3] & 0xFF) >> 6) | (short) ((compressedPolyVecCipherText[count + 4] & 0xFF) << 2));
                    count += 5;
                    for (int k = 0; k < 4; k++) {
                        vec[i].coeffs[4 * j + k] = (short) (((t[k] & 0x3FF) * cryKeyMLKEM.KyberQ + 512) >> 10);
                    }
                }
            }
        } else {
            short[] t = new short[8];
            for (int i = 0; i < engine.KyberK; i++) {
                for (int j = 0; j < cryKeyMLKEM.KyberN / 8; j++) {
                    t[0] = (short) (((compressedPolyVecCipherText[count] & 0xFF) >> 0) | ((short) (compressedPolyVecCipherText[count + 1] & 0xFF) << 8));
                    t[1] = (short) (((compressedPolyVecCipherText[count + 1] & 0xFF) >> 3) | ((short) (compressedPolyVecCipherText[count + 2] & 0xFF) << 5));
                    t[2] = (short) (((compressedPolyVecCipherText[count + 2] & 0xFF) >> 6) | ((short) (compressedPolyVecCipherText[count + 3] & 0xFF) << 2) | ((short) ((compressedPolyVecCipherText[count + 4] & 0xFF) << 10)));
                    t[3] = (short) (((compressedPolyVecCipherText[count + 4] & 0xFF) >> 1) | ((short) (compressedPolyVecCipherText[count + 5] & 0xFF) << 7));
                    t[4] = (short) (((compressedPolyVecCipherText[count + 5] & 0xFF) >> 4) | ((short) (compressedPolyVecCipherText[count + 6] & 0xFF) << 4));
                    t[5] = (short) (((compressedPolyVecCipherText[count + 6] & 0xFF) >> 7) | ((short) (compressedPolyVecCipherText[count + 7] & 0xFF) << 1) | ((short) ((compressedPolyVecCipherText[count + 8] & 0xFF) << 9)));
                    t[6] = (short) (((compressedPolyVecCipherText[count + 8] & 0xFF) >> 2) | ((short) (compressedPolyVecCipherText[count + 9] & 0xFF) << 6));
                    t[7] = (short) (((compressedPolyVecCipherText[count + 9] & 0xFF) >> 5) | ((short) (compressedPolyVecCipherText[count + 10] & 0xFF) << 3));
                    count += 11;
                    for (int k = 0; k < 8; k++) {
                        vec[i].coeffs[8 * j + k] = (short) (((t[k] & 0x7FF) * cryKeyMLKEM.KyberQ + 1024) >> 11);
                    }
                }
            }
        }
    }

    public void conditionalSubQ() {
        for (int i = 0; i < engine.KyberK; i++) {
            vec[i].conditionalSubQ();
        }
    }

    public void reducePoly() {
        for (int i = 0; i < engine.KyberK; i++) {
            vec[i].reduce();
        }
    }

    public void addPoly(cryKeyMLKEMvec b) {
        for (int i = 0; i < engine.KyberK; i++) {
            vec[i].addCoeffs(b.vec[i]);
        }
    }

    public byte[] toBytes() {
        byte[] r = new byte[engine.KyberPolyVecBytes];
        for (int i = 0; i < engine.KyberK; i++) {
            bits.byteCopy(vec[i].toBytes(), 0, r, i * cryKeyMLKEM.KyberPolyBytes, cryKeyMLKEM.KyberPolyBytes);
        }
        return r;
    }

    public void fromBytes(byte[] inputBytes) {
        for (int i = 0; i < engine.KyberK; i++) {
            byte[] buf = new byte[cryKeyMLKEM.KyberPolyBytes];
            bits.byteCopy(inputBytes, i * cryKeyMLKEM.KyberPolyBytes, buf, 0, buf.length);
            vec[i].fromBytes(buf);
        }
    }

}
