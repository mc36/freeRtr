package org.freertr.cry;

import org.freertr.pack.packHolder;
import org.freertr.util.bits;

/**
 * module lattice key exchange
 *
 * @author matecsaba
 */
class cryKeyML extends cryKeyGeneric {

    /**
     * create instance
     */
    public cryKeyML() {
    }

    /**
     * magic values
     */
    public final static int KyberN = 256;

    /**
     * magic values
     */
    public final static int KyberQ = 3329;

    /**
     * magic values
     */
    public final static int KyberQinv = 62209;

    /**
     * magic values
     */
    public final static int KyberSymBytes = 32;

    /**
     * magic values
     */
    public final static int KyberPolyBytes = 384;

    /**
     * magic values
     */
    public final static int KyberEta2 = 2;

    /**
     * magic values
     */
    public final int sessionKeyLength = 32;

    /**
     * magic values
     */
    public int KyberK;

    /**
     * magic values
     */
    public int KyberEta1;

    /**
     * magic values
     */
    public int KyberPolyVecBytes;

    /**
     * magic values
     */
    public int KyberPolyCompressedBytes;

    /**
     * magic values
     */
    public int KyberPolyVecCompressedBytes;

    /**
     * magic values
     */
    public int KyberIndCpaPublicKeyBytes;

    private byte[] common;

    private byte[] clntPriv;

    private byte[] clntPub;

    private byte[] servPriv;

    private byte[] servPub;

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
        cryMLpolyVec sp = new cryMLpolyVec(this);
        cryMLpolyVec publicKeyPolyVec = new cryMLpolyVec(this);
        cryMLpolyVec errorPolyVector = new cryMLpolyVec(this);
        cryMLpolyVec bp = new cryMLpolyVec(this);
        cryMLpolyVec[] aMatrixTranspose = new cryMLpolyVec[KyberK];
        cryMLpolyOne errorPoly = new cryMLpolyOne(this);
        cryMLpolyOne v = new cryMLpolyOne(this);
        cryMLpolyOne k = new cryMLpolyOne(this);
        byte[] seed = unpackPublicKey(publicKeyPolyVec, publicKeyInput);
        k.fromMsg(msg);
        for (int i = 0; i < KyberK; i++) {
            aMatrixTranspose[i] = new cryMLpolyVec(this);
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
        cryMLpolyVec bp = new cryMLpolyVec(this);
        cryMLpolyVec secretKeyPolyVec = new cryMLpolyVec(this);
        cryMLpolyOne v = new cryMLpolyOne(this);
        cryMLpolyOne mp = new cryMLpolyOne(this);
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
        cryMLpolyVec secretKey = new cryMLpolyVec(this);
        cryMLpolyVec publicKey = new cryMLpolyVec(this);
        cryMLpolyVec e = new cryMLpolyVec(this);
        byte[] buf = new byte[64];
        symmetricHashG(buf, bits.byteConcat(d, new byte[]{(byte) KyberK}));
        byte[] publicSeed = new byte[32];
        byte[] noiseSeed = new byte[32];
        bits.byteCopy(buf, 0, publicSeed, 0, 32);
        bits.byteCopy(buf, 32, noiseSeed, 0, 32);
        byte count = (byte) 0;
        cryMLpolyVec[] aMatrix = new cryMLpolyVec[KyberK];
        for (int i = 0; i < KyberK; i++) {
            aMatrix[i] = new cryMLpolyVec(this);
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
        return new byte[][]{packPublicKey(publicKey, publicSeed), packSecretKey(secretKey)};
    }

    private byte[] kemDecrypt(byte[] secretKey, byte[] cipherText) {
        return kemDecryptInternal(secretKey, cipherText);
    }

    private byte[][] kemEncrypt(byte[] publicKeyInput) {
        byte[] randBytes = new byte[32];
        for (int i = 0; i < randBytes.length; i++) {
            randBytes[i] = (byte) bits.randomB();
        }
        cryMLpolyVec polyVec = new cryMLpolyVec(this);
        byte[] seed = unpackPublicKey(polyVec, publicKeyInput);
        return kemEncryptInternal(publicKeyInput, randBytes);
    }

    private byte[][] generateKemKeyPair() {
        byte[] d = new byte[KyberSymBytes];
        for (int i = 0; i < d.length; i++) {
            d[i] = (byte) bits.randomB();
        }
        return kemKeygenInternal(d);
    }

    private byte[] packCipherText(cryMLpolyVec b, cryMLpolyOne v) {
        byte[] outBuf = new byte[KyberPolyVecCompressedBytes + KyberPolyCompressedBytes];
        bits.byteCopy(b.compressPolyVec(), 0, outBuf, 0, KyberPolyVecCompressedBytes);
        bits.byteCopy(v.compressPoly(), 0, outBuf, KyberPolyVecCompressedBytes, KyberPolyCompressedBytes);
        return outBuf;
    }

    private void unpackCipherText(cryMLpolyVec b, cryMLpolyOne v, byte[] cipherText) {
        byte[] compressedPolyVecCipherText = new byte[KyberPolyVecCompressedBytes];
        bits.byteCopy(cipherText, 0, compressedPolyVecCipherText, 0, compressedPolyVecCipherText.length);
        b.decompressPolyVec(compressedPolyVecCipherText);
        byte[] compressedPolyCipherText = new byte[cipherText.length - KyberPolyVecCompressedBytes];
        bits.byteCopy(cipherText, KyberPolyVecCompressedBytes, compressedPolyCipherText, 0, compressedPolyCipherText.length);
        v.decompressPoly(compressedPolyCipherText);
    }

    private byte[] packPublicKey(cryMLpolyVec publicKeyPolyVec, byte[] seed) {
        byte[] buf = new byte[KyberIndCpaPublicKeyBytes];
        bits.byteCopy(publicKeyPolyVec.toBytes(), 0, buf, 0, KyberPolyVecBytes);
        bits.byteCopy(seed, 0, buf, KyberPolyVecBytes, cryKeyML.KyberSymBytes);
        return buf;
    }

    private byte[] unpackPublicKey(cryMLpolyVec publicKeyPolyVec, byte[] publicKey) {
        byte[] outputSeed = new byte[cryKeyML.KyberSymBytes];
        publicKeyPolyVec.fromBytes(publicKey);
        bits.byteCopy(publicKey, KyberPolyVecBytes, outputSeed, 0, cryKeyML.KyberSymBytes);
        return outputSeed;
    }

    private byte[] packSecretKey(cryMLpolyVec secretKeyPolyVec) {
        return secretKeyPolyVec.toBytes();
    }

    private void unpackSecretKey(cryMLpolyVec secretKeyPolyVec, byte[] secretKey) {
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

    private void generateMatrix(cryMLpolyVec[] aMatrix, byte[] seed, boolean transposed) {
        int xofBlockBytes = 168;
        int matrixNBlocks = ((12 * cryKeyML.KyberN / 8 * (1 << 12) / cryKeyML.KyberQ + xofBlockBytes) / xofBlockBytes);
        byte[] buf = new byte[matrixNBlocks * xofBlockBytes + 2];
        cryHashSha3256 h = new cryHashSha3256();
        for (int i = 0; i < KyberK; i++) {
            for (int j = 0; j < KyberK; j++) {
                h.init();
                h.update(seed, 0, seed.length);
                if (transposed) {
                    h.update(new byte[]{(byte) i}, 0, 1);
                    h.update(new byte[]{(byte) j}, 0, 1);
                } else {
                    h.update(new byte[]{(byte) j}, 0, 1);
                    h.update(new byte[]{(byte) i}, 0, 1);
                }
                int outLen = matrixNBlocks * xofBlockBytes;
                for (int outOfs = 0; outOfs < outLen;) {
                    byte[] res = h.finish();
                    int o = res.length;
                    int p = outLen - outOfs;
                    if (o > p) {
                        o = p;
                    }
                    bits.byteCopy(res, 0, buf, outOfs, o);
                    outOfs += o;
                }
                aMatrix[i].vec[j].rejectionSampling(0, cryKeyML.KyberN, buf, outLen);
            }
        }
    }

    public String toString() {
        return "k=" + KyberK;
    }

    public String algName() {
        return "ml";
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
        return true;
    }

    public boolean keyMakeIke(int id) {
        return true;
    }

    public int keyMakeVal() {
        return KyberK;
    }

    public boolean keyVerify() {
        return false;
    }

    public int keySize() {
        return KyberPolyVecCompressedBytes;
    }

    public String keyDump() {
        return bits.byteDump(clntPriv, 0, -1) + " " + bits.byteDump(clntPub, 0, -1) + " " + bits.byteDump(servPriv, 0, -1) + " " + bits.byteDump(servPub, 0, -1);
    }

    public void keyClntInit() {
        byte[][] k = generateKemKeyPair();
        clntPub = k[0];
        clntPriv = k[1];
    }

    public void keyServInit() {
        byte[][] k = generateKemKeyPair();
        servPub = k[0];
        servPriv = k[1];
    }

    public void keyClntCalc() {
        byte[][] k = kemEncrypt(servPub);
        common = k[0];
        clntPub = k[1];
        common = kemDecrypt(clntPriv, servPub);
    }

    public void keyServCalc() {
        byte[][] k = kemEncrypt(clntPub);
        common = k[0];
        servPub = k[1];
        common = kemDecrypt(servPriv, clntPub);
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
        keyClntTls(buf,ofs);
        return false;
    }

    public boolean keyServSsh(byte[] buf, int ofs) {
        keyServTls(buf,ofs);
        return false;
    }

    public byte[] keyClntIke() {
        return clntPub;
    }

    public byte[] keyServIke() {
        return servPub;
    }

    public boolean keyClntIke(byte[] buf, int ofs) {
        keyClntTls(buf,ofs);
        return false;
    }

    public boolean keyServIke(byte[] buf, int ofs) {
        keyServTls(buf,ofs);
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

class cryMLpolyOne {

    public static final short[] nttZetas = new short[]{
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

    public static final short[] nttZetasInv = new short[]{
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

    private cryKeyML engine;

    public cryMLpolyOne(cryKeyML ng) {
        coeffs = new short[cryKeyML.KyberN];
        engine = ng;
    }

    public void polyNtt() {
        short[] r = new short[cryKeyML.KyberN];
        System.arraycopy(coeffs, 0, r, 0, r.length);
        int j;
        short t, zeta;
        int k = 1;
        for (int i = 128; i >= 2; i >>= 1) {
            for (int l = 0; l < 256; l = j + i) {
                zeta = nttZetas[k++];
                for (j = l; j < l + i; ++j) {
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
        short[] r = new short[cryKeyML.KyberN];
        System.arraycopy(coeffs, 0, r, 0, cryKeyML.KyberN);
        int j;
        short t, zeta;
        int k = 0;
        for (int i = 2; i <= 128; i <<= 1) {
            for (int l = 0; l < 256; l = j + i) {
                zeta = nttZetasInv[k++];
                for (j = l; j < l + i; ++j) {
                    t = r[j];
                    r[j] = barretReduce((short) (t + r[j + i]));
                    r[j + i] = (short) (t - r[j + i]);
                    r[j + i] = factorQMulMont(zeta, r[j + i]);
                }
            }
        }
        for (j = 0; j < 256; ++j) {
            r[j] = factorQMulMont(r[j], nttZetasInv[127]);
        }
        coeffs = r;
    }

    public void reduce() {
        for (int i = 0; i < cryKeyML.KyberN; i++) {
            coeffs[i] = barretReduce(coeffs[i]);
        }
    }

    public void pointwiseAccountMontgomery(cryMLpolyVec inp1, cryMLpolyVec inp2, cryKeyML engine) {
        cryMLpolyOne t = new cryMLpolyOne(engine);
        baseMultMontgomery(inp1.vec[0], inp2.vec[0]);
        for (int i = 1; i < engine.KyberK; i++) {
            t.baseMultMontgomery(inp1.vec[i], inp2.vec[i]);
            addCoeffs(t);
        }
        reduce();
    }

    public void baseMultMontgomery(cryMLpolyOne a, cryMLpolyOne b) {
        for (int i = 0; i < cryKeyML.KyberN / 4; i++) {
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

    public void addCoeffs(cryMLpolyOne b) {
        for (int i = 0; i < cryKeyML.KyberN; i++) {
            coeffs[i] = (short) (coeffs[i] + b.coeffs[i]);
        }
    }

    public void convertToMont() {
        final short f = (short) (((long) 1 << 32) % cryKeyML.KyberQ);
        for (int i = 0; i < cryKeyML.KyberN; i++) {
            coeffs[i] = montgomeryReduce(coeffs[i] * f);
        }
    }

    public byte[] compressPoly() {
        byte[] t = new byte[8];
        byte[] r = new byte[engine.KyberPolyCompressedBytes];
        int count = 0;
        conditionalSubQ();
        if (engine.KyberPolyCompressedBytes == 128) {
            for (int i = 0; i < cryKeyML.KyberN / 8; i++) {
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
            for (int i = 0; i < cryKeyML.KyberN / 8; i++) {
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
            for (int i = 0; i < cryKeyML.KyberN / 2; i++) {
                coeffs[2 * i + 0] = (short) ((((short) ((compressedPolyCipherText[count] & 0xFF) & 15) * cryKeyML.KyberQ) + 8) >> 4);
                coeffs[2 * i + 1] = (short) ((((short) ((compressedPolyCipherText[count] & 0xFF) >> 4) * cryKeyML.KyberQ) + 8) >> 4);
                count += 1;
            }
        } else {
            byte[] t = new byte[8];
            for (int i = 0; i < cryKeyML.KyberN / 8; i++) {
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
                    coeffs[8 * i + j] = (short) (((t[j] & 31) * cryKeyML.KyberQ + 16) >> 5);
                }
            }
        }
    }

    public byte[] toBytes() {
        byte[] r = new byte[cryKeyML.KyberPolyBytes];
        conditionalSubQ();
        for (int i = 0; i < cryKeyML.KyberN / 2; i++) {
            short t0 = coeffs[2 * i];
            short t1 = coeffs[2 * i + 1];
            r[3 * i] = (byte) (t0 >> 0);
            r[3 * i + 1] = (byte) ((t0 >> 8) | (t1 << 4));
            r[3 * i + 2] = (byte) (t1 >> 4);
        }
        return r;

    }

    public void fromBytes(byte[] inpBytes) {
        for (int i = 0; i < cryKeyML.KyberN / 2; i++) {
            coeffs[2 * i + 0] = (short) ((((inpBytes[3 * i + 0] & 0xFF) >> 0) | ((inpBytes[3 * i + 1] & 0xFF) << 8)) & 0xFFF);
            coeffs[2 * i + 1] = (short) ((((inpBytes[3 * i + 1] & 0xFF) >> 4) | (long) ((inpBytes[3 * i + 2] & 0xFF) << 4)) & 0xFFF);
        }
    }

    public byte[] toMsg() {
        int LOWER = cryKeyML.KyberQ >>> 2;
        int UPPER = cryKeyML.KyberQ - LOWER;
        byte[] outMsg = new byte[cryKeyML.KyberSymBytes];
        conditionalSubQ();
        for (int i = 0; i < cryKeyML.KyberN / 8; i++) {
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
        for (int i = 0; i < cryKeyML.KyberN / 8; i++) {
            for (int j = 0; j < 8; j++) {
                short mask = (short) ((-1) * (short) (((msg[i] & 0xFF) >> j) & 1));
                coeffs[8 * i + j] = (short) (mask & (short) ((cryKeyML.KyberQ + 1) / 2));
            }
        }
    }

    public void conditionalSubQ() {
        for (int i = 0; i < cryKeyML.KyberN; i++) {
            coeffs[i] = conditionalSubQ(coeffs[i]);
        }
    }

    public void symmetricPrf(byte[] out, byte[] seed, byte nonce) {
        cryHashSha3256 h = new cryHashSha3256();
        byte[] extSeed = new byte[seed.length + 1];
        bits.byteCopy(seed, 0, extSeed, 0, seed.length);
        extSeed[seed.length] = nonce;
        h.init();
        h.update(extSeed, 0, extSeed.length);
        byte[] buf = h.finish();
        bits.byteCopy(buf, 0, out, 0, buf.length);
    }

    public void getEta1Noise(byte[] seed, byte nonce) {
        byte[] buf = new byte[cryKeyML.KyberN * engine.KyberEta1 / 4];
        symmetricPrf(buf, seed, nonce);
        mlCBD(buf, engine.KyberEta1);
    }

    public void getEta2Noise(byte[] seed, byte nonce) {
        byte[] buf = new byte[cryKeyML.KyberN * cryKeyML.KyberEta2 / 4];
        symmetricPrf(buf, seed, nonce);
        mlCBD(buf, cryKeyML.KyberEta2);
    }

    public void polySubtract(cryMLpolyOne b) {
        for (int i = 0; i < cryKeyML.KyberN; i++) {
            coeffs[i] = (short) (b.coeffs[i] - coeffs[i]);
        }
    }

    public int rejectionSampling(int coeffOff, int len, byte[] inpBuf, int inpBufLen) {
        int ctr = 0;
        for (int pos = 0; (ctr < len) && (pos + 3 <= inpBufLen);) {
            short val0 = (short) (((((short) (inpBuf[pos] & 0xFF)) >> 0) | (((short) (inpBuf[pos + 1] & 0xFF)) << 8)) & 0xFFF);
            short val1 = (short) (((((short) (inpBuf[pos + 1] & 0xFF)) >> 4) | (((short) (inpBuf[pos + 2] & 0xFF)) << 4)) & 0xFFF);
            pos = pos + 3;
            if (val0 < (short) cryKeyML.KyberQ) {
                coeffs[coeffOff + ctr] = (short) val0;
                ctr++;
            }
            if (ctr < len && val1 < (short) cryKeyML.KyberQ) {
                coeffs[coeffOff + ctr] = (short) val1;
                ctr++;
            }
        }
        return ctr;
    }

    public void mlCBD(byte[] bytes, int eta) {
        if (eta == 3) {
            for (int i = 0; i < cryKeyML.KyberN / 4; i++) {
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
            for (int i = 0; i < cryKeyML.KyberN / 8; i++) {
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

    public static short factorQMulMont(short a, short b) {
        return montgomeryReduce((int) (a * b));
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

    public static short montgomeryReduce(int a) {
        int t;
        short u;
        u = (short) (a * cryKeyML.KyberQinv);
        t = (int) (u * cryKeyML.KyberQ);
        t = a - t;
        t >>= 16;
        return (short) t;
    }

    public static short barretReduce(short a) {
        short t;
        long shift = (((long) 1) << 26);
        short v = (short) ((shift + (cryKeyML.KyberQ / 2)) / cryKeyML.KyberQ);
        t = (short) ((v * a) >> 26);
        t = (short) (t * cryKeyML.KyberQ);
        return (short) (a - t);
    }

    public static short conditionalSubQ(short a) {
        a -= cryKeyML.KyberQ;
        a += (a >> 15) & cryKeyML.KyberQ;
        return a;
    }

}

class cryMLpolyVec {

    public cryMLpolyOne[] vec;

    private cryKeyML engine;

    public cryMLpolyVec(cryKeyML ng) {
        engine = ng;
        vec = new cryMLpolyOne[engine.KyberK];
        for (int i = 0; i < engine.KyberK; i++) {
            vec[i] = new cryMLpolyOne(ng);
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
                for (int j = 0; j < cryKeyML.KyberN / 4; j++) {
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
                for (int j = 0; j < cryKeyML.KyberN / 8; j++) {
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
                for (int j = 0; j < cryKeyML.KyberN / 4; j++) {
                    t[0] = (short) (((compressedPolyVecCipherText[count] & 0xFF) >> 0) | (short) ((compressedPolyVecCipherText[count + 1] & 0xFF) << 8));
                    t[1] = (short) (((compressedPolyVecCipherText[count + 1] & 0xFF) >> 2) | (short) ((compressedPolyVecCipherText[count + 2] & 0xFF) << 6));
                    t[2] = (short) (((compressedPolyVecCipherText[count + 2] & 0xFF) >> 4) | (short) ((compressedPolyVecCipherText[count + 3] & 0xFF) << 4));
                    t[3] = (short) (((compressedPolyVecCipherText[count + 3] & 0xFF) >> 6) | (short) ((compressedPolyVecCipherText[count + 4] & 0xFF) << 2));
                    count += 5;
                    for (int k = 0; k < 4; k++) {
                        vec[i].coeffs[4 * j + k] = (short) (((t[k] & 0x3FF) * cryKeyML.KyberQ + 512) >> 10);
                    }
                }
            }
        } else {
            short[] t = new short[8];
            for (int i = 0; i < engine.KyberK; i++) {
                for (int j = 0; j < cryKeyML.KyberN / 8; j++) {
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
                        vec[i].coeffs[8 * j + k] = (short) (((t[k] & 0x7FF) * cryKeyML.KyberQ + 1024) >> 11);
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

    public void addPoly(cryMLpolyVec b) {
        for (int i = 0; i < engine.KyberK; i++) {
            vec[i].addCoeffs(b.vec[i]);
        }
    }

    public byte[] toBytes() {
        byte[] r = new byte[engine.KyberPolyVecBytes];
        for (int i = 0; i < engine.KyberK; i++) {
            bits.byteCopy(vec[i].toBytes(), 0, r, i * cryKeyML.KyberPolyBytes, cryKeyML.KyberPolyBytes);
        }
        return r;
    }

    public void fromBytes(byte[] inputBytes) {
        for (int i = 0; i < engine.KyberK; i++) {
            byte[] buf = new byte[cryKeyML.KyberPolyBytes];
            bits.byteCopy(inputBytes, i * cryKeyML.KyberPolyBytes, buf, 0, buf.length);
            vec[i].fromBytes(buf);
        }
    }

}







/*
class MLKEMtest {

    public cryKeyML mlkemParams;

    public MLKEMPublicKeyParameters pub;

    public MLKEMPrivateKeyParameters priv;

    public void init(cryKeyML param) {
        this.mlkemParams = param;
    }

    public byte[][] generateEncapsulated(MLKEMPublicKeyParameters key) {
        byte[][] kemEncrypt = mlkemParams.kemEncrypt(key.getEncoded());
        return kemEncrypt;
    }

    public byte[] extractSecret(MLKEMPrivateKeyParameters key, byte[] encapsulation) {
        return mlkemParams.kemDecrypt(key.getEncoded(), encapsulation);
    }

    public void generateKeyPair() {
        byte[][] keyPair = mlkemParams.generateKemKeyPair();
        pub = new MLKEMPublicKeyParameters(mlkemParams, keyPair[0], keyPair[1]);
        priv = new MLKEMPrivateKeyParameters(mlkemParams, keyPair[2], keyPair[3], keyPair[4], keyPair[0], keyPair[1]);
    }

    public static byte[] fromHex(String a) {
        byte[] buf = new byte[a.length() / 2];
        for (int i = 0; i < buf.length; i++) {
            buf[i] = (byte) bits.fromHex(a.substring(i * 2, i * 2 + 2));
        }
        return buf;
    }

    public static byte[] copyOfRange(byte[] original, int from, int to) {
        byte[] copy = new byte[to - from];
        System.arraycopy(original, from, copy, 0, copy.length);
        return copy;
    }

    public void main(String[] args) throws Exception {
        byte[][] secretEncap;
        byte[] sharedSecret;

        init(cryKeyML.mlkem768);
        generateKeyPair();
        secretEncap = generateEncapsulated(pub);
        sharedSecret = extractSecret(priv, secretEncap[1]);
        System.out.println(bits.toHex(secretEncap[0]).compareTo(bits.toHex(sharedSecret)));

        init(cryKeyML.mlkem512);
        priv = new MLKEMPrivateKeyParameters(mlkemParams, fromHex("C0E30CD83BB723EB78F9E8374BFCA1CCFA37972A138CB406D6B20D909BBFDB98C270B77828397DA1DCA3AD9635366CCA843AB003896F819888120649CFD30991543A447ABC89561626509CD805C1AEEA9A0E5B4785F57F6F4B42953149ED139DE2A4297D413414502249CC208579456DE19FF381AA61015F8800C7938CA98B504E59F9B29028AFFD377E6C044FBF7461E21107A9E39932C6356B962B55603C4E2B90C8759B8CA68BAFF62FE363304D79807BF5464A597A9EA7B269FA5B9D5A2C9DBA3002916EB6813680703A1D9B99821B77CC3831A393C33CEA3EE0643618D559DA8954A0731737476FEB95811BD24C1BE38A88EBA55AA98F7379B74AD052E2C262F904A18996AE5C04CEA94B187EB003BCB50728758A80C67A70E3CEAC417F6422AB7668440B7213B0B9290B031B7C8B4F9CB75CB6267129E1C9AF440B5B33933E860DCC89A6ADB46B0DA643A95310E8B4928505BC7AB5BC1FDA88F9085BFEA0526610247EFC23C11A07E7EC9DD3BC368C215365032295455989537C75F1BE137424FA022BCACA9CC93325DADC6892295C12207D3E678D187436600648788290288796AF5586D24AC7B763408017B7029A1ED5C71347A8CE933C57A729195FDAB2D1AABCBC5CC8A072A1F0B78ED277B6E8A324F4D78D690022057484C0EC21657BAA823A296B3822B84B21756727DD3859FE40597329C0C672B907DCBC079346DBFAC81C816CE582C541E6C008B5548C238AC328AAD92034F00278B49895D2275C4BF685170C3536040E0DBBCFB5FA7611A5886099B41B70338DA9B2707C15B2E05D79164B6F041852EABC0E129845B56DB47899383053D7211F1672410D6940DC0329A79A301F0A3717151CB372BBBC6616A87B7C4508AD24169A6C9A5D8E7B6F3A3A2F5FFB4223BA5378F13DBA235A7A34AF08D3A9FEABAF2DA9041CA600B5907D79B268FDA68610AB2271607E9033B39DF28D52C17DD3A3CA2B334BDD557FA9A881DC31B28C56483FD86EC61CCBBB9345303B6EF1D699F6325DC258CAC1548BE779018CC3A5BC45737180CEA64231F38B0113B08A6344577D69677100C5B0E511F2E3C1B53634B585059FFC2573C20107C84F5F1C1744D2AC22FC14D1F92D87EC15A27187F49BC2CD66972D8B358E5B646934161C7A6563AC261DA8959562CC3EA9A7941BB75D72A54C82C248A8909A482F9A195644427A04D76778744317F51C473C2B91E392740BC539A1C8DEE4696EEC0DB4B3C39383646507714A9A73DAE01E9126C1E5AB4ADC3B52A7B1B31E7BB19F922984606D02B46621097E83361A7188750C7507E546165A551FAAF660CF9453FB367B701043C3C05F5447CDBCF96A47B0887FE77B82F0B31E784220187F738B1BDA106B75D341D0A194D78A8D5E9157A862B9AAC6564F9744567CBB5D7A49FF554357AC564DEA272C1CC10E2410148B8C0D0090AB322A252816677005888C9E107838CCA0ADB6900D058A31F4BB4C6E62526C84CB77E3BFC8F967DD44BA5A242901D4B9F72881C2378E7F0013E91B17BBC9B468615563B1089592CC88A179A88356A349A5D8F006078C825A3B6F7C0184C907ADCD5C7E5F0ACA6197B5D6F10CEE34CA9E5254C6083A5CC39A2753C7DEB603A09352F1691FCC6C2188D586B2E5A15548BE4AC47D2390551A76633C1B1CE7709E7840A125A313DFC39B982B6A20BBBE9ED41C1CEB6881434FD71AA1C58B5408E540C5E68960976612920B6BF81E64D39B005788D4C0695EA7AEDFF8B258E438BAC600EA4B90F74B659312610C12748C4C0CFCF6C4B443AA30697F857497C21C55101C38A789AF5E06B2585390F00C2A712C490DE45BEF7B58BAD1AE10F0161BC84A2B03BA1B3974C9C0CBFA6876E892730FDA4086E80EB3D57F00A5422083999AECB46080588926AF21553DF60005FC7C368EF45722DA9676CB0E33D23BB63031E966A88ED10AB247AAB04A9AF8A57E5D039C14879973A04BFAB5700889A2F2957C2E87C99DB3C98B53212AD49F7B013818F7CB4FF7C9503302ABD259A84311BC0635DC2A2865C2B5454C3BED8A0A37F776E52CB16AC98A4071B529A05392EA259E653439335168A6C22F1A1AB7C351660B1AB62213C0682F002217E7A85D1C86CB42447D1B7B63707709DBE72C81145354A422E882CD100A69FD961B497F3876C842570D3AFEFA002F4CD5959DAE0A0C497873D39D1DCCE42EE18026C8B7969D8B174149AED20A800D295F385EEBCC40364975510EA8598FA1F6EDDC170B0FDFE3A88BEEFFF8A8E515E1C0389781CA859D711EC5D957F5D5AC"));
        sharedSecret = extractSecret(priv, fromHex("AF9A75A80CA59C0B62079E0D51D4008891DC8E89396112007B751C6360386C60473FAF9DCCFE3EDA110F3C3ACC3537B352FB7317B4EEC825CCD74F8283F83B9120EE4DBB2D3D679B68D29827100E1D11731F13F5A446CFCA56320898906C3676370DC57A8BA3DABE8210BFF992C4A296BBB6461A70B77E3A76026A7F98D241CA0386BEC24483F499478380044E330E5C517DC4F35576A9A95A92DB21886AA21439FF5CA476EF03033FA938554E874333483649C9DC19A3289B3BA42E9C250BA57CDDF0477123B7F5FCF5E22AC0C90AD2AF5FEBF65B12CD6135655CBC3F56FAC7040944389ADC0B42CF54702CA89F9FBD8FA7EB01159FBD7D90CA562DE3DA056615AD8CF95B3880261A200EFC69DF92BD010C81E867DBAC17FFA5DC55D9C3C3493FBC418AB5045E06B3675B3519058DF1A83C81AA36386889D002D8365D77A909334329B727EE069CD2A97E8A27C031B5A2E6A029798990C089207BED80EA181F74652057D217AA4C46B4580F9E26DA7F3A2764E264BBD8B6A8FC80BC9736B3517035E7015E0A3CF8FEC2111A3A21686663772E659F4948AAE9417A84D4C71C72B0A1BA72510A38141B3CC0879CD04BF7DA4B48D7E25248B62B292DA07498DC25CA7A79EB3855F4A6842835F78721CA54299DBCD2699D57EC85D07932D37AACA5D0795A3A0AEF24751A5005984FCCDF0C13C7ADF95B75C5B0FB37E26A5D77B71BF4366CBC67756CDB4792514B127F191926E2E58DDEFAF09E9C8FDB15159E5791321CCE1DA58742130B9C4D55D262970FE49672E6F458DD6E057A9D4B2EA897D2CB6980C33D2894F1075B5C3ED4BD559A92A50FCF4B6F393DBAE1183DCCE4FB825DCF0112D6D9627CE6C44B7B7573360FCA6DE7296A1C8CF816463A8FEDE8023849CAAD8862AF922EA382F4CA9CE9C5EBA2B0DAC89750CF33C6C607D3A7CECD4B92E6517BDD4DB5B2E29C46413E08974360475B344C6610F174E9999DABBCFBBB6FB884DD6FF1E3F792E3FFCEE0DEC92FEED391769B634A6977B71A1CFA8640B44C93805F92C14754357379B02E0F51F69343DF5FD3787147D9AC97C2FDF2A663"));
        System.out.println("2546B38AE3250161D8C028791B980E1E509EA558E8DD7ADE419C25D18592F3EC".compareTo(bits.toHex(sharedSecret).toUpperCase()));

        init(cryKeyML.mlkem768);
        priv = new MLKEMPrivateKeyParameters(mlkemParams, fromHex("5B22B9A4D43DCA48B8018AAFA3BC0B37D6BF5199BDC730472231A0FE17707CF77D8B2BBFD0504F18329F258BAF5075B20CD33C6D306B1E567ECD386DBC026B3E572EBDE849B163354CE47A6816C735F090DB4BA9C4C44658EA75C27BC5DAA902B87C37FC420EBB93C4DB050DDD247F2B4197A3F7324FABB5C300B05AA3A688D5612449C8A0217E13D91AFE15A874C99D7B48AC6C62297F979577DB15005013D4FA0968E18DC4B56D9C85A268F775EC1784D1A26E1828078CD13091868B51847E002A9209613010F0279BA15014E256DF88092056BC989889C78839EB30CE58AC37F1299DDAC539D2C687A7A760F41A836DDA2E94AA0E62AA96EF41557AA41A8A668DD25648CA5789F4AC3591A14481A84005501002091CAE0C11DEB9365D30AACC25413FC72CE1AA6E084AA01A15929F530F8528306CF249076A4C76939089E31D5251AF6AF6A4DCA8163A91BE4811A424DBCE26762A69D3AA9A988E9814CB49D808371088755A253E34534219243C058510DC740C35B11153B91EE12FB754CF41ACC12231659433908228A9A1D5815A1B8A38E7B0D2012285BC60D3E009477C8830CA069DB98239ACBC732132C1C238D8B51D2A37BCF2D16292430A27273FA6DC9711F1BCF152B2127662D1594307093CA08BA4DDB15E6927230BD2059BBCAAC3438FE4902A2E3816F5CB40C119C9FB071CE30B2051AA3960BB7F82066217A90A5F4649FA1ACD70D37469FBA8449C908F58A7BE4BC9EE34C1AFC1467E99A9C0C99C10976E5BD36B1ED78BF693543E06C233BA53337810E373B343BA82898675A2A60B7F934131C87FE7545E60784E64542730C89B2A51451C8B2906F94C32D86C3574C40C97C0B52C419386A82E7CB327191AF724BA668A4B3D7504D30376EE7A97608CB3D72521948501C59CCB3DF78D93797EDBE83E53570E75611129659E5DE5AE63F85B2D0617350691C3014FABB404467846161C230C275FB8AA4B45754E5B806BE8DC70BC59924FEA2015AB02B8447D0301AB01B15835E70991369AD6B227F952BB19804148F5498068517C701D4508625629BB4DA825A0F5CAE292580AD81D72F2B1146BA20BCC85D332850437B624D9372F5C341FEB53D0F375AD11C7DA81029A6052CA2B2A1252533BE952CC7A785BDB05B9F306C02586D046163EC08FEC905590F58F873480A36B5B9240916F6B495C5344324BC58FE0164A126E70F4BC4A71BD58D554143C7E21D9917CC4CC88213B8A2771E75B6145FB8D980683D056C1A7E7926E932F30119063B0A0FA471A1B8B57A4D63C8FF01253871CAC33BD6938ADDBCC85B58614EFEC6294603FD4261C43D881CAD762187289AA1643D64A2BB3B230EE5905B963CCAAD314C9BB90B578A1D4C73DA6F43248125F2DF21CC1A01A1EB653D0F35E271013937211700364175C2459F10C122AC377BB39A3479466B3275BF9406B59249ECA2DD82C40C5407696508254174406A51BFC5C24D0AC22D4FB78ABB346196B24468237792C0852D865949302FC781C9A526BD59B0A361A21778448BDEB22BAE089D6E2C55B850DADF6A36752C6D69046B51063EB4A00E05C8438021D6935AC084605FDE14261884A690B986EDBC88E70976EB9790B203B9D8A2A785AAE5C57BD4F0389C1242AB1A7452D62C83B11B3AB9880D88C418B0CC8183AAFA2EB43A1FA35954B10FADC87E66794A3EAA2355AC06A41BC91C1C82AD8C67D4651B63989E6965F310C2AF85139F525B8C34A3DBD09BEB7E9C4F75240B165ABE8F5B14F850B50A80227EA13A5B640A229C3AE4B4EBC3082F4D567BEAA59D0847BAC70AA15349CBBA08248F1025908BEE5D6CF2890B19C835851663D155213C235A2D75867681A8FC51307D4718763F7CA6C5B1BADA090CDC08E93A117314457986CBA5E13CB7DAB69AB42AA0614BECB829282B2493CA95EF26C1B35613D4F82B6D3966E3175264F5BC763498E85B23FBE3339BE4B8DB8A56CCAF89BE9E212A024BD7E2C3B5BC5A11B193F175683BF5C934A1A51C33A66FE0131E3147F6288C405090E333B6D48B31723C7C0DA5B9872085DCF179EA7E5468850426986822037564D2A716B4A4355134D0F480B528C0C9ECCAD17CA537A718CD8759135B950092A84815BA08ABC1CFE95058F1B0BE9424ABE33B8E4F720D73A764E737AEFEB8393FC940814C0C7A2CA653CBDD7D57B988B3FD3C48B33BC6D5F458305E5B9BD677A8AD25239B7355255BB7487925084B1CB4C54888AC7362B871868723343698511BB441A9FEAA17F0993A2574104093CC20764AA3B072F1FB043C653A85E0263E9A654621843B3F0AB6212885C2A4261970C12A24C6252619B517D11D1B3229A5EDF934E8EA965F379CD12C91C6D7957DA28441D85A8A4B87475D028310B2AEE82720D79564A3B83D65833F0E25796B5C6C18B2ED00A97DCF42085FC43E946974E23AADE3B275B37B289E363DF595B5E83BEC5016EAE234A5B57C7271BCF3FCA1CFDE25202580E32EA00D2C4838EE027D9A74DD5B9C3CCB16334807C2EE18906A056E6332C37939474B6BB9F8286BDF825F129B31B57700BC63096BA8FAEA10FC47456311480EC097DFBD6324F57BCD6C03FDA7BBF3157403557ACFF9A240EC5B093A7973F6663620576FC8ABDD8D8B6E639A83D79A24BE11E0575737BA1B578F423B901C1DEE04B4D0604B730AB317794EC8B895E14205AB9C50AF2AEAD0AB1D1F51725047B6C34A0A036C8D9106134D2A8A132C88CBC53BE4916DE229A87D77826A7119259A219EC7A0388537DF35EC6B32174181985525D09A76E90825464C766774530681B7E4A789A287772B5FC0791C330D677226F094F99C63E8211926C483718C4670AE08222A82E9E6151286CAAE2A87AAAF7383FFA424F268192B476A0C232DBC711EED07187B8A702A34CC9490909D672B1FA1B81323758A6209C4086D6E53F73D657B8887950F835664B7A69717625908E8908ABF23699274BCA3E9B0EEBEA8D31040E5EEB27F6DA13E3A09DB663B1E456A42A43409E76331DF81583047B519151AC415FAEC6AEA2E7B0F3C1ACE907562633B3449209E6BBBC6B21C9C636CB2BD15F63950DDF6B99E002CAF038C715E4A0DDBC417080A26ACA4764E293324A013D8C7F0E78C52C22BB6C59CD42967AF8405AFD558D1A332041040EC0B94F2EA45F6FD53B15E21C925CABECB02485CB4EE741ADD6F6BA484351821CC9D4B2CF28324949846C2ADC0F12E27F5C6982DD325743FB4BAAB3F722BA20E1CE0840DBA77965BB3B6C9E8E6FEF46BA7EAB8F6D8987E230224E11581E7E0693F9A88BCF7A5F2E3CA2C6B66F57E3909C77C1C12286205B726B3FC90F1E84AF8EF30005D347ADA26361E2B03383F851DD72504D5815274350C3"));
        sharedSecret = extractSecret(priv, fromHex("A2A8B6B1BC8AE1583AC59EAA93B3FC083929646FDCCC42716EAEBAEDFC9E245F1CDF8C20467583358E93D66CDC86949D4746547C60C22BDDDB65D0D7DE7B2D66B96BCCAA64BAAD8C4AE841AEBDD260D5A19C4B0ABADAD9F8F5B5E74900F9BE7DF61B6CC1AE79B93186BA00FB61F11BE0EE444A4092ABF400178F006503E3A2D213B1D61928FF8955E6009B8E4380F7EEA26B1F13C770279EB7A19304E5D29404BDBC0F75BE6E0E04D9DD9B06D94B03921730D4DB4FD7FE68A8EB1A8804C5D2D7F3363139A04E9C12C9A5A4A0A476050332AF325845AE2A1A9DF8A2A31BA06123FC7BB7C52E03D685B5275B56B871AEF0313CCA4C25EB99CBE1F4A7310EF041DA9B95E85EF82B4C4E8C7EBD092B44AA75192368509E628B2EA72C2D0515BAA92C8C8E1B448E2ED787D09AB08C978473894BA31EABF23D82A613F12C0056C8A0B491BF0D6AB096B3F852A66B91941D86899D94E066C2C0B3C80638BE83E80DDF841B8F99AD886F61066A87728FBE23163F30B815562A287D783DD691B6E9F64F61F6BE9A2D670BF83D4CDDB826A760B5D5D89F6E7F61CFAC72317949FCB30978E9C0E33998FE068D91D80E29A6421A78688B5F6F78AB6F988839419C5A9653E723D9C938E291A8C20FEA8E7EC4B15C0D74044885047CADD10127829DCEE2950B940A32B932CCE8C4587CAB8993617E1307929754B6D61712574B28BC4D9AE7AD6471C4C42991098F0AC5F10E2ED3CBD3B3E55AC977A7FBD9EE3648B0BEDDB3D8BD0A03B33E119C971C4CABA608E4359C7DBE4D9FB85DAA5388CE137188CADE0C5FCCA8A9D571FBBC2F52891A028329C891F7747112D573C02883DDBB9194DF26290CB141D7C7EAF68E4431A6AA87093AF05433EB69E67289FC9524E83A54DA7636D0DCB9F993CD936FF992FBC2FB6C0C8734E31A6FF7FF00784CD3EB7934BC59139F1961423278DFBCA7C21812C39AECEFA50DDCEF52B69A8AE02FD04C9E51D79FEF2287F9397F484C4F42E3D5A38A0A6AA9984D111106211E5D2B613D64C7AF609C5612975BB33D1B4BA6C438512DF53254911CAAC2F836233ABBB037D0B070ADE39661471ACEC5D5240AEFF194EEEFAFBFB00BC23BD16CAB054FEB909EC87E152C84DAC583504246AAC53DB0EDD03610DF641C3DA83A6707C83B83E1E9665D4BBF5CB7011C5BE74A0C80D8CD4D245083EC9C751379BCA7B1D45A0BD7A67D1A26348E58FD88CD7379FE2F0B0C0472347DDD25FC2F1131CEC2B31333067966091407C65E8C9D72FAB57BF7DE441B22894099A5A11D593AA06777B8362550675C24B374BC259917FEC9C4C5150E21F3BCCF419789A95E3D5D0D982D642FF737EE5EF12CF24D96E1F64F06473690FD8A7DEBA523FB5CB60FC46D1B80D6E847442B7D7B4B3F8E7E73B1E361E1BCE5E8FF1C80888DB1199D092213A4C799D4553C68667373E5E5ED32EF8C153B02A89D41375E229AB80ABA6823BEE9585AAA1C6742260A7451378D31372041DABAD3AF9309C33C7DFA7092E09933C478FE07632AFE10"));
        System.out.println("B3958C785AA2B435386B19CE30DDF7E56A928EDBC974619A284F8019D063ACFC".compareTo(bits.toHex(sharedSecret).toUpperCase()));

        init(cryKeyML.mlkem1024);
        priv = new MLKEMPrivateKeyParameters(mlkemParams, fromHex("FFE93F3CC892EAE8BE5DA54475079A3AD9118457ACAABB67A3868A9CB7392C94B243A02A1C1A5F8E50325F226ADC091BB6E6A4CD32C459C8B4CD84B08203197D685FA7370176550FDF998708608A13217F6915087A7790474A61B3770793D8990E60BBC3D841E07BC0E6D20A1E86A7EC2C34E831CEE910896D37C5A099AFF10B180A00830007A8E341214713075105069AC58BDE92308ADB94F28361F9019688330B73DB7C81E86FD22743DC4A7C155C7AD3276A9CF654A086C1DAC4B516470FAC8299308B1DD33180173738AB4C9801D731DA223F6A48C0B68A45B5231425D24037245E856689534898E92651BC229F84C8B1C5BB24D102683D56A7A5DB3F172558F72513D68838B1075328F80C4F58384E680C05D174CD23CB3BB14449025F9210ADCC9BC32FD45D69A2C096400FD832942E97C45F5262EBE22F489716450AA7082A37E9CCC2D79C85C2AAA06D09B96185641426AEF44A83247945E2408F85F1720640208DB23984657ABD28A958112EAAF014E0D32B13B05EDF9692DA272AF080ABBAEA4FFDB9BD21F1C73D9C9EE7F9033D958331B428F6EB450E2647A9F362E9782A180030017C2F0E5B901F762F9FA1BC9EC4842B98A8DB8B843FC8831C468ED060978D9CA488D23EFE248F1E1492965C6B67708EAC5069ACD5B52601897DA3CB22E25A4A98C32C25B03F1B8E39F01801A04C5E762DC0FBC6B3D2A1AD187D525B748C58A05611428E0AAA54C63959C20237C832A8F97079C3A142CA6DCFC38069A9C2250AAEA97A9C11D97A6CF433C786963E30A3EFA64051269B1156672BB87ACC470968971B34437A415434BB4C6C22EB936E512AE2D0AD7BA31E5FACABEFBB7799E7A9F8397369426334639821F70295C96F0B41768E72671EC69D77EAC71247A02183410698BC9538804E53B431F743137229B740BA2E3946771533F8B4B2C4C66C3E1496AB44C1D8A1B01A50C288AC43F0AAAF8E55B2A0925209983CA7A38DBF7BACD2D1AA38155A8A039039F4511014A3197AA08B579575B458D76CC8E0D1B729BC8E0F5081398B9C981602B715A4B1E03DD169406CF0C06007B54CB36B450345F1959F4D9BA1193CC254AC4D65E70A24C720288401796251F4B19475A2355109ACC1002F9D119540C39CA5CCA3B864354631AE0255B7F85C685A6B3BF4201A67C9AA09301E54218FEFA8B088ACB1E687B1F1A400356714A792C9E67675D0599BFF490570332608098A6FC62DF1843A4452683A81A775E2374547AFA2C56A5EE2054739B920993FDBD16F437809D98284753B3669961D5AFA9D69C53D2F5876BC3CCC7D400DF204CC1752A68E809725E16088ABB9569B83A08A4A9A555DE3891DA2282F8CD97B55290AFB214CE559768F4B1B5053C97F41AA4FA1616A8701242025B846225B0C4F17384C28BA673A568563E1C972404364EB93672A7D00C609628B344DC53402CA894C88B2C7E529B33A73E3523B8DB53FB324A04F4A6DF0AB0D2319A1A8F840E73377C32564D009B8FE592D35644DA664A2D58306E25278A817C8BF9C6BCAF89320682E7533C7ECA8CE4F88A1B793BD13BA67520C3343210B29225FC605CE53C858D9E659002A17B777BCED6703FC262DB4F7558151CAA11A6D5EE4B6F8A47C5E6591D0DBCFBE362F1EA8A21238658C995C97BC4E19D0A8310B7D187A1AEFB9320ED8CD9F0866E8490981465BFE7C4E58E35457C7A24DA9CA565620427482182B3319F33F36C43F0BC9337C5C1CBBA68B10613123E6729CA9C34250CCAE36A7A246911BA1697614C5540B6505D67396C69E50444EB03AC7D8B1ABF2D99C0FF95A75531D5FC61D2E420A26EC3B8E774D652227E232C7C4D056E55066008D423C6A1C6BCAC1B63917E7126DAAB6C782285ED81AC4A837A9310405CD926A30D08E962C58DE13A9786BC31E8759461BAF3FF28371F5926E58687067C4C6C2836E427FE06483D9A970B8808277A77234D888CC1B666A7907DA0A6E3385CCC9F67D42988E4293737A068A80A8B5BCF12FB95B91DE329893180C8769ACDE0736D2A1A19FAAB6935B74C622599D2A789D0698798660C1566B9CF52EFDBC97C925A642F26FE47C5DC40A541569A55E22168164A8C8542B2DE392FFEA941EA720524263536A182616CD82B5CD99DC3E4F5C81451531E0C090FF7263F1A44F83C36FFEB870163181855521C8A3A0DCC42B4508849EB251BADC94ED063AB6A1CD5CB1BD2F0129E3423DE150AF1F9B5BDA68C51A0C574D538968F8BA11C14ECC1C2F546227C2E1AFDB578173388A7442032CB8A8E606094E876A4925229E30BA1BBC1439359F9E2822B7BCC3B7346D4B8348C3862ADDD4475315350981B297DC017D8B9B4F10336869C356E4BC21702949918B8E7B1AF171733D1190A261688CE283C13B19A4FB2F2B0125D827869BC70A4E9004B4C1C99DA32288F0A2C55B1D73CB4282379391040046474881551874E06653E23E8118AB9708A047190CF9AA0B390837BE1CBCDD22119024AC7CD99CF542C4487A3107133AE853C8AB4B036C1288A0E358A5B3592448A67F8912DBD12378114305B97D7435349C579F89C61FC1CC8673E16C50056DAE3A2392DC6F90875654E91C55E69D4F48A53C76686F5A0B79697246F52FAAA02714609DA1D9A89D6C1E8E6B9198B992E878C0B6B3153D81AB33A95D65BA4E3AC739A5BB738528CF68311DFAE6379DA9B2F25410DAD65ECDEC0E6D7A77AD9376A90435E2586BEF78A6A8BCBBA30C46F41CB8365014B34935F8090971859893B1830AE62F2CE21AD45A2EB47B96DF455866E4065332617E823DC0F62EEFE47FB6973B3D280DB8937D77851FE9B526D1936F5495770899895A1C1E81E935ACA7032C91B5A7E59ABA4A3C3B3434F4F57791C2B9E5856AFA3B9412829C2C0A1FE2E509CDC37AA008A8DCBC38EB8ABE1F8BC817E8802B116C09F34245462F80D631FBB1234FBA54AED632DCB695A7E92CA1A81BD64C257DE6801547180CFA8D3E595B26C0666A418E92054987573881633DE50A04F44579C7B74DC113064116512D5119A03858F369CD00FD2624E9192F78AF56F3A34BF1B2C1DA7526CBA7ABCA0A4451698A8556C42C1E2E885BAFE4912B86A109F8AE463A7F75862B9D841BFBB921094BBDF7342878157331355EFC2CA462192A99E7AC795CB313D732B89C248BB0476354C805775B1172BD79B5404EABBEF2C10333C8C933A5BE8C6B25586ABA9F39229C3BBC424AC4BB4448D9301E899020BB536995A6AB30C95B3A255D31E33F0DB53DE9EC654CC11635A44669B752DE660AB895CB2C2A6977A8864BA69CBD22019E86B561828894B55AC658B210263CD41BC870F5B82353495DE9048986CBC4F05E3F3A2620C822D3A4A30E86251A5338B8D1764C152DD55C85A007B369B010C099B62B75316C7B777A68B4B8C72242491107215EEDF76030C986F4E94A3E18269338C49327BA4895B70D113553D99B30F84DAB5A6F06BB7C4F0A1BC50A6E4403979E7CAC892898AABC7AA689395D279B7E7C705A5506784A1FB1227E3D08B42A955C24CA261487B555038958B714B51A8B5C117CA40201D69941485219D307773F24C04ED57840036973736ABE52700B362D2FC473BFF46442777DC58C6433E69D1B198FB57A1C9A10959C778E33D40F0BC42EDB3848C4D59A1A02CCCF810AA793BD88051A8CB275DA3C9952A62A782292CC059F843C6377312CADDC1FEC47BB3639591C1A9110A25E5A19C030D28D92A342C6604B0CBC06698B44EBE1602C1218808050CF77913176C6E94B22338AB87C66A6AB1643AFB623CF02230C087FD23166142600F267A51BB02C81A3242D4CC2F123C667C81A04018A26500037078DF90203C38C543EC090293A01D5D825FFC04C311562E59A363B4B58B1F0BDEFC7120D828445061740975B77848101BAC79A1573E0B87705A89FF62C7C1D63C8C9E53B0336C6E80541FDBA2DFF832EA5764DAD070DCEC919BD920436314D2A6C544C4684D4E93927CB91CFF5B8C2743838FC86BC0B4AA12B3E725587A9285829695DFF29596CE83060531831D3C474275E4B08854BAC44E7E9BCAF51435269437C81C6B095733A19722E476CCF006C9570CC206743B12C86CCC03A9A6023DEF6BC206A064EA77F9C543974037C66C01E3D5C06A9BA3D2DAC617599A5E022C04EE4C4853321D30BC109852E50958CDD92AEE5D31C4ACA4322A1912BD8444CA8366CD25B347B337521A443432B3B9093A55998A29137F3D07326E95F388018ABEB8FBA308289FB5BE3734F0219CED34570104BB7B47C712E4A413C3C1E2372A4FC1A2241EAB3B2E257BDF84D55E88978971B3CB40D8E5DE06AAD529CE8A6F3D9AB6BBEB582A3E263A980565386021C80C49BCF551AADD77E710F8A13DA8B1EA1C6185E43F11EC99D0D33102F8F4564891AC788CDDD06256AB96DAAAA2FA8C9F8C2164570333B3C60189BFFC12943B5051299"));
        sharedSecret = extractSecret(priv, fromHex("AADD2B4EA9587CD4490099967F6E49DA27ADCCA71AC2801BFB077C8A6E48F2613835FC3EEEE4D2371B9F3C7AC6450F37EFDE1B8DA1CA0BEC3C94AA88978C3B90904DD1BD4A0C1CD5D585B024AB7146EB41946747866E33846334F535D304DB5AB930E80A4220790AB4AFFDE1424121F13DCC5AF845CA62182B45E437F33158C39F3C0C90869C1AE6F8AE27016058578F8A3145197232D856C2EC21AD974CAA479B3274F5E40947F1A0F561B9146697F74C0ED627DBA23E3BF632E01106B2251D4F4901E51F6BE872A054968EC9507303CA0F778986AA96C585FA1C3C96AB4C537472C0F0F725ED67BB9A2F9BE0CF947D731A668AF123F2826423A1F2279FC874BE1810009AAFD90CDDD33F81A8EE0293D3D58ED214820BAE17BF5915FDC7A875BA4EE1B44EE4123FC97797DEA6B9D4C94A40789BF5F6BC464F7920E4EBFB3C0293ABD81814E1B81AA46378FE540E2E6AE756BF8F1B284901F692CC65CB0770A8716DDC9DFC21C53804CFA6CE2EA74CF77DACE3D9038F6F977C3AAB560775A27921B3BE0FE336E7DFD469C3465B3F580D2015E2F8668C4E515B9D5515DB383EE25802D0F5F6E2C1F1F0DA81E7BED5ECC63E4DAA2B57B91E07C7ED160BADEF4BCE312950423A67257D1F573BEFCF8979654133D014EE0BD4AC926DA8C8AE08C2F2A997E3482E1359FC7183242FC07B881A8C9E7361EBAF9808CAE5B16AFDF22B20885CCEDDCD14A18F7B453680B2A1CA07C871FB085B7218D8FC8215C662D64FA3BD575A38C0C0507DCE5A2562CAAA94ED41D94F7FA6B6BDE870119480713007CCA22110C105B46E72F4BD03C2FD82F3995A74CB4D3B38A34F402CA20B8E983BBB006C803B33EEEC22B18F50E8B9A0B869D6EC94555DAE1287BE92E048CACB8C4F20F876CE8A41B349549583FD33B869F8A1920BE8B5BE046191854A2D968502B80A0B7E2BE37850782EC1E741186C463CF5D0DBCF09D9F7DD7A5FD15D80185866008A7D2C37B4A3481B40BE651E24A5A4F3E614C00964454B7D785073734AC4A730DA9CD85C5D1D9577588FD3D9E5F9BDB02FA7697407CD50AE552637D2B74045645B17FA91F1021BA6947B47F52B6E2EC74E74A46778A05321A672A756B3293ABAF3BA84D6DFD0C8FD13DDDBDBBAAFB2009028AF6548DC197B2CE2450934312BB67DCCB25A5225BB8CD82B9D4663FE9B9285A61C8F67E01258417A4842BFB6F2C88ADE466D197F24791985E6AFC51C757817F1971A53445244761036AF9DE5784E7967F06436467EC76DA4C503DAE64D11CC6236815F0DF2F89C8C72A95C678888F9A4ADAA88AE1F0134FCBEC8DA0FA1A9B25B4D7E94F404FAA9FBCCC86C4AEB6A24D92FAF2D88E9AA6BEDD79732CEB8E390B86C27A6590CD3CD579FCA04E04D70BE06D865C4F593B0BF85D4FF7B1C50E6A14DE039F284CFB719EC6D65C8A5844F6F0BFBDA25A522FDEFE530056D4C3BBA1BB0C4C9D9BFCB6EBAB814DD7B531060654AF9E0B84005E12F2B43A755B652B5BE9E370483CE349001B99192BD351A8C86A9107A445F88DC3F688121289746866A70728C43288FC2F316B9C290052A1068D1F4C36E3B9F940BED189DF72D244961B6750A3FF860DB8E683277143A79FB38ECB58C8C74A45829334D0B78C26BB3ACEECF8F3FE9A0A77E0C3ED7830C8DDA20FABF0202E40F8284D7FD1CE09ECD773A8A1F8C2F1CE731F1744F25DF609C9703C8FD390BC9651BA7C5FE39EB658E86D6CD278609DC851E47A94C967E1FE5AD7AE5774F4882D818124481D695103FE48FDB1792913AB79C0CF4A8A7E9DFAC8D87AB6256841434B601679389A4E1B09CA450BC7CD6C16802B1240EC16DB9FF8056A8369AA47FADA9A5B59785094906BA17CA2A394CBB5A9D71AB16430EB0582325D83B9E9DD11D7D1D3422A5B34FA64B2A5A1FD0A00CF5A1954B0D6A20786100E46FD1447740A22DB095191088713B4E85B1994A644A75F4921D53C4472A2425F3C646B7216C15A96D9FD554C2EEDBB24CBB18F3A2B3298C4FEA61740E5F1B99254D5C4F58FBF91E60853D320509C64794FFCC0869FB967ACFAF7CA297051E47AD45FD8F9FC1830804F6DB59A844475AB3A434A7A0E3DBA476EAD6FD2C198111AD253FCD41AFA465964B843178A034CF246C3C1242CE3F41D100FCA6D3C67B4F42C46205A834DCAB4C3F66AF4138BBF23B0B270ED8971D2A67945C0D64D6B5"));
        System.out.println("4AD6475CEB9C1A97F74D06B94A41965630B71F67C16C097CA768F38BA21A7F58".compareTo(bits.toHex(sharedSecret).toUpperCase()));

        init(cryKeyML.mlkem768);
        priv = new MLKEMPrivateKeyParameters(mlkemParams, fromHex("c2871b21a2c04cb240e566ba01d8005e27c4e4fca0f3fc4a2e8aa8a9a615af2c665fc396b294062ba6cd6a27aeb1458771d75abfa345d38c0030b00237e8675336abccc2ae933a152cab069af24ca664b39c0c04cf5c5c90394c3e85028d7b5b7f60164de8b61abbc9fd108444ca990a0642eedbaf8dc65485fbc4f3eb712763acb152a46b087dae6a0db6d9a1e19b74385c3d903300746aaab1f2c4edc791b2104317d8ce285675ec228dea8b73c6b693d2e31bd384b44a08252f242ddf95c51d345992a269d383adb0d58af6278a9bd92738f54b1772782073315661c6d26800a18686dbc9b335c20a77230e588164d6494dbec0cd5de408be21358480b71bc137efe335bc028a0d5562b7c3a4bd165f2c7ab2ceb08beafac507e7b683fb0eff19bce487a438a85dd1452c9e850adfeab5705166ae523660a642249370de9c83ad9b620c50bad7d18e29b34e5ec5a46c51a1fc02b2e488afced368a8f25424fa160b03a1fdea63a7e884ccea2d84813fa9e660507ca5bdf025a3c703baa6c056d2a80203359c0171ee06c10a877bb3fb4028435f5b85912b66843b3680d0385744c02e87e05be8c18ff83124b1b14846e46e1d8b9be286669b7b96637a46a4c5be1fc091cc3a2d9cc2302c6554a1bb7f1c7143ebbbb3f959bf86795a52f84fc8912bae5a8258430cc25ca0aab90d6e474fe8d391ec4933ef48ce7aa64e52a73a598272d625902459360f6841761c9310d74f82477e1f3cb7e9923de5164ab22423dd78087fb6631a340eb912817f0120fc106d28122ea7431715c7a6c9010b9c94a9b8635b39e52e3b590dccb01958db74ef0a4149d8593da16ade60c09a5892c1821a3bfc5ab4cb4f4617a79b1828bcf879cc001b2b5367e421889402a197479b4a53b86fd1cd6124468ed198f34ac497848c404b5b411083a0b397479328cbf809b5a4773b4551943076665c72788506ecf175a3dcbd5ac272d4537394324363d580b7528f9c120086f788bcfc4a290c7644fb5cf00769c2c07d8bda352e5b97206461c8fc3cd667b63972aea41219fd6aa7f1b1724a7884d3f51fa4bb9cd88143a1a950e98b2d4c5063a1bb7c04a6bc4cb76ed9752ae30c6a2f8398e1e76c2ba24dd510b6ff7041e2981279f6570c443065c020ebf08fbf3c3b0a80722c508eef27bd3773c2eef2b89864889f8b2d8f995dad309c7228ac4f9109575037009944892524e71a2799eb4c0cac69c0f61ddd9709f681ab3df92047043ea9721178a68b154194de822824a7232536c189c84670cc8c271aa85279c55a48746e3225e63ac7a51a3565bc0713759670f17ee672538b8c2c7d502d942b74d071bf8ec02139f5a3665b218791139e5256d94c915dc44b05170224e92389e17878c091e22631cae61ddaaab518a4a0cd40a73f8935c298a5ea239a79a5b5c608bed5c5bcffd5cf2be55dd78b06e5aa90c63bc5dc580f37a38126308da4642b5a91821ea60adb4197ff8712378a3abe4c445bf74443e71604f27edb604e8695ae70aa9dc134afaeb8661efa8010198aaa073392a06b34734e89a409bc25330043493d51309bab52bf3c5154888c4f0749ff1437dc79bf7d951702b484aedcab0de034c7a590027873ef581806e85485f491783951c6414263b2b4342c97fd6a4cafaa3aa5603196f747439aab797a680ea45c267cb4e534167d1b5234961c15a9b08bb196181742b992cd00090ad5a0bd47f6414cfc14998673ef14490c2568a2f82060e9162b0019db428ed777665cb4bc6dcca2a21c188897cc1e68b6e5652888e1bec41569ad58a7b451b9231357f27aa6ba543f61c3b319da59a5b9a16f2b3726079314c65e0cf54c84f96fb956ae80a3036e67bab20560853430826b8cadd25f51a79c81977cd14651eb16bd7feb2799fa6b48a6bd2178b87de48169266d6cd528dbeb5b2f99b283472e1f24bed6baa52d9a8c7871cce1090037d0ac67228712e88e1f90c93a9a1187c79aa14c2de3582a5d7360dcf3aa30d823ebaccd920aaa071a16254624cf8860c6f6b5f5a71e74f0c003ebc8a925746172a5f26b2dde7276344177b491738e5982383c532496c620921350a5216e3c1ddb13b397ca57c7c638253441bc8aaae7d681fcf0905b917db750745354785dd6360f6821663768340b5dc36737e1381c73a38e4d631722bb2218524754d8be39ab4b38327d3794ceb870ca6aa4374c4a8b3e96583f9cbcb437c6b9d0431ae78f4b89383ff628baec2b87e8bceb525a903549edb8ac1844a0a020cc5191c57e8608efb4911c6b78631922a9d351c84a988565aad6b62c385b182d706e0d9b51a0e4b2e5910034c41b37a019cd3a2826587b78c9cd08c27bf72ac7c2243f5a72a56ae54166b96b9421799c3941004d11b6a00a85947d25d857c5a78b4df634c48844ce8646d6487c868241f1b266a614887fa289e57c3d9279b585e63da0aa97bb6b1514d50ca9251626857e72a7bb81f10274675c96b2511be8a6641b67d6e2532699282da213fc12cfad8787bff500c73c0f1409b76d81654fc96c1fe8b39080506a732f27d2662c4749a2eb08a3998bd59595c37832cd5bbab0a9c4eee065650a004c65765b12ce90a5c869879fef604e9b18a9fbb24f6d3557d549169126c1e4714d80eb102539c25e4066e4e63ffd60074ad3af1269cd8c7443d8b70aa67c258ec864bc5a40e2f78fd64aa9d49c966cc35ea3368d57317ff2fba55aa244e774a80352437e4215c5a271b3f27334eb200c6bb397bcbef5591896a5cf29d622049964a06b01125cb99d0b7193098e915c6d7b5b9fbcf8c7ddb73fef876534cc77913803d2c21697a5884c94b32eaca3aec650a63a9f8a03a2c9ba1d7ccb6bc8a5c376741a501b7e292532ab98cc4a411c84a67bd676435455866823ae106ac1416965246853921960d7bccc12d01c16387b22a4b126d64f47112a6ad495923a1b8949adb7059b06a47cae88bb36518af3d9164b5b757db346ba3a5e9714ba93555b1abc6b4390084b462148ebaae48b0705a05127030deba896fa309a3561694485be7c75ce8f4024dae8bf4994447823152608cee6d7c15d3001c0941c3f431925915e4dab9737b521a5b5cfd7215d2aa941643b34b1e5c6a75899d3e1b48a995313ec34f135a89c11b5d14c59d10a25ad577310480e9a56c39828129d2b88bb7529219c587c43115439868573cefe4499d54595eac44f4fd16ffae62a40d70e2e616275b51edfde8c93f46bd53fcf32e71575a8cce9d37905617aef1094847daa4b2a1413a4a78102f9ae62cbb1c12597c99e7dc599977522e9110deca3e3ee01380aa110057cf59dced003ec81ea17953fc44bc0e3c79e674b4bd6f878080d2b2d55cb5a8b"));
        sharedSecret = extractSecret(priv, fromHex("bf0742c2479e5ec0084f2855209b1db9c4f46b8aa7e9be7e3258ad7c3ce6c0f25193301cf9193379ed012e72b9959cef7750047efc79aef4ae811ee09924d0a1acff9fd5563c2e4c5b391b8baebaf9f04a749fd219300036f1f6ac21d313610b72ddaf962d8db023fac4650c5588929f76202bd876794b0ceb3d7e3e10f96f18e84a90551ad24163ac0a4c2ae44b6215f7af513a9cad79c99335c6c9e6736f648a051964918ee6c8d6e8fa00b65f86e9f5da0f34319294c0ae51a3341f5f3942fd2a6fbdaca28a858fe4ef3bb53c7c8fd987e9a7f863a21001c619d3e498ceb2952264b90d28840858b05789bd1d9f127359a70356a4f850f02bc1bd10a7ccfd151b9bd4123c9f725164c913e475d591defe8d7a187cb048a0ba8da3c36d032f63e4c323c5fba37a808d0832a5f7dbf282118ded2aa1cdc68c353ae3c894076704c070f371ea33a80ccc09f592f3d104b3d4169c718134b32203b76db60880ee930dae50bd1ded527a7c4bbfde15a5229fedcd99f273753d577bcb982afc440e78dd6e0e0e0a2f834cc17fb68765023b6e5f84f57e838357c0a3a4d462a63d7b031c1d92ac5d38670a03fc2a9ca08e1be4bf5a44d2584e42814bfd202919f7bc2805345e226ac1f65afb79f4ead983df3e08ec646b20903a03921cee129c3c66f4e7521684746e5b539440e00912ea73d7e53055da0a29205216358c985bd57bf6733bd8ab435a4c23c880df2323138d4bb4447d68db76ec4a5c8d11f6e5cbc62f0c11ab3466759c8ace40100e1bd0aa9d6d3f55f794acf2bbaf3e98f1c8564be4ee96a03824cfd0172987b32efbaf692e17b6730912426f52609619f4f73e6ae3d844560749afc48c72d87e5517f49d6a56dbe73133d4068c2a2f3c6920151b714ba985f28193543d9c46d218f607ff645d1ddc7c88f5b533c0a95d92049b8513357a74f6fffc48c69095a529143a3c4778c5aba8b3e6d0543623b79bb5ae4cf237c9e9f88159bc2a6e8d836d58338c76b0c28084d0563de6e7ab151a5956334c3499cec17ba4de35c115122e9429aceff38f30fbd79f1bc5779b826394b1fd4b6fec9ea4d2bbc8825ddac47c715fb7362f987376d506a32cbb8a3dfffb2ad6591982d050112a955fa3a10e944ace68700066c93fbeb29b07dd01339dc7cf23bf36c426c59353595c60ed507a779c0872fdb109945e0bdc7ff0123f991be7a0146b2784a1bd59685d95dccda4fe685575b036d077f42eb37d310dc42f41b7697c99d23d90868112803f63a75e39195177d671b24d38ef03037cacfaa12d8073a41566520bc3018a8fc641fc8b1483d6321acaf0bf4d9bb67b7c33b7ca45fc38f11d7494b25efb7801d7be9c4d5cd47940375b04674444264c8f2a0ea4d12e43fa7e76f57547114aa85eb8847bef747a5ffc2937a4b4bf67aaf0ad3f868fd683e630cc6d717b8301f3ff4d811fb0a6e3dfdd15d20f1aecad700668b11421054998e1a5e372c29c6bd64a64678e2694d050d536ff4d6478f96c7b068f4430e490"));
        System.out.println("A32D9C771DBA8E11D51070BFE29B6FEC55F6358056D7A8DBD6428695F059B0DB".compareTo(bits.toHex(sharedSecret).toUpperCase()));

    }

    class MLKEMPublicKeyParameters {

        final cryKeyML p;
        final byte[] t;
        final byte[] rho;

        public MLKEMPublicKeyParameters(cryKeyML params, byte[] t, byte[] rho) {
            p = params;
            this.t = t;
            this.rho = rho;
        }

        public MLKEMPublicKeyParameters(cryKeyML params, byte[] encoding) {
            p = params;
            this.t = MLKEMtest.copyOfRange(encoding, 0, encoding.length - cryKeyML.KyberSymBytes);
            this.rho = MLKEMtest.copyOfRange(encoding, encoding.length - cryKeyML.KyberSymBytes, encoding.length);
        }

        public byte[] getEncoded() {
            return bits.byteConcat(t, rho);
        }

    }

    class MLKEMPrivateKeyParameters {

        final cryKeyML p;
        final byte[] s;
        final byte[] hpk;
        final byte[] nonce;
        final byte[] t;
        final byte[] rho;

        public MLKEMPrivateKeyParameters(cryKeyML params, byte[] s, byte[] hpk, byte[] nonce, byte[] t, byte[] rho) {
            this.p = params;
            this.s = s;
            this.hpk = hpk;
            this.nonce = nonce;
            this.t = t;
            this.rho = rho;
        }

        public MLKEMPrivateKeyParameters(cryKeyML params, byte[] encoding) {
            p = params;
            int index = 0;
            this.s = MLKEMtest.copyOfRange(encoding, 0, p.KyberPolyVecBytes);
            index += p.KyberPolyVecBytes;
            this.t = MLKEMtest.copyOfRange(encoding, index, index + p.KyberIndCpaPublicKeyBytes - cryKeyML.KyberSymBytes);
            index += p.KyberIndCpaPublicKeyBytes - cryKeyML.KyberSymBytes;
            this.rho = MLKEMtest.copyOfRange(encoding, index, index + 32);
            index += 32;
            this.hpk = MLKEMtest.copyOfRange(encoding, index, index + 32);
            index += 32;
            this.nonce = MLKEMtest.copyOfRange(encoding, index, index + cryKeyML.KyberSymBytes);
        }

        public byte[] getEncoded() {
            byte[] r = bits.byteConcat(s, t);
            r = bits.byteConcat(r, rho);
            r = bits.byteConcat(r, hpk);
            return bits.byteConcat(r, nonce);
        }

    }

}

*/