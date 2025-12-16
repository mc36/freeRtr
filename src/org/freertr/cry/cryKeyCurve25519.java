package org.freertr.cry;

import java.util.Arrays;
import org.freertr.pack.packHolder;
import org.freertr.util.bits;

/**
 * curve25519
 *
 * @author matecsaba
 */
public class cryKeyCurve25519 extends cryKeyGeneric {

    /**
     * create instance
     */
    public cryKeyCurve25519() {
    }

    /**
     * tls value
     */
    public final static int tlsVal = 29;

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
     * server private value
     */
    protected byte[] servPriv;

    /**
     * server public value
     */
    protected byte[] servPub;

    private final static int NUM_LIMBS_255BIT = 10;

    private final static int NUM_LIMBS_510BIT = 20;

    private int[] A;

    private int[] AA;

    private int[] B;

    private int[] BB;

    private int[] C;

    private int[] CB;

    private int[] D;

    private int[] DA;

    private int[] E;

    private long[] t1;

    private int[] t2;

    private int[] x_1;

    private int[] x_2;

    private int[] x_3;

    private int[] z_2;

    private int[] z_3;

    private byte[] makePirvKey() {
        byte[] priv = new byte[32];
        for (int i = 0; i < priv.length; i++) {
            priv[i] = (byte) bits.randomB();
        }
        priv[0] &= (byte) 248;
        priv[31] &= 127;
        priv[31] |= 64;
        return priv;
    }

    private byte[] calcCommon(byte[] priv, byte[] pub) {
        x_1 = new int[NUM_LIMBS_255BIT];
        x_2 = new int[NUM_LIMBS_255BIT];
        x_3 = new int[NUM_LIMBS_255BIT];
        z_2 = new int[NUM_LIMBS_255BIT];
        z_3 = new int[NUM_LIMBS_255BIT];
        A = new int[NUM_LIMBS_255BIT];
        B = new int[NUM_LIMBS_255BIT];
        C = new int[NUM_LIMBS_255BIT];
        D = new int[NUM_LIMBS_255BIT];
        E = new int[NUM_LIMBS_255BIT];
        AA = new int[NUM_LIMBS_255BIT];
        BB = new int[NUM_LIMBS_255BIT];
        DA = new int[NUM_LIMBS_255BIT];
        CB = new int[NUM_LIMBS_255BIT];
        t1 = new long[NUM_LIMBS_510BIT];
        t2 = new int[NUM_LIMBS_510BIT];
        Arrays.fill(x_1, 0);
        if (pub != null) {
            for (int i = 0; i < 32; i++) {
                int bit = (i * 8) % 26;
                int word = (i * 8) / 26;
                int val = pub[i] & 0xFF;
                if (bit <= (26 - 8)) {
                    x_1[word] |= val << bit;
                } else {
                    x_1[word] |= val << bit;
                    x_1[word] &= 0x03FFFFFF;
                    x_1[word + 1] |= val >> (26 - bit);
                }
            }
            reduceQuick(x_1);
            reduceQuick(x_1);
        } else {
            x_1[0] = 9;
        }
        Arrays.fill(x_2, 0);
        x_2[0] = 1;
        Arrays.fill(z_2, 0);
        System.arraycopy(x_1, 0, x_3, 0, x_1.length);
        Arrays.fill(z_3, 0);
        z_3[0] = 1;
        int sposn = 31;
        int sbit = 6;
        int svalue = priv[sposn] | 0x40;
        int swap = 0;
        while (true) {
            int select = (svalue >> sbit) & 0x01;
            swap ^= select;
            cswap(swap, x_2, x_3);
            cswap(swap, z_2, z_3);
            swap = select;
            add(A, x_2, z_2);
            square(AA, A);
            sub(B, x_2, z_2);
            square(BB, B);
            sub(E, AA, BB);
            add(C, x_3, z_3);
            sub(D, x_3, z_3);
            mul(DA, D, A);
            mul(CB, C, B);
            add(x_3, DA, CB);
            square(x_3, x_3);
            sub(z_3, DA, CB);
            square(z_3, z_3);
            mul(z_3, z_3, x_1);
            mul(x_2, AA, BB);
            mulA24(z_2, E);
            add(z_2, z_2, AA);
            mul(z_2, z_2, E);
            if (sbit > 0) {
                sbit--;
            } else if (sposn == 0) {
                break;
            } else if (sposn == 1) {
                sposn--;
                svalue = priv[sposn] & 0xF8;
                sbit = 7;
            } else {
                sposn--;
                svalue = priv[sposn];
                sbit = 7;
            }
        }
        cswap(swap, x_2, x_3);
        cswap(swap, z_2, z_3);
        recip(z_3, z_2);
        mul(x_2, x_2, z_3);
        byte[] res = new byte[32];
        for (int i = 0; i < 32; i++) {
            int bit = (i * 8) % 26;
            int word = (i * 8) / 26;
            if (bit <= (26 - 8)) {
                res[i] = (byte) (x_2[word] >> bit);
            } else {
                res[i] = (byte) ((x_2[word] >> bit) | (x_2[word + 1] << (26 - bit)));
            }
        }
        return res;
    }

    private void sub(int[] result, int[] x, int[] y) {
        int borrow = 0;
        for (int i = 0; i < NUM_LIMBS_255BIT; i++) {
            borrow = x[i] - y[i] - ((borrow >> 26) & 0x01);
            result[i] = borrow & 0x03FFFFFF;
        }
        borrow = result[0] - ((-((borrow >> 26) & 0x01)) & 19);
        result[0] = borrow & 0x03FFFFFF;
        for (int i = 1; i < NUM_LIMBS_255BIT; i++) {
            borrow = result[i] - ((borrow >> 26) & 0x01);
            result[i] = borrow & 0x03FFFFFF;
        }
        result[NUM_LIMBS_255BIT - 1] &= 0x001FFFFF;
    }

    private void add(int[] result, int[] x, int[] y) {
        int carry = x[0] + y[0];
        result[0] = carry & 0x03FFFFFF;
        for (int i = 1; i < NUM_LIMBS_255BIT; i++) {
            carry = (carry >> 26) + x[i] + y[i];
            result[i] = carry & 0x03FFFFFF;
        }
        reduceQuick(result);
    }

    private void cswap(int select, int[] x, int[] y) {
        select = -select;
        for (int i = 0; i < NUM_LIMBS_255BIT; i++) {
            int dummy = select & (x[i] ^ y[i]);
            x[i] ^= dummy;
            y[i] ^= dummy;
        }
    }

    private void mul(int[] result, int[] x, int[] y) {
        long v = x[0];
        for (int i = 0; i < NUM_LIMBS_255BIT; i++) {
            t1[i] = v * y[i];
        }
        for (int i = 1; i < NUM_LIMBS_255BIT; i++) {
            v = x[i];
            for (int j = 0; j < (NUM_LIMBS_255BIT - 1); j++) {
                t1[i + j] += v * y[j];
            }
            t1[i + NUM_LIMBS_255BIT - 1] = v * y[NUM_LIMBS_255BIT - 1];
        }
        v = t1[0];
        t2[0] = ((int) v) & 0x03FFFFFF;
        for (int i = 1; i < NUM_LIMBS_510BIT; i++) {
            v = (v >> 26) + t1[i];
            t2[i] = ((int) v) & 0x03FFFFFF;
        }
        reduce(result, t2, NUM_LIMBS_255BIT);
    }

    private void mulA24(int[] result, int[] x) {
        long a24 = 121665;
        long carry = 0;
        for (int i = 0; i < NUM_LIMBS_255BIT; i++) {
            carry += a24 * x[i];
            t2[i] = ((int) carry) & 0x03FFFFFF;
            carry >>= 26;
        }
        t2[NUM_LIMBS_255BIT] = ((int) carry) & 0x03FFFFFF;
        reduce(result, t2, 1);
    }

    private void pow250(int[] result, int[] x) {
        square(A, x);
        for (int j = 0; j < 9; j++) {
            square(A, A);
        }
        mul(result, A, x);
        for (int i = 0; i < 23; i++) {
            for (int j = 0; j < 10; j++) {
                square(A, A);
            }
            mul(result, result, A);
        }
        square(A, result);
        mul(result, result, A);
        for (int j = 0; j < 8; j++) {
            square(A, A);
            mul(result, result, A);
        }
    }

    private void recip(int[] result, int[] x) {
        pow250(result, x);
        square(result, result);
        square(result, result);
        mul(result, result, x);
        square(result, result);
        square(result, result);
        mul(result, result, x);
        square(result, result);
        mul(result, result, x);
    }

    private void reduce(int[] result, int[] x, int size) {
        int carry = 0;
        int limb = x[NUM_LIMBS_255BIT - 1] >> 21;
        x[NUM_LIMBS_255BIT - 1] &= 0x001FFFFF;
        for (int i = 0; i < size; i++) {
            limb += x[NUM_LIMBS_255BIT + i] << 5;
            carry += (limb & 0x03FFFFFF) * 19 + x[i];
            x[i] = carry & 0x03FFFFFF;
            limb >>= 26;
            carry >>= 26;
        }
        if (size < NUM_LIMBS_255BIT) {
            for (int i = size; i < NUM_LIMBS_255BIT; i++) {
                carry += x[i];
                x[i] = carry & 0x03FFFFFF;
                carry >>= 26;
            }
        }
        carry = (x[NUM_LIMBS_255BIT - 1] >> 21) * 19;
        x[NUM_LIMBS_255BIT - 1] &= 0x001FFFFF;
        for (int i = 0; i < NUM_LIMBS_255BIT; i++) {
            carry += x[i];
            result[i] = carry & 0x03FFFFFF;
            carry >>= 26;
        }
        reduceQuick(result);
    }

    private void reduceQuick(int[] x) {
        int carry = 19;
        for (int i = 0; i < NUM_LIMBS_255BIT; i++) {
            carry += x[i];
            t2[i] = carry & 0x03FFFFFF;
            carry >>= 26;
        }
        int mask = -((t2[NUM_LIMBS_255BIT - 1] >> 21) & 0x01);
        int nmask = ~mask;
        t2[NUM_LIMBS_255BIT - 1] &= 0x001FFFFF;
        for (int i = 0; i < NUM_LIMBS_255BIT; i++) {
            x[i] = (x[i] & nmask) | (t2[i] & mask);
        }
    }

    private void square(int[] result, int[] x) {
        mul(result, x, x);
    }

    public String algName() {
        return "curve25519";
    }

    public String sshName() {
        return "curve25519-sha256";
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
        return id != 31;
    }

    public int keyMakeVal() {
        return tlsVal;
    }

    public boolean keyVerify() {
        return false;
    }

    public int keySize() {
        return 255;
    }

    public String keyDump() {
        return "cln=" + bits.byteDump(clntPub, 0, -1) + " srv=" + bits.byteDump(servPub, 0, -1) + " res=" + bits.byteDump(common, 0, -1);
    }

    public void keyClntInit() {
        clntPriv = makePirvKey();
        clntPub = calcCommon(clntPriv, null);
    }

    public void keyServInit() {
        servPriv = makePirvKey();
        servPub = calcCommon(servPriv, null);
    }

    public void keyClntCalc() {
        common = calcCommon(clntPriv, servPub);
    }

    public void keyServCalc() {
        common = calcCommon(servPriv, clntPub);
    }

    public byte[] keyCommonTls() {
        return common;
    }

    public byte[] keyCommonSsh() {
        if (common == null) {
            return null;
        }
        return cryUtils.buffer2bigInt(common, 0, common.length).toByteArray();
    }

    public byte[] keyCommonIke() {
        return keyCommonTls();
    }

    public byte[] keyClntTls() {
        return keyClntSsh();
    }

    public byte[] keyServTls() {
        return keyServSsh();
    }

    public boolean keyClntTls(byte[] buf, int ofs) {
        return keyClntSsh(buf, ofs);
    }

    public boolean keyServTls(byte[] buf, int ofs) {
        return keyServSsh(buf, ofs);
    }

    public byte[] keyClntSsh() {
        return clntPub;
    }

    public byte[] keyServSsh() {
        return servPub;
    }

    public boolean keyClntSsh(byte[] buf, int ofs) {
        clntPub = new byte[32];
        bits.byteCopy(buf, ofs, clntPub, 0, clntPub.length);
        return false;
    }

    public boolean keyServSsh(byte[] buf, int ofs) {
        servPub = new byte[32];
        bits.byteCopy(buf, ofs, servPub, 0, servPub.length);
        return false;
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
        servPriv = key;
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
