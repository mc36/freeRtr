package org.freertr.cry;

import java.util.Arrays;
import org.freertr.util.bits;

/**
 * curve25519
 *
 * @author matecsaba
 */
public class cryKeyCurve25519 {

    /**
     * create instance
     */
    public cryKeyCurve25519() {
    }

    /**
     * local private key
     */
    public byte[] locPriv;

    /**
     * remote public key
     */
    public byte[] remPub;

    /**
     * common secret
     */
    public byte[] common;

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

    /**
     * make key
     */
    public void makePirvKey() {
        locPriv = new byte[32];
        for (int i = 0; i < locPriv.length; i++) {
            locPriv[i] = (byte) bits.randomB();
        }
        locPriv[0] &= (byte) 248;
        locPriv[31] &= 127;
        locPriv[31] |= 64;
    }

    /**
     * calculate keys
     */
    public void calcCommon() {
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
        if (remPub != null) {
            for (int i = 0; i < 32; i++) {
                int bit = (i * 8) % 26;
                int word = (i * 8) / 26;
                int value = remPub[i] & 0xFF;
                if (bit <= (26 - 8)) {
                    x_1[word] |= value << bit;
                } else {
                    x_1[word] |= value << bit;
                    x_1[word] &= 0x03FFFFFF;
                    x_1[word + 1] |= value >> (26 - bit);
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
        evalCurve(locPriv);
        recip(z_3, z_2);
        mul(x_2, x_2, z_3);
        common = new byte[32];
        for (int i = 0; i < 32; i++) {
            int bit = (i * 8) % 26;
            int word = (i * 8) / 26;
            if (bit <= (26 - 8)) {
                common[i] = (byte) (x_2[word] >> bit);
            } else {
                common[i] = (byte) ((x_2[word] >> bit) | (x_2[word + 1] << (26 - bit)));
            }
        }
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

    private void evalCurve(byte[] s) {
        int sposn = 31;
        int sbit = 6;
        int svalue = s[sposn] | 0x40;
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
                svalue = s[sposn] & 0xF8;
                sbit = 7;
            } else {
                sposn--;
                svalue = s[sposn];
                sbit = 7;
            }
        }
        cswap(swap, x_2, x_3);
        cswap(swap, z_2, z_3);
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

}
