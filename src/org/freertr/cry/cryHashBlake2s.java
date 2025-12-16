package org.freertr.cry;

import org.freertr.util.bits;

/**
 * blake2s (rfc7693) hash
 *
 * @author matecsaba
 */
public class cryHashBlake2s extends cryHashGeneric {

    /**
     * create instance
     */
    public cryHashBlake2s() {
    }

    private final static int[] blake2s_IV = {
        0x6a09e667, 0xbb67ae85, 0x3c6ef372,
        0xa54ff53a, 0x510e527f, 0x9b05688c,
        0x1f83d9ab, 0x5be0cd19
    };

    private final static byte[][] blake2s_sigma = {
        {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15},
        {14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3},
        {11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4},
        {7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8},
        {9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13},
        {2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9},
        {12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11},
        {13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10},
        {6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5},
        {10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0}};

    private final static int ROUNDS = 10;

    private final static int BLOCK_LENGTH_BYTES = 64;

    private int digestLength = 32;

    private int keyLength = 0;

    private byte[] key = null;

    private int fanout = 1;

    private int depth = 1;

    private int leafLength = 0;

    private long nodeOffset = 0;

    private int nodeDepth = 0;

    private int innerHashLength = 0;

    private byte[] buffer = null;

    private int bufferPos = 0;

    private int[] internalState = new int[16];

    private int[] chainValue = null;

    private int t0 = 0;

    private int t1 = 0;

    private int f0 = 0;

    /**
     * create instance
     *
     * @param key a key up to 32 bytes or null
     * @param digestBytes from 1 up to 32 bytes
     */
    public cryHashBlake2s(byte[] key, int digestBytes) {
        digestLength = digestBytes;
        init(key);
    }

    private void init(byte[] ky) {
        buffer = new byte[BLOCK_LENGTH_BYTES];
        bufferPos = 0;
        keyLength = 0;
        if ((ky != null) && (ky.length > 0)) {
            key = new byte[ky.length];
            bits.byteCopy(ky, 0, key, 0, ky.length);
            keyLength = ky.length;
            bits.byteCopy(ky, 0, buffer, 0, ky.length);
            bufferPos = BLOCK_LENGTH_BYTES;
        }
        chainValue = new int[8];
        chainValue[0] = blake2s_IV[0] ^ (digestLength | (keyLength << 8) | ((fanout << 16) | (depth << 24)));
        chainValue[1] = blake2s_IV[1] ^ leafLength;
        int nofHi = (int) (nodeOffset >> 32);
        int nofLo = (int) nodeOffset;
        chainValue[2] = blake2s_IV[2] ^ nofLo;
        chainValue[3] = blake2s_IV[3] ^ (nofHi | (nodeDepth << 16) | (innerHashLength << 24));
        chainValue[4] = blake2s_IV[4];
        chainValue[5] = blake2s_IV[5];
        chainValue[6] = blake2s_IV[6];
        chainValue[7] = blake2s_IV[7];
        t0 = 0;
        t1 = 0;
        f0 = 0;
    }

    /**
     * Update the message digest with a block of bytes.
     *
     * @param message the byte array containing the data.
     * @param offset the offset into the byte array where the data starts.
     * @param len the length of the data.
     */
    public void update(byte[] message, int offset, int len) {
        if (len < 1) {
            return;
        }
        int remainingLength = 0;
        if (bufferPos != 0) {
            remainingLength = BLOCK_LENGTH_BYTES - bufferPos;
            if (remainingLength < len) {
                bits.byteCopy(message, offset, buffer, bufferPos, remainingLength);
                t0 += BLOCK_LENGTH_BYTES;
                if (t0 == 0) {
                    t1++;
                }
                compress(buffer, 0);
                bufferPos = 0;
                buffer = new byte[buffer.length];
            } else {
                bits.byteCopy(message, offset, buffer, bufferPos, len);
                bufferPos += len;
                return;
            }
        }
        int messagePos;
        int blockWiseLastPos = offset + len - BLOCK_LENGTH_BYTES;
        for (messagePos = offset + remainingLength; messagePos < blockWiseLastPos; messagePos += BLOCK_LENGTH_BYTES) {
            t0 += BLOCK_LENGTH_BYTES;
            if (t0 == 0) {
                t1++;
            }
            compress(message, messagePos);
        }
        bits.byteCopy(message, messagePos, buffer, 0, offset + len - messagePos);
        bufferPos += offset + len - messagePos;
    }

    public byte[] finish() {
        f0 = 0xFFFFFFFF;
        t0 += bufferPos;
        if ((t0 < 0) && (bufferPos > -t0)) {
            t1++;
        }
        compress(buffer, 0);
        byte[] res = new byte[digestLength];
        for (int i = 0; i < chainValue.length && (i * 4 < digestLength); i++) {
            byte[] bytes = new byte[4];
            bits.lsbPutD(bytes, 0, chainValue[i]);
            if (i * 4 < digestLength - 4) {
                bits.byteCopy(bytes, 0, res, i * 4, 4);
            } else {
                bits.byteCopy(bytes, 0, res, i * 4, digestLength - (i * 4));
            }
        }
        return res;
    }

    private void compress(byte[] message, int messagePos) {
        System.arraycopy(chainValue, 0, internalState, 0, chainValue.length);
        System.arraycopy(blake2s_IV, 0, internalState, chainValue.length, 4);
        internalState[12] = t0 ^ blake2s_IV[4];
        internalState[13] = t1 ^ blake2s_IV[5];
        internalState[14] = f0 ^ blake2s_IV[6];
        internalState[15] = blake2s_IV[7];
        int[] m = new int[16];
        for (int j = 0; j < 16; j++) {
            m[j] = bits.lsbGetD(message, messagePos + j * 4);
        }
        for (int round = 0; round < ROUNDS; round++) {
            G(m[blake2s_sigma[round][0]], m[blake2s_sigma[round][1]], 0, 4, 8, 12);
            G(m[blake2s_sigma[round][2]], m[blake2s_sigma[round][3]], 1, 5, 9, 13);
            G(m[blake2s_sigma[round][4]], m[blake2s_sigma[round][5]], 2, 6, 10, 14);
            G(m[blake2s_sigma[round][6]], m[blake2s_sigma[round][7]], 3, 7, 11, 15);
            G(m[blake2s_sigma[round][8]], m[blake2s_sigma[round][9]], 0, 5, 10, 15);
            G(m[blake2s_sigma[round][10]], m[blake2s_sigma[round][11]], 1, 6, 11, 12);
            G(m[blake2s_sigma[round][12]], m[blake2s_sigma[round][13]], 2, 7, 8, 13);
            G(m[blake2s_sigma[round][14]], m[blake2s_sigma[round][15]], 3, 4, 9, 14);
        }
        for (int offset = 0; offset < chainValue.length; offset++) {
            chainValue[offset] = chainValue[offset] ^ internalState[offset] ^ internalState[offset + 8];
        }
    }

    private void G(int m1, int m2, int posA, int posB, int posC, int posD) {
        internalState[posA] = internalState[posA] + internalState[posB] + m1;
        internalState[posD] = rotr32(internalState[posD] ^ internalState[posA], 16);
        internalState[posC] = internalState[posC] + internalState[posD];
        internalState[posB] = rotr32(internalState[posB] ^ internalState[posC], 12);
        internalState[posA] = internalState[posA] + internalState[posB] + m2;
        internalState[posD] = rotr32(internalState[posD] ^ internalState[posA], 8);
        internalState[posC] = internalState[posC] + internalState[posD];
        internalState[posB] = rotr32(internalState[posB] ^ internalState[posC], 7);
    }

    private int rotr32(int x, int rot) {
        return x >>> rot | (x << (32 - rot));
    }

    public void init() {
        init(key);
    }

    public String getName() {
        return "blake2s";
    }

    /**
     * read oid of hash
     *
     * @return name of hash
     */
    public byte[] getPkcs() {
        return null;
    }

    public int getHashSize() {
        return digestLength;
    }

    public int getBlockSize() {
        return 64;
    }

}
