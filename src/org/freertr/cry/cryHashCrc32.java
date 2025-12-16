package org.freertr.cry;

import org.freertr.util.bits;

/**
 * cyclic redundancy check
 *
 * @author matecsaba
 */
public class cryHashCrc32 extends cryHashGeneric {

    /**
     * crc32c polynominal: 1 + x + x**6 + x**8 + x**9 + x**10 + x**11 + x**13 +
     * x**14 + x**18 + x**19 + x**20 + x**22 + x**23 + x**25 + x**26 + x**27 +
     * x**28
     */
    public final static cryHashCrc32 polyCrc32c = new cryHashCrc32(0x1edc6f41, 0xffffffff, 0xffffffff, false);

    /**
     * crc32ieee polynominal: 1 + x + x**2 + x**4 + x**5 + x**7 + x**8 + x**10 +
     * x**11 + x**12 + x**16 + x**22 + x**23 + x**26
     */
    public final static cryHashCrc32 polyCrc32i = new cryHashCrc32(0x04c11db7, 0xffffffff, 0xffffffff, true);

    private final int[] tab;

    private final boolean ord;

    private final int ini;

    private final int xor;

    private int crc;

    /**
     * create instance
     *
     * @param po polynominal
     * @param in initializer
     * @param xr xorer
     * @param or byte order, true=msb, false=lsb
     */
    public cryHashCrc32(int po, int in, int xr, boolean or) {
        ord = or;
        tab = new int[256];
        ini = in;
        xor = xr;
        for (int i = 0; i < tab.length; i++) {
            tab[i] = mkTabEntry(po, i);
        }
    }

    /**
     * create instance
     *
     * @param o where to clone from
     */
    public cryHashCrc32(cryHashCrc32 o) {
        tab = o.tab;
        ord = o.ord;
        ini = o.ini;
        xor = o.xor;
    }

    /**
     * reverse bits
     *
     * @param b value
     * @return reversed
     */
    public static int reverse(int b) {
        int r = 0;
        for (int i = 0; i < 32; i++) {
            if ((b & 1) != 0) {
                r |= 1 << (31 - i);
            }
            b >>>= 1;
        }
        return r;
    }

    private int mkTabEntry(int p, int c) {
        c = reverse(c);
        for (int i = 0; i < 8; i++) {
            if ((c & 0x80000000) != 0) {
                c = (c << 1) ^ p;
            } else {
                c <<= 1;
            }
        }
        return reverse(c);
    }

    /**
     * initialize
     */
    public void init() {
        crc = ini;
    }

    /**
     * set frame checksum
     *
     * @param i new value
     */
    public void setCrc(int i) {
        crc = i;
    }

    /**
     * get frame checksum
     *
     * @return value
     */
    public int getCrc() {
        return crc;
    }

    /**
     * get name
     *
     * @return name
     */
    public String getName() {
        return "crc32";
    }

    /**
     * read oid of hash
     *
     * @return name of hash
     */
    public byte[] getPkcs() {
        return null;
    }

    /**
     * get hash size
     *
     * @return size
     */
    public int getHashSize() {
        return 4;
    }

    /**
     * get block size
     *
     * @return size
     */
    public int getBlockSize() {
        return 1;
    }

    private void updateByte(int i) {
        crc = tab[(crc ^ i) & 0xff] ^ (crc >>> 8);
    }

    /**
     * compute block
     *
     * @param buf buffer
     * @param ofs offset
     * @param siz size
     */
    public void update(byte[] buf, int ofs, int siz) {
        for (int i = 0; i < siz; i++) {
            updateByte(buf[ofs + i]);
        }
    }

    /**
     * finish
     *
     * @return computed
     */
    public byte[] finish() {
        byte[] buf = new byte[4];
        if (ord) {
            bits.msbPutD(buf, 0, crc ^ xor);
        } else {
            bits.lsbPutD(buf, 0, crc ^ xor);
        }
        return buf;
    }

}
