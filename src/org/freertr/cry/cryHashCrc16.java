package org.freertr.cry;

import org.freertr.util.bits;

/**
 * cyclic redundancy check
 *
 * @author matecsaba
 */
public class cryHashCrc16 extends cryHashGeneric {

    /**
     * crc16ccitt (rfc1662) polynominal: 16bits x**0 + x**5 + x**12 + x**16
     */
    public final static cryHashCrc16 polyCrc16c = new cryHashCrc16(0x1021, 0xffff, 0xffff, false);

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
    public cryHashCrc16(int po, int in, int xr, boolean or) {
        ord = or;
        ini = in;
        xor = xr;
        tab = new int[256];
        for (int i = 0; i < tab.length; i++) {
            tab[i] = mkTabEntry(po, i);
        }
    }

    /**
     * create instance
     *
     * @param o where to clone from
     */
    public cryHashCrc16(cryHashCrc16 o) {
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
    public static int reverse32(int b) {
        int r = 0;
        for (int i = 0; i < 32; i++) {
            if ((b & bits.bitVals[i]) != 0) {
                r |= bits.bitVals[31 - i];
            }
        }
        return r;
    }

    /**
     * reverse bits
     *
     * @param b value
     * @return reversed
     */
    public static int reverse16(int b) {
        int r = 0;
        for (int i = 0; i < 16; i++) {
            if ((b & bits.bitVals[i]) != 0) {
                r |= bits.bitVals[15 - i];
            }
        }
        return r;
    }

    /**
     * reverse bits
     *
     * @param b value
     * @return reversed
     */
    public static int reverse8(int b) {
        int r = 0;
        for (int i = 0; i < 8; i++) {
            if ((b & bits.bitVals[i]) != 0) {
                r |= bits.bitVals[7 - i];
            }
        }
        return r;
    }

    private int mkTabEntry(int p, int v) {
        v <<= 8;
        for (int i = 0; i < 8; i++) {
            v <<= 1;
            if ((v & 0x10000) != 0) {
                v ^= p;
            }
        }
        return v & 0xffff;
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
        return "crc16";
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
        return 2;
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
        crc = ((crc << 8) & 0xffff) ^ tab[(crc >>> 8) ^ (i & 0xff)];
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
        byte[] buf = new byte[2];
        if (ord) {
            bits.msbPutW(buf, 0, crc ^ xor);
        } else {
            bits.lsbPutW(buf, 0, crc ^ xor);
        }
        return buf;
    }

}
