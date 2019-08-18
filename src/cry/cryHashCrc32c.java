package cry;

import util.bits;

/**
 * cyclic redundancy check 32bits 1 + x + x**6 + x**8 + x**9 + x**10 + x**11 +
 * x**13 + x**14 + x**18 + x**19 + x**20 + x**22 + x**23 + x**25 + x**26 + x**27
 * + x**28
 *
 * @author matecsaba
 */
public class cryHashCrc32c extends cryHashGeneric {

    private static int[] tab = null;

    private int crc;

    private int reverse(int b) {
        int r = 0;
        for (int i = 0; i < 32; i++) {
            if ((b & 1) != 0) {
                r |= 1 << (31 - i);
            }
            b >>>= 1;
        }
        return r;
    }

    private int mkTabEntry(int c) {
        c = reverse(c);
        for (int i = 0; i < 8; i++) {
            if ((c & 0x80000000) != 0) {
                c = (c << 1) ^ 0x1edc6f41;
            } else {
                c <<= 1;
            }
        }
        return reverse(c);
    }

    private void makeTab() {
        tab = new int[256];
        for (int i = 0; i < 256; i++) {
            tab[i] = mkTabEntry(i);
        }
    }

    /**
     * initialize
     */
    public void init() {
        if (tab == null) {
            makeTab();
        }
        crc = 0xffffffff;
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
     * get name
     *
     * @return name
     */
    public String getName() {
        return "crc32c";
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
        crc = (crc >>> 8) ^ tab[(crc ^ i) & 0xFF];
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
        bits.lsbPutD(buf, 0, crc ^ 0xffffffff);
        return buf;
    }

}
