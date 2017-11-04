package cry;

import util.bits;

/**
 * cyclic redundancy check 32bits 1 + x + x**2 + x**4 + x**5 + x**7 + x**8 +
 * x**10 + x**11 + x**12 + x**16 + x**22 + x**23 + x**26
 *
 * @author matecsaba
 */
public class cryHashCrc32 extends cryHashGeneric {

    private static int[] tab = null;

    private int crc;

    private int mkTabEntry(int c) {
        for (int k = 0; k < 8; k++) {
            if ((c & 1) != 0) {
                c = 0xedb88320 ^ (c >>> 1);
            } else {
                c = c >>> 1;
            }
        }
        return c & 0xffffffff;
    }

    private void makeTab() {
        tab = new int[256];
        for (int i = 0; i < 256; i++) {
            tab[i] = mkTabEntry(i);
        }
    }

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

    public String getName() {
        return "crc32";
    }

    public int getHashSize() {
        return 4;
    }

    public int getBlockSize() {
        return 1;
    }

    public void update(int i) {
        crc = tab[(crc ^ i) & 0xff] ^ (crc >>> 8);
    }

    public byte[] finish() {
        byte[] buf = new byte[4];
        bits.msbPutD(buf, 0, crc ^ 0xffffffff);
        return buf;
    }

}
