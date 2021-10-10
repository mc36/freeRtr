package net.freertr.cry;

import net.freertr.util.bits;

/**
 * cyclic redundancy check (xmodem) 16bits x**0 + x**5 + x**12 + x**16
 *
 * @author matecsaba
 */
public class cryHashCrc16 extends cryHashGeneric {

    /**
     * create instance
     */
    public cryHashCrc16() {
    }

    private static int[] tab = null;

    private int fcs;

    private int mkTabEntry(int v) {
        v <<= 8;
        for (int i = 0; i < 8; ++i) {
            v <<= 1;
            if ((v & 0x10000) != 0) {
                v ^= 0x1021;
            }
        }
        return v & 0xffff;
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
        fcs = 0xffff;
    }

    /**
     * set frame checksum
     *
     * @param i new value
     */
    public void setCrc(int i) {
        fcs = i;
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
        fcs = ((fcs << 8) & 0xffff) ^ tab[(fcs >>> 8) ^ (i & 0xff)];
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
        bits.msbPutW(buf, 0, fcs ^ 0xffff);
        return buf;
    }

}
