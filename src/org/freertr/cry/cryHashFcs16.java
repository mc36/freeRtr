package org.freertr.cry;

import org.freertr.util.bits;

/**
 * cyclic redundancy check (rfc1662) 16bits x**0 + x**5 + x**12 + x**16
 *
 * @author matecsaba
 */
public class cryHashFcs16 extends cryHashGeneric {

    /**
     * create instance
     */
    public cryHashFcs16() {
    }

    private static int[] tab = null;

    private int fcs;

    private int mkTabEntry(int v) {
        for (int i = 0; i < 8; i++) {
            if ((v & 1) != 0) {
                v = (v >>> 1) ^ 0x8408;
            } else {
                v = v >>> 1;
            }
        }
        return v & 0xffff;
    }

    private void makeTab() {
        tab = new int[256];
        for (int i = 0; i < tab.length; i++) {
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
    public void setFcs(int i) {
        fcs = i;
    }

    /**
     * get frame checksum
     *
     * @return value
     */
    public int getFcs() {
        return fcs;
    }

    /**
     * get name
     *
     * @return name
     */
    public String getName() {
        return "fcs16";
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
        fcs = (fcs >>> 8) ^ tab[(fcs ^ i) & 0xff];
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
        bits.lsbPutW(buf, 0, fcs ^ 0xffff);
        return buf;
    }

}
