package org.freertr.cry;

/**
 * cyclic redundancy check
 *
 * @author matecsaba
 */
public class cryHashCrc8 extends cryHashGeneric {

    /**
     * crc8ccitt polynominal: 8bits x**8 + x**2 + x + 1
     */
    public final static cryHashCrc8 polyCrc8c = new cryHashCrc8(0x107, 0x00, 0x55);

    /**
     * dvb-s2 polynominal: 8bits x**8 + x**7 + x**6 + x**4 + x**2 + 1
     */
    public final static cryHashCrc8 polyCrc8d = new cryHashCrc8(0xd5, 0x00, 0x00);

    private final int[] tab;

    private final int ini;

    private final int xor;

    private int crc;

    /**
     * create instance
     *
     * @param po polynominal
     * @param in initializer
     * @param xr xorer
     */
    public cryHashCrc8(int po, int in, int xr) {
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
    public cryHashCrc8(cryHashCrc8 o) {
        tab = o.tab;
        ini = o.ini;
        xor = o.xor;
    }

    private int mkTabEntry(int p, int i) {
        for (int o = 0; o < 8; o++) {
            if ((i & 0x80) != 0) {
                i = (i << 1) ^ p;
            } else {
                i = (i << 1);
            }
        }
        return i & 0xff;
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
        return "crc8";
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
        return 1;
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
        crc = tab[(crc ^ i) & 0xff];
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
        byte[] buf = new byte[1];
        buf[0] = (byte) ((crc ^ xor) & 0xff);
        return buf;
    }

}
