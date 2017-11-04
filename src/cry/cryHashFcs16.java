package cry;

import util.bits;

/**
 * cyclic redundancy check (rfc1662) 16bits x**0 + x**5 + x**12 + x**16
 *
 * @author matecsaba
 */
public class cryHashFcs16 extends cryHashGeneric {

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
        for (int i = 0; i < 256; i++) {
            tab[i] = mkTabEntry(i);
        }
    }

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

    public String getName() {
        return "fcs16";
    }

    public int getHashSize() {
        return 2;
    }

    public int getBlockSize() {
        return 1;
    }

    public void update(int i) {
        fcs = (fcs >>> 8) ^ tab[(fcs ^ i) & 0xff];
    }

    public byte[] finish() {
        byte[] buf = new byte[2];
        bits.lsbPutW(buf, 0, fcs ^ 0xffff);
        return buf;
    }

}
