package cry;

/**
 * atm header checksum 8bits x**8 + x**2 + x + 1
 *
 * @author matecsaba
 */
public class cryHashHec8 extends cryHashGeneric {

    private static int[] tab = null;

    private int sum;

    private int mkTabEntry(int i) {
        for (int o = 0; o < 8; o++) {
            if ((i & 0x80) != 0) {
                i = (i << 1) ^ 0x107;
            } else {
                i = (i << 1);
            }
        }
        return i & 0xff;
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
        sum = 0;
    }

    /**
     * set frame checksum
     *
     * @param i new value
     */
    public void setSum(int i) {
        sum = i;
    }

    public String getName() {
        return "hec8";
    }

    public int getHashSize() {
        return 1;
    }

    public int getBlockSize() {
        return 1;
    }

    public void update(int i) {
        sum = tab[(sum ^ i) & 0xff];
    }

    public byte[] finish() {
        byte[] buf = new byte[1];
        buf[0] = (byte) ((sum ^ 0x55) & 0xff);
        return buf;
    }

}
