package cry;

/**
 * generic hash
 *
 * @author matecsaba
 */
public abstract class cryHashGeneric {

    /**
     * initialize hash
     */
    public abstract void init();

    /**
     * read name of hash
     *
     * @return name of hash
     */
    public abstract String getName();

    /**
     * read size of hash
     *
     * @return size in bytes
     */
    public abstract int getHashSize();

    /**
     * read size of block
     *
     * @return size in bytes
     */
    public abstract int getBlockSize();

    /**
     * update with buffer
     *
     * @param buf buffer to use
     * @param ofs offset in buffer
     * @param siz bytes to add
     */
    public abstract void update(byte[] buf, int ofs, int siz);

    /**
     * finish hash
     *
     * @return result
     */
    public abstract byte[] finish();

    /**
     * update with a byte
     *
     * @param i byte value
     */
    public void update(int i) {
        byte[] buf = new byte[1];
        buf[0] = (byte) (i & 0xff);
        update(buf, 0, buf.length);
    }

    /**
     * update with buffer
     *
     * @param buf buffer to use
     */
    public void update(byte[] buf) {
        update(buf, 0, buf.length);
    }

    /**
     * compute hash over a buffer
     *
     * @param alg algorithm to use
     * @param buf buffer to use
     * @param ofs offset in buffer
     * @param siz bytes to add
     * @return result
     */
    public static byte[] compute(cryHashGeneric alg, byte[] buf, int ofs, int siz) {
        alg.init();
        alg.update(buf, ofs, siz);
        return alg.finish();
    }

    /**
     * compute hash over a buffer
     *
     * @param alg algorithm to use
     * @param buf buffer to use
     * @return result
     */
    public static byte[] compute(cryHashGeneric alg, byte[] buf) {
        return compute(alg, buf, 0, buf.length);
    }

}
