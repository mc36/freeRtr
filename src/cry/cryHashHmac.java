package cry;

/**
 * keyed hashing (rfc2104) message authentication
 *
 * @author matecsaba
 */
public class cryHashHmac extends cryHashGeneric {

    private cryHashGeneric alg;

    private byte[] ipad;

    private byte[] opad;

    /**
     * set up hmac
     *
     * @param mac mac algorithm
     * @param key key to use
     */
    public cryHashHmac(cryHashGeneric mac, byte[] key) {
        alg = mac;
        alg.init();
        ipad = new byte[alg.getBlockSize()];
        opad = new byte[ipad.length];
        if (key.length > ipad.length) {
            key = cryHashGeneric.compute(alg, key);
        }
        for (int i = 0; i < ipad.length; i++) {
            ipad[i] = 0x36;
            opad[i] = 0x5c;
        }
        for (int i = 0; i < key.length; i++) {
            ipad[i] ^= key[i];
            opad[i] ^= key[i];
        }
    }

    /**
     * initialize
     */
    public void init() {
        alg.init();
        alg.update(ipad);
    }

    /**
     * get name
     *
     * @return name
     */
    public String getName() {
        return "hmac-" + alg.getName();
    }

    /**
     * get hash size
     *
     * @return size
     */
    public int getHashSize() {
        return alg.getHashSize();
    }

    /**
     * get block size
     *
     * @return size
     */
    public int getBlockSize() {
        return alg.getBlockSize();
    }

    /**
     * compute block
     *
     * @param buf buffer
     * @param ofs offset
     * @param siz size
     */
    public void update(byte[] buf, int ofs, int siz) {
        alg.update(buf, ofs, siz);
    }

    /**
     * finish
     *
     * @return computed
     */
    public byte[] finish() {
        byte[] buf = alg.finish();
        alg.init();
        alg.update(opad);
        alg.update(buf);
        return alg.finish();
    }

}
