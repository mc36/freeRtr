package cry;

import java.security.MessageDigest;

import util.logger;

/**
 * secure hash algorithm 2-28 (fips180-2) hash
 *
 * @author matecsaba
 */
public class cryHashSha2224 extends cryHashGeneric {

    private MessageDigest digest;

    /**
     * initialize
     */
    public void init() {
        final String name = "SHA-224";
        try {
            digest = MessageDigest.getInstance(name);
            digest.reset();
        } catch (Exception e) {
            logger.exception(e);
        }
    }

    /**
     * get name
     *
     * @return name
     */
    public String getName() {
        return "sha224";
    }

    /**
     * get hash size
     *
     * @return size
     */
    public int getHashSize() {
        return 28;
    }

    /**
     * get block size
     *
     * @return
     */
    public int getBlockSize() {
        return 64;
    }

    /**
     * compute block
     *
     * @param buf buffer
     * @param ofs offset
     * @param siz size
     */
    public void update(byte[] buf, int ofs, int siz) {
        digest.update(buf, ofs, siz);
    }

    /**
     * finish
     *
     * @return computed
     */
    public byte[] finish() {
        return digest.digest();
    }

}
