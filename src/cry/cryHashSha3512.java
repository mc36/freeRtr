package cry;

import java.security.MessageDigest;

import util.logger;

/**
 * secure hash algorithm 3-64 (fips202) hash
 *
 * @author matecsaba
 */
public class cryHashSha3512 extends cryHashGeneric {

    private MessageDigest digest;

    /**
     * initialize
     */
    public void init() {
        final String name = "SHA3-512";
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
        return "sha3-512";
    }

    /**
     * get hash size
     *
     * @return size
     */
    public int getHashSize() {
        return 64;
    }

    /**
     * get block size
     *
     * @return size
     */
    public int getBlockSize() {
        return 128;
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
