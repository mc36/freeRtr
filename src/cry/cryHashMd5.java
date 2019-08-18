package cry;

import java.security.MessageDigest;

import util.logger;

/**
 * the message digest 5 (rfc1321) hash
 *
 * @author matecsaba
 */
public class cryHashMd5 extends cryHashGeneric {

    private MessageDigest digest;

    /**
     * initialize
     */
    public void init() {
        final String name = "MD5";
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
        return "md5";
    }

    /**
     * get hash size
     *
     * @return size
     */
    public int getHashSize() {
        return 16;
    }

    /**
     * get block size
     *
     * @return size
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
