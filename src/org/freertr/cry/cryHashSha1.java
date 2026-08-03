package org.freertr.cry;

import java.security.MessageDigest;
import org.freertr.util.logger;

/**
 * secure hash algorithm 1 (fips180-1) hash
 *
 * @author matecsaba
 */
public class cryHashSha1 extends cryHashGeneric {

    /**
     * create instance
     */
    public cryHashSha1() {
    }

    private MessageDigest digest;

    /**
     * initialize
     */
    public void init() {
        final String name = "SHA-1";
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
        return "sha1";
    }

    /**
     * read oid of hash
     *
     * @return name of hash
     */
    public byte[] getPkcs() {
        return new byte[]{0x00, 0x30, 0x21, 0x30, 0x09, 0x06, 0x05, 0x2b, 0x0e, 0x03, 0x02, 0x1a, 0x05, 0x00, 0x04, 0x14};
    }

    /**
     * get hash size
     *
     * @return size
     */
    public int getHashSize() {
        return 20;
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
