package org.freertr.cry;

import java.security.MessageDigest;
import org.freertr.util.logger;

/**
 * secure hash algorithm 3-48 (fips202) hash
 *
 * @author matecsaba
 */
public class cryHashSha3384 extends cryHashGeneric {

    /**
     * create instance
     */
    public cryHashSha3384() {
    }

    private MessageDigest digest;

    /**
     * initialize
     */
    public void init() {
        final String name = "SHA3-384";
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
        return "sha3-384";
    }

    /**
     * read oid of hash
     *
     * @return name of hash
     */
    public byte[] getPkcs() {
        return new byte[]{0x00, 0x30, 0x41, 0x30, 0x0d, 0x06, 0x09, 0x60, (byte) 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x02, 0x09, 0x05, 0x00, 0x04, 0x30};
    }

    /**
     * get hash size
     *
     * @return size
     */
    public int getHashSize() {
        return 48;
    }

    /**
     * get block size
     *
     * @return size
     */
    public int getBlockSize() {
        return 104;
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
