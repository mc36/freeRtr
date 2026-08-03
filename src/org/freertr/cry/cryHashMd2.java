package org.freertr.cry;

import java.security.MessageDigest;
import org.freertr.util.logger;

/**
 * the message digest 2 (rfc1319) hash
 *
 * @author matecsaba
 */
public class cryHashMd2 extends cryHashGeneric {

    /**
     * create instance
     */
    public cryHashMd2() {
    }

    private MessageDigest digest;

    /**
     * initialize
     */
    public void init() {
        final String name = "MD2";
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
        return "md2";
    }

    /**
     * read oid of hash
     *
     * @return name of hash
     */
    public byte[] getPkcs() {
        return new byte[]{0x00, 0x30, 0x20, 0x30, 0x0c, 0x06, 0x08, 0x2a, (byte) 0x86, 0x48, (byte) 0x86, (byte) 0xf7, 0x0d, 0x02, 0x02, 0x05, 0x00, 0x04, 0x10};
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
