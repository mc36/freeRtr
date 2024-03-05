package org.freertr.cry;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;
import org.freertr.util.logger;

/**
 * chacha20 cipher
 *
 * @author matecsaba
 */
public class cryEncrChacha20 extends cryEncrGeneric {

    private Cipher crypter;

    /**
     * create instance
     */
    public cryEncrChacha20() {
    }

    /**
     * initialize
     *
     * @param key key
     * @param iv iv
     * @param encrypt mode
     */
    public void init(byte[] key, byte[] iv, boolean encrypt) {
        final String name = "ChaCha20";
        int mode;
        if (encrypt) {
            mode = Cipher.ENCRYPT_MODE;
        } else {
            mode = Cipher.DECRYPT_MODE;
        }
        try {
            SecretKeySpec keyspec = new SecretKeySpec(key, name);
            crypter = Cipher.getInstance(name);
            crypter.init(mode, keyspec);
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
        return "chacha20";
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
     * get key size
     *
     * @return size
     */
    public int getKeySize() {
        return 32;
    }

    /**
     * compute block
     *
     * @param buf buffer
     * @param ofs offset
     * @param siz size
     * @return computed block
     */
    public byte[] compute(byte[] buf, int ofs, int siz) {
        return crypter.update(buf, ofs, siz);
    }

    /**
     * get next iv
     *
     * @return iv
     */
    public byte[] getNextIV() {
        return crypter.getIV();
    }

    /**
     * read iv size
     *
     * @return size in bytes
     */
    public int getIVsize() {
        return getBlockSize();
    }

    /**
     * get tag size
     *
     * @return size
     */
    public int getTagSize() {
        return 0;
    }

    /**
     * authenticate buffer
     *
     * @param buf buffer to use
     * @param ofs offset in buffer
     * @param siz bytes to add
     */
    public void authAdd(byte[] buf, int ofs, int siz) {
    }

}
