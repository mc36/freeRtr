package org.freertr.cry;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import org.freertr.util.logger;

/**
 * chacha20 cipher
 *
 * @author matecsaba
 */
public class cryEncrChacha20poly1305 extends cryEncrGeneric {

    private Cipher crypter;

    /**
     * create instance
     */
    public cryEncrChacha20poly1305() {
    }

    /**
     * initialize
     *
     * @param key key
     * @param iv iv
     * @param encrypt mode
     */
    public void init(byte[] key, byte[] iv, boolean encrypt) {
        int mode;
        if (encrypt) {
            mode = Cipher.ENCRYPT_MODE;
        } else {
            mode = Cipher.DECRYPT_MODE;
        }
        try {
            SecretKeySpec keyspec = new SecretKeySpec(key, "ChaCha20");
            IvParameterSpec ivspec = new IvParameterSpec(iv, 0, iv.length);
            crypter = Cipher.getInstance("ChaCha20-Poly1305/None/NoPadding");
            crypter.init(mode, keyspec, ivspec);
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
        return "chacha20poly1305";
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
     * read iv of key
     *
     * @return size in bytes
     */
    public int getIVsize() {
        return 12;
    }

    /**
     * get tag size
     *
     * @return size
     */
    public int getTagSize() {
        return 16;
    }

    /**
     * authenticate buffer
     *
     * @param buf buffer to use
     * @param ofs offset in buffer
     * @param siz bytes to add
     */
    public void authAdd(byte[] buf, int ofs, int siz) {
        crypter.updateAAD(buf, ofs, siz);
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
        try {
            return crypter.doFinal(buf, ofs, siz);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * get next iv
     *
     * @return iv
     */
    public byte[] getNextIV() {
        return crypter.getIV();
    }

}
