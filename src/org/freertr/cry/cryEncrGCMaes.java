package org.freertr.cry;

import javax.crypto.Cipher;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import org.freertr.util.logger;

/**
 * advanced encryption standard (rijndael)
 *
 * @author matecsaba
 */
public class cryEncrGCMaes extends cryEncrGeneric {

    private Cipher crypter;

    /**
     * create instance
     */
    public cryEncrGCMaes() {
    }

    /**
     * initialize
     *
     * @param key key
     * @param iv iv
     * @param encrypt mode
     */
    public void init(byte[] key, byte[] iv, boolean encrypt) {
        final String name = "AES";
        int mode;
        if (encrypt) {
            mode = Cipher.ENCRYPT_MODE;
        } else {
            mode = Cipher.DECRYPT_MODE;
        }
        try {
            SecretKeySpec keyspec = new SecretKeySpec(key, name);
            GCMParameterSpec ivspec = new GCMParameterSpec(128, iv);
            crypter = Cipher.getInstance(name + "/GCM/NoPadding");
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
        return "aes";
    }

    /**
     * get block size
     *
     * @return size
     */
    public int getBlockSize() {
        return 16;
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
     * get tag size
     *
     * @return size
     */
    public int getTagSize() {
        return 16;
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
