package org.freertr.cry;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import org.freertr.util.logger;

/**
 * rivest cipher 2
 *
 * @author matecsaba
 */
public class cryEncrPCBCrc2 extends cryEncrGeneric {

    private Cipher crypter;

    /**
     * create instance
     */
    public cryEncrPCBCrc2() {
    }

    public void init(byte[] key, byte[] iv, boolean encrypt) {
        final String name = "RC2";
        int mode;
        if (encrypt) {
            mode = Cipher.ENCRYPT_MODE;
        } else {
            mode = Cipher.DECRYPT_MODE;
        }
        try {
            SecretKeySpec keyspec = new SecretKeySpec(key, name);
            IvParameterSpec ivspec = new IvParameterSpec(iv, 0, iv.length);
            crypter = Cipher.getInstance(name + "/PCBC/NoPadding");
            crypter.init(mode, keyspec, ivspec);
        } catch (Exception e) {
            logger.exception(e);
        }
    }

    public String getName() {
        return "rc2";
    }

    public int getBlockSize() {
        return 8;
    }

    public int getKeySize() {
        return 16;
    }

    public byte[] compute(byte[] buf, int ofs, int siz) {
        return crypter.update(buf, ofs, siz);
    }

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
