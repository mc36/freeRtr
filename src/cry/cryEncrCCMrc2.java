package cry;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import util.logger;

/**
 * rivest cipher 2
 *
 * @author matecsaba
 */
public class cryEncrCCMrc2 extends cryEncrGeneric {

    private Cipher crypter;

    /**
     * initialize
     *
     * @param key key
     * @param iv iv
     * @param encrypt mode
     */
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
            crypter = Cipher.getInstance(name + "/CCM/NoPadding");
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
        return "rc2";
    }

    /**
     * get block size
     *
     * @return size
     */
    public int getBlockSize() {
        return 8;
    }

    /**
     * get key size
     *
     * @return size
     */
    public int getKeySize() {
        return 16;
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

}
