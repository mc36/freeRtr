package cry;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;

import util.logger;

/**
 * blowfish by counterpane
 *
 * @author matecsaba
 */
public class cryEncrECBblowfish extends cryEncrGeneric {

    private Cipher crypter;

    public void init(byte[] key, byte[] iv, boolean encrypt) {
        final String name = "BLOWFISH";
        int mode;
        if (encrypt) {
            mode = Cipher.ENCRYPT_MODE;
        } else {
            mode = Cipher.DECRYPT_MODE;
        }
        try {
            SecretKeySpec keyspec = new SecretKeySpec(key, name);
            crypter = Cipher.getInstance(name + "/ECB/NoPadding");
            crypter.init(mode, keyspec);
        } catch (Exception e) {
            logger.exception(e);
        }
    }

    public String getName() {
        return "blowfish";
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

}
