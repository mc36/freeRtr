package cry;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;

import util.logger;

/**
 * rivest cipher 4
 *
 * @author matecsaba
 */
public class cryEncrRc4 extends cryEncrGeneric {

    private Cipher crypter;

    public void init(byte[] key, byte[] iv, boolean encrypt) {
        final String name = "RC4";
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
        return "rc4";
    }

    public int getBlockSize() {
        return 1;
    }

    public int getKeySize() {
        return 8;
    }

    public byte[] compute(byte[] buf, int ofs, int siz) {
        return crypter.update(buf, ofs, siz);
    }

    public byte[] getNextIV() {
        return crypter.getIV();
    }

}
