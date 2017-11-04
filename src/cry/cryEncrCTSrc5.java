package cry;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import util.logger;

/**
 * rivest cipher 5
 *
 * @author matecsaba
 */
public class cryEncrCTSrc5 extends cryEncrGeneric {

    private Cipher crypter;

    public void init(byte[] key, byte[] iv, boolean encrypt) {
        final String name = "RC5";
        int mode;
        if (encrypt) {
            mode = Cipher.ENCRYPT_MODE;
        } else {
            mode = Cipher.DECRYPT_MODE;
        }
        try {
            SecretKeySpec keyspec = new SecretKeySpec(key, name);
            IvParameterSpec ivspec = new IvParameterSpec(iv, 0, iv.length);
            crypter = Cipher.getInstance(name + "/CTS/NoPadding");
            crypter.init(mode, keyspec, ivspec);
        } catch (Exception e) {
            logger.exception(e);
        }
    }

    public String getName() {
        return "rc5";
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
