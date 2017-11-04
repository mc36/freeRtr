package cry;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;

import util.logger;

/**
 * digital encryption standard
 *
 * @author matecsaba
 */
public class cryEncrCFBdes extends cryEncrGeneric {

    private Cipher crypter;

    public void init(byte[] key, byte[] iv, boolean encrypt) {
        final String name = "DES";
        int mode;
        if (encrypt) {
            mode = Cipher.ENCRYPT_MODE;
        } else {
            mode = Cipher.DECRYPT_MODE;
        }
        try {
            SecretKeySpec keyspec = new SecretKeySpec(key, name);
            IvParameterSpec ivspec = new IvParameterSpec(iv, 0, iv.length);
            crypter = Cipher.getInstance(name + "/CFB/NoPadding");
            crypter.init(mode, keyspec, ivspec);
        } catch (Exception e) {
            logger.exception(e);
        }
    }

    public String getName() {
        return "des";
    }

    public int getBlockSize() {
        return 8;
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
