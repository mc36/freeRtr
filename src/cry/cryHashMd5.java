package cry;

import java.security.MessageDigest;

import util.logger;

/**
 * the message digest 5 (rfc1321) hash
 *
 * @author matecsaba
 */
public class cryHashMd5 extends cryHashGeneric {

    private MessageDigest digest;

    public void init() {
        final String name = "MD5";
        try {
            digest = MessageDigest.getInstance(name);
            digest.reset();
        } catch (Exception e) {
            logger.exception(e);
        }
    }

    public String getName() {
        return "md5";
    }

    public int getHashSize() {
        return 16;
    }

    public int getBlockSize() {
        return 64;
    }

    public void update(int i) {
        digest.update((byte) i);
    }

    public byte[] finish() {
        return digest.digest();
    }

}
