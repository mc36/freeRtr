package cry;

import java.security.MessageDigest;

import util.logger;

/**
 * secure hash algorithm 1 (fips180-1) hash
 *
 * @author matecsaba
 */
public class cryHashSha1 extends cryHashGeneric {

    private MessageDigest digest;

    public void init() {
        final String name = "SHA-1";
        try {
            digest = MessageDigest.getInstance(name);
            digest.reset();
        } catch (Exception e) {
            logger.exception(e);
        }
    }

    public String getName() {
        return "sha1";
    }

    public int getHashSize() {
        return 20;
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
