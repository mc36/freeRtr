package cry;

import java.security.MessageDigest;

import util.logger;

/**
 * secure hash algorithm 2-48 (fips180-2) hash
 *
 * @author matecsaba
 */
public class cryHashSha384 extends cryHashGeneric {

    private MessageDigest digest;

    public void init() {
        final String name = "SHA-384";
        try {
            digest = MessageDigest.getInstance(name);
            digest.reset();
        } catch (Exception e) {
            logger.exception(e);
        }
    }

    public String getName() {
        return "sha384";
    }

    public int getHashSize() {
        return 64;
    }

    public int getBlockSize() {
        return 128;
    }

    public void update(int i) {
        digest.update((byte) i);
    }

    public byte[] finish() {
        return digest.digest();
    }

}
