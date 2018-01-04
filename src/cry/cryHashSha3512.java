package cry;

import java.security.MessageDigest;

import util.logger;

/**
 * secure hash algorithm 3-64 (fips202) hash
 *
 * @author matecsaba
 */
public class cryHashSha3512 extends cryHashGeneric {

    private MessageDigest digest;

    public void init() {
        final String name = "SHA3-512";
        try {
            digest = MessageDigest.getInstance(name);
            digest.reset();
        } catch (Exception e) {
            logger.exception(e);
        }
    }

    public String getName() {
        return "sha3-512";
    }

    public int getHashSize() {
        return 64;
    }

    public int getBlockSize() {
        return 128;
    }

    public void update(byte[] buf, int ofs, int siz) {
        digest.update(buf, ofs, siz);
    }

    public byte[] finish() {
        return digest.digest();
    }

}
