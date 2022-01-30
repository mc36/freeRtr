package net.freertr.cry;

/**
 * null hash
 *
 * @author matecsaba
 */
public class cryHashNone extends cryHashGeneric {

    public void init() {
    }

    public String getName() {
        return "none";
    }

    public byte[] getPkcs() {
        return new byte[0];
    }

    public int getHashSize() {
        return 0;
    }

    public int getBlockSize() {
        return 1;
    }

    public void update(byte[] buf, int ofs, int siz) {
    }

    public byte[] finish() {
        return new byte[0];
    }

}
