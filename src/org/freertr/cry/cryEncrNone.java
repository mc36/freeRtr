package org.freertr.cry;

import org.freertr.util.bits;

/**
 * null encryption
 *
 * @author matecsaba
 */
public class cryEncrNone extends cryEncrGeneric {

    /**
     * create instance
     */
    public cryEncrNone() {
    }

    public void init(byte[] key, byte[] iv, boolean encrypt) {
    }

    public String getName() {
        return "none";
    }

    public int getBlockSize() {
        return 1;
    }

    public int getKeySize() {
        return 0;
    }

    public int getTagSize() {
        return 0;
    }

    public int getIVsize() {
        return 0;
    }

    public byte[] getNextIV() {
        return new byte[0];
    }

    public byte[] compute(byte[] buf, int ofs, int siz) {
        byte[] res = new byte[siz];
        bits.byteCopy(buf, ofs, res, 0, res.length);
        return res;
    }

    public void authAdd(byte[] buf, int ofs, int siz) {
    }

}
