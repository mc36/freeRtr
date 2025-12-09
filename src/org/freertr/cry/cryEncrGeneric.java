package org.freertr.cry;

import org.freertr.util.bits;

/**
 * generic cipher
 *
 * @author matecsaba
 */
public abstract class cryEncrGeneric {

    /**
     * create instance
     */
    public cryEncrGeneric() {
    }

    /**
     * initialize cipher
     *
     * @param key key material
     * @param iv init vector
     * @param encrypt true for encryption, false for decryption
     */
    public abstract void init(byte[] key, byte[] iv, boolean encrypt);

    /**
     * read name of hash
     *
     * @return name of hash
     */
    public abstract String getName();

    /**
     * read size of block
     *
     * @return size in bytes
     */
    public abstract int getBlockSize();

    /**
     * read size of key
     *
     * @return size in bytes
     */
    public abstract int getKeySize();


    /**
     * read size of tag
     *
     * @return size in bytes
     */
    public abstract int getTagSize();

    /**
     * update buffer
     *
     * @param buf buffer to use
     * @param ofs offset in buffer
     * @param siz bytes to add
     * @return updated buffer
     */
    public abstract byte[] compute(byte[] buf, int ofs, int siz);

    /**
     * get next iv to use
     *
     * @return value
     */
    public abstract byte[] getNextIV();

    /**
     * read iv size
     *
     * @return size in bytes
     */
    public abstract int getIVsize();

    /**
     * authenticate buffer
     *
     * @param buf buffer to use
     * @param ofs offset in buffer
     * @param siz bytes to add
     */
    public abstract void authAdd(byte[] buf, int ofs, int siz);

    /**
     * authenticate buffer
     *
     * @param buf buffer to use
     */
    public void authAdd(byte[] buf) {
        authAdd(buf, 0, buf.length);
    }

    /**
     * update with buffer
     *
     * @param buf buffer to use
     * @return updated buffer
     */
    public byte[] compute(byte[] buf) {
        return compute(buf, 0, buf.length);
    }

    /**
     * update with buffer
     *
     * @param buf buffer to use
     * @param ofs offset in buffer
     * @param siz bytes to add
     * @return -1 on error, number of bytes on success
     */
    public int update(byte[] buf, int ofs, int siz) {
        byte[] part = compute(buf, ofs, siz);
        if (part == null) {
            return -1;
        }
        bits.byteCopy(part, 0, buf, ofs, part.length);
        return part.length;
    }

    /**
     * compute a buffer
     *
     * @param alg algorithm to use
     * @param key key material
     * @param iv init vector
     * @param encrypt true for encryption, false for decryption
     * @param buf buffer to use
     * @param ofs offset in buffer
     * @param siz bytes to add
     */
    public static void update(cryEncrGeneric alg, byte[] key, byte[] iv, boolean encrypt, byte[] buf, int ofs, int siz) {
        alg.init(key, iv, encrypt);
        alg.update(buf, ofs, siz);
    }

    /**
     * compute a buffer
     *
     * @param alg algorithm to use
     * @param key key material
     * @param iv init vector
     * @param encrypt true for encryption, false for decryption
     * @param buf buffer to use
     * @return updated buffer
     */
    public static byte[] compute(cryEncrGeneric alg, byte[] key, byte[] iv, boolean encrypt, byte[] buf) {
        alg.init(key, iv, encrypt);
        return alg.compute(buf);
    }

    /**
     * compute a buffer
     *
     * @param alg algorithm to use
     * @param key key material
     * @param iv init vector
     * @param encrypt true for encryption, false for decryption
     * @param auth authentication data, null if nothing
     * @param buf buffer to use
     * @param ofs offset in buffer
     * @param siz bytes to add
     * @return -1 on error, number of bytes on success
     */
    public static int update(cryEncrGeneric alg, byte[] key, byte[] iv, boolean encrypt, byte[] auth, byte[] buf, int ofs, int siz) {
        alg.init(key, iv, encrypt);
        if (auth != null) {
            alg.authAdd(auth);
        }
        return alg.update(buf, ofs, siz);
    }

    /**
     * compute a buffer
     *
     * @param alg algorithm to use
     * @param key key material
     * @param iv init vector
     * @param encrypt true for encryption, false for decryption
     * @param auth authentication data, null if nothing
     * @param buf buffer to use
     * @return updated buffer, null on error
     */
    public static byte[] compute(cryEncrGeneric alg, byte[] key, byte[] iv, boolean encrypt, byte[] auth, byte[] buf) {
        alg.init(key, iv, encrypt);
        if (auth != null) {
            alg.authAdd(auth);
        }
        return alg.compute(buf);
    }

}
