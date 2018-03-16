package snd;

/**
 * codec handler
 *
 * @author matecsaba
 */
public abstract class sndCodec {

    private int[] decode;

    private int[] encode;

    /**
     * calculate one decoded value
     *
     * @param val value to decode
     * @return decoded value
     */
    protected abstract int calcDecodeOneValue(int val);

    /**
     * get en/decode buffer
     *
     * @param dir true=encode, false=decode
     * @return buffer
     */
    protected abstract int[] getBuffer(boolean dir);

    /**
     * set en/decode buffer
     *
     * @param dir true=encode, false=decode
     * @param buf buffer
     */
    protected abstract void setBuffer(boolean dir, int[] buf);

    /**
     * get rtp type
     *
     * @return type
     */
    public abstract int getRTPtype();

    /**
     * get rtp name
     *
     * @return name
     */
    public abstract String getRTPname();

    /**
     * get wav type
     *
     * @return type
     */
    public abstract int getWAVtype();

    /**
     * construct tables
     */
    public sndCodec() {
        decode = getBuffer(false);
        if (decode == null) {
            decode = new int[128];
            for (int i = 0; i < decode.length; i++) {
                decode[i] = calcDecodeOneValue(i);
            }
            setBuffer(false, decode);
        }
        encode = getBuffer(true);
        if (encode == null) {
            encode = new int[32768];
            for (int i = 0; i < encode.length; i++) {
                int o = -1;
                int p = 0x100000;
                for (int q = 0; q < decode.length; q++) {
                    int r = i - decode[q];
                    if (r < 0) {
                        r = -r;
                    }
                    if (r > p) {
                        continue;
                    }
                    o = q;
                    p = r;
                }
                encode[i] = o;
            }
            setBuffer(true, encode);
        }
    }

    /**
     * encode one value
     *
     * @param val value to encode
     * @return encoded value
     */
    public int encodeInt(int val) {
        if (val < 0) {
            return encode[-val] | 0x80;
        } else {
            return encode[val];
        }
    }

    /**
     * decode one value
     *
     * @param val value to decode
     * @return decoded value
     */
    public int decodeInt(int val) {
        if ((val & 0x80) != 0) {
            return -decode[val & 0x7f];
        } else {
            return decode[val];
        }
    }

    /**
     * encode buffer
     *
     * @param buf buffer to use
     * @return result
     */
    public byte[] encodeBuf(int[] buf) {
        return encodeBuf(buf, 0, buf.length);
    }

    /**
     * encode buffer
     *
     * @param buf buffer to use
     * @param ofs offset where from
     * @param siz number of samples
     * @return result
     */
    public byte[] encodeBuf(int[] buf, int ofs, int siz) {
        byte[] res = new byte[siz];
        for (int i = 0; i < res.length; i++) {
            res[i] = (byte) encodeInt(buf[ofs + i]);
        }
        return res;
    }

    /**
     * decode buffer
     *
     * @param buf buffer to use
     * @param ofs offset where from
     * @param siz number of samples
     * @return result
     */
    public int[] decodeBuf(byte[] buf, int ofs, int siz) {
        int[] res = new int[siz];
        for (int i = 0; i < res.length; i++) {
            res[i] = decodeInt(buf[ofs + i]);
        }
        return res;
    }

    /**
     * decode buffer
     *
     * @param buf buffer to use
     * @return result
     */
    public int[] decodeBuf(byte[] buf) {
        return decodeBuf(buf, 0, buf.length);
    }

    /**
     * decode buffer
     *
     * @param buf buffer to use
     * @param ofs offset where from
     * @param siz number of samples
     * @return result
     */
    public byte[] degradeBuf(byte[] buf, int ofs, int siz) {
        byte[] res = new byte[siz];
        for (int i = 0; i < res.length; i++) {
            res[i] = (byte) (decodeInt(buf[ofs + i]) >>> 8);
        }
        return res;
    }

    /**
     * decode buffer
     *
     * @param buf buffer to use
     * @return result
     */
    public byte[] degradeBuf(byte[] buf) {
        return degradeBuf(buf, 0, buf.length);
    }

}
