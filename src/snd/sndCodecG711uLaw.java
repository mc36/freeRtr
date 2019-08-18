package snd;

/**
 * itu g711 u law codec handler
 *
 * @author matecsaba
 */
public class sndCodecG711uLaw extends sndCodec {

    private static int[] decode;

    private static int[] encode;

    /**
     * get rtp type
     *
     * @return type
     */
    public int getRTPtype() {
        return 0;
    }

    /**
     * get rtp name
     *
     * @return name
     */
    public String getRTPname() {
        return "PCMU";
    }

    /**
     * get wav type
     *
     * @return type
     */
    public int getWAVtype() {
        return 7;
    }

    /**
     * get buffer
     *
     * @param dir direction
     * @return buffer
     */
    protected int[] getBuffer(boolean dir) {
        if (dir) {
            return encode;
        } else {
            return decode;
        }
    }

    /**
     * set buffer
     *
     * @param dir direction
     * @param buf buffer
     */
    protected void setBuffer(boolean dir, int[] buf) {
        if (dir) {
            encode = buf;
        } else {
            decode = buf;
        }
    }

    /**
     * calculate value
     *
     * @param val value
     * @return value
     */
    protected int calcDecodeOneValue(int val) {
        val ^= 0x7f;
        boolean signed = (val & 0x80) != 0;
        int exp = (val >>> 4) & 0x7;
        int man = val & 0xf;
        man |= 0x10;
        val = man << (exp + 3);
        if (signed) {
            val = -val;
        }
        return val;
    }

}
