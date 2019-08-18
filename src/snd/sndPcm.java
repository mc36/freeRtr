package snd;

/**
 * pulse code modulation
 *
 * @author matecsaba
 */
public class sndPcm extends sndCodec {

    private static int[] decode;

    private static int[] encode;

    /**
     * get rtp type
     *
     * @return type
     */
    public int getRTPtype() {
        return 5;
    }

    /**
     * get rtp name
     *
     * @return name
     */
    public String getRTPname() {
        return "PCM";
    }

    /**
     * get wav type
     *
     * @return type
     */
    public int getWAVtype() {
        return 1;
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
        if ((val & 0x80) != 0) {
            return -((0x80 - (val & 0x7f)) << 8);
        } else {
            return (val & 0x7f) << 8;
        }
    }

}
