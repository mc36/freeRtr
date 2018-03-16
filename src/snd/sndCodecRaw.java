package snd;

/**
 * raw codec
 *
 * @author matecsaba
 */
public class sndCodecRaw extends sndCodec {

    private static int[] decode;

    private static int[] encode;

    public int getRTPtype() {
        return 7;
    }

    public String getRTPname() {
        return "RAW";
    }

    public int getWAVtype() {
        return 1;
    }

    protected int[] getBuffer(boolean dir) {
        if (dir) {
            return encode;
        } else {
            return decode;
        }
    }

    protected void setBuffer(boolean dir, int[] buf) {
        if (dir) {
            encode = buf;
        } else {
            decode = buf;
        }
    }

    protected int calcDecodeOneValue(int val) {
        if ((val & 0x80) == 0) {
            return val << 8;
        }
        return (256 - val) << 8;
    }

}
