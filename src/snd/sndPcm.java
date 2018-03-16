package snd;

/**
 * pulse code modulation
 *
 * @author matecsaba
 */
public class sndPcm extends sndCodec {

    private static int[] decode;

    private static int[] encode;

    public int getRTPtype() {
        return 5;
    }

    public String getRTPname() {
        return "PCM";
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
        if ((val & 0x80) != 0) {
            return -((0x80 - (val & 0x7f)) << 8);
        } else {
            return (val & 0x7f) << 8;
        }
    }

}
