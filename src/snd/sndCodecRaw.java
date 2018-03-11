package snd;

/**
 * raw codec
 *
 * @author matecsaba
 */
public class sndCodecRaw extends sndCodec {

    public int getRTPtype() {
        return 7;
    }

    public int getWAVtype() {
        return 1;
    }

    protected int calcDecodeOneValue(int val) {
        if ((val & 0x80) == 0) {
            return val << 8;
        }
        return (256 - val) << 8;
    }

}
