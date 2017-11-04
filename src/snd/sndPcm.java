package snd;

/**
 * pulse code modulation
 *
 * @author matecsaba
 */
public class sndPcm extends sndCodec {

    public int getRTPtype() {
        return -1;
    }

    protected int calcDecodeOneValue(int val) {
        if ((val & 0x80) != 0) {
            return -((0x80 - (val & 0x7f)) << 8);
        } else {
            return (val & 0x7f) << 8;
        }
    }

}
