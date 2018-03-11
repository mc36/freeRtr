package snd;

/**
 * itu g711 a law codec handler
 *
 * @author matecsaba
 */
public class sndCodecG711aLaw extends sndCodec {

    public int getRTPtype() {
        return 8;
    }

    public int getWAVtype() {
        return 6;
    }

    protected int calcDecodeOneValue(int val) {
        val ^= 0x55;
        boolean signed = (val & 0x80) != 0;
        int exp = (val >>> 4) & 0x7;
        int man = val & 0xf;
        if (exp == 0) {
            exp++;
        } else {
            man |= 0x10;
        }
        val = man << (exp + 3);
        if (signed) {
            val = -val;
        }
        return val;
    }

}
