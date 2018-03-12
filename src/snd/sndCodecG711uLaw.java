package snd;

/**
 * itu g711 u law codec handler
 *
 * @author matecsaba
 */
public class sndCodecG711uLaw extends sndCodec {

    public int getRTPtype() {
        return 0;
    }

    public String getRTPname() {
        return "PCMU";
    }

    public int getWAVtype() {
        return 7;
    }

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
