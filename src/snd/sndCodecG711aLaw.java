package snd;

/**
 * itu g711 a law codec handler
 *
 * @author matecsaba
 */
public class sndCodecG711aLaw extends sndCodec {

    private static int[] decode;

    private static int[] encode;

    public int getRTPtype() {
        return 8;
    }

    public String getRTPname() {
        return "PCMA";
    }

    public int getWAVtype() {
        return 6;
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
