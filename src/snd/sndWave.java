package snd;

import util.bits;

/**
 * wave file format
 *
 * @author matecsaba
 */
public class sndWave {

    /**
     * make header
     *
     * @param buf buffer to use
     * @param comp compression: 1=pcm, 6=aLaw, 7=uLaw
     */
    public static void makeHeader(byte[] buf, int comp) {
        bits.msbPutD(buf, 0, 0x52494646);
        bits.lsbPutD(buf, 4, buf.length - 8);
        bits.msbPutD(buf, 8, 0x57415645);
        bits.msbPutD(buf, 12, 0x666D7420);
        bits.lsbPutD(buf, 16, 0x10);
        bits.lsbPutW(buf, 20, comp);
        bits.lsbPutW(buf, 22, 1); // channels
        bits.lsbPutD(buf, 24, 8000); // sample rate
        bits.lsbPutD(buf, 28, 8000); // bytes / sec
        bits.lsbPutW(buf, 32, 1); // align
        bits.lsbPutW(buf, 34, 8); // bits / sample
        bits.msbPutD(buf, 36, 0x64617461);
        bits.lsbPutD(buf, 40, buf.length - 44);
    }

}
