package snd;

/**
 * goertzel's discrete fourier transform
 *
 * @author matecsaba
 */
public class sndDft {

    /**
     * sample rate
     */
    public int sampRate = 8000;

    /**
     * sample data
     */
    public int[] sampDat;

    /**
     * sample position
     */
    public int sampPos;

    /**
     * sample size
     */
    public int sampSiz;

    /**
     * phase of generated sound
     */
    public double phase;

    /**
     * test one frequency
     *
     * @param len length to test
     * @param freq frequency to test
     * @return result
     */
    public int testFreq(int len, int freq) {
        int max = 1;
        for (int i = 0; i < len; i++) {
            int o = sampDat[sampPos + i];
            if (o < 0) {
                o = -o;
            }
            if (max < o) {
                max = o;
            }
        }
        max *= 7;
        double val1 = 0;
        double val2 = 0;
        double coeff = 2 * Math.cos(2 * Math.PI * freq / sampRate);
        for (int i = 0; i < len; i++) {
            double curr = ((double) sampDat[sampPos + i] / max) + (coeff * val1) - val2;
            val2 = val1;
            val1 = curr;
        }
        val1 = (val2 * val2) + (val1 * val1) - (coeff * val2 * val1);
        return (int) val1;
    }

    /**
     * generate tone
     *
     * @param len length to generate
     * @param freq frequency to use
     */
    public void makeFreq(int len, int freq) {
        for (int i = 0; i < len; i++) {
            phase = (phase + (2 * Math.PI * freq / sampRate)) % (2 * Math.PI);
            sampDat[sampPos + i] = (int) (Math.sin(phase) * 28000);
        }
    }

    /**
     * delete samples
     *
     * @param num number of samples to delete
     */
    public void sampDel(int num) {
        sampPos -= num;
        sampSiz -= num;
        if (sampPos < 0) {
            sampPos = 0;
        }
        if (sampSiz < 0) {
            sampSiz = 0;
        }
        for (int i = 0; i < sampSiz; i++) {
            sampDat[i] = sampDat[num + i];
        }
    }

    /**
     * add samples
     *
     * @param buf samples
     */
    public void sampAdd(int[] buf) {
        for (int i = 0; i < buf.length; i++) {
            sampDat[sampSiz + i] = buf[i];
        }
        sampSiz += buf.length;
    }

}
