package snd;

/**
 * frequency shift keying
 *
 * @author matecsaba
 */
public class sndFsk extends sndDft {

    /**
     * itu v21 carriers
     */
    public final static int[] ituV21carrier = {1080, 1750};

    /**
     * bell 103 carriers
     */
    public final static int[] bell103carrier = {1170, 2125};

    /**
     * bit / second
     */
    public int bitPerSec = 300;

    /**
     * bit / byte
     */
    public int bitPerByte = 8;

    /**
     * carrier frequency
     */
    public int carrier;

    private int freqRes0;

    private int freqRes1;

    /**
     * saw carrier
     */
    public boolean sawCarrier;

    /**
     * find start of byte
     *
     * @return false on success, true on error
     */
    public boolean findStart() {
        sawCarrier = false;
        for (;;) {
            int i = decodeBit();
            if (i == -2) {
                return true;
            }
            if ((i == 0) && (freqRes1 == 0)) {
                sawCarrier = true;
                return false;
            }
            if ((i == 1) && (freqRes0 == 0)) {
                sawCarrier = true;
                sampPos += sampRate / bitPerSec / 2;
            }
            sampPos++;
        }
    }

    private int decodeBit() {
        final int siz = sampRate / bitPerSec;
        if ((sampPos + siz) > sampSiz) {
            return -2;
        }
        freqRes0 = testFreq(siz, carrier + 100);
        freqRes1 = testFreq(siz, carrier - 100);
        if (freqRes0 < freqRes1) {
            return 1;
        }
        if (freqRes0 > freqRes1) {
            return 0;
        }
        return -1;
    }

    private void encodeBit(int val, int siz) {
        if (siz < 0) {
            siz = sampRate / bitPerSec;
        }
        if ((val & 1) == 0) {
            makeFreq(siz, carrier + 100);
        } else {
            makeFreq(siz, carrier - 100);
        }
        sampPos += siz;
    }

    /**
     * decode byte
     *
     * @return decoded value
     */
    public int decodeByte() {
        sampPos += sampRate / bitPerSec;
        int vl = 0;
        int bg = sampPos;
        for (int ps = 0; ps < bitPerByte; ps++) {
            sampPos = bg + ((sampRate * ps) / bitPerSec);
            vl |= (decodeBit() << ps);
        }
        sampPos += sampRate / bitPerSec;
        return vl;
    }

    /**
     * minimum bytes to have
     *
     * @return samples needed
     */
    public int decodeMin() {
        return (sampRate * 12) / bitPerSec;
    }

    /**
     * encode byte
     *
     * @param val value
     */
    public void encodeByte(int val) {
        encodeBit(1, -1);
        encodeBit(0, -1);
        int bg = sampPos;
        for (int ps = 0; ps < bitPerByte; ps++) {
            encodeBit(val >>> ps, bg + ((sampRate * (ps + 1)) / bitPerSec) - sampPos);
        }
        encodeBit(1, -1);
    }

    /**
     * generate silence
     */
    public void encodeSilence() {
        encodeBit(1, -1);
    }

}
