package snd;

/**
 * dual tone multi frequency signaling
 *
 * @author matecsaba
 */
public class sndDtmf extends sndDft {

    private final static int[] dtmfCols = {1209, 1336, 1477, 1633};

    private final static int[] dtmfRows = {697, 770, 852, 941};

    private final static char[][] dtmfRowXcol = {
        {'1', '2', '3', 'a'},
        {'4', '5', '6', 'b'},
        {'7', '8', '9', 'c'},
        {'*', '0', '#', 'd'}
    };

    private final static int[] busy = {480, 620};

    private final static int[] ringback = {440, 480};

    private final static int[] dialTone = {350, 440};

    private static int detectWinner(int[] sen) {
        int bn = 0;
        int bv = sen[bn];
        for (int i = 1; i < sen.length; i++) {
            if (sen[i] < bv) {
                continue;
            }
            bn = i;
            bv = sen[bn];
        }
        if (bv < 0) {
            return -1;
        }
        bv /= 1000000;
        for (int i = 0; i < sen.length; i++) {
            if (i == bn) {
                continue;
            }
            if (sen[i] > bv) {
                return -1;
            }
        }
        return bn;
    }

    /**
     * get dtmf codes
     *
     * @return detected code
     */
    public String getCode() {
        if ((sampSiz * 4) < sampRate) {
            return "";
        }
        int[] cf = new int[dtmfCols.length];
        for (int i = 0; i < cf.length; i++) {
            cf[i] = testFreq(sampSiz, dtmfCols[i]);
        }
        int[] rf = new int[dtmfRows.length];
        for (int i = 0; i < rf.length; i++) {
            rf[i] = testFreq(sampSiz, dtmfRows[i]);
        }
        sampDel(sampSiz);
        int cb = detectWinner(cf);
        if (cb < 0) {
            return "";
        }
        int rb = detectWinner(rf);
        if (rb < 0) {
            return "";
        }
        return "" + dtmfRowXcol[rb][cb];
    }

}
