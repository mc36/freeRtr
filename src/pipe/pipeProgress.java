package pipe;

import util.bits;

/**
 * progress bar
 *
 * @author matecsaba
 */
public class pipeProgress {

    private pipeSide pipe; // pipeline

    private long max = 0; // upper limit

    private long curr = 0; // current value

    private long last = 0; // last update

    private int stat = 0; // status value

    /**
     * create new progress bar
     *
     * @param pip
     */
    public pipeProgress(pipeSide pip) {
        pipe = pip;
    }

    /**
     * set maximum
     *
     * @param lim upper limit
     */
    public void setMax(long lim) {
        max = lim;
        setCurr(curr);
    }

    /**
     * get maximum value
     *
     * @return upper limit
     */
    public long getMax() {
        return max;
    }

    private void putLn(pipeSide.modTyp mod, String s) {
        s = bits.padEnd(s, 16, " ");
        pipeSide.modTyp i = pipe.lineTx;
        pipe.lineTx = mod;
        pipe.linePut(s);
        pipe.lineTx = i;
    }

    /**
     * put one line
     *
     * @param s string to write
     */
    public void putLine(String s) {
        putLn(pipeSide.modTyp.modeCR, s);
    }

    /**
     * set current value
     *
     * @param cur value
     */
    public void setCurr(long cur) {
        curr = cur;
        cur = bits.getTime();
        if ((cur - last) < 500) {
            return;
        }
        last = cur;
        String s = bits.padBeg(bits.percent(curr, max), 8, " ");
        stat = (stat + 1) % 4;
        String a;
        switch (stat) {
            case 0:
                a = "|";
                break;
            case 1:
                a = "/";
                break;
            case 2:
                a = "-";
                break;
            case 3:
                a = "\\";
                break;
            default:
                a = "?";
                break;
        }
        s = "(" + a + ") " + s;
        putLine(s);
    }

    /**
     * put debugging tx message
     *
     * @param s string
     */
    public void debugTx(String s) {
        putLn(pipeSide.modTyp.modeCRLF, "tx:" + s);
    }

    /**
     * put debugging rx message
     *
     * @param s string
     */
    public void debugRx(String s) {
        putLn(pipeSide.modTyp.modeCRLF, "rx:" + s);
    }

    /**
     * put debugging status message
     *
     * @param s string
     */
    public void debugStat(String s) {
        putLn(pipeSide.modTyp.modeCRLF, " - " + s);
    }

    /**
     * put debugging result message
     *
     * @param s string
     */
    public void debugRes(String s) {
        putLn(pipeSide.modTyp.modeCRLF, " * " + s);
    }

    /**
     *
     * @param que question
     * @param hide true to hide input
     * @return entered string
     */
    public String userInput(String que, boolean hide) {
        pipe.strPut(que);
        pipeSide.modTyp mod = pipe.lineTx;
        pipe.lineRx = pipeSide.modTyp.modeCRtryLF;
        int red = 0x32;
        if (hide) {
            red |= 1;
        }
        String res = pipe.lineGet(red);
        pipe.lineRx = mod;
        putLn(pipeSide.modTyp.modeCRLF, "");
        return res;
    }

}
