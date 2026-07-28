
/**
 * measure vu level
 *
 * @author matecsaba
 */
public class vuDoer {

    private double avgL = 0.0;

    private double avgR = 0.0;

    private int cnt = 0;

    private static double getSam(byte[] buf, int ofs) {
        int i = (buf[ofs + 0] << 8) + (buf[ofs + 1] & 0xff);
        return (double) i / 32768.0;
    }

    private static double getVu(double sum, int len) {
        double rms = Math.sqrt(sum * (double) (2 * devicer.smpb) / (double) len);
        return Math.max(0.0, Math.min(50.0, (50.0 * Math.log10(rms)) + 50.0));
    }

    private static int barE(byte[] out, int pos, double val) {
        int cur = (int) val;
        for (int i = cur; i < 50; i++) {
            out[pos] = ' ';
            pos++;
        }
        return pos;
    }

    private static int barM(byte[] out, int pos, double val) {
        int cur = (int) val;
        for (int i = 0; i < cur; i++) {
            out[pos] = '*';
            pos++;
        }
        return pos;
    }

    private static int barC(byte[] out, int pos, byte c) {
        out[pos] = c;
        pos++;
        return pos;
    }

    private static String bars(double l, double r, byte e) {
        byte[] out = new byte[200];
        int pos = 0;
        pos = barE(out, pos, l);
        pos = barM(out, pos, l);
        pos = barC(out, pos, (byte) 32);
        pos = barC(out, pos, (byte) 32);
        pos = barM(out, pos, r);
        pos = barE(out, pos, r);
        pos = barC(out, pos, e);
        return new String(out, 0, pos);
    }

    public void doer(byte[] buf, int len) {
        if (cnt >= devicer.rate) {
            avgL /= ((double) devicer.rate / (double) devicer.payl);
            avgR /= ((double) devicer.rate / (double) devicer.payl);
            System.out.print(bars(avgL, avgR, (byte) 10));
            avgL = 0.0;
            avgR = 0.0;
            cnt = 0;
        }
        double sumL = 0;
        double sumR = 0;
        for (int i = 0; i < len; i += devicer.smpb * 2) {
            double o = getSam(buf, i + 0);
            sumL += o * o;
            o = getSam(buf, i + devicer.smpb);
            sumR += o * o;
        }
        sumL = getVu(sumL, len);
        sumR = getVu(sumR, len);
        avgL += sumL;
        avgR += sumR;
        cnt += len;
        System.out.print(bars(sumL, sumR, (byte) 13));
    }

}
