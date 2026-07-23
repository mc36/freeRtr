
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
        double rms = Math.sqrt(sum * 4.0 / (double) len);
        return (20.0 * Math.log10(rms)) + 3.8;
    }

    private static double getAng(double vu) {
        if (vu <= -20) {
            return -25;
        }
        if (vu >= 3) {
            return 25;
        }
        if (vu <= -20) {
            return -23;
        }
        if (vu <= -10) {
            return -23 + ((vu + 20) / 10) * 7;
        }
        if (vu <= -7) {
            return -16 + ((vu + 10) / 3) * 4;
        }
        if (vu <= -5) {
            return -12 + ((vu + 7) / 2) * 4;
        }
        if (vu <= -3) {
            return -8 + ((vu + 5) / 2) * 5;
        }
        if (vu <= -2) {
            return -3 + ((vu + 3) / 1) * 3;
        }
        if (vu <= -1) {
            return 0 + ((vu + 2) / 1) * 3.5;
        }
        if (vu <= 0) {
            return 3.5 + ((vu + 1) / 1) * 4.5;
        }
        if (vu <= 1) {
            return 8 + (vu / 1) * 5;
        }
        if (vu <= 2) {
            return 13 + ((vu - 1) / 1) * 5;
        }
        if (vu <= 3) {
            return 18 + ((vu - 2) / 1) * 7;
        } else {
            return 25;
        }
    }

    private static String barL(double val) {
        String a = "";
        int cur = 25 + (int) val;
        for (int i = cur; i < 50; i++) {
            a += ' ';
        }
        for (int i = 0; i < cur; i++) {
            a += '*';
        }
        return a;
    }

    private static String barR(double val) {
        String a = "";
        int cur = 25 + (int) val;
        for (int i = 0; i < cur; i++) {
            a += '*';
        }
        for (int i = cur; i < 50; i++) {
            a += ' ';
        }
        return a;
    }

    public void doer(byte[] buf, int len) {
        if (cnt >= devicer.rate) {
            avgL /= (devicer.rate / rtper.payload);
            avgR /= (devicer.rate / rtper.payload);
            System.out.println(barL(avgL) + "  " + barR(avgR));
            avgL = 0.0;
            avgR = 0.0;
            cnt = 0;
        }
        double sumL = 0;
        double sumR = 0;
        for (int i = 0; i < len; i += 4) {
            double o = getSam(buf, i + 0);
            sumL += o * o;
            o = getSam(buf, i + 2);
            sumR += o * o;
        }
        sumL = getAng(getVu(sumL, len));
        sumR = getAng(getVu(sumR, len));
        avgL += sumL;
        avgR += sumR;
        cnt += len;
        System.out.print(barL(sumL) + "  " + barR(sumR) + "\r");
    }

}
