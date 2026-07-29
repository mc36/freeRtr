
public class visDoer {

    private final static int maxX = 52;

    private final static int maxY = 14;

    private final double[] vl = new double[2048];

    private final double[] vr = new double[2048];

    public visDoer() {
        System.out.print("\033[0;0H");
        System.out.print("\033[2J");
    }

    public void doer(byte[] buf, int len) {
        System.out.print("\033[0;0H");
        int num = len / (devicer.smpb * 2);
        int pos = vl.length - num;
        System.arraycopy(vl, num, vl, 0, pos);
        System.arraycopy(vr, num, vr, 0, pos);
        for (int i = 0; i < len; i += devicer.smpb * 2) {
            vl[pos] = sample(buf, i + 0);
            vr[pos] = sample(buf, i + devicer.smpb);
            pos++;
        }
        char[][] sl;
        char[][] sr;
        sl = wav(vl);
        sr = wav(vr);
        print(sl, sr);
        sl = rms(vl, 1, -1);
        sr = rms(vr, 0, 1);
        print(sl, sr);
        sl = fft(vl);
        sr = fft(vr);
        print(sl, sr);
    }

    private static void print(char[][] sl, char[][] sr) {
        for (int i = 0; i < sl.length; i++) {
            System.out.println(new String(sl[i]) + "    " + new String(sr[i]));
        }

    }

    private static double sample(byte[] buf, int ofs) {
        int i = (buf[ofs + 0] << 8) + (buf[ofs + 1] & 0xff);
        return (double) i / 32768.0;
    }

    private static int bitrev(int j, int nu) {
        int j1 = j;
        int k = 0;
        for (int i = 1; i <= nu; i++) {
            int j2 = j1 / 2;
            k = 2 * k + j1 - 2 * j2;
            j1 = j2;
        }
        return k;
    }

    private static int freqs(int x, int l, int m) {
        return (int) (Math.exp((x - l) / (double) l) * m);
    }

    private static char[][] fft(double sam[]) {
        double xre[] = new double[sam.length];
        System.arraycopy(sam, 0, xre, 0, sam.length);
        int num = xre.length;
        int nu = (int) (Math.log(num) / Math.log(2));
        int n2 = num / 2;
        int nu1 = nu - 1;
        double[] xim = new double[num];
        double[] mag = new double[n2];
        double tr, ti, p, arg, c, s;
        for (int l = 1; l <= nu; l++) {
            for (int k = 0; k < num; k += n2) {
                for (int i = 1; i <= n2; i++) {
                    p = bitrev(k >> nu1, nu);
                    arg = 2 * Math.PI * p / num;
                    c = Math.cos(arg);
                    s = Math.sin(arg);
                    tr = xre[k + n2] * c + xim[k + n2] * s;
                    ti = xim[k + n2] * c - xre[k + n2] * s;
                    xre[k + n2] = xre[k] - tr;
                    xim[k + n2] = xim[k] - ti;
                    xre[k] += tr;
                    xim[k] += ti;
                    k++;
                }
            }
            nu1--;
            n2 = n2 / 2;
        }
        for (int k = 0; k < num; k++) {
            int r = bitrev(k, nu);
            if (r > k) {
                tr = xre[k];
                ti = xim[k];
                xre[k] = xre[r];
                xim[k] = xim[r];
                xre[r] = tr;
                xim[r] = ti;
            }
        }
        mag[0] = Math.sqrt(xre[0] * xre[0] + xim[0] * xim[0]) / num;
        for (int i = 1; i < num / 2; i++) {
            mag[i] = 2 * Math.sqrt(xre[i] * xre[i] + xim[i] * xim[i]) / num;
        }
        double bin[] = new double[maxX];
        double max = Double.MIN_VALUE;
        for (int i = 0; i < bin.length; i++) {
            int pos = freqs(i, bin.length, mag.length);
            num = freqs(i + 1, bin.length, mag.length) - pos;
            double cur = 0;
            for (int o = 0; o < num; o++) {
                cur += mag[pos];
                pos++;
            }
            max = Math.max(max, cur);
            bin[i] = cur;
        }
        char[][] scr = new char[maxY][bin.length];
        for (int i = 0; i < bin.length; i++) {
            bin[i] = scr.length * bin[i] / max;
        }
        for (int i = 0; i < bin.length; i++) {
            int pos = scr.length - (int) bin[i];
            for (int o = 0; o < pos; o++) {
                scr[o][i] = ' ';
            }
            for (int o = pos; o < scr.length; o++) {
                scr[o][i] = '*';
            }
        }
        return scr;
    }

    private static char[][] rms(double sam[], int beg, int dir) {
        double rms = 0;
        for (int i = 0; i < sam.length; i++) {
            double o = sam[i];
            rms += o * o;
        }
        rms = Math.sqrt(rms / (double) sam.length);
        char[] scr = new char[maxX];
        int pos = beg * (scr.length + dir);
        rms = (scr.length * Math.log10(rms)) + scr.length;
        rms = Math.max(0, rms);
        rms = Math.min(scr.length, rms);
        int cur = (int) rms;
        for (int i = 0; i < cur; i++) {
            scr[pos] = '#';
            pos += dir;
        }
        for (int i = cur; i < scr.length; i++) {
            scr[pos] = ' ';
            pos += dir;
        }
        return new char[][]{scr};
    }

    private static char[][] wav(double sam[]) {
        char[][] scr = new char[maxY][maxX];
        for (int i = 0; i < scr.length; i++) {
            for (int o = 0; o < scr[0].length; o++) {
                scr[i][o] = ' ';
            }
        }
        int hlf = scr.length / 2;
        double[] bin = new double[scr[0].length];
        int num = sam.length / bin.length;
        for (int i = 0; i < bin.length; i++) {
            int pos = i * num;
            double cur = 0;
            for (int o = 0; o < num; o++) {
                cur += sam[pos];
                pos++;
            }
            bin[i] = cur / (double) num * (double) hlf;
        }
        for (int i = 0; i < bin.length; i++) {
            int o = hlf + (int) bin[i];
            scr[o][i] = '-';
        }
        return scr;
    }

}
