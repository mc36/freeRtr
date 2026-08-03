
import javax.sound.sampled.TargetDataLine;

/**
 * measure tone accuracy
 *
 * @author matecsaba
 */
public class measFreq {

    public static void main(String[] args) throws Exception {
        int frq = Integer.parseInt(args[3]);
        int vol = Integer.parseInt(args[4]);
        TargetDataLine dataLine = devicer.getRecord(args[0]);
        rtper rtp = new rtper(args[1], args[2]);
        byte[] buf = new byte[consts.payl];
        byte[] nxt = new byte[buf.length];
        long pos = 0;
        int[] rcv = new int[(consts.rate / consts.payl) * consts.payl];
        int got = 0;
        int top = -1;
        for (;;) {
            int len = dataLine.read(buf, 0, buf.length);
            if (len < 1) {
                break;
            }
            rtp.write(nxt, nxt.length);
            pos += nxt.length;
            measFreq.toneGen(nxt, pos, frq, vol);
            if (got < 1) {
                top = findTop(buf);
                if (top < 0) {
                    continue;
                }
            }
            for (int i = 0; i < buf.length; i += consts.smpb * 2) {
                rcv[got] = readSmp(buf, i);
                got++;
            }
            if (got < rcv.length) {
                continue;
            }
            int syn = 0;
            for (;;) {
                measFreq.toneGen(buf, syn, frq, vol);
                int i = findTop(buf);
                if (i >= 0) {
                    syn += i;
                    break;
                }
                syn += buf.length;
            }
            syn -= top;
            int max = Integer.MIN_VALUE;
            for (int i = 0; i < rcv.length; i++) {
                int o = rcv[i];
                if (o < 0) {
                    o = -o;
                }
                if (o > max) {
                    max = o;
                }
            }
            for (int i = 0; i < rcv.length; i++) {
                int o = rcv[i];
                o *= vol;
                o /= max;
                rcv[i] = o;
            }
            int all = 0;
            max = Integer.MIN_VALUE;
            for (got = 0; got < rcv.length;) {
                measFreq.toneGen(buf, syn, frq, vol);
                syn += buf.length;
                for (int i = 0; i < buf.length; i += consts.smpb * 2) {
                    int p = rcv[got] - readSmp(buf, i);
                    if (p > max) {
                        max = p;
                    }
                    all += p;
                    got++;
                }
            }
            syn -= rcv.length * consts.smpb * 2;
            System.out.println("syn=" + syn + " all=" + all + " max=" + max + " avg=" + (all / rcv.length));
            got = 0;
            top = -1;
        }
    }

    public static void toneGen(byte[] buf, long beg, int freq, int amp) {
        for (int i = 0; i < buf.length; i += consts.smpb * 2) {
            int val = (int) (amp * Math.sin((beg + i) * Math.PI * freq / (consts.rate * consts.smpb)));
            byte hi = (byte) (val >> 8);
            byte lo = (byte) (val & 0xff);
            buf[i + 0] = hi;
            buf[i + 1] = lo;
            buf[i + 0 + consts.smpb] = hi;
            buf[i + 1 + consts.smpb] = lo;
        }
    }

    public static int readSmp(byte[] buf, int ofs) {
        int i = buf[ofs] + buf[consts.smpb + ofs];
        i <<= 8;
        ofs++;
        return i + buf[ofs] + buf[consts.smpb + ofs];
    }

    public static int findTop(byte[] buf) {
        int old = readSmp(buf, 0);
        int cntU = 0;
        int cntD = 0;
        final int need = 50;
        for (int i = consts.smpb * 2; i < buf.length; i += consts.smpb * 2) {
            int cur = readSmp(buf, i);
            if (old < cur) {
                cntU++;
            }
            if (old > cur) {
                cntD++;
            }
            if (cntU < need) {
                continue;
            }
            if (cntD < need) {
                continue;
            }
            return i - need;
        }
        return -1;
    }

}
