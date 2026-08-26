
/**
 * measure tone accuracy
 *
 * @author matecsaba
 */
public class measFreq {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 5) {
            System.out.println("usage: java this <device> <group> <port> <frequency> <volume>");
            return;
        }
        int frq = Integer.parseInt(args[3]);
        int vol = Integer.parseInt(args[4]);
        devicer lin = devicer.getRecord(args[0]);
        packet trg = packer.sender(args[1], args[2]).string2kind(null);
        byte[] buf = new byte[devicer.payl];
        byte[] nxt = new byte[buf.length];
        long pos = 0;
        int[] rcv = new int[(devicer.rate / devicer.payl) * devicer.payl];
        int got = 0;
        int top = -1;
        for (;;) {
            int len = lin.read(buf);
            if (len < 1) {
                break;
            }
            trg.writeKind(nxt, nxt.length);
            pos += nxt.length;
            measFreq.toneGen(nxt, pos, frq, vol);
            if (got < 1) {
                top = findTop(buf);
                if (top < 0) {
                    continue;
                }
            }
            for (int i = 0; i < buf.length; i += devicer.smpb * 2) {
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
                for (int i = 0; i < buf.length; i += devicer.smpb * 2) {
                    int p = rcv[got] - readSmp(buf, i);
                    if (p > max) {
                        max = p;
                    }
                    all += p;
                    got++;
                }
            }
            syn -= rcv.length * devicer.smpb * 2;
            System.out.println("syn=" + syn + " all=" + all + " max=" + max + " avg=" + (all / rcv.length));
            got = 0;
            top = -1;
        }
    }

    /**
     * generate tone
     *
     * @param buf buffer
     * @param beg bytes already written
     * @param freq frequency
     * @param amp amplitude
     */
    public static void toneGen(byte[] buf, long beg, int freq, int amp) {
        for (int i = 0; i < buf.length; i += devicer.smpb * 2) {
            int val = (int) (amp * Math.sin((beg + i) * Math.PI * freq / (devicer.rate * devicer.smpb)));
            byte hi = (byte) (val >> 8);
            byte lo = (byte) (val & 0xff);
            buf[i + 0] = hi;
            buf[i + 1] = lo;
            buf[i + 0 + devicer.smpb] = hi;
            buf[i + 1 + devicer.smpb] = lo;
        }
    }

    private static int readSmp(byte[] buf, int ofs) {
        int i = buf[ofs] + buf[devicer.smpb + ofs];
        i <<= 8;
        ofs++;
        return i + buf[ofs] + buf[devicer.smpb + ofs];
    }

    private static int findTop(byte[] buf) {
        int old = readSmp(buf, 0);
        int cntU = 0;
        int cntD = 0;
        final int need = 50;
        for (int i = devicer.smpb * 2; i < buf.length; i += devicer.smpb * 2) {
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
