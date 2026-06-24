
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import javax.sound.sampled.TargetDataLine;

/**
 * compare streams
 *
 * @author matecsaba
 */
public class comparer {

    public static void main(String[] args) throws Exception {
        int sec = Integer.parseInt(args[4]);
        TargetDataLine dataLine = devicer.getRecord(args[0]);
        comparerOne dev = new comparerDev(dataLine, sec);
        DatagramChannel channel = rtper.receive(args[1], args[2], args[3]);
        comparerOne net = new comparerNet(channel, sec);
        comparerOne d = new comparerOne(sec);
        comparerOne n = new comparerOne(sec);
        for (;;) {
            Thread.sleep(100);
            dev.doCopy(d);
            net.doCopy(n);
            d.doFull(n);
        }
    }

}

class comparerOne {

    public volatile byte[] cur;

    public volatile int pos;

    public byte max;

    public float div;

    public comparerOne(int sec) {
        cur = new byte[devicer.rate * sec];
        pos = 0;
    }

    public void doCopy(comparerOne r) {
        System.arraycopy(cur, 0, r.cur, 0, cur.length);
        r.pos = pos;
    }

    public void addBuf(byte[] buf, int len) {
        int o = pos;
        for (int i = 0; i < len; i += 4) {
            int p = (int) buf[i];
            p += (int) buf[i + 2];
            if (p < 0) {
                p = -p;
            }
            p /= 2;
            cur[o] = (byte) p;
            o = (o + 1) % cur.length;
        }
        pos = o;
    }

    public void doBuf() {
        max = Byte.MIN_VALUE;
        for (int i = 0; i < cur.length; i++) {
            byte p = cur[i];
            if (p < max) {
                continue;
            }
            max = p;
        }
        div = (float) max / 120.0f;
        for (int i = 0; i < cur.length; i++) {
            float p = (float) cur[i];
            p /= div;
            cur[i] = (byte) p;
        }
    }

    public int doDiff(comparerOne oth, int beg, int max) {
        int r = 0;
        int p = (beg + pos) % cur.length;
        int o = oth.pos;
        for (int i = 0; i < (cur.length / 2); i++) {
            int q = (int) cur[p] - (int) oth.cur[o];
            if (p >= cur.length) {
                p = 0;
            }
            if (o >= cur.length) {
                o = 0;
            }
            if (q < 0) {
                q = -q;
            }
            r += q;
            if (r > max) {
                return r;
            }
        }
        return r;
    }

    public void doFull(comparerOne oth) {
        doBuf();
        oth.doBuf();
        int m = doDiff(oth, 0, Integer.MAX_VALUE);
        int p = 0;
        for (int i = 1; i < (cur.length / 2); i++) {
            int o = doDiff(oth, i, m);
            if (o >= m) {
                continue;
            }
            p = i;
            m = o;
        }
        m /= cur.length / devicer.rate;
        int q = (p * 1000) / devicer.rate;
        System.out.println(m + " @ " + p + " (" + q + "ms) dev=" + this + " net=" + oth);
    }

    public String toString() {
        return "" + max;
    }

}

class comparerDev extends comparerOne implements Runnable {

    private TargetDataLine dataLine;

    public comparerDev(TargetDataLine dl, int sec) {
        super(sec);
        dataLine = dl;
        new Thread(this).start();
    }

    public void run() {
        byte[] buf = new byte[rtper.payload];
        for (;;) {
            int i = dataLine.read(buf, 0, buf.length);
            if (i < 1) {
                break;
            }
            addBuf(buf, i);
        }
    }

}

class comparerNet extends comparerOne implements Runnable {

    private DatagramChannel channel;

    public comparerNet(DatagramChannel ch, int sec) {
        super(sec);
        channel = ch;
        new Thread(this).start();
    }

    public void run() {
        ByteBuffer buffer = ByteBuffer.allocate(4096);
        byte[] buf = new byte[rtper.payload];
        for (;;) {
            buffer.clear();
            try {
                channel.receive(buffer);
            } catch (Exception e) {
            }
            int i = rtper.decode(buffer, buf);
            if (i < 1) {
                break;
            }
            addBuf(buf, i);
        }

    }

}
