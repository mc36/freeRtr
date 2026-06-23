
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
        dev.doStart();
        net.doStart();
        for (;;) {
            Thread.sleep(1000);
            dev.getBuf(0);
            net.getBuf(0);
            int m = dev.getDiff(net);
            int p = 0;
            for (int i = 1; i < rtper.payload * sec; i++) {
                dev.getBuf(i);
                int o = dev.getDiff(net);
                if (o > m) {
                    continue;
                }
                p = i;
                m = o;
            }
            System.out.println(m + " @ " + p + " dev=" + dev + " net=" + net);
        }
    }

}

abstract class comparerOne implements Runnable {

    public volatile byte[] old;

    public volatile int pos;

    public byte[] cur;

    public byte min;

    public byte max;

    public float div;

    public comparerOne(int sec) {
        old = new byte[rtper.payload * sec * 2];
        pos = 0;
        cur = new byte[rtper.payload * sec];
    }

    public void doStart() {
        new Thread(this).start();
    }

    public void addBuf(byte[] buf, int len) {
        int o = pos;
        for (int i = 0; i < len; i += 4) {
            int p = (int) buf[i] + (int) buf[i + 2];
            p /= 2;
            old[o] = (byte) p;
            o = (o + 1) % old.length;
        }
        pos = o;
    }

    public void getBuf(int beg) {
        int o = pos + beg;
        min = Byte.MAX_VALUE;
        max = Byte.MIN_VALUE;
        for (int i = 0; i < cur.length; i++) {
            o = (o + 1) % old.length;
            byte p = old[o];
            cur[i] = p;
            if (p < min) {
                min = p;
            }
            if (p > max) {
                max = p;
            }
        }
        div = ((float) max - (float) min) / 100.0f;
        for (int i = 0; i < cur.length; i++) {
            float p = (float) (cur[i] - min);
            p /= div;
            cur[i] = (byte) p;
        }
    }

    public int getDiff(comparerOne o) {
        int r = 0;
        for (int i = 0; i < cur.length; i++) {
            int p = (int) cur[i] - (int) o.cur[i];
            if (p < 0) {
                p = -p;
            }
            r += p;
        }
        return r;
    }

    public String toString() {
        return min + ".." + max;
    }

}

class comparerDev extends comparerOne {

    private TargetDataLine dataLine;

    public comparerDev(TargetDataLine dl, int sec) {
        super(sec);
        dataLine = dl;
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

class comparerNet extends comparerOne {

    private DatagramChannel channel;

    public comparerNet(DatagramChannel ch, int sec) {
        super(sec);
        channel = ch;
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
