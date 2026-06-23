
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
    }

}

abstract class comparerOne implements Runnable {

    public volatile byte[] old;

    public volatile int pos;

    public comparerOne(int sec) {
        old = new byte[rtper.payload * sec];
        pos = 0;
    }

    public void doStart() {
        new Thread(this).start();
    }

    public void addBuf(byte[] buf, int len) {
        int o = pos;
        for (int i = 0; i < len; i++) {
            old[o] = buf[i];
            o++;
        }
        pos = o % old.length;
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
