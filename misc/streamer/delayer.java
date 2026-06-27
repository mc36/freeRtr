
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;

/**
 * delayed forward stream
 *
 * @author matecsaba
 */
public class delayer {

    public static void main(String[] args) throws Exception {
        int i = Integer.parseInt(args[4]);
        byte[][] buf = new byte[i][rtper.payload];
        int[] len = new int[i];
        int pos = 0;
        for (i = 0; i < len.length; i++) {
            len[i] = rtper.payload;
        }
        DatagramChannel source = rtper.receive(args[0], args[1]);
        rtper rtp = new rtper(args[2], args[3]);
        ByteBuffer buffer = ByteBuffer.allocate(4096);
        for (;;) {
            buffer.clear();
            source.receive(buffer);
            i = rtper.decode(buffer, buf[pos]);
            if (i < 1) {
                break;
            }
            len[pos] = i;
            i = (pos + 1) % len.length;
            rtp.write(buf[i], len[i]);
            pos = i;
        }
    }

}
