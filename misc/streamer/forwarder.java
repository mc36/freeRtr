
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;

/**
 * forward stream
 *
 * @author matecsaba
 */
public class forwarder {

    public static void main(String[] args) throws Exception {
        DatagramChannel source = rtper.receive(args[0], args[1]);
        rtper rtp = new rtper(args[2], args[3]);
        ByteBuffer buffer = ByteBuffer.allocate(4096);
        byte[] buf = new byte[rtper.payload];
        for (;;) {
            buffer.clear();
            source.receive(buffer);
            int i = rtper.decode(buffer, buf);
            if (i < 1) {
                break;
            }
            rtp.write(buf, i);
        }
    }

}
