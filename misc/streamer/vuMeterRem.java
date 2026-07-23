
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;

/**
 * measure vu level
 *
 * @author matecsaba
 */
public class vuMeterRem {

    public static void main(String[] args) throws Exception {
        DatagramChannel channel = rtper.receive(args[0], args[1], args[2]);
        ByteBuffer buffer = ByteBuffer.allocate(4096);
        byte[] buf = new byte[rtper.payload];
        vuDoer vu = new vuDoer();
        for (;;) {
            buffer.clear();
            channel.receive(buffer);
            int i = rtper.decode(buffer, buf);
            if (i < 1) {
                break;
            }
            vu.doer(buf, i);
        }
    }

}
