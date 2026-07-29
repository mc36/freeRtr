
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;

/**
 * measure remote level
 *
 * @author matecsaba
 */
public class visMeterRem {

    public static void main(String[] args) throws Exception {
        DatagramChannel channel = rtper.receive(args[0], args[1], args[2]);
        ByteBuffer buffer = ByteBuffer.allocate(4096);
        byte[] buf = new byte[devicer.payl];
        visDoer vu = new visDoer();
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
