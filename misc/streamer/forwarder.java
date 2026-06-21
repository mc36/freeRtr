
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;

public class forwarder {

    public static void main(String[] args) throws Exception {
        DatagramChannel source = rtper.receive(args[0], args[1]);
        rtper rtp = new rtper(args[2], args[3]);
        ByteBuffer buffer = ByteBuffer.allocate(4096);
        for (;;) {
            buffer.clear();
            source.receive(buffer);
            byte[] buf = rtper.decode(buffer);
            rtp.write(buf, buf.length);
        }
    }

}
