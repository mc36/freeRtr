
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import javax.sound.sampled.SourceDataLine;

public class receiver {

    public static void main(String[] args) throws Exception {
        SourceDataLine dataLine = devicer.getPlayback(args[0]);
        DatagramChannel channel = rtper.receive(args[1], args[2], args[3]);
        ByteBuffer buffer = ByteBuffer.allocate(4096);
        for (;;) {
            buffer.clear();
            channel.receive(buffer);
            byte[] buf = rtper.decode(buffer);
            dataLine.write(buf, 0, buf.length);
        }
    }

}
