
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import javax.sound.sampled.SourceDataLine;

/**
 * play back stream
 *
 * @author matecsaba
 */
public class receiver {

    public static void main(String[] args) throws Exception {
        SourceDataLine dataLine = devicer.getPlayback(args[0]);
        DatagramChannel channel = rtper.receive(args[1], args[2], args[3]);
        ByteBuffer buffer = ByteBuffer.allocate(4096);
        byte[] buf = new byte[rtper.payload];
        for (;;) {
            buffer.clear();
            channel.receive(buffer);
            int i = rtper.decode(buffer,buf);
            if (i < 1) {
                break;
            }
            dataLine.write(buf, 0, i);
        }
    }

}
