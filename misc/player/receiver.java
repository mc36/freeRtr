
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.MulticastSocket;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.SourceDataLine;

public class receiver {

    public static void main(String[] args) throws Exception {
        InetAddress group = InetAddress.getByName(args[0]);
        InetAddress source = InetAddress.getByName(args[1]);
        int port = Integer.parseInt(args[2]);

        AudioFormat audioFormat = new AudioFormat(44100, 16, 2, true, true);
        SourceDataLine sourceDataLine = AudioSystem.getSourceDataLine(audioFormat);
        sourceDataLine.open(audioFormat);
        sourceDataLine.start();

        MulticastSocket mcstsck = new MulticastSocket();
        DatagramChannel channel = DatagramChannel.open();
        channel.socket().bind(new InetSocketAddress(port));
        channel.join(group, mcstsck.getNetworkInterface(), source);
        ByteBuffer buffer = ByteBuffer.allocate(4096);
        for (;;) {
            buffer.clear();
            channel.receive(buffer);
            sourceDataLine.write(buffer.array(), 12, buffer.position() - 12);
        }
    }

}
