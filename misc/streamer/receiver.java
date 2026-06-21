
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.MulticastSocket;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Mixer;
import javax.sound.sampled.SourceDataLine;

public class receiver {

    public static void main(String[] args) throws Exception {
        Mixer.Info mixer = devicer.findDevice(args[0]);
        InetAddress group = InetAddress.getByName(args[1]);
        InetAddress source = InetAddress.getByName(args[2]);
        int port = Integer.parseInt(args[3]);

        AudioFormat format = devicer.getFormat();
        SourceDataLine dataLine = AudioSystem.getSourceDataLine(format, mixer);
        dataLine.open(format);
        dataLine.start();

        MulticastSocket mcstsck = new MulticastSocket();
        DatagramChannel channel = DatagramChannel.open();
        channel.socket().bind(new InetSocketAddress(port));
        channel.join(group, mcstsck.getNetworkInterface(), source);
        ByteBuffer buffer = ByteBuffer.allocate(4096);
        for (;;) {
            buffer.clear();
            channel.receive(buffer);
            dataLine.write(buffer.array(), rtper.size, buffer.position() - rtper.size);
        }
    }

}
