
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.SourceDataLine;

public class receiver {

    public static void main(String[] args) throws Exception {
        AudioFormat audioFormat = new AudioFormat(44100, 16, 2, true, true);
        SourceDataLine sourceDataLine = AudioSystem.getSourceDataLine(audioFormat);
        sourceDataLine.open(audioFormat);
        sourceDataLine.start();

        MulticastSocket socket = new MulticastSocket(1234);
        socket.joinGroup(InetAddress.getByName("232.2.3.2"));
        byte[] buffer = new byte[65536];
        DatagramPacket packet = new DatagramPacket(buffer, buffer.length);
        for (;;) {
            socket.receive(packet);
            sourceDataLine.write(buffer, 12, packet.getLength() - 12);
        }
    }

}
