
import java.net.InetAddress;
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Mixer;
import javax.sound.sampled.TargetDataLine;

public class streamer {

    public static void main(String[] args) throws Exception {
        Mixer.Info[] mixers = AudioSystem.getMixerInfo();
        int mixerc = -1;
        for (int i = 0; i < mixers.length; i++) {
            if (mixers[i].getName().contains(args[0])) {
                mixerc = i;
            }
            System.out.println(mixers[i].getName() + " - " + mixers[i].getDescription());
        }

        AudioFormat audioFormat = new AudioFormat(44100, 16, 2, true, true);
        TargetDataLine dataLine = AudioSystem.getTargetDataLine(audioFormat, mixers[mixerc]);
        dataLine.open(audioFormat);
        dataLine.start();

        InetAddress group = InetAddress.getByName(args[1]);
        int port = Integer.parseInt(args[2]);
        rtper rtp = new rtper(group, port);
        for (;;) {
            byte[] buf = new byte[1024];
            int i = dataLine.read(buf, 0, buf.length);
            if (i < 1) {
                break;
            }
            rtp.write(buf, i);
        }
    }

}
