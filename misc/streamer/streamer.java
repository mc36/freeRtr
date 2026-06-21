
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Mixer;
import javax.sound.sampled.TargetDataLine;

public class streamer {

    public static void main(String[] args) throws Exception {
        Mixer.Info mixer = devicer.findDevice(args[0]);
        AudioFormat format = devicer.getFormat();
        TargetDataLine dataLine = AudioSystem.getTargetDataLine(format, mixer);
        dataLine.open(format);
        dataLine.start();

        rtper rtp = new rtper(args[1], args[2]);
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
