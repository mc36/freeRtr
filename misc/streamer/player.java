
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Mixer;
import javax.sound.sampled.SourceDataLine;

public class player {

    public static void main(String[] args) throws Exception {
        Mixer.Info mixer = devicer.findDevice(args[0]);
        decoder dec = new decoder(args[1], args[2]);

        AudioFormat format = devicer.getFormat();
        SourceDataLine dataLine = AudioSystem.getSourceDataLine(format, mixer);
        dataLine.open(format);
        dataLine.start();

        for (;;) {
            byte[] buf = new byte[1024];
            int i = dec.read(buf);
            if (i < 0) {
                break;
            }
            dataLine.write(buf, 0, i);
        }
    }

}
