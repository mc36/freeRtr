
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Mixer;
import javax.sound.sampled.SourceDataLine;
import javax.sound.sampled.TargetDataLine;

/**
 * device helpers
 *
 * @author matecsaba
 */
public class devicer {

    public static final int rate = 48000;

    public static final int smpb = 2;

    public static final int payl = 1280;

    public static final int rtpl = 12;

    public static Mixer.Info findDevice(String dev) {
        dev = ".*" + dev + ".*";
        Mixer.Info[] mixers = AudioSystem.getMixerInfo();
        int mixerc = -1;
        for (int i = 0; i < mixers.length; i++) {
            String a = mixer2name(mixers[i]);
            if (a.matches(dev)) {
                mixerc = i;
            }
            System.out.println(a);
        }
        System.out.println("selected: " + mixer2name(mixers[mixerc]));
        return mixers[mixerc];
    }

    public static String mixer2name(Mixer.Info mixer) {
        return mixer.getName() + " - " + mixer.getDescription();
    }

    public static AudioFormat getFormat() {
        return new AudioFormat(rate, smpb * 8, 2, true, true);
    }

    public static SourceDataLine getPlayback(String dev) throws Exception {
        Mixer.Info mixer = devicer.findDevice(dev);
        AudioFormat format = devicer.getFormat();
        SourceDataLine dataLine = AudioSystem.getSourceDataLine(format, mixer);
        dataLine.open(format, payl);
        dataLine.start();
        return dataLine;
    }

    public static TargetDataLine getRecord(String dev) throws Exception {
        Mixer.Info mixer = devicer.findDevice(dev);
        AudioFormat format = devicer.getFormat();
        TargetDataLine dataLine = AudioSystem.getTargetDataLine(format, mixer);
        dataLine.open(format, payl);
        dataLine.start();
        return dataLine;
    }

}
