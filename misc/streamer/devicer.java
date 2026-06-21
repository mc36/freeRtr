
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Mixer;
import javax.sound.sampled.SourceDataLine;
import javax.sound.sampled.TargetDataLine;

public class devicer {

    public final static int rate = 48000;

    public static Mixer.Info findDevice(String dev) {
        Mixer.Info[] mixers = AudioSystem.getMixerInfo();
        int mixerc = -1;
        for (int i = 0; i < mixers.length; i++) {
            if (mixers[i].getName().contains(dev)) {
                mixerc = i;
            }
            System.out.println(mixers[i].getName() + " - " + mixers[i].getDescription());
        }
        return mixers[mixerc];
    }

    public static AudioFormat getFormat() {
        return new AudioFormat(rate, 16, 2, true, true);
    }

    public static SourceDataLine getPlayback(String dev) throws Exception {
        Mixer.Info mixer = devicer.findDevice(dev);
        AudioFormat format = devicer.getFormat();
        SourceDataLine dataLine = AudioSystem.getSourceDataLine(format, mixer);
        dataLine.open(format);
        dataLine.start();
        return dataLine;
    }

    public static TargetDataLine getRecord(String dev) throws Exception {
        Mixer.Info mixer = devicer.findDevice(dev);
        AudioFormat format = devicer.getFormat();
        TargetDataLine dataLine = AudioSystem.getTargetDataLine(format, mixer);
        dataLine.open(format);
        dataLine.start();
        return dataLine;
    }

}
