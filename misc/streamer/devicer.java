
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.Mixer;

public class devicer {

    public static int rate = 44100;
    
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

}
