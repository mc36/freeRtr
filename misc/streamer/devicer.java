
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

    /**
     * sampling rate
     */
    public static final int rate = 48000;

    /**
     * bytes per sample
     */
    public static final int smpb = 3;

    /**
     * bytes per payload
     */
    public static final int payl = 1200;

    /**
     * bytes in rtp header
     */
    public static final int rtpl = 12;

    /**
     * type in rtp header
     */
    public static final int rtpt = 96;

    /**
     * bytes in scream header
     */
    public static final int scrl = 5;

    /**
     * scream rate bits
     */
    public static final int scrb = (rate % 44100) != 0 ? rate / 48000 : 128 | (rate / 44100);

    /**
     * scream channel layout
     */
    public static final int scrt = 3;

    /**
     * find device
     *
     * @param dev regex
     * @return device
     */
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

    /**
     * device name
     *
     * @param mixer mixer
     * @return name
     */
    public static String mixer2name(Mixer.Info mixer) {
        return mixer.getName() + " - " + mixer.getDescription();
    }

    /**
     * get format
     *
     * @return format
     * @throws Exception on error
     */
    public static AudioFormat getFormat() throws Exception {
        if ((payl % (smpb * 2)) != 0) {
            throw new Exception("samples not fully fit");
        }
        return new AudioFormat(rate, smpb * 8, 2, true, true);
    }

    /**
     * get playback device
     *
     * @param dev regex
     * @return device
     * @throws Exception on error
     */
    public static SourceDataLine getPlayback(String dev) throws Exception {
        Mixer.Info mixer = devicer.findDevice(dev);
        AudioFormat format = devicer.getFormat();
        SourceDataLine dataLine = AudioSystem.getSourceDataLine(format, mixer);
        dataLine.open(format, payl);
        dataLine.start();
        return dataLine;
    }

    /**
     * get recorder device
     *
     * @param dev regex
     * @return device
     * @throws Exception on error
     */
    public static TargetDataLine getRecord(String dev) throws Exception {
        Mixer.Info mixer = devicer.findDevice(dev);
        AudioFormat format = devicer.getFormat();
        TargetDataLine dataLine = AudioSystem.getTargetDataLine(format, mixer);
        dataLine.open(format, payl);
        dataLine.start();
        return dataLine;
    }

}
