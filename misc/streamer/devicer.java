
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
     * playback line
     */
    private SourceDataLine playLine;

    /**
     * record line
     */
    private TargetDataLine recLine;

    private devicer() {
    }

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
        if ((consts.payl % (consts.smpb * 2)) != 0) {
            throw new Exception("samples not fully fit");
        }
        return new AudioFormat(consts.rate, consts.smpb * 8, 2, true, true);
    }

    /**
     * get playback device
     *
     * @param dev regex
     * @return device
     * @throws Exception on error
     */
    public static devicer getPlayback(String dev) throws Exception {
        devicer r = new devicer();
        Mixer.Info mixer = devicer.findDevice(dev);
        AudioFormat format = devicer.getFormat();
        r.playLine = AudioSystem.getSourceDataLine(format, mixer);
        r.playLine.open(format, consts.payl);
        r.playLine.start();
        return r;
    }

    /**
     * get recorder device
     *
     * @param dev regex
     * @return device
     * @throws Exception on error
     */
    public static devicer getRecord(String dev) throws Exception {
        devicer r = new devicer();
        Mixer.Info mixer = devicer.findDevice(dev);
        AudioFormat format = devicer.getFormat();
        r.recLine = AudioSystem.getTargetDataLine(format, mixer);
        r.recLine.open(format, consts.payl);
        r.recLine.start();
        return r;
    }

    /**
     * write sample data
     *
     * @param buf msb bytes
     * @param len length
     */
    public void write(byte[] buf, int len) {
        playLine.write(buf, 0, len);
    }

    /**
     * read sample data
     *
     * @param buf msb bytes
     * @return bytes
     */
    public int read(byte[] buf) {
        return recLine.read(buf, 0, buf.length);
    }

}
