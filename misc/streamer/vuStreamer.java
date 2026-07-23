
import javax.sound.sampled.TargetDataLine;

/**
 * stream live capture
 *
 * @author mc36
 */
public class vuStreamer {

    public static void main(String[] args) throws Exception {
        TargetDataLine dataLine = devicer.getRecord(args[0]);
        rtper rtp = new rtper(args[1], args[2]);
        byte[] buf = new byte[rtper.payload];
        vuDoer vu = new vuDoer();
        for (;;) {
            int i = dataLine.read(buf, 0, buf.length);
            if (i < 1) {
                break;
            }
            rtp.write(buf, i);
            vu.doer(buf, i);
        }
    }

}
