
import javax.sound.sampled.TargetDataLine;

/**
 * stream live capture
 *
 * @author matecsaba
 */
public class streamer {

    public static void main(String[] args) throws Exception {
        TargetDataLine dataLine = devicer.getRecord(args[0]);
        rtper rtp = new rtper(args[1], args[2]);
        byte[] buf = new byte[rtper.payload];
        for (;;) {
            int i = dataLine.read(buf, 0, buf.length);
            if (i < 1) {
                break;
            }
            rtp.write(buf, i);
        }
    }

}
