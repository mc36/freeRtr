
import javax.sound.sampled.SourceDataLine;

/**
 * play back file
 *
 * @author matecsaba
 */
public class playback {

    public static void main(String[] args) throws Exception {
        decoder dec = new decoder(args[0], args[1]);
        SourceDataLine dataLine = devicer.getPlayback(args[2]);
        byte[] buf = new byte[rtper.payload];
        for (;;) {
            int i = dec.read(buf);
            if (i < 0) {
                break;
            }
            dataLine.write(buf, 0, i);
        }
    }

}
