
import javax.sound.sampled.SourceDataLine;

/**
 * play and vu file
 *
 * @author matecsaba
 */
public class vuPlayback {

    public static void main(String[] args) throws Exception {
        decoder dec = new decoder(args[0], args[1]);
        SourceDataLine dataLine = devicer.getPlayback(args[2]);
        byte[] buf = new byte[devicer.payl];
        vuDoer vu = new vuDoer();
        for (;;) {
            int i = dec.read(buf);
            if (i < 0) {
                break;
            }
            dataLine.write(buf, 0, i);
            vu.doer(buf, i);
        }
    }

}
