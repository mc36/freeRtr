
import javax.sound.sampled.SourceDataLine;

/**
 * play back file
 *
 * @author matecsaba
 */
public class playback {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 3) {
            System.out.println("usage: java this <file> <seek> <device>");
            return;
        }
        decoder dec = new decoder(args[0], args[1]);
        SourceDataLine dataLine = devicer.getPlayback(args[2]);
        byte[] buf = new byte[devicer.payl];
        for (;;) {
            int i = dec.read(buf);
            if (i < 0) {
                break;
            }
            dataLine.write(buf, 0, i);
        }
    }

}
