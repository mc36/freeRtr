
import javax.sound.sampled.SourceDataLine;

public class playback {

    public static void main(String[] args) throws Exception {
        decoder dec = new decoder(args[0], args[1]);
        SourceDataLine dataLine = devicer.getPlayback(args[2]);
        for (;;) {
            byte[] buf = new byte[rtper.payload];
            int i = dec.read(buf);
            if (i < 0) {
                break;
            }
            dataLine.write(buf, 0, i);
        }
    }

}
