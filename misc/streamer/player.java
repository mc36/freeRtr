
import javax.sound.sampled.SourceDataLine;

public class player {

    public static void main(String[] args) throws Exception {
        decoder dec = new decoder(args[1], args[2]);
        SourceDataLine dataLine = devicer.getPlayback(args[0]);
        for (;;) {
            byte[] buf = new byte[1024];
            int i = dec.read(buf);
            if (i < 0) {
                break;
            }
            dataLine.write(buf, 0, i);
        }
    }

}
