
import javax.sound.sampled.SourceDataLine;

/**
 * play and show stream
 *
 * @author matecsaba
 */
public class visReceiver {

    public static void main(String[] args) throws Exception {
        SourceDataLine dataLine = devicer.getPlayback(args[0]);
        rtper channel = rtper.receive(args[1], args[2], args[3]);
        byte[] buf = new byte[devicer.payl];
        visDoer vu = new visDoer();
        for (;;) {
            int i = channel.read(buf);
            if (i < 1) {
                break;
            }
            dataLine.write(buf, 0, i);
            vu.doer(buf, i);
        }
    }

}
