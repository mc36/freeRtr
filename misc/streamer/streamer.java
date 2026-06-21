
import javax.sound.sampled.TargetDataLine;

public class streamer {

    public static void main(String[] args) throws Exception {
        TargetDataLine dataLine = devicer.getRecord(args[0]);
        rtper rtp = new rtper(args[1], args[2]);
        for (;;) {
            byte[] buf = new byte[1024];
            int i = dataLine.read(buf, 0, buf.length);
            if (i < 1) {
                break;
            }
            rtp.write(buf, i);
        }
    }

}
