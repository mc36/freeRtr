
import javax.sound.sampled.TargetDataLine;

/**
 * stream live capture
 *
 * @author mc36
 */
public class visStreamerRtp {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        TargetDataLine dataLine = devicer.getRecord(args[0]);
        packer rtp = packer.sender(args[1], args[2]);
        byte[] buf = new byte[devicer.payl];
        visDoer vu = new visDoer();
        for (;;) {
            int i = dataLine.read(buf, 0, buf.length);
            if (i < 1) {
                break;
            }
            rtp.rtp_write(buf, i);
            vu.doer(buf, i);
        }
    }

}
