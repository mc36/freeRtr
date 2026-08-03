
import javax.sound.sampled.TargetDataLine;

/**
 * measure local level
 *
 * @author matecsaba
 */
public class visMeterLoc {

    public static void main(String[] args) throws Exception {
        TargetDataLine dataLine = devicer.getRecord(args[0]);
        byte[] buf = new byte[consts.payl];
        visDoer vu = new visDoer();
        for (;;) {
            int i = dataLine.read(buf, 0, buf.length);
            if (i < 1) {
                break;
            }
            vu.doer(buf, i);
        }
    }

}
