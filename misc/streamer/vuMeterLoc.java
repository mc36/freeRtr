
import javax.sound.sampled.TargetDataLine;

/**
 * measure vu level
 *
 * @author matecsaba
 */
public class vuMeterLoc {

    public static void main(String[] args) throws Exception {
        TargetDataLine dataLine = devicer.getRecord(args[0]);
        byte[] buf = new byte[rtper.payload];
        vuDoer vu = new vuDoer();
        for (;;) {
            int i = dataLine.read(buf, 0, buf.length);
            if (i < 1) {
                break;
            }
            vu.doer(buf, i);
        }
    }

}
