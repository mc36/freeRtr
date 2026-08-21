
/**
 * measure local level
 *
 * @author matecsaba
 */
public class visMeterLoc {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 1) {
            System.out.println("usage: java this <device>");
            return;
        }
        devicer dataLine = devicer.getRecord(args[0]);
        byte[] buf = new byte[devicer.payl];
        visDoer vu = new visDoer();
        for (;;) {
            int i = dataLine.read(buf);
            if (i < 1) {
                break;
            }
            vu.doer(buf, i);
        }
    }

}
