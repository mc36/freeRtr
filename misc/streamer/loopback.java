
/**
 * play live capture
 *
 * @author matecsaba
 */
public class loopback {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 2) {
            System.out.println("usage: java this <device> <device>");
            return;
        }
        devicer src = devicer.getRecord(args[0]);
        devicer trg = devicer.getPlayback(args[1]);
        byte[] buf = new byte[consts.payl];
        for (;;) {
            int i = src.read(buf);
            if (i < 1) {
                break;
            }
            trg.write(buf, i);
        }
    }

}
