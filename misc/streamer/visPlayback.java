
/**
 * play and show file
 *
 * @author matecsaba
 */
public class visPlayback {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 4) {
            System.out.println("usage: java this <file> <seek> <vol> <device>");
            return;
        }
        decoder src = decoder.getPlayback(args[0], args[1], args[2]);
        devicer trg = devicer.getPlayback(args[3]);
        byte[] buf = new byte[devicer.payl];
        visDoer vu = new visDoer();
        for (;;) {
            int i = src.read(buf);
            if (i < 0) {
                break;
            }
            trg.write(buf, i);
            vu.doer(buf, i);
        }
    }

}
