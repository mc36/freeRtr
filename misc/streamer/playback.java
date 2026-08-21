
/**
 * play back file
 *
 * @author matecsaba
 */
public class playback {

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
        decoder dec = new decoder(args[0], args[1], args[2]);
        devicer dataLine = devicer.getPlayback(args[3]);
        byte[] buf = new byte[devicer.payl];
        for (;;) {
            int i = dec.read(buf);
            if (i < 0) {
                break;
            }
            dataLine.write(buf, i);
        }
    }

}
