
/**
 * play and show stream
 *
 * @author matecsaba
 */
public class visReceiverWfa {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 4) {
            System.out.println("usage: java this <device> <group> <source> <port>");
            return;
        }
        devicer dataLine = devicer.getPlayback(args[0]);
        packer channel = packer.receiver(args[1], args[2], args[3]);
        byte[] buf = new byte[devicer.payl];
        visDoer vu = new visDoer();
        for (;;) {
            int i = channel.readWfa(buf);
            if (i < 1) {
                break;
            }
            dataLine.write(buf, i);
            vu.doer(buf, i);
        }
    }

}
