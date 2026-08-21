
/**
 * play back stream
 *
 * @author matecsaba
 */
public class receiverScr {

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
        for (;;) {
            int i = channel.readScr(buf);
            if (i < 1) {
                break;
            }
            dataLine.write(buf, i);
        }
    }

}
