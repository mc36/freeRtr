
/**
 * forward stream
 *
 * @author matecsaba
 */
public class forwarder {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 4) {
            System.out.println("usage: java this <source> <port> <group> <port>");
            return;
        }
        packer source = packer.receiver(args[0], args[1]);
        packer rtp = packer.sender(args[2], args[3]);
        byte[] buf = new byte[devicer.payl];
        for (;;) {
            int i = source.readRtp(buf);
            if (i < 1) {
                break;
            }
            rtp.writeRtp(buf, i);
        }
    }

}
