
/**
 * forward stream
 *
 * @author matecsaba
 */
public class forwardWfa2scr {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 5) {
            System.out.println("usage: java this <group> <source> <port> <group> <port>");
            return;
        }
        packer source = packer.receiver(args[0], args[1], args[2]);
        packer rtp = packer.sender(args[3], args[4]);
        byte[] buf = new byte[devicer.payl];
        for (;;) {
            int i = source.readWfa(buf);
            if (i < 1) {
                break;
            }
            rtp.writeScr(buf, i);
        }
    }

}
