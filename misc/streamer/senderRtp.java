
/**
 * stream from file
 *
 * @author matecsaba
 */
public class senderRtp {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 5) {
            System.out.println("usage: java this <file> <seek> <vol> <group> <port>");
            return;
        }
        decoder dec = new decoder(args[0], args[1], args[2]);
        packer rtp = packer.sender(args[3], args[4]);
        byte[] buf = new byte[devicer.payl];
        for (;;) {
            int i = dec.read(buf);
            if (i < 1) {
                break;
            }
            rtp.writeRtp(buf, i);
        }
    }

}
