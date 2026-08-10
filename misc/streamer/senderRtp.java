
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
        decoder dec = new decoder(args[0], args[1]);
        packer rtp = packer.sender(args[2], args[3]);
        byte[] buf = new byte[devicer.payl];
        for (;;) {
            int i = dec.read(buf);
            if (i < 1) {
                break;
            }
            rtp.rtp_write(buf, i);
        }
    }

}
