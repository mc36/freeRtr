
/**
 * stream from file
 *
 * @author matecsaba
 */
public class sender {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 6) {
            System.out.println("usage: java this <file> <seek> <vol> <kind> <group> <port>");
            return;
        }
        decoder dec = new decoder(args[0], args[1], args[2]);
        packer chn = packer.sender(args[4], args[5]);
        packet knd = packet.string2kind(args[3], chn);
        byte[] buf = new byte[devicer.payl];
        for (;;) {
            int i = dec.read(buf);
            if (i < 1) {
                break;
            }
            knd.writeKind(buf, i);
        }
    }

}
