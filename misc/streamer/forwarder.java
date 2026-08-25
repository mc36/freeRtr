
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
        if (args.length < 7) {
            System.out.println("usage: java this <kind> <group> <source> <port> <kind> <group> <port>");
            return;
        }
        packer srcNet = packer.receiver(args[1], args[2], args[3]);
        packet srcKnd = packet.string2kind(args[0], srcNet);
        packer trgNet = packer.sender(args[5], args[6]);
        packet trgKnd = packet.string2kind(args[4], trgNet);
        byte[] buf = new byte[devicer.payl];
        for (;;) {
            int i = srcKnd.readKind(buf);
            if (i < 1) {
                break;
            }
            trgKnd.writeKind(buf, i);
        }
    }

}
