
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
        packet src = packer.receiver(args[1], args[2], args[3]).string2kind(args[0]);
        packet trg = packer.sender(args[5], args[6]).string2kind(args[4]);
        byte[] buf = new byte[devicer.payl];
        for (;;) {
            int i = src.readKind(buf);
            if (i < 1) {
                break;
            }
            trg.writeKind(buf, i);
        }
    }

}
