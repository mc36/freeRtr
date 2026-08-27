
/**
 *
 * @author mc36
 */
public class recorder {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 5) {
            System.out.println("usage: java this <file> <kind> <group> <source> <port>");
            return;
        }
        packet src = packer.receiver(args[2], args[3], args[4]).string2kind(args[1]);
        decoder trg = decoder.getRecord(args[0]);
        byte[] buf = new byte[consts.payl];
        for (;;) {
            int i = src.readKind(buf);
            if (i < 0) {
                break;
            }
            trg.write(buf, i);
        }
    }

}
