
/**
 * measure remote level
 *
 * @author matecsaba
 */
public class visMeterRem {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 4) {
            System.out.println("usage: java this <kind> <group> <source> <port>");
            return;
        }
        packet knd = packer.receiver(args[1], args[2], args[3]).string2kind(args[0]);
        byte[] buf = new byte[consts.payl];
        visDoer vu = new visDoer();
        for (;;) {
            int i = knd.readKind(buf);
            if (i < 1) {
                break;
            }
            vu.doer(buf, i);
        }
    }

}
