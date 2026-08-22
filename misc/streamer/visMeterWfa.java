
/**
 * measure remote level
 *
 * @author matecsaba
 */
public class visMeterWfa {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 3) {
            System.out.println("usage: java this <group> <source> <port>");
            return;
        }
        packer channel = packer.receiver(args[0], args[1], args[2]);
        byte[] buf = new byte[devicer.payl];
        visDoer vu = new visDoer();
        for (;;) {
            int i = channel.readWfa(buf);
            if (i < 1) {
                break;
            }
            vu.doer(buf, i);
        }
    }

}
