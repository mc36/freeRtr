
/**
 * delayed forward stream
 *
 * @author matecsaba
 */
public class delayer {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 6) {
            System.out.println("usage: java this <group> <source> <port> <group> <port> <packets>");
            return;
        }
        int i = Integer.parseInt(args[5]);
        byte[][] buf = new byte[i][consts.payl];
        int[] len = new int[i];
        int pos = 0;
        for (i = 0; i < len.length; i++) {
            len[i] = consts.payl;
        }
        packet src = packer.receiver(args[0], args[1], args[2]).string2kind(null);
        packet trg = packer.sender(args[3], args[4]).string2kind(null);
        for (;;) {
            i = src.readKind(buf[pos]);
            if (i < 1) {
                break;
            }
            len[pos] = i;
            i = (pos + 1) % len.length;
            trg.writeKind(buf[i], len[i]);
            pos = i;
        }
    }

}
