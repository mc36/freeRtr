
/**
 * make stream stereo
 *
 * @author matecsaba
 */
public class monoForwardVol {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 6) {
            System.out.println("usage: java this <group> <source> <port> <group> <port> <volume>");
            return;
        }
        packet src = packer.receiver(args[0], args[1], args[2]).string2kind(null);
        packet trg = packer.sender(args[3], args[4]).string2kind(null);
        int vol = (int) (Float.parseFloat(args[5]) * 100);
        byte[] buf = new byte[consts.payl];
        int cur[] = new int[buf.length / consts.smpb];
        for (;;) {
            int o = src.readKind(buf);
            if (o < 1) {
                break;
            }
            trg.coder.decode(cur, buf, o);
            monoDoer.duplicate(cur, 0, 0, vol);
            monoDoer.duplicate(cur, 1, 1, vol);
            trg.coder.encode(cur, buf, o);
            trg.writeKind(buf, o);
        }
    }

}
