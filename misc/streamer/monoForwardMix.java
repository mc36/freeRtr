
/**
 * make stream mono
 *
 * @author matecsaba
 */
public class monoForwardMix {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 7) {
            System.out.println("usage: java this <group> <source> <port> <group> <source> <port> <volume>");
            return;
        }
        packet src = packer.receiver(args[0], args[1], args[2]).string2kind(null);
        packet rtp = packer.sender(args[3], args[4], args[5]).string2kind(null);
        int vol = (int) (Float.parseFloat(args[6]) * 100);
        byte[] buf = new byte[consts.payl];
        int cur[] = new int[buf.length / consts.smpb];
        for (;;) {
            int o = src.readKind(buf);
            if (o < 1) {
                break;
            }
            rtp.coder.decode(cur, buf, o);
            monoDoer.mixer(cur, vol);
            rtp.coder.encode(cur, buf, o);
            rtp.writeKind(buf, o);
        }
    }

}
