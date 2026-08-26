
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
        if (args.length < 6) {
            System.out.println("usage: java this <group> <source> <port> <group> <port> <volume>");
            return;
        }
        packet src = packer.receiver(args[0], args[1], args[2]).string2kind(null);
        packet rtp = packer.sender(args[3], args[4]).string2kind(null);
        int vol = (int) (Float.parseFloat(args[5]) * 100);
        byte[] buf = new byte[devicer.payl];
        int cur[] = new int[buf.length / devicer.smpb];
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
