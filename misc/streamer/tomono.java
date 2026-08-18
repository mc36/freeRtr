
/**
 * make stream mono
 *
 * @author matecsaba
 */
public class tomono {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 5) {
            System.out.println("usage: java this <group> <source> <port> <group> <port>");
            return;
        }
        packer source = packer.receiver(args[0], args[1], args[2]);
        packer rtp = packer.sender(args[3], args[4]);
        byte[] buf = new byte[devicer.payl];
        int cur[] = new int[buf.length / devicer.smpb];
        for (;;) {
            int o = source.readRtp(buf);
            if (o < 1) {
                break;
            }
            rtp.coder.decode(cur, buf, o);
            int p = o / devicer.smpb;
            for (int i = 0; i < p; i += 2) {
                long res = cur[i];
                res += cur[i + 1];
                res /= 2;
                cur[i + 0] = (int) res;
                cur[i + 1] = (int) res;
            }
            rtp.coder.encode(cur, buf, o);
            rtp.writeRtp(buf, o);
        }
    }

}
