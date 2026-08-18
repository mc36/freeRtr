
/**
 * make stream stereo
 *
 * @author matecsaba
 */
public class unmono {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 6) {
            System.out.println("usage: java this <group> <source> <port> <group> <port> <channel>");
            return;
        }
        packer source = packer.receiver(args[0], args[1], args[2]);
        packer rtp = packer.sender(args[3], args[4]);
        int chS = Integer.parseInt(args[5]) & 1;
        int chT = (chS + 1) & 1;
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
                cur[i + chT] = cur[i + chS];
            }
            rtp.coder.encode(cur, buf, o);
            rtp.writeRtp(buf, o);
        }
    }

}
