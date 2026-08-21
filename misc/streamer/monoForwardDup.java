
/**
 * make stream stereo
 *
 * @author matecsaba
 */
public class monoForwardDup {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 7) {
            System.out.println("usage: java this <group> <source> <port> <group> <port> <volume> <channel>");
            return;
        }
        packer source = packer.receiver(args[0], args[1], args[2]);
        packer rtp = packer.sender(args[3], args[4]);
        int vol = (int) (Float.parseFloat(args[5]) * 100);
        int chS = Integer.parseInt(args[6]) & 1;
        int chT = (chS + 1) & 1;
        byte[] buf = new byte[devicer.payl];
        int cur[] = new int[buf.length / devicer.smpb];
        for (;;) {
            int o = source.readRtp(buf);
            if (o < 1) {
                break;
            }
            rtp.coder.decode(cur, buf, o);
            monoDoer.duplicate(cur, chS, chT, vol);
            rtp.coder.encode(cur, buf, o);
            rtp.writeRtp(buf, o);
        }
    }

}
