
/**
 * stream live capture
 *
 * @author matecsaba
 */
public class monoStreamMix {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 4) {
            System.out.println("usage: java this <device> <volume> <group> <port>");
            return;
        }
        devicer dataLine = devicer.getRecord(args[0]);
        int vol = (int) (Float.parseFloat(args[1]) * 100);
        packer rtp = packer.sender(args[2], args[3]);
        byte[] buf = new byte[devicer.payl];
        int cur[] = new int[buf.length / devicer.smpb];
        for (;;) {
            int o = dataLine.read(buf);
            if (o < 1) {
                break;
            }
            rtp.coder.decode(cur, buf, o);
            monoDoer.mixer(cur, vol);
            rtp.coder.encode(cur, buf, o);
            rtp.writeRtp(buf, o);
        }
    }

}
