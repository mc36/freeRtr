
/**
 * play and show stream
 *
 * @author matecsaba
 */
public class visReceiver {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 5) {
            System.out.println("usage: java this <device> <kind> <group> <source> <port>");
            return;
        }
        devicer lin = devicer.getPlayback(args[0]);
        packer chn = packer.receiver(args[2], args[3], args[4]);
        packet knd = packet.string2kind(args[1], chn);
        byte[] buf = new byte[devicer.payl];
        visDoer vu = new visDoer();
        for (;;) {
            int i = knd.readKind(buf);
            if (i < 1) {
                break;
            }
            lin.write(buf, i);
            vu.doer(buf, i);
        }
    }

}
