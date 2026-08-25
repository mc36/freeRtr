
/**
 * stream live capture
 *
 * @author mc36
 */
public class visStreamer {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 4) {
            System.out.println("usage: java this <device> <kind> <group> <port>");
            return;
        }
        devicer lin = devicer.getRecord(args[0]);
        packer chn = packer.sender(args[2], args[3]);
        packet knd = packet.string2kind(args[1], chn);
        byte[] buf = new byte[devicer.payl];
        visDoer vu = new visDoer();
        for (;;) {
            int i = lin.read(buf);
            if (i < 1) {
                break;
            }
            knd.writeKind(buf, i);
            vu.doer(buf, i);
        }
    }

}
