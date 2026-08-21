
/**
 * stream live capture
 *
 * @author mc36
 */
public class visStreamerVba {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 3) {
            System.out.println("usage: java this <device> <group> <port>");
            return;
        }
        devicer dataLine = devicer.getRecord(args[0]);
        packer rtp = packer.sender(args[1], args[2]);
        byte[] buf = new byte[devicer.payl];
        visDoer vu = new visDoer();
        for (;;) {
            int i = dataLine.read(buf);
            if (i < 1) {
                break;
            }
            rtp.writeVba(buf, i);
            vu.doer(buf, i);
        }
    }

}
