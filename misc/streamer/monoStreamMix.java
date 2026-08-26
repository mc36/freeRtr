
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
        devicer lin = devicer.getRecord(args[0]);
        int vol = (int) (Float.parseFloat(args[1]) * 100);
        packet trg = packer.sender(args[2], args[3]).string2kind(null);
        byte[] buf = new byte[devicer.payl];
        int cur[] = new int[buf.length / devicer.smpb];
        for (;;) {
            int o = lin.read(buf);
            if (o < 1) {
                break;
            }
            trg.coder.decode(cur, buf, o);
            monoDoer.mixer(cur, vol);
            trg.coder.encode(cur, buf, o);
            trg.writeKind(buf, o);
        }
    }

}
