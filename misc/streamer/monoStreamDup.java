
/**
 * stream live capture
 *
 * @author matecsaba
 */
public class monoStreamDup {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 6) {
            System.out.println("usage: java this <device> <volume> <channel> <group> <source> <port>");
            return;
        }
        devicer dataLine = devicer.getRecord(args[0]);
        int vol = (int) (Float.parseFloat(args[1]) * 100);
        int chS = Integer.parseInt(args[2]) & 1;
        int chT = (chS + 1) & 1;
        packet trg = packer.sender(args[3], args[4], args[5]).string2kind(null);
        byte[] buf = new byte[consts.payl];
        int cur[] = new int[buf.length / consts.smpb];
        for (;;) {
            int o = dataLine.read(buf);
            if (o < 1) {
                break;
            }
            trg.coder.decode(cur, buf, o);
            monoDoer.duplicate(cur, chS, chT, vol);
            monoDoer.duplicate(cur, chS, chS, vol);
            trg.coder.encode(cur, buf, o);
            trg.writeKind(buf, o);
        }
    }

}
