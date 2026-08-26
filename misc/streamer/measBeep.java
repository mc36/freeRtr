
/**
 * measure beep delay
 *
 * @author matecsaba
 */
public class measBeep {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 5) {
            System.out.println("usage: java this <device> <group> <port> <period> <multiplier>");
            return;
        }
        int per = (Integer.parseInt(args[3]) * devicer.smpb * 2 * devicer.rate) / devicer.payl;
        int mul = Integer.parseInt(args[4]);
        devicer lin = devicer.getRecord(args[0]);
        packet trg = packer.sender(args[1], args[2]).string2kind(null);
        byte[] buf = new byte[devicer.payl];
        byte[] sln = new byte[buf.length];
        byte[] snd = new byte[buf.length];
        measFreq.toneGen(snd, 0, 1000, 32767);
        measFreq.toneGen(sln, 0, 1000, 127);
        int pos = 0;
        int ned = Integer.MAX_VALUE;
        int avg = 0;
        for (;;) {
            int len = lin.read(buf);
            if (len < 1) {
                break;
            }
            if (pos > per) {
                trg.writeKind(snd, snd.length);
                pos = 0;
                ned = avg * mul;
            } else {
                if (pos < (devicer.rate / devicer.payl)) {
                    trg.writeKind(snd, snd.length);
                } else {
                    trg.writeKind(sln, sln.length);
                }
            }
            pos++;
            avg = 0;
            for (int i = 0; i < buf.length; i += devicer.smpb) {
                int o = buf[i + 0];
                if (o < 0) {
                    o = -o;
                }
                avg += o;
            }
            avg /= buf.length / 2;
            if (avg < 1) {
                avg = 1;
            }
            if (avg < ned) {
                continue;
            }
            int i = (pos * buf.length) / (2 * devicer.smpb);
            int q = (i * 1000) / devicer.rate;
            System.out.println(avg + " > " + ned + " @ " + pos + " [" + i + "] (" + q + "ms)");
            ned = Integer.MAX_VALUE;
        }
    }

}
