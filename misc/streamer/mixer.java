
/**
 * mix multiple streams
 *
 * @author matecsaba
 */
public class mixer {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        if (args.length < 5) {
            System.out.println("usage: java this <group> <port>  <group> <source> <port>  <group> <source> <port>  ...");
            return;
        }
        packet target = packer.sender(args[0], args[1]).string2kind(null);
        mixerOne source[] = new mixerOne[(args.length - 2) / 3];
        for (int i = 0; i < source.length; i++) {
            int p = (i * 3) + 2;
            packet s = packer.receiver(args[p + 0], args[p + 1], args[p + 2]).string2kind(null);
            source[i] = new mixerOne(s);
        }
        for (int i = 1; i < source.length; i++) {
            new Thread(source[i]).start();
        }
        byte[] buf = new byte[devicer.payl];
        int cur[] = new int[buf.length / devicer.smpb];
        long res[] = new long[cur.length];
        for (;;) {
            source[0].readRound();
            for (int i = 0; i < res.length; i++) {
                res[i] = 0;
            }
            for (int o = 0; o < source.length; o++) {
                target.coder.decode(cur, source[o].lst, buf.length);
                for (int i = 0; i < res.length; i++) {
                    res[i] += cur[i];
                }
            }
            for (int i = 0; i < res.length; i++) {
                cur[i] = (int) (res[i] / source.length);
            }
            target.coder.encode(cur, buf, buf.length);
            target.writeKind(buf, buf.length);
        }
    }

}

class mixerOne implements Runnable {

    private final packet src;

    private final byte[][] buf;

    private int pos;

    public byte[] lst;

    public mixerOne(packet s) {
        src = s;
        buf = new byte[3][devicer.payl];
        pos = 0;
        lst = buf[0];
    }

    public void readRound() throws Exception {
        lst = buf[pos];
        pos = (pos + 1) % buf.length;
        byte[] cur = buf[pos];
        int o = src.readKind(cur);
        if (o < 1) {
            throw new Exception("read failed");
        }
        for (int i = o; i < cur.length; i++) {
            cur[i] = 0;
        }
    }

    public void run() {
        for (;;) {
            try {
                readRound();
            } catch (Exception e) {
                break;
            }
        }
    }

}
