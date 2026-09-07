
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
        if (args.length < 6) {
            System.out.println("usage: java this <group> <port>  <group> <source> <port> <vol>   <group> <source> <port> <vol>  ...");
            return;
        }
        packet target = packer.sender(args[0], args[1]).string2kind(null);
        mixerOne source[] = new mixerOne[(args.length - 2) / 4];
        for (int i = 0; i < source.length; i++) {
            int p = (i * 4) + 2;
            packet s = packer.receiver(args[p + 0], args[p + 1], args[p + 2]).string2kind(null);
            source[i] = new mixerOne(s, args[p + 3]);
        }
        new Thread(new mixerCon(source)).start();
        for (int i = 1; i < source.length; i++) {
            new Thread(source[i]).start();
        }
        byte[] buf = new byte[consts.payl];
        int cur[] = new int[buf.length / consts.smpb];
        long res[] = new long[cur.length];
        for (;;) {
            source[0].readRound();
            for (int i = 0; i < res.length; i++) {
                res[i] = 0;
            }
            for (int o = 0; o < source.length; o++) {
                int[] now = source[o].lst;
                long vol = source[o].vol;
                for (int i = 0; i < res.length; i++) {
                    long val = now[i];
                    val *= vol;
                    val /= 100L;
                    res[i] += val;
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

class mixerCon implements Runnable {

    private final mixerOne[] src;

    private int cur = 0;

    public mixerCon(mixerOne[] s) {
        src = s;
    }

    private void doShow() {
        String a = "";
        for (int i = 0; i < src.length; i++) {
            a += i + ":" + src[i].vol + "  ";
        }
        System.out.print("\r" + a);
    }

    private void doKey() throws Exception {
        int i = System.in.read();
        switch (i) {
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                cur = i - '0';
                if (cur < src.length) {
                    break;
                }
                cur = src.length - 1;
                break;
            case '+':
                src[cur].vol++;
                break;
            case '-':
                src[cur].vol--;
                break;
            case 'x':
                System.exit(0);
                break;
        }

    }

    public void run() {
        for (;;) {
            try {
                doShow();
                doKey();
            } catch (Exception e) {
                break;
            }
        }
    }

}

class mixerOne implements Runnable {

    private final packet src;

    private final int[][] buf;

    private final byte[] cur;

    public int vol;

    private int pos;

    public int[] lst;

    public mixerOne(packet s, String v) {
        src = s;
        cur = new byte[consts.payl];
        buf = new int[3][cur.length / consts.smpb];
        pos = 0;
        lst = buf[0];
        vol = Integer.parseInt(v);
    }

    public void readRound() throws Exception {
        lst = buf[pos];
        pos = (pos + 1) % buf.length;
        int o = src.readKind(cur);
        if (o < 1) {
            throw new Exception("read failed");
        }
        for (int i = o; i < cur.length; i++) {
            cur[i] = 0;
        }
        src.coder.decode(buf[pos], cur, cur.length);
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
