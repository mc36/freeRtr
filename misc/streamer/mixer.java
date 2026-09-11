
/**
 * mix multiple streams
 *
 * @author matecsaba
 */
public class mixer implements Runnable {

    /**
     * the main
     *
     * @param args arguments
     * @throws Exception on error
     */
    public static void main(String[] args) throws Exception {
        new mixer().doer(args);
    }

    private mixerOne source[];

    private long outVol;

    private int outLst[];

    private packet target;

    private int selected;

    private void doer(String[] args) throws Exception {
        if (args.length < 8) {
            System.out.println("usage: java this <group> <source> <port> <vol>  <group> <source> <port> <vol>   <group> <source> <port> <vol>  ...");
            return;
        }
        source = new mixerOne[(args.length - 2) / 4];
        target = packer.sender(args[0], args[1], args[2]).string2kind(null);
        outVol = volume2range(Integer.parseInt(args[3]), 0);
        selected = -1;
        for (int i = 0; i < source.length; i++) {
            int p = (i * 4) + 4;
            packet s = packer.receiver(args[p + 0], args[p + 1], args[p + 2]).string2kind(null);
            long v = volume2range(Integer.parseInt(args[p + 3]), 0);
            source[i] = new mixerOne(s, v);
        }
        for (int i = 1; i < source.length; i++) {
            new Thread(source[i]).start();
        }
        new Thread(this).start();
        byte[] buf = new byte[consts.payl];
        outLst = new int[buf.length / consts.smpb];
        long res[] = new long[outLst.length];
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
                    val /= 100;
                    res[i] += val;
                }
            }
            for (int i = 0; i < res.length; i++) {
                long val = res[i];
                val /= source.length;
                val *= outVol;
                val /= 100;
                outLst[i] = (int) val;
            }
            target.coder.encode(outLst, buf, buf.length);
            target.writeKind(buf, buf.length);
        }
    }

    /**
     * update volume
     *
     * @param cur currently
     * @param dir direction
     * @return updated
     */
    public static long volume2range(long cur, int dir) {
        long mov = cur / 10;
        if (mov < 1) {
            mov = 1;
        }
        cur += dir * mov;
        if (cur < 0) {
            cur = 0;
        }
        if (cur > 999) {
            cur = 999;
        }
        return cur;
    }

    /**
     * run console
     */
    public void run() {
        for (;;) {
            try {
                String a = "\ro:" + outVol + "%  ";
                for (int i = 0; i < source.length; i++) {
                    a += i + ":" + source[i].vol + "%  ";
                }
                System.out.print(a);
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
                        selected = i - '0';
                        if (selected < source.length) {
                            break;
                        }
                        selected = source.length - 1;
                        break;
                    case 'o':
                    case 'O':
                        selected = -1;
                        break;
                    case 'x':
                    case 'X':
                    case 'q':
                    case 'Q':
                        System.out.println("\r");
                        System.exit(0);
                        break;
                    case '+':
                        if (selected < 0) {
                            outVol = volume2range(outVol, +1);
                            break;
                        }
                        source[selected].vol = volume2range(source[selected].vol, +1);
                        break;
                    case '-':
                        if (selected < 0) {
                            outVol = volume2range(outVol, -1);
                            break;
                        }
                        source[selected].vol = volume2range(source[selected].vol, -1);
                        break;
                    case ' ':
                        System.out.println("\r");
                        System.out.println("\ro " + visDoer.rms(outLst));
                        for (i = 0; i < source.length; i++) {
                            System.out.println("\r" + i + " " + visDoer.rms(source[i].lst) + " " + source[i].pkt);
                        }
                        System.out.println("\r");
                        break;
                }
            } catch (Exception e) {
                e.printStackTrace();
                break;
            }
        }
    }

}

class mixerOne implements Runnable {

    private final packet src;

    private final int[][] buf;

    private final byte[] cur;

    private int pos;

    public int pkt;

    public long vol;

    public int[] lst;

    public mixerOne(packet s, long v) {
        src = s;
        cur = new byte[consts.payl];
        buf = new int[3][cur.length / consts.smpb];
        pos = 0;
        lst = buf[0];
        vol = v;
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
        pkt++;
    }

    public void run() {
        for (;;) {
            try {
                readRound();
            } catch (Exception e) {
                e.printStackTrace();
                break;
            }
        }
    }

}
