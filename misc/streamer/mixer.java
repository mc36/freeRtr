
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
            System.out.println("usage: java this <group> <source> <port> <vol>  <group> <source> <port> <volL> <volR>   <group> <source> <port> <volL> <volR>  ...");
            return;
        }
        source = new mixerOne[(args.length - 2) / 5];
        target = packer.sender(args[0], args[1], args[2]).string2kind(null);
        outVol = volume2range(Integer.parseInt(args[3]), 0);
        selected = -1;
        for (int i = 0; i < source.length; i++) {
            int p = (i * 5) + 4;
            packet s = packer.receiver(args[p + 0], args[p + 1], args[p + 2]).string2kind(null);
            long vl = volume2range(Integer.parseInt(args[p + 3]), 0);
            long vr = volume2range(Integer.parseInt(args[p + 4]), 0);
            source[i] = new mixerOne(i, s, vl, vr);
        }
        for (int i = 1; i < source.length; i++) {
            new Thread(source[i]).start();
        }
        new Thread(this).start();
        byte[] buf = new byte[consts.payl];
        outLst = new int[buf.length / consts.smpb];
        long res[] = new long[outLst.length];
        for (;;) {
            for (int i = 0; i < outLst.length; i++) {
                res[i] = 0;
            }
            source[0].readRound();
            for (int i = 0; i < source.length; i++) {
                source[i].mixRound(res);
            }
            for (int i = 0; i < outLst.length; i++) {
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
                    a += source[i].getSum();
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
                        selected = (i - '1' + 10) % 10;
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
                        source[selected].volL = volume2range(source[selected].volL, +1);
                        source[selected].volR = volume2range(source[selected].volR, +1);
                        break;
                    case '-':
                        if (selected < 0) {
                            outVol = volume2range(outVol, -1);
                            break;
                        }
                        source[selected].volL = volume2range(source[selected].volL, -1);
                        source[selected].volR = volume2range(source[selected].volR, -1);
                        break;
                    case '[':
                        if (selected < 0) {
                            break;
                        }
                        source[selected].volL = volume2range(source[selected].volL, +1);
                        source[selected].volR = volume2range(source[selected].volR, -1);
                        break;
                    case ']':
                        if (selected < 0) {
                            break;
                        }
                        source[selected].volL = volume2range(source[selected].volL, -1);
                        source[selected].volR = volume2range(source[selected].volR, +1);
                        break;
                    case ' ':
                        System.out.println("\r\n\r\n\ro " + visDoer.rms(outLst));
                        for (i = 0; i < source.length; i++) {
                            System.out.println("\r" + source[i].getDet());
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

    private final int num;

    private final packet src;

    private final int[][] buf;

    private final byte[] cur;

    private int pos;

    private int pkt;

    private int[] lst;

    public long volL;

    public long volR;

    public mixerOne(int n, packet s, long vl, long vr) {
        num = n;
        src = s;
        cur = new byte[consts.payl];
        buf = new int[3][cur.length / consts.smpb];
        pos = 0;
        lst = buf[0];
        volL = vl;
        volR = vr;
    }

    public String getDet() {
        return (num + 1) + " " + visDoer.rms(lst) + " " + pkt;
    }

    public String getSum() {
        return (num + 1) + ":" + volL + "%," + volR + "%  ";
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

    public void mixRound(long[] res) {
        int[] now = lst;
        for (int i = 0; i < now.length; i += 2) {
            long val = now[i + 0];
            val *= volL;
            val /= 100;
            res[i + 0] += val;
            val = now[i + 1];
            val *= volR;
            val /= 100;
            res[i + 1] += val;
        }
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
