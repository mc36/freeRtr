
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
        if (args.length < 9) {
            System.out.println("usage: java this  <bufs> <kind>  <group> <source> <port> <vol>  <group> <source> <port> <volL> <volR>   <group> <source> <port> <volL> <volR>  ...");
            return;
        }
        mixerOne[] source = new mixerOne[(args.length - 6) / 5];
        packet target = packer.sender(args[2], args[3], args[4]).string2kind(args[1]);
        long outVol = volume2range(Integer.parseInt(args[5]), 0);
        int selected = Integer.parseInt(args[0]);
        for (int i = 0; i < source.length; i++) {
            int p = (i * 5) + 6;
            packet s = packer.receiver(args[p + 0], args[p + 1], args[p + 2]).string2kind(args[1]);
            long vl = volume2range(Integer.parseInt(args[p + 3]), 0);
            long vr = volume2range(Integer.parseInt(args[p + 4]), 0);
            source[i] = new mixerOne(selected, s, vl, vr);
        }
        source[0].src.pck.setBlock(true);
        for (int i = 1; i < source.length; i++) {
            source[i].src.pck.setBlock(false);
        }
        selected = -1;
        byte[] buf = new byte[consts.payl];
        int outLst[] = new int[buf.length / consts.smpb];
        long res[] = new long[outLst.length];
        for (;;) {
            for (int i = 0; i < outLst.length; i++) {
                res[i] = 0;
            }
            source[0].readRound();
            for (int i = 1; i < source.length; i++) {
                source[i].readRounds();
            }
            for (int i = 0; i < source.length; i++) {
                source[i].mixRound(res);
            }
            for (int i = 0; i < outLst.length; i++) {
                long val = res[i];
                val *= outVol;
                val /= source.length;
                val /= 100;
                outLst[i] = (int) val;
            }
            target.coder.encode(outLst, buf, buf.length);
            target.writeKind(buf, buf.length);
            if (System.in.available() < 1) {
                continue;
            }
            int i = System.in.read();
            mixerOne cur = selected < 0 ? null : source[selected];
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
                case 'm':
                case 'M':
                    if (cur == null) {
                        outVol = 0;
                        break;
                    }
                    cur.volL = 0;
                    cur.volR = 0;
                    break;
                case '+':
                case 'u':
                case 'U':
                    if (cur == null) {
                        outVol = volume2range(outVol, +1);
                        break;
                    }
                    cur.volL = volume2range(cur.volL, +1);
                    cur.volR = volume2range(cur.volR, +1);
                    break;
                case '-':
                case 'd':
                case 'D':
                    if (cur == null) {
                        outVol = volume2range(outVol, -1);
                        break;
                    }
                    cur.volL = volume2range(cur.volL, -1);
                    cur.volR = volume2range(cur.volR, -1);
                    break;
                case '[':
                case 'l':
                case 'L':
                    if (cur == null) {
                        break;
                    }
                    cur.volL = volume2range(cur.volL, +1);
                    cur.volR = volume2range(cur.volR, -1);
                    break;
                case ']':
                case 'r':
                case 'R':
                    if (cur == null) {
                        break;
                    }
                    cur.volL = volume2range(cur.volL, -1);
                    cur.volR = volume2range(cur.volR, +1);
                    break;
                case 'x':
                case 'X':
                case 'q':
                case 'Q':
                    System.out.println("\r");
                    System.exit(0);
                    break;
                case '?':
                    System.out.println("enter=status, space=detail, 1..9=input, o=output, +,-,u,d=volume up/down, ],[,l,r=balance left/right, m=mute, c=clear, x=exit");
                    break;
                case 'c':
                case 'C':
                    for (i = 0; i < source.length; i++) {
                        cur = source[i];
                        cur.pkt = 0;
                        cur.ovr = 0;
                        cur.ovr = 0;
                        cur.und = 0;
                        cur.len = 0;
                    }
                    break;
                case ' ':
                    System.out.println("\r\n\r\n\ro " + visDoer.rms(outLst) + " pkt mis len ovr und");
                    for (i = 0; i < source.length; i++) {
                        cur = source[i];
                        System.out.println("\r" + (i + 1) + " " + cur.getRms() + " " + cur.pkt + " " + (source[0].pkt - cur.pkt) + " " + cur.len + " " + cur.ovr + " " + cur.und);
                    }
                    System.out.println("\r");
                    break;
            }
            String a = selected < 0 ? "o" : "" + (1 + selected);
            a = "\rs:" + a + "  o:" + outVol + "%  ";
            for (i = 0; i < source.length; i++) {
                cur = source[i];
                a += (i + 1) + ":" + cur.volL + "%," + cur.volR + "%  ";
            }
            System.out.print(a + "    \r");
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

}

class mixerOne {

    public final packet src;

    private final byte[] cur;

    private final int[][] buf;

    private int posW;

    private int posR;

    public int pkt;

    public int ovr;

    public int und;

    public int len;

    public long volL;

    public long volR;

    public mixerOne(int b, packet s, long vl, long vr) {
        src = s;
        cur = new byte[consts.payl];
        buf = new int[b][cur.length / consts.smpb];
        volL = vl;
        volR = vr;
    }

    public String getRms() {
        return visDoer.rms(buf[posR]);
    }

    public void readRounds() throws Exception {
        int don = 0;
        for (;;) {
            boolean b = readRound();
            if (b) {
                break;
            }
            don++;
        }
        if (don >= buf.length) {
            ovr++;
        }
        if (don < 1) {
            und++;
        }
    }

    public boolean readRound() throws Exception {
        int o = src.readKind(cur);
        if (o < 1) {
            return true;
        }
        if (o != cur.length) {
            for (int i = o; i < cur.length; i++) {
                cur[i] = 0;
            }
            len++;
        }
        src.coder.decode(buf[posW], cur, cur.length);
        posW = (posW + 1) % buf.length;
        pkt++;
        return false;
    }

    public void mixRound(long[] res) {
        int[] now = buf[posR];
        posR = (posR + 1) % buf.length;
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

}
