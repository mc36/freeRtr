
import javax.sound.sampled.TargetDataLine;

/**
 * measure live delay
 *
 * @author matecsaba
 */
public class measurer {

    public static void main(String[] args) throws Exception {
        int per = (Integer.parseInt(args[3]) * 4 * devicer.rate) / rtper.payload;
        int mul = Integer.parseInt(args[4]);
        TargetDataLine dataLine = devicer.getRecord(args[0]);
        rtper rtp = new rtper(args[1], args[2]);
        byte[] buf = new byte[rtper.payload];
        byte[] sln = new byte[buf.length];
        byte[] snd = new byte[buf.length];
        for (int i = 0; i < snd.length; i += 4) {
            int val = (int) (32767 * Math.sin(i * Math.PI * rtper.payload / devicer.rate));
            byte hi = (byte) (val >> 8);
            byte lo = (byte) (val & 0xff);
            snd[i + 0] = hi;
            snd[i + 1] = lo;
            snd[i + 2] = hi;
            snd[i + 3] = lo;
            sln[i + 0] = 0;
            sln[i + 1] = hi;
            sln[i + 2] = 0;
            sln[i + 3] = hi;
        }
        int pos = 0;
        int ned = Integer.MAX_VALUE;
        for (;;) {
            int len = dataLine.read(buf, 0, buf.length);
            if (len < 1) {
                break;
            }
            int avg = 0;
            for (int i = 0; i < len; i += 2) {
                int o = buf[i + 0];
                if (o < 0) {
                    o = -o;
                }
                avg += o;
            }
            avg /= buf.length >> 1;
            if (avg < 1) {
                avg = 1;
            }
            if (avg > ned) {
                int i = pos * len;
                i >>= 2;
                int q = (i * 1000) / devicer.rate;
                System.out.println(avg + " > " + ned + " @ " + pos + " [" + i + "] (" + q + "ms)");
                ned = Integer.MAX_VALUE;
            }
            if (pos > per) {
                rtp.write(snd, len);
                pos = 0;
                ned = avg * mul;
            } else {
                rtp.write(sln, len);
            }
            pos++;
        }
    }

}
