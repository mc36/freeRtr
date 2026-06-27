
import javax.sound.sampled.TargetDataLine;

/**
 * measure live delay
 *
 * @author matecsaba
 */
public class measurer {

    public static void main(String[] args) throws Exception {
        int per = Integer.parseInt(args[3]);
        TargetDataLine dataLine = devicer.getRecord(args[0]);
        rtper rtp = new rtper(args[1], args[2]);
        byte[] buf = new byte[rtper.payload];
        byte[] sln = new byte[buf.length];
        byte[] snd = new byte[buf.length];
        for (int i = 0; i < snd.length; i += 4) {
            int o = (int) (32767 * Math.sin(i * Math.PI * rtper.payload / devicer.rate));
            byte h = (byte) (o >> 8);
            byte l = (byte) (o & 0xff);
            snd[i + 0] = h;
            snd[i + 1] = l;
            snd[i + 2] = h;
            snd[i + 3] = l;
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
                int q = (i * 1000) / devicer.rate;
                System.out.println(avg + " > " + ned + " @ " + pos + " [" + i + "] (" + q + "ms)");
                ned = Integer.MAX_VALUE;
            }
            if (pos > per) {
                rtp.write(snd, len);
                pos = 0;
                ned = avg * 3;
            } else {
                rtp.write(sln, len);
            }
            pos++;
        }
    }

}
