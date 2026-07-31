
import javax.sound.sampled.TargetDataLine;

/**
 * measure beep delay
 *
 * @author matecsaba
 */
public class measBeep {

    public static void main(String[] args) throws Exception {
        int per = (Integer.parseInt(args[3]) * devicer.smpb * 2 * devicer.rate) / devicer.payl;
        int mul = Integer.parseInt(args[4]);
        TargetDataLine dataLine = devicer.getRecord(args[0]);
        rtper rtp = new rtper(args[1], args[2]);
        byte[] buf = new byte[devicer.payl];
        byte[] sln = new byte[buf.length];
        byte[] snd = new byte[buf.length];
        for (int i = 0; i < snd.length; i += devicer.smpb * 2) {
            int val = (int) (32767 * Math.sin(i * Math.PI * devicer.payl / devicer.rate));
            byte hi = (byte) (val >> 8);
            byte lo = (byte) (val & 0xff);
            snd[i + 0] = hi;
            snd[i + 1] = lo;
            snd[i + 0 + devicer.smpb] = hi;
            snd[i + 1 + devicer.smpb] = lo;
            sln[i + 1] = hi;
            sln[i + 1 + devicer.smpb] = hi;
        }
        int pos = 0;
        int ned = Integer.MAX_VALUE;
        int avg = 0;
        for (;;) {
            int len = dataLine.read(buf, 0, buf.length);
            if (len < 1) {
                break;
            }
            if (pos > per) {
                rtp.write(snd, len);
                pos = 0;
                ned = avg * mul;
            } else {
                rtp.write(sln, len);
            }
            pos++;
            avg = 0;
            for (int i = 0; i < len; i += devicer.smpb) {
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
            int i = (pos * len) / (2 * devicer.smpb);
            int q = (i * 1000) / devicer.rate;
            System.out.println(avg + " > " + ned + " @ " + pos + " [" + i + "] (" + q + "ms)");
            ned = Integer.MAX_VALUE;
        }
    }

}
