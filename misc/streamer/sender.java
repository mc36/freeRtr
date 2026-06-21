
import java.io.InputStream;
import java.net.InetAddress;

public class sender {

    public static void main(String[] args) throws Exception {
        InetAddress group = InetAddress.getByName(args[2]);
        int port = Integer.parseInt(args[3]);
        rtper rtp = new rtper(group, port);

        String[] cmd = {
            "ffmpeg",
            "-ss", args[1],
            "-re",
            "-i", args[0],
            "-vn", "-sn",
            "-ar", "44100",
            "-ac", "2",
            "-c:a", "pcm_s16be",
            "-f", "s16be",
            "-"};
        Process process = Runtime.getRuntime().exec(cmd);
        InputStream stream = process.getInputStream();

        for (;;) {
            byte[] buf = new byte[1024];
            int i = stream.read(buf, 0, buf.length);
            if (i < 0) {
                break;
            }
            if (i < 1) {
                if (!process.isAlive()) {
                    break;
                }
                continue;
            }
            rtp.write(buf, i);
        }
    }

}
