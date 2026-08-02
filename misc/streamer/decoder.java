
import java.io.InputStream;

/**
 * file helpers
 *
 * @author matecsaba
 */
public class decoder {

    private Process process;

    private InputStream stream;

    public decoder(String fil, String pos) throws Exception {
        String[] cmd = {
            "ffmpeg",
            "-hide_banner",
            "-loglevel", "quiet",
            "-ss", pos,
            "-re",
            "-i", fil,
            "-vn", "-sn",
            "-ar", "" + consts.rate,
            "-ac", "2",
            "-c:a", "pcm_s" + (consts.smpb * 8) + "be",
            "-f", "s" + (consts.smpb * 8) + "be",
            "-"};
        process = Runtime.getRuntime().exec(cmd);
        stream = process.getErrorStream();
        stream.close();
        stream = process.getInputStream();
    }

    public int read(byte[] buf) throws Exception {
        if (!process.isAlive()) {
            if (stream.available() < buf.length) {
                return -2;
            }
        }
        int i = stream.read(buf, 0, buf.length);
        return i;
    }

}
