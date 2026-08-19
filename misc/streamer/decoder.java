
import java.io.InputStream;

/**
 * file helpers
 *
 * @author matecsaba
 */
public class decoder {

    private Process process;

    private InputStream stream;

    /**
     * create instance
     *
     * @param fil file
     * @param pos position
     * @param vol volume
     * @throws Exception on error
     */
    public decoder(String fil, String pos, String vol) throws Exception {
        String[] cmd = {
            "ffmpeg",
            "-hide_banner",
            "-loglevel", "quiet",
            "-ss", pos,
            "-re",
            "-i", fil,
            "-vn", "-sn",
            "-af", "volume=" + vol,
            "-ar", "" + devicer.rate,
            "-ac", "2",
            "-c:a", "pcm_s" + (devicer.smpb * 8) + "be",
            "-f", "s" + (devicer.smpb * 8) + "be",
            "-"};
        process = Runtime.getRuntime().exec(cmd);
        stream = process.getErrorStream();
        stream.close();
        stream = process.getInputStream();
    }

    /**
     * read data
     *
     * @param buf buffer
     * @return bytes read
     * @throws Exception on error
     */
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
