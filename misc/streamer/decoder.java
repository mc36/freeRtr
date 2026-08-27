
import java.io.FileOutputStream;
import java.io.InputStream;

/**
 * file helpers
 *
 * @author matecsaba
 */
public class decoder {

    /**
     * the codec
     */
    public final codec coder = codec.getCodec();

    private Process process;

    private InputStream stream;

    private FileOutputStream fil;

    private decoder() {
    }

    /**
     * get playback
     *
     * @param fil file
     * @param pos position
     * @param vol volume
     * @return instance
     * @throws Exception on error
     */
    public static decoder getPlayback(String fil, String pos, String vol) throws Exception {
        decoder r = new decoder();
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
        r.process = Runtime.getRuntime().exec(cmd);
        r.stream = r.process.getErrorStream();
        r.stream.close();
        r.stream = r.process.getInputStream();
        return r;
    }

    /**
     * get recorder
     *
     * @param fil file
     * @return instance
     * @throws Exception on error
     */
    public static decoder getRecord(String fil) throws Exception {
        decoder r = new decoder();
        r.fil = new FileOutputStream(fil, false);
        return r;
    }

    /**
     * write sample data
     *
     * @param buf msb bytes
     * @param len length
     * @throws Exception on error
     */
    public void write(byte[] buf, int len) throws Exception {
        coder.byteSwap(buf, len);
        fil.write(buf, 0, len);
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
