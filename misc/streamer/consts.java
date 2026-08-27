
/**
 * constants
 *
 * @author matecsaba
 */
public class consts {

    /**
     * sampling rate
     */
    public static final int rate = 48000;

    /**
     * bytes per sample
     */
    public static final int smpb = 4;

    /**
     * bytes per payload
     */
    public static final int payl = 1200;

    /**
     * bytes in w64 header
     */
    public static final int wavl = 104;

    /**
     * bytes in rtp header
     */
    public static final int rtpl = 12;

    /**
     * type in rtp header
     */
    public static final int rtpt = 96;

    /**
     * bytes in scream header
     */
    public static final int scrl = 5;

    /**
     * scream rate bits
     */
    public static final int scrb = (rate % 44100) != 0 ? rate / 48000 : 128 | (rate / 44100);

    /**
     * scream channel layout
     */
    public static final int scrt = 3;

    /**
     * bytes in vban header
     */
    public static final int vbal = 28;

    /**
     * vban magic bytes
     */
    public static final int vbam = 0x5642414e;

    /**
     * vbab cached result
     */
    private static int vbac = -1;

    /**
     * bytes in wfas header
     */
    public static final int wfal = 10;

    /**
     * wfas magic bytes
     */
    public static final int wfam = 0x57460200;

    /**
     * vban rate bits
     *
     * @return value
     */
    public static final int vbab() {
        if (vbac >= 0) {
            return vbac;
        }
        int[] vals = {
            6000, 12000, 24000, 48000, 96000, 192000, 384000,
            8000, 16000, 32000, 64000, 128000, 256000, 512000,
            11025, 22050, 44100, 88200, 176400, 352800, 705600
        };
        vbac = 256;
        for (int i = 0; i < vals.length; i++) {
            if (vals[i] == rate) {
                vbac = i;
                break;
            }
        }
        return vbac;
    }

}
