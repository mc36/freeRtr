
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
    public static final int payl = 1024;

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
     * bytes in jack-udp header
     */
    public static final int jkul = 8;

    /**
     * bytes in jacktrip header
     */
    public static final int jktl = 16;

    /**
     * jacktrip cached result
     */
    private static int jktc = -1;

    private static final int findInt(int def, int[] vals) {
        for (int i = 0; i < vals.length; i++) {
            if (vals[i] == rate) {
                return i;
            }
        }
        return def;
    }

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
        vbac = findInt(256, vals);
        return vbac;
    }

    /**
     * jacktrip rate bits
     *
     * @return value
     */
    public static final int jktb() {
        if (jktc >= 0) {
            return jktc;
        }
        int[] vals = {
            22050,
            32000,
            44100,
            48000,
            88200,
            96000,
            192000
        };
        jktc = findInt(256, vals) << 24 | (consts.smpb << 19) | 0x200;
        return jktc;
    }

}
