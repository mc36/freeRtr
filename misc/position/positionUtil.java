
/**
 * utils
 *
 * @author matecsaba
 */
public class positionUtil {

    /**
     * convert string to number
     *
     * @param s string to convert
     * @return value of string, 0 if failed to convert
     */
    public static float str2num(String s) {
        float i = 0;
        s = s.trim();
        try {
            i = Float.parseFloat(s);
        } catch (Exception e) {
        }
        return i;
    }

    /**
     * convert to hex
     *
     * @param i number
     * @return converted
     */
    public static String toHex(int i) {
        String a = Integer.toString(i, 16);
        for (; a.length() < 4;) {
            a = "0" + a;
        }
        return a;
    }

    /**
     * signal to meters
     *
     * @param mhz frequency
     * @param dbm signal
     * @return meters
     */
    public static double signal2distance(float mhz, float dbm) {
        return Math.round(Math.pow(10.0, (27.55 - (20.0 * Math.log10(mhz)) - dbm) / 20.0));
    }

}
