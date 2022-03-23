
import java.util.Date;

/**
 * utils
 *
 * @author matecsaba
 */
public class motionUtil {

    /**
     * convert string to number
     *
     * @param s string to convert
     * @return value of string, 0 if failed to convert
     */
    public static int str2num(String s) {
        int i = 0;
        s = s.trim();
        try {
            i = Integer.parseInt(s);
        } catch (Exception e) {
        }
        return i;
    }

    /**
     * pad up to beginning for a minimum size
     *
     * @param str string to pad up
     * @param min minimum size to use
     * @param pad padding string
     * @return padded string
     */
    public static String padBeg(String str, int min, String pad) {
        for (; str.length() < min;) {
            str = pad + str;
        }
        return str;
    }

    /**
     * get millisecs since 1970 jan 1
     *
     * @return current time
     */
    public static long getTime() {
        return new Date().getTime();
    }

}
