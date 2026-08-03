
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

    /**
     * convert time to string
     *
     * @param tim current time
     * @param when time to write
     * @return nice string
     */
    public static String timePast(long tim, long when) {
        long i = (tim - when) / 1000;
        String a = (i % 60) + "s";
        i = i / 60;
        if (i > 0) {
            a = (i % 60) + "m" + a;
            i = i / 60;
        }
        if (i > 0) {
            a = (i % 24) + "h" + a;
            i = i / 24;
        }
        if (i > 0) {
            a = i + "d";
        }
        return a;
    }

}
