
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.TimeZone;

/**
 * utils
 *
 * @author matecsaba
 */
public class temperUtil {

    /**
     * get millisecs since 1970 jan 1
     *
     * @return current time
     */
    public static long getTime() {
        return new Date().getTime();
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
     * convert time to string
     *
     * @param tzd time zone to use
     * @param tim time to convert
     * @return string
     */
    public static String time2str(String tzd, long tim) {
        Calendar cal = new GregorianCalendar(TimeZone.getTimeZone(tzd));
        cal.setTime(new Date(tim));
        String date = cal.get(Calendar.YEAR) + padBeg("" + (cal.get(Calendar.MONTH) + 1), 2, "0") + padBeg("" + cal.get(Calendar.DAY_OF_MONTH), 2, "0");
        String time = padBeg("" + cal.get(Calendar.HOUR_OF_DAY), 2, "0") + padBeg("" + cal.get(Calendar.MINUTE), 2, "0") + padBeg("" + cal.get(Calendar.SECOND), 2, "0");
        return date + " " + time;
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

    /**
     * sleep some time without wakeups
     *
     * @param msec time
     */
    public static void sleep(int msec) {
        if (msec < 1) {
            return;
        }
        try {
            synchronized (sleeper) {
                sleeper.wait(msec);
            }
        } catch (Exception e) {
        }
    }

    private final static Object sleeper = new Object();

    /**
     * append one line
     *
     * @param fn file name
     * @param ln line
     */
    public static void append(String fn, String ln) {
        try {
            BufferedWriter f = new BufferedWriter(new FileWriter(fn, true));
            f.append(ln + "\n");
            f.close();
        } catch (Exception e) {
        }
    }

    /**
     * read up file
     *
     * @param fn file name
     * @return lines
     */
    public static List<String> readup(String fn) {
        List<String> l = new ArrayList<String>();
        try {
            BufferedReader f = new BufferedReader(new FileReader(fn));
            while (f.ready()) {
                l.add(f.readLine());
            }
            f.close();
        } catch (Exception e) {
        }
        return l;
    }

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

}
