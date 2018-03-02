
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.InputStream;
import java.io.PrintStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * utils
 *
 * @author matecsaba
 */
public class playerUtil {

    /**
     * debug net io
     */
    public static boolean debugNet = false;

    /**
     * debug file io
     */
    public static boolean debugFile = false;

    /**
     * debug cache
     */
    public static boolean localCache = false;

    /**
     * days in millis
     *
     * @param days days
     * @return millis
     */
    public static long daysInMs(long days) {
        return days * 24 * 60 * 60 * 1000;
    }

    /**
     * put one line
     *
     * @param s string to write
     */
    public static void put(String s) {
        System.out.println(s);
    }

    /**
     * check if key pressed
     *
     * @return true if was
     */
    public static boolean keyPress() {
        try {
            return System.in.available() > 0;
        } catch (Exception e) {
            return true;
        }
    }

    /**
     * put out lines
     *
     * @param l lines to write
     */
    public static void put(playerLyric l) {
        for (int i = 0; i < l.size(); i++) {
            put(l.get(i));
        }
    }

    /**
     * get time
     *
     * @return time in millis
     */
    public static long getTime() {
        return new Date().getTime();
    }

    /**
     * save as
     *
     * @param buf buffer to save
     * @param pat file to overwrite
     */
    public static void saveas(playerLyric buf, String pat) {
        if (debugFile) {
            put("writing " + pat + "...");
        }
        try {
            FileOutputStream ot = new FileOutputStream(pat, false);
            PrintStream pr = new PrintStream(ot);
            for (int i = 0; i < buf.size(); i++) {
                pr.println(buf.get(i));
            }
            pr.flush();
            pr.close();
        } catch (Exception e) {
        }
    }

    /**
     * text to binary
     *
     * @param lst text
     * @param mod mode
     * @return binary
     */
    public static byte[] txt2bin(playerLyric lst, int mod) {
        List<Integer> res = new ArrayList<Integer>();
        for (int i = 0; i < lst.size(); i++) {
            byte[] buf = lst.get(i).getBytes();
            for (int o = 0; o < buf.length; o++) {
                res.add(buf[o] & 0xff);
            }
            switch (mod) {
                case 13:
                    res.add(13);
                    break;
                case 10:
                    res.add(10);
                    break;
                case 1013:
                    res.add(10);
                    res.add(13);
                    break;
                case 1310:
                    res.add(13);
                    res.add(10);
                    break;
            }
        }
        byte[] buf = new byte[res.size()];
        for (int i = 0; i < buf.length; i++) {
            buf[i] = (byte) ((int) res.get(i));
        }
        return buf;
    }

    /**
     * download one file
     *
     * @param pat url
     * @return downloaded
     */
    public static playerLyric download(String pat) {
        String cache = "cache." + playerLyric.justWords(pat).replaceAll("\\ ", "#");
        if (localCache) {
            playerLyric res = readup(cache);
            if (res != null) {
                return res;
            }
        }
        if (debugNet) {
            put("getting " + pat + "...");
        }
        try {
            playerLyric res = doRead(new URL(pat).openStream());
            if (res == null) {
                return null;
            }
            if (localCache) {
                saveas(res, cache);
            }
            return res;
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * html to text
     *
     * @param src url
     * @return text
     */
    public static String html2text(String src) {
        String res = "";
        for (;;) {
            int i = src.indexOf("&");
            if (i < 0) {
                res += src;
                break;
            }
            res += src.substring(0, i);
            src = src.substring(i + 1, src.length());
            i = src.indexOf(";");
            if (i < 0) {
                res += src;
                break;
            }
            String a = src.substring(0, i);
            src = src.substring(i + 1, src.length());
            if (a.equals("nbsp")) {
                res += " ";
                continue;
            }
            if (a.equals("lt")) {
                res += "<";
                continue;
            }
            if (a.equals("gt")) {
                res += ">";
                continue;
            }
            if (a.equals("amp")) {
                res += "&";
                continue;
            }
            if (!a.startsWith("#")) {
                res += "&" + a + ";";
                continue;
            }
            a = a.substring(1, a.length());
            i = str2int(a);
            if ((i < 1) || (i > 254)) {
                res += "&#" + a + ";";
                continue;
            }
            byte[] buf = new byte[1];
            buf[0] = (byte) (i & 0xff);
            res += new String(buf);
        }
        return res.trim();
    }

    /**
     * string to integer
     *
     * @param s string
     * @return integer
     */
    public static int str2int(String s) {
        try {
            return Integer.parseInt(s);
        } catch (Exception e) {
            return 0;
        }
    }

    /**
     * sleep some time without wakeups
     *
     * @param msec
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
     * html to text
     *
     * @param src url
     * @return text
     */
    public static playerLyric html2text(playerLyric src) {
        playerLyric res = new playerLyric();
        if (src == null) {
            return res;
        }
        String cur = "";
        for (int i = 0; i < src.size(); i++) {
            cur += src.get(i);
            for (;;) {
                int o = cur.indexOf("<");
                if (o < 0) {
                    if (cur.length() < 1) {
                        break;
                    }
                    res.add("1" + html2text(cur));
                    cur = "";
                    break;
                }
                if (o > 0) {
                    res.add("1" + html2text(cur.substring(0, o)));
                }
                cur = cur.substring(o, cur.length());
                o = cur.indexOf(">");
                if (o < 0) {
                    break;
                }
                String s = cur.substring(1, o).trim();
                if (s.endsWith("/")) {
                    s = s.substring(0, s.length() - 1);
                }
                s = s.replaceAll("  ", " ").trim().toLowerCase();
                res.add("2" + s);
                cur = cur.substring(o + 1, cur.length());
            }
        }
        if (cur.length() > 0) {
            res.add("1" + html2text(cur));
        }
        return res;
    }

    /**
     * read up file
     *
     * @param pat pathname
     * @return text
     */
    public static playerLyric readup(String pat) {
        if (debugFile) {
            put("reading " + pat + "...");
        }
        try {
            File fil = new File(pat);
            long tim = fil.lastModified();
            playerLyric res = doRead(new FileInputStream(pat));
            if (res == null) {
                return null;
            }
            res.modified = tim;
            return res;
        } catch (Exception e) {
            return null;
        }
    }

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
     * read up stream
     *
     * @param reader stream to read
     * @return text
     */
    public static playerLyric doRead(InputStream reader) {
        playerLyric res = new playerLyric();
        String ln = "";
        int pr = -1;
        for (;;) {
            int i;
            try {
                i = reader.read();
            } catch (Exception e) {
                break;
            }
            if (i < 0) {
                break;
            }
            int prv = pr;
            pr = i;
            switch (i) {
                case 10:
                case 13:
                    if (prv == (23 - i)) {
                        pr = -1;
                        break;
                    }
                    res.add(ln);
                    ln = "";
                    break;
                default:
                    byte[] buf = new byte[1];
                    buf[0] = (byte) i;
                    ln += new String(buf);
                    break;
            }
        }
        if (ln.length() > 0) {
            res.add(ln);
        }
        try {
            reader.close();
        } catch (Exception e) {
        }
        return res;
    }

}
