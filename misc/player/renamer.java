
import java.io.File;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * rename files
 *
 * @author matecsaba
 */
public class renamer {

    /**
     * current year
     */
    public static int year;

    /**
     * the main
     *
     * @param args arguments
     */
    public static void main(String[] args) {
        year = LocalDateTime.now().getYear();
        String prepend = "";
        String remover = "";
        boolean really = true;
        boolean basic = false;
        for (int i = 1; i < args.length; i++) {
            String s = args[i].toLowerCase();
            if (s.equals("predir")) {
                prepend = new File(".").getAbsolutePath();
                int o = prepend.lastIndexOf("/");
                prepend = prepend.substring(0, o);
                o = prepend.lastIndexOf("/");
                prepend = prepend.substring(o + 1, prepend.length()) + " ";
            }
            if (s.equals("prepend")) {
                i++;
                prepend = args[i] + " ";
            }
            if (s.equals("remover")) {
                i++;
                remover = args[i] + " ";
            }
            if (s.equals("noremover")) {
                remover = "";
            }
            if (s.equals("noprepend")) {
                prepend = "";
            }
            if (s.equals("demo")) {
                really = false;
            }
            if (s.equals("really")) {
                really = true;
            }
            if (s.equals("extended")) {
                basic = false;
            }
            if (s.equals("basic")) {
                basic = true;
            }
        }
        String s = args[0];
        playerUtil.put("reading " + s + "...");
        String[] l1 = new File(s).list();
        List<String> l2 = new ArrayList<String>();
        for (int i = 0; i < l1.length; i++) {
            l2.add(l1[i]);
        }
        Collections.sort(l2, new duplicates());
        int tried = 0;
        int failed = 0;
        for (int i = 0; i < l2.size(); i++) {
            s = l2.get(i);
            String a = guessName(prepend + s, basic);
            a = guessName(a, basic);
            a = guessName(a, basic);
            if (a == null) {
                playerUtil.put(s + " have no guess");
                continue;
            }
            if (a.startsWith(remover)) {
                a = a.substring(remover.length(), a.length()).trim();
            }
            if (a.equals(s)) {
                continue;
            }
            playerUtil.put(s + " --> " + a);
            tried++;
            if (!really) {
                continue;
            }
            try {
                new File(args[0] + s).renameTo(new File(args[0] + a));
            } catch (Exception e) {
                failed++;
            }
        }
    }

    /**
     * path separator
     */
    public final static String sep = "/";

    /**
     * guess name
     *
     * @param s string to use
     * @param basic guess mode
     * @return guessed name
     */
    public static String guessName(String s, boolean basic) {
        if (s == null) {
            return null;
        }
        if (s.length() < 1) {
            return null;
        }
        if (s.startsWith(".")) {
            return null;
        }
        if (s.indexOf(sep) >= 0) {
            return null;
        }
        s = s.replaceAll("á", "a");
        s = s.replaceAll("Á", "a");
        s = s.replaceAll("é", "e");
        s = s.replaceAll("É", "e");
        s = s.replaceAll("í", "i");
        s = s.replaceAll("Í", "i");
        s = s.replaceAll("ó", "o");
        s = s.replaceAll("Ó", "o");
        s = s.replaceAll("ö", "o");
        s = s.replaceAll("Ö", "o");
        s = s.replaceAll("ú", "u");
        s = s.replaceAll("Ú", "u");
        s = s.replaceAll("ü", "u");
        s = s.replaceAll("Ü", "u");
        s = s.replaceAll("̸", "");
        s = s.replaceAll("ː", "");
        s = s.replaceAll("：", "");
        s = s.replaceAll("？", "");
        s = s.replaceAll("…", "");
        s = s.replaceAll("”", "");
        s = s.replaceAll("“", "");
        s = s.replaceAll("’", "");
        s = s.replaceAll("=", " ");
        s = s.replaceAll("_", " ");
        s = s.replaceAll("~", " ");
        s = s.replaceAll("–", "-");
        s = s.replaceAll("&", " ");
        s = s.replaceAll("@", " ");
        s = s.replaceAll("#", " ");
        s = s.replaceAll("\\$", " ");
        s = s.replaceAll("%", " ");
        s = s.replaceAll("\\+", " ");
        s = s.replaceAll("\\^", " ");
        s = s.replaceAll("\\*", " ");
        s = s.replaceAll("\\(", " ");
        s = s.replaceAll("\\)", " ");
        s = s.replaceAll("\\[", " ");
        s = s.replaceAll("\\]", " ");
        s = s.replaceAll("\\{", " ");
        s = s.replaceAll("\\}", " ");
        s = s.replaceAll("\\\\", " ");
        s = s.replaceAll("<", " ");
        s = s.replaceAll(">", " ");
        s = s.replaceAll("/", " ");
        s = s.replaceAll("!", " ");
        s = s.replaceAll("\\?", " ");
        s = s.replaceAll(",", " ");
        s = s.replaceAll(":", " ");
        s = s.replaceAll(";", " ");
        s = s.replaceAll("\\.", " ");
        s = s.replaceAll("'", "");
        s = s.replaceAll("`", "");
        s = s.replaceAll("\"", "");
        s = s.replaceAll("  ", " ");
        s = s.trim();
        String a = "";
        for (;;) {
            if (s.length() < 1) {
                break;
            }
            int i = s.indexOf(" ");
            String b;
            if (i < 1) {
                b = s;
                s = "";
            } else {
                b = s.substring(0, i);
                s = s.substring(i, s.length()).trim();
            }
            b = b.trim();
            if (b.length() < 1) {
                continue;
            }
            if (b.length() > 1) {
                b = b.substring(0, 1).toUpperCase() + b.substring(1, b.length()).toLowerCase();
            }
            a += " " + b;
        }
        s = a.trim();
        if (basic) {
            return s;
        }
        int p = s.lastIndexOf(" ");
        a = s.substring(p + 1, s.length());
        if ("|-|Ep|Live|Demo|Split|Promo||Single|Instrumental|Remastered|Rerecorded|Reissue|Re-recorded|Re-mastered|Re-issue|Compilation|Deluxe|Limited|Edition|".indexOf("|" + a + "|") >= 0) {
            s = s.substring(0, p).trim();
        }
        p = -1;
        for (int i = s.length(); i >= 0; i--) {
            if (isDate(s, i)) {
                p = i;
            }
        }
        if (p < 0) {
            return s;
        }
        a = s.substring(p, p + 4);
        s = s.substring(0, p) + sep + s.substring(p + 4, s.length());
        s = doAlign(s);
        if (s.endsWith(sep)) {
            s = s.substring(0, s.length() - 1);
            int i = s.indexOf("-");
            if (i < 1) {
                return null;
            }
            s = s.substring(0, i) + sep + s.substring(i + 1, s.length());
        }
        s = doAlign(s);
        s = s.replaceAll(sep, " - " + a + " - ").trim();
        return s;
    }

    private static String doAlign(String s) {
        s = s.trim();
        for (int i = 0; i < 5; i++) {
            s = s.replaceAll(" " + sep, sep).replaceAll(sep + " ", sep);
            s = s.replaceAll("\\[" + sep, sep).replaceAll(sep + "\\]", sep);
            s = s.replaceAll("\\(" + sep, sep).replaceAll(sep + "\\)", sep);
            s = s.replaceAll("-" + sep, sep).replaceAll(sep + "-", sep);
        }
        return s.trim();
    }

    /**
     * test if this is a date
     *
     * @param s string to use
     * @param p position
     * @return true on success
     */
    public static boolean isDate(String s, int p) {
        int i;
        try {
            s = s.substring(p, p + 4);
            i = Integer.parseInt(s);
        } catch (Exception e) {
            return false;
        }
        if (i < (year - 40)) {
            return false;
        }
        if (i > (year + 1)) {
            return false;
        }
        return true;
    }

}
