
import java.util.ArrayList;
import java.util.List;

/**
 * lyric
 *
 * @author matecsaba
 */
public class playerLyric {

    private List<String> text = new ArrayList<String>();

    /**
     * modified
     */
    public long modified;

    /**
     * in range
     *
     * @param i number
     * @return true if not, false if yes
     */
    public boolean inRange(int i) {
        if (i < 0) {
            return true;
        }
        if (i >= text.size()) {
            return true;
        }
        return false;
    }

    /**
     * title
     *
     * @return title
     */
    public String title() {
        String s = text.get(0);
        text.remove(0);
        return s;
    }

    /**
     * get line
     *
     * @param i line
     * @return text
     */
    public String get(int i) {
        return text.get(i);
    }

    /**
     * delete line
     *
     * @param i line
     */
    public void del(int i) {
        text.remove(i);
    }

    /**
     * add line
     *
     * @param s text
     */
    public void add(String s) {
        text.add(s);
    }

    /**
     * add lines
     *
     * @param l text
     */
    public void addAll(playerLyric l) {
        for (int i = 0; i < l.size(); i++) {
            text.add(l.get(i));
        }
    }

    /**
     * find
     *
     * @param s text
     * @return index
     */
    public int findFirst(String s) {
        s = s.toLowerCase();
        for (int i = 0; i < text.size(); i++) {
            if (text.get(i).toLowerCase().equals(s)) {
                return i;
            }
        }
        return -1;
    }

    /**
     * find
     *
     * @param s text
     * @return index
     */
    public int findLast(String s) {
        s = s.toLowerCase();
        for (int i = text.size() - 1; i >= 0; i--) {
            if (text.get(i).toLowerCase().equals(s)) {
                return i;
            }
        }
        return -1;
    }

    /**
     * find
     *
     * @param s text
     * @return index
     */
    public int findRegex(String s) {
        for (int i = text.size() - 1; i >= 0; i--) {
            if (text.get(i).matches(s)) {
                return i;
            }
        }
        return -1;
    }

    /**
     * find
     *
     * @param s text
     * @return count
     */
    public int findCount(String s) {
        int o = 0;
        s = s.toLowerCase();
        for (int i = 0; i < text.size(); i++) {
            if (text.get(i).toLowerCase().equals(s)) {
                o++;
            }
        }
        return o;
    }

    /**
     * lines
     *
     * @return lines
     */
    public int size() {
        return text.size();
    }

    /**
     * break
     *
     * @param s text
     * @return true if yes
     */
    public static boolean isBreak(String s) {
        return s.equals("2br");
    }

    /**
     * just words
     *
     * @param s string
     * @return string
     */
    public static String justWords(String s) {
        s = s.replaceAll("\\.", " ");
        s = s.replaceAll("\\,", " ");
        s = s.replaceAll("\\!", " ");
        s = s.replaceAll("\\?", " ");
        s = s.replaceAll("\\@", " ");
        s = s.replaceAll("\\#", " ");
        s = s.replaceAll("\\$", " ");
        s = s.replaceAll("\\%", " ");
        s = s.replaceAll("\\&", " ");
        s = s.replaceAll("\\*", " ");
        s = s.replaceAll("\\(", " ");
        s = s.replaceAll("\\)", " ");
        s = s.replaceAll("\\<", " ");
        s = s.replaceAll("\\>", " ");
        s = s.replaceAll("\\[", " ");
        s = s.replaceAll("\\]", " ");
        s = s.replaceAll("\\{", " ");
        s = s.replaceAll("\\}", " ");
        s = s.replaceAll("\\-", " ");
        s = s.replaceAll("\\=", " ");
        s = s.replaceAll("\\+", " ");
        s = s.replaceAll("\\`", " ");
        s = s.replaceAll("\\'", " ");
        s = s.replaceAll("\\\"", " ");
        s = s.replaceAll("\\;", " ");
        s = s.replaceAll("\\:", " ");
        s = s.replaceAll("\\\\", " ");
        s = s.replaceAll("\\/", " ");
        s = s.replaceAll("\\|", " ");
        for (;;) {
            String a = s;
            s = s.replaceAll("\\ \\ ", " ");
            if (a.equals(s)) {
                break;
            }
        }
        return s.trim();
    }

    /**
     * is text
     *
     * @param s string
     * @return true if yes
     */
    public static boolean isText(String s) {
        return s.startsWith("1");
    }

    /**
     * is tag
     *
     * @param s text
     * @return true if yes
     */
    public static boolean isTag(String s) {
        return s.startsWith("2");
    }

    /**
     * just words
     *
     * @return string
     */
    public String justWords() {
        String s = "";
        if (text == null) {
            return s;
        }
        for (int i = 0; i < text.size(); i++) {
            s += " " + text.get(i).trim() + " ";
        }
        return s;
    }

    /**
     * just content
     *
     * @return text
     */
    public playerLyric justContent() {
        playerLyric res = new playerLyric();
        for (int i = 0; i < text.size(); i++) {
            String a = text.get(i).trim();
            if (a.length() < 1) {
                continue;
            }
            res.add(a);
        }
        return res;
    }

    /**
     * line differs
     *
     * @param src source
     * @return differing lines
     */
    public playerLyric linesDiffer(playerLyric src) {
        playerLyric res1 = new playerLyric();
        playerLyric res2 = new playerLyric();
        for (int i = 0; i < src.size(); i++) {
            String b = src.get(i);
            String a = justWords(b);
            if (a.length() < 1) {
                continue;
            }
            res1.add(a);
            res2.add(b);
        }
        for (int i = 0; i < text.size(); i++) {
            int o = res1.findFirst(justWords(text.get(i)));
            if (o < 0) {
                continue;
            }
            res1.del(o);
            res2.del(o);
        }
        return res2.justContent();
    }

    /**
     * clear script lines
     *
     * @return cleared script
     */
    public playerLyric clearScript() {
        playerLyric res = new playerLyric();
        for (int i = 0; i < text.size(); i++) {
            String a = text.get(i);
            String b = a.trim().toLowerCase();
            if (b.length() > 512) {
                continue;
            }
            if (b.indexOf("document.createelement") >= 0) {
                continue;
            }
            if (b.indexOf("document.getelement") >= 0) {
                continue;
            }
            if (b.indexOf("<a href") >= 0) {
                continue;
            }
            if (b.indexOf("can't find lyrics for") >= 0) {
                continue;
            }
            if (b.indexOf("we don't have lyrics for") >= 0) {
                continue;
            }
            if (b.indexOf("no lyrics found") >= 0) {
                continue;
            }
            if (b.indexOf("<title>") >= 0) {
                return new playerLyric();
            }
            res.add(a);
        }
        return res;
    }

}
