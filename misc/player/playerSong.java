
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * song
 *
 * @author matecsaba
 */
public class playerSong implements Comparator<playerSong> {

    /**
     * file name
     */
    public String file;

    /**
     * artist - title
     */
    public String title;

    public String toString() {
        return file + "|" + title;
    }

    /**
     * just artist
     *
     * @return path
     */
    public String justArtist() {
        int i = title.indexOf(" - ");
        if (i < 0) {
            return "";
        }
        return title.substring(0, i).trim();
    }

    /**
     * just artist
     *
     * @return path
     */
    public String justTitle() {
        int i = title.indexOf(" - ");
        if (i < 0) {
            return "";
        }
        return title.substring(i + 2, title.length()).trim();
    }

    /**
     * just path
     *
     * @return path
     */
    public String justPath() {
        String a = file;
        int i = a.lastIndexOf("/");
        if (i > 0) {
            a = a.substring(0, i);
        }
        return a;
    }

    /**
     * lyric file
     *
     * @return file
     */
    public String lyrFile() {
        int i = file.lastIndexOf(".");
        String s;
        if (i < 0) {
            s = file;
        } else {
            s = file.substring(0, i);
        }
        return s + ".lyric";
    }

    /**
     * playlist entry
     *
     * @param num number
     * @return text
     */
    public playerLyric plsEntry(int num) {
        playerLyric res = new playerLyric();
        res.add("File" + num + "=" + file);
        res.add("Title" + num + "=" + title);
        return res;
    }

    /**
     * playlist entry
     *
     * @return text
     */
    public playerLyric m3uEntry() {
        playerLyric res = new playerLyric();
        res.add("");
        res.add("#EXTINF:0," + title);
        res.add(file);
        return res;
    }

    /**
     * playlist entry
     *
     * @param num number
     * @return text
     */
    public playerLyric cueEntry(int num) {
        playerLyric res = new playerLyric();
        res.add("FILE \"" + file + "\"");
        res.add("  TRACK " + num + " AUDIO");
        res.add("    TITLE \"" + justTitle() + "\"");
        res.add("    PERFORMER \"" + justArtist() + "\"");
        res.add("    INDEX 01 00:00:00");
        return res;
    }

    public int compare(playerSong o1, playerSong o2) {
        return o1.file.compareTo(o2.file);
    }

    /**
     * read up pls
     *
     * @param res source
     * @param src text
     * @return result
     */
    public static List<playerSong> txt2pls(List<playerSong> res, playerLyric src) {
        if (res == null) {
            res = new ArrayList<playerSong>();
        }
        playerSong cur = new playerSong();
        for (int i = 0; i < src.size(); i++) {
            String a = src.get(i);
            int o = a.indexOf("=");
            if (o < 0) {
                continue;
            }
            String b = a.substring(o + 1, a.length());
            a = a.substring(0, o).toLowerCase();
            if (a.startsWith("file")) {
                cur.file = b;
                continue;
            }
            if (a.startsWith("title")) {
                cur.title = b;
                res.add(cur);
                cur = new playerSong();
                continue;
            }
        }
        return res;
    }

    /**
     * read up m3u
     *
     * @param res source
     * @param src text
     * @return result
     */
    public static List<playerSong> txt2m3u(List<playerSong> res, playerLyric src) {
        if (res == null) {
            res = new ArrayList<playerSong>();
        }
        for (int i = 0; i < src.size(); i++) {
            String a = src.get(i);
            if (!a.startsWith("#EXTINF:")) {
                continue;
            }
            int o = a.indexOf(",");
            if (o < 0) {
                continue;
            }
            a = a.substring(o + 1, a.length());
            playerSong cur = new playerSong();
            cur.title = a;
            cur.file = src.get(i + 1);
            res.add(cur);
        }
        return res;
    }

    /**
     * write down pls
     *
     * @param src songs
     * @return text
     */
    public static playerLyric pls2txt(List<playerSong> src) {
        playerLyric res = new playerLyric();
        res.add("[playlist]");
        res.add("NumberOfEntries=" + src.size());
        for (int i = 0; i < src.size(); i++) {
            res.addAll(src.get(i).plsEntry(i + 1));
        }
        return res;
    }

    /**
     * write down m3u
     *
     * @param src songs
     * @return text
     */
    public static playerLyric m3u2txt(List<playerSong> src) {
        playerLyric res = new playerLyric();
        res.add("#EXTM3U");
        for (int i = 0; i < src.size(); i++) {
            res.addAll(src.get(i).m3uEntry());
        }
        return res;
    }

    /**
     * write down cue
     *
     * @param src songs
     * @return text
     */
    public static playerLyric cue2txt(List<playerSong> src) {
        playerLyric res = new playerLyric();
        for (int i = 0; i < src.size(); i++) {
            res.addAll(src.get(i).cueEntry(i + 1));
        }
        return res;
    }

    /**
     * sort playlist
     *
     * @param lst songs
     */
    public static void sort(List<playerSong> lst) {
        Collections.sort(lst, new playerSong());
    }

    /**
     * find entry
     *
     * @param trg where to find
     * @param ntry entry to find
     * @return index, -1 on error
     */
    public static int find(List<playerSong> trg, playerSong ntry) {
        for (int i = 0; i < trg.size(); i++) {
            if (ntry.compare(ntry, trg.get(i)) != 0) {
                continue;
            }
            return i;
        }
        return -1;
    }

}
