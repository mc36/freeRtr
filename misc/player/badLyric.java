
import java.io.File;

/**
 * collect bad lyric files
 *
 * @author matecsaba
 */
public class badLyric {

    /**
     * the main
     *
     * @param args arguments
     */
    public static void main(String[] args) {
        String s = "./";
        if (args.length > 0) {
            s = args[0];
        }
        playerUtil.put("processing " + s + "...");
        File[] fl = new File(s).listFiles();
        if (fl == null) {
            return;
        }
        for (int i = 0; i < fl.length; i++) {
            String a = fl[i].getName();
            if (a.startsWith(".")) {
                continue;
            }
            if (!fl[i].isDirectory()) {
                continue;
            }
            doDir(s + a);
        }
    }

    private static void doDir(String s) {
        File[] fl = new File(s).listFiles();
        int q = 0;
        for (int i = 0; i < fl.length; i++) {
            String a = fl[i].getName();
            if (a.equals(".")) {
                continue;
            }
            if (a.equals("..")) {
                continue;
            }
            if (fl[i].isDirectory()) {
                delFile(s + "/" + a, true);
                continue;
            }
            int o = a.lastIndexOf(".");
            if (o < 0) {
                continue;
            }
            String b = a.substring(o, a.length()).toLowerCase() + ".";
            if (".cue.".indexOf(b) >= 0) {
                delFile(s + "/" + a, false);
                continue;
            }
            if (".mp4.avi.mov.flv.mkv.ini.torrent.url.pdf.rtf.doc.docx.ppt.pptx.exe.db.iso.log.txt.m3u.m3u8.pls.log.jpg.jpeg.png.gif.tif.nfo.sfv.tags.".indexOf(b) >= 0) {
                delFile(s + "/" + a, true);
                continue;
            }
            if (!b.equals(".lyric.")) {
                q++;
                continue;
            }
            playerLyric res = playerUtil.readup(s + "/" + a);
            if (res == null) {
                delFile(s + "/" + a, true);
                continue;
            }
            int p = res.justWords().length();
            res = res.clearScript();
            b = res.justWords();
            if (b.length() != p) {
                if (b.trim().length() < 1) {
                    delFile(s + "/" + a, true);
                    continue;
                }
                playerUtil.put("upd " + s + "/" + a);
                playerUtil.saveas(res, s + "/" + a);
            }
            b = a.substring(0, o + 1);
            p = -1;
            for (o = 0; o < fl.length; o++) {
                if (o == i) {
                    continue;
                }
                if (!fl[o].getName().startsWith(b)) {
                    continue;
                }
                p = o;
                break;
            }
            if (p >= 0) {
                continue;
            }
            delFile(s + "/" + a, true);
        }
        if (q > 0) {
            return;
        }
        delFile(s, true);
    }

    private static void delFile(String s, boolean d) {
        playerUtil.put((d ? "del" : "wrn") + " " + s);
        if (!d) {
            return;
        }
        new File(s).delete();
    }

}
