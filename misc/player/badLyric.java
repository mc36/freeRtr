
import java.io.File;
import java.util.List;

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
        List<File> fl = findSongs.doFindDir(s);
        for (int i = 0; i < fl.size(); i++) {
            File f = fl.get(i);
            String a = f.getName();
            if (a.startsWith(".")) {
                continue;
            }
            if (!f.isDirectory()) {
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
                String b = a.toLowerCase();
                b = b.replaceAll("-", " ");
                b = b.trim();
                if (b.matches("cd\s*[0-9].*")) {
                    moveFiles(s + "/" + a + "/", s + "/");
                }
                if (b.matches("dvd\s*[0-9].*")) {
                    moveFiles(s + "/" + a + "/", s + "/");
                }
                if (b.matches("disc\s*[0-9].*")) {
                    moveFiles(s + "/" + a + "/", s + "/");
                }
                if (b.matches("bonus.*")) {
                    moveFiles(s + "/" + a + "/", s + "/");
                }
                if (b.matches("scan.*")) {
                    moveFiles(s + "/" + a + "/", s + "/");
                }
                if (b.matches("cover.*")) {
                    moveFiles(s + "/" + a + "/", s + "/");
                }
                delFile(s + "/" + a, true);
                continue;
            }
            int o = a.lastIndexOf(".");
            if (o < 0) {
                continue;
            }
            String b = a.substring(o, a.length()).toLowerCase() + ".";
            if (".cue.ape.flac.m4a.".indexOf(b) >= 0) {
                delFile(s + "/" + a, false);
                continue;
            }
            if (".mp4.avi.mov.mpg.flv.viv.mkv.vob.wmv.ini.torrent.url.pdf.rtf.doc.docx.mobi.epub.ppt.pptx.html.htm.mht.exe.db.iso.log.md5.ffp.txt.xml.m3u.m3u8.pls.log.jpg.jpeg.png.gif.bmp.jfif.tif.nfo.sfv.tab.ffp.ncd.nra.accurip.!ut.dctmp.tags.".indexOf(b) >= 0) {
                delFile(s + "/" + a, true);
                continue;
            }
            if (!b.equals(".lyric.")) {
                if (findSongs.audioExt.indexOf(b) < 0) {
                    delFile(s + "/" + a, false);
                }
                q++;
                continue;
            }
            playerLyric res = playerUtil.readup(s + "/" + a);
            if (res == null) {
                delFile(s + "/" + a, true);
                continue;
            }
            int p = res.countScript();
            res = res.clearScript();
            if (res.countScript() != p) {
                if (res.countScript() < 1) {
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

    private static void moveFiles(String s, String t) {
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
            File f = new File(t + a);
            if (f.exists()) {
                for (int o = 1; o < 10; o++) {
                    f = new File(t + o + a);
                    if (!f.exists()) {
                        break;
                    }
                }
                if (f.exists()) {
                    playerUtil.put("wrn " + s + a);
                    continue;
                }
            }
            playerUtil.put("mov " + s + a);
            fl[i].renameTo(f);
        }
    }

}
