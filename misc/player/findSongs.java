
import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * find songs
 *
 * @author matecsaba
 */
public class findSongs {

    /**
     * list of found files
     */
    protected List<playerSong> lst = new ArrayList<playerSong>();

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
        findSongs fs = new findSongs();
        fs.doFind(s);
        s = s.replaceAll("/", "-");
        if (args.length > 1) {
            s = args[1];
        }
        playerUtil.saveas(playerSong.pls2txt(fs.lst), s);
    }

    /**
     * find files
     *
     * @param s directory, without leading /
     */
    protected void doFind(String s) {
        playerUtil.put("processing " + s + "...");
        File[] fl;
        try {
            fl = new File(s).listFiles();
        } catch (Exception e) {
            return;
        }
        if (fl == null) {
            return;
        }
        for (int i = 0; i < fl.length; i++) {
            String a = fl[i].getName();
            if (a.startsWith(".")) {
                continue;
            }
            if (fl[i].isDirectory()) {
                doFind(s + "/" + a);
                continue;
            }
            int o = a.lastIndexOf(".");
            if (o < 0) {
                continue;
            }
            String b = a.substring(o, a.length()).toLowerCase() + ".";
            if (".mp3.flac.".indexOf(b) < 0) {
                continue;
            }
            playerSong sng = new playerSong();
            sng.file = s + "/" + a;
            sng.title = a.substring(0, o);
            lst.add(sng);
        }
    }

}
