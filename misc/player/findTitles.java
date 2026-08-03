
import java.util.List;

/**
 * find songs
 *
 * @author matecsaba
 */
public class findTitles {

    /**
     * the main
     *
     * @param args arguments
     */
    public static void main(String[] args) {
        playerUtil.put("reading " + args[0]);
        playerLyric txt = playerUtil.readup(args[0]);
        playerUtil.put("converting " + txt.size() + " lines");
        findSongs fs = new findSongs();
        fs.lst = playerSong.txt2pls(null, txt);
        playerUtil.put("sorting " + fs.lst.size() + " songs");
        playerSong.sort(fs.lst);
        fs.doTitle(true);
        playerUtil.put("converting " + fs.lst.size() + " songs");
        txt = playerSong.pls2txt(fs.lst);
        playerUtil.put("writing " + txt.size() + " lines to " + args[0]);
        playerUtil.saveas(txt, args[0]);
    }

}
