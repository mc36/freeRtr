
import java.util.List;

/**
 * convert playlist
 *
 * @author matecsaba
 */
public class m3u2pls {

    /**
     * the main
     *
     * @param args arguments
     */
    public static void main(String[] args) {
        if (args.length != 2) {
            playerUtil.put("usage: m3u2pls <source.m3u> <target.pls>");
            return;
        }
        playerUtil.put("reading " + args[0]);
        playerLyric txt = playerUtil.readup(args[0]);
        playerUtil.put("converting " + txt.size() + " lines");
        List<playerSong> pls = playerSong.txt2m3u(null, txt);
        playerUtil.put("sorting " + pls.size() + " songs");
        playerSong.sort(pls);
        playerUtil.put("converting " + pls.size() + " songs");
        txt = playerSong.pls2txt(pls);
        playerUtil.put("writing " + txt.size() + " lines to " + args[1]);
        playerUtil.saveas(txt, args[1]);
    }

}
