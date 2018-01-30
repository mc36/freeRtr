
import java.util.List;


/**
 * convert playlist
 *
 * @author matecsaba
 */
public class pls2cue {

    /**
     * the main
     *
     * @param args arguments
     */
    public static void main(String[] args) {
        if (args.length != 2) {
            playerUtil.put("usage: pls2cue <source.pls> <target.cue>");
            return;
        }
        playerUtil.put("reading " + args[0]);
        playerLyric txt = playerUtil.readup(args[0]);
        playerUtil.put("converting " + txt.size() + " lines");
        List<playerSong> pls = playerSong.txt2pls(null, txt);
        playerUtil.put("sorting " + pls.size() + " songs");
        playerSong.sort(pls);
        playerUtil.put("converting " + pls.size() + " songs");
        txt = playerSong.cue2txt(pls);
        playerUtil.put("writing " + txt.size() + " lines to " + args[1]);
        playerUtil.saveas(txt, args[1]);
    }


}
