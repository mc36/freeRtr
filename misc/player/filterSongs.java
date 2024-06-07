
import java.util.ArrayList;
import java.util.List;

/**
 * filter songs
 *
 * @author matecsaba
 */
public class filterSongs {

    /**
     * the main
     *
     * @param args arguments
     */
    public static void main(String[] args) {
        List<playerSong> src = playerSong.txt2pls(null, playerUtil.readup(args[0]));
        playerUtil.put("read " + src.size());
        List<playerSong> trg = new ArrayList<playerSong>();
        for (int i = 0; i < src.size(); i++) {
            playerSong ntry = src.get(i);
            if (!ntry.file.matches(args[2])) {
                continue;
            }
            trg.add(ntry);
        }
        playerUtil.put("write " + trg.size());
        playerUtil.saveas(playerSong.pls2txt(trg), args[1]);
    }

}
