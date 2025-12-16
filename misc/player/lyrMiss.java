
import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * missing lyrics
 *
 * @author matecsaba
 */
public class lyrMiss {

    /**
     * the main
     *
     * @param args arguments
     */
    public static void main(String[] args) {
        List<playerSong> need = playerSong.txt2pls(null, playerUtil.readup("need.pls"));
        List<playerSong> miss = new ArrayList<playerSong>();
        playerUtil.put("need=" + need.size());
        for (int i = 0; i < need.size(); i++) {
            playerSong ntry = need.get(i);
            if (new File(ntry.lyrFile()).exists()) {
                continue;
            }
            miss.add(ntry);
        }
        playerUtil.put("miss=" + miss.size());
        playerUtil.saveas(playerSong.pls2txt(miss), "miss.pls");
    }

}
