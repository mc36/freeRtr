
import java.util.ArrayList;
import java.util.List;

/**
 * filter songs
 *
 * @author matecsaba
 */
public class pathFilter {

    public final static int doFilter(List<playerSong> src, List<playerSong> trg, String flt) {
        playerUtil.put("filtering with " + flt + "...");
        int done = 0;
        for (int i = 0; i < src.size(); i++) {
            playerSong ntry = src.get(i);
            if (!ntry.file.matches(flt)) {
                continue;
            }
            trg.add(ntry);
            done++;
        }
        playerUtil.put(done + " found...");
        return done;
    }

    /**
     * the main
     *
     * @param args arguments
     */
    public static void main(String[] args) {
        String fn = args[0];
        playerUtil.put("reading " + fn + "...");
        List<playerSong> src = playerSong.txt2pls(null, playerUtil.readup(fn));
        playerUtil.put("read " + src.size());
        List<playerSong> trg = new ArrayList<playerSong>();
        doFilter(src, trg, args[2]);
        fn = args[1];
        playerUtil.put("write " + trg.size() + " to " + fn);
        playerUtil.saveas(playerSong.pls2txt(trg), fn);
    }

}
