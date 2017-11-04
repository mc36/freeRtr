
import java.util.ArrayList;
import java.util.List;

/**
 * lyric fetcher
 *
 * @author matecsaba
 */
public class lyrFetch {

    /**
     * the main
     *
     * @param args arguments
     */
    public static void main(String[] args) {
        List<playerSong> need = playerSong.txt2pls(null, playerUtil.readup("need.pls"));
        List<playerSong> done = new ArrayList<playerSong>();
        List<playerSong> fail = new ArrayList<playerSong>();
        List<playerSong> skip = new ArrayList<playerSong>();
        for (int i = 0; i < need.size(); i++) {
            playerSong ntry = need.get(i);
            switch (doSong(ntry)) {
                case 0:
                    done.add(ntry);
                    break;
                case 1:
                    skip.add(ntry);
                    break;
                default:
                    fail.add(ntry);
                    break;
            }
        }
        playerUtil.saveas(playerSong.pls2txt(done), "done.pls");
        playerUtil.saveas(playerSong.pls2txt(fail), "fail.pls");
        playerUtil.saveas(playerSong.pls2txt(skip), "skip.pls");
        playerUtil.put("--> need=" + need.size() + " skip=" + skip.size() + " done=" + done.size() + " fail=" + fail.size());
    }

    /**
     * do one song
     *
     * @param song song to search
     * @return 0 on success
     */
    public static int doSong(playerSong song) {
        playerUtil.put("--> song " + song.file + " <--");
        playerLyric lyric = playerUtil.readup(song.lyrFile());
        if (lyric != null) {
            playerUtil.put("already exists!");
            return 1;
        }
        String cm[] = new String[2];
        cm[0] = "./lyrFetch.sh";
        cm[1] = song.title;
        Process prc;
        try {
            Runtime rtm = Runtime.getRuntime();
            prc = rtm.exec(cm);
            prc.waitFor();
        } catch (Exception e) {
            playerUtil.put("failed to exec!");
            return 2;
        }
        playerLyric res = playerUtil.doRead(prc.getInputStream());
        if (res == null) {
            playerUtil.put("failed to download!");
            return 2;
        }
        res = res.clearScript();
        if (res.size() < 1) {
            playerUtil.put("got no lyrics!");
            return 3;
        }
        playerUtil.saveas(res, song.lyrFile());
        return 0;
    }

}
