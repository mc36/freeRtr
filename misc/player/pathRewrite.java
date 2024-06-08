
import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * path rewrite
 *
 * @author matecsaba
 */
public class pathRewrite {

    /**
     * repair one path
     *
     * @param s path to repair
     * @return repaired path
     */
    public final static String doOnePath(String s) {
        if (!s.startsWith("/")) {
            s = "/" + s;
        }
        if (!s.endsWith("/")) {
            s = s + "/";
        }
        return s;
    }

    public final static int doRewrite(List<playerSong> need, String src, String trg) {
        playerUtil.put("rewriting " + src + " to " + trg + "...");
        int done = 0;
        int srcl = src.length();
        for (int i = 0; i < need.size(); i++) {
            playerSong ntry = need.get(i);
            if (ntry == null) {
                continue;
            }
            String a = ntry.file;
            if (!a.startsWith(src)) {
                continue;
            }
            a = a.substring(srcl, a.length());
            ntry.file = trg + a;
            done++;
        }
        playerUtil.put(done + " rewritten...");
        return done;
    }

    /**
     * the main
     *
     * @param args arguments
     */
    public static void main(String[] args) {
        String fn = args[0];
        String src = doOnePath(args[1]);
        String trg = doOnePath(args[2]);
        playerUtil.put("reading " + fn + "...");
        List<playerSong> need = playerSong.txt2pls(null, playerUtil.readup(fn));
        playerUtil.put("sorting " + need.size() + " entries");
        playerSong.sort(need);
        int don = doRewrite(need, src, trg);
        playerUtil.put("writing " + fn + "...");
        playerUtil.saveas(playerSong.pls2txt(need), fn);
    }

}
