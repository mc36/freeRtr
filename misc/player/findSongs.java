
import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
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
     * artist
     */
    protected String art;

    /**
     * album
     */
    protected String alb;

    /**
     * date
     */
    protected String dat;

    /**
     * title
     */
    protected String tit;

    /**
     * extensions
     */
    public final static String audioExt = ".mp3.m4a.opus.ogg.flac.mpc.wav.wma.ape.ra.au.";

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
        playerUtil.put("scanning " + s);
        findSongs fs = new findSongs();
        fs.doFindSongs(s);
        playerUtil.put("sorting");
        fs.doSort();
        if (args.length > 2) {
            playerUtil.put("reading");
            findSongs old = new findSongs();
            old.lst = playerSong.txt2pls(null, playerUtil.readup(args[2]));
            playerUtil.put("sorting");
            old.doSort();
            findSongs frs = new findSongs();
            playerUtil.put("diffing");
            frs.doMerge(fs.lst, old.lst);
            playerUtil.put(frs.lst.size() + " new");
            frs.doTitle();
            if (args.length > 3) {
                String a = args[3];
                playerUtil.put("writing " + a);
                playerUtil.saveas(playerSong.pls2txt(frs.lst), a);
            }
        }
        s = s.replaceAll("/", "-");
        if (args.length > 1) {
            s = args[1];
        }
        playerUtil.put("writing " + s);
        fs.doSort();
        playerUtil.put(fs.lst.size() + " total");
        playerUtil.saveas(playerSong.pls2txt(fs.lst), s);
    }

    /**
     * sort files
     */
    protected void doSort() {
        playerSong.sort(lst);
    }

    /**
     * find files
     *
     * @param s directory, without leading /
     * @return list of files
     */
    protected final static List<File> doFindDir(String s) {
        List<File> fa = new ArrayList<File>();
        playerUtil.put("processing " + s + "...");
        File[] fl;
        try {
            fl = new File(s).listFiles();
        } catch (Exception e) {
            return fa;
        }
        if (fl == null) {
            return fa;
        }
        for (int i = 0; i < fl.length; i++) {
            fa.add(fl[i]);
        }
        Collections.sort(fa);
        return fa;
    }

    /**
     * find files
     *
     * @param s directory, without leading /
     */
    protected void doFindSongs(String s) {
        List<File> fa = doFindDir(s);
        for (int i = 0; i < fa.size(); i++) {
            File f = fa.get(i);
            String a = f.getName();
            if (a.startsWith(".")) {
                continue;
            }
            if (f.isDirectory()) {
                doFindSongs(s + "/" + a);
                continue;
            }
            int o = a.lastIndexOf(".");
            if (o < 0) {
                continue;
            }
            String b = a.substring(o, a.length()).toLowerCase() + ".";
            if (audioExt.indexOf(b) < 0) {
                continue;
            }
            playerSong sng = new playerSong();
            sng.file = s + "/" + a;
            sng.title = a.substring(0, o);
            lst.add(sng);
        }
    }

    /**
     * merge from old
     *
     * @param src source list
     * @param old old list
     */
    protected void doMerge(List<playerSong> src, List<playerSong> old) {
        for (int i = 0; i < src.size(); i++) {
            playerSong ntry = src.get(i);
            int o = playerSong.find(old, ntry);
            if (o < 0) {
                lst.add(ntry);
                continue;
            }
            playerSong res = old.get(o);
            ntry.title = res.title;
        }
    }

    /**
     * fetch titles
     */
    protected void doTitle() {
        for (int i = 0; i < lst.size(); i++) {
            doTitle(lst.get(i), true);
        }
    }

    /**
     * fetch titles
     */
    protected void doDir() {
        for (int i = 0; i < lst.size(); i++) {
            doTitle(lst.get(i), false);
            if (art == null) {
                continue;
            }
            if (alb == null) {
                continue;
            }
            if (dat == null) {
                continue;
            }
            break;
        }
    }

    /**
     * fetch title
     *
     * @param sng song to update
     * @param cln clean findings
     * @return false on success, true on error;
     */
    protected boolean doTitle(playerSong sng, boolean cln) {
        String cm[] = new String[2];
        cm[0] = "ffprobe";
        cm[1] = sng.file;
        playerUtil.put("--> song " + cm[1] + " <--");
        playerLyric res;
        try {
            Runtime rtm = Runtime.getRuntime();
            Process prc = rtm.exec(cm);
            res = playerUtil.doRead(prc.getErrorStream());
            prc.waitFor();
            prc.destroy();
        } catch (Exception e) {
            playerUtil.put("failed to exec!");
            return true;
        }
        if (res == null) {
            playerUtil.put("failed to fetch!");
            return true;
        }
        if (cln) {
            art = null;
            alb = null;
            dat = null;
            tit = null;
        }
        for (int i = 0; i < res.size(); i++) {
            String a = res.get(i).trim();
            int o = a.indexOf(":");
            if (o < 0) {
                continue;
            }
            String b = a.substring(o + 1, a.length()).trim();
            a = a.substring(0, o).toLowerCase().trim();
            if (a.equals("artist")) {
                art = b;
                continue;
            }
            if (a.equals("album")) {
                alb = b;
                continue;
            }
            if (a.equals("date")) {
                dat = b;
                continue;
            }
            if (a.equals("title")) {
                tit = b;
                continue;
            }
            o = a.indexOf(" ");
            if (o < 0) {
                continue;
            }
            a = a.substring(0, o);
            if (a.equals("stream")) {
                break;
            }
        }
        if (art == null) {
            playerUtil.put("no artist!");
            return true;
        }
        if (tit == null) {
            playerUtil.put("no title!");
            return true;
        }
        sng.title = art + " - " + tit;
        playerUtil.put("got: " + sng.title);
        return false;
    }

}
