
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
        fs.doFind(s);
        fs.doSort();
        if (args.length > 2) {
            playerUtil.put("diffing");
            List<playerSong> old = playerSong.txt2pls(null, playerUtil.readup(args[2]));
            fs.doMerge(old);
            findSongs frs = new findSongs();
            frs.lst.addAll(fs.lst);
            frs.lst.removeAll(old);
            frs.doTitle();
        }
        s = s.replaceAll("/", "-");
        if (args.length > 1) {
            s = args[1];
        }
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
            if (".mp3.m4a.flac.wav.wma.".indexOf(b) < 0) {
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
     * @param src old list
     */
    protected void doMerge(List<playerSong> src) {
        for (int i = 0; i < src.size(); i++) {
            playerSong ntry = src.get(i);
            int o = playerSong.find(lst, ntry);
            if (o < 0) {
                continue;
            }
            lst.remove(o);
            lst.add(ntry);
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
        Process prc;
        try {
            Runtime rtm = Runtime.getRuntime();
            prc = rtm.exec(cm);
            prc.waitFor();
        } catch (Exception e) {
            playerUtil.put("failed to exec!");
            return true;
        }
        playerLyric res = playerUtil.doRead(prc.getErrorStream());
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
