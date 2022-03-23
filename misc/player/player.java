
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Random;

/**
 * web player
 *
 * @author matecsaba
 */
public class player implements Runnable {

    /**
     * this is needed for cli startup
     *
     * @param args command line parameters
     */
    public static void main(String[] args) {
        player app = new player();
        String a;
        try {
            ByteArrayOutputStream buf = new ByteArrayOutputStream();
            a = "" + app.getClass().getName() + ".";
            a = player.httpRequest("http://localhost/" + a, "./" + a, "cli", "clibrowser", "user", args, buf);
            a = "type=" + a + "\r\ndata:\r\n" + buf.toString();
        } catch (Exception e) {
            a = "exception " + e.getMessage();
        }
        System.out.println(a);
    }

    /**
     * where i'm located in ram
     */
    protected static player staticPlayer = null;

    /**
     * where i'm located on host
     */
    protected String path = null;

    /**
     * where i'm located on net, relative
     */
    protected String urlR = null;

    /**
     * where i'm located on net, full
     */
    protected String urlF = null;

    /**
     * wether i'm initialized or nor
     */
    protected boolean ready = false;

    private final static Object sleeper = new Object();

    private String mixer = "Master";

    private String srate = "44100";

    private boolean headEnd = false;

    private int volMin = 0;

    private int volMax = 100;

    private Random rndSeed = new Random();

    private playerLyric playlists = null;

    private List<playerSong> playlist = new ArrayList<playerSong>();

    private List<playerSong> prelock = null;

    private Process currProc = null;

    private int prevSong = -1;

    private int currSong = -1;

    private List<Integer> nextSong = new ArrayList<Integer>();

    private playerLyric currLyrc = null;

    private long currTime = 0;

    private int currVlme = 0;

    /**
     * do one request
     *
     * @param url url of app
     * @param path path of app
     * @param peer client address
     * @param agent user agent
     * @param user auth data
     * @param par parameters
     * @param buf result buffer, if empty, pathname must present
     * @return [pathname"][file name.]extension
     * @throws Exception if something went wrong
     */
    public static String httpRequest(String url, String path, String peer, String agent, String user, String[] par, ByteArrayOutputStream buf) throws Exception {
        if (staticPlayer == null) {
            staticPlayer = new player();
            staticPlayer.path = path.substring(0, path.lastIndexOf("."));
            staticPlayer.urlF = url;
            staticPlayer.urlR = new URL(url).getPath();
            staticPlayer.doInit();
            new Thread(staticPlayer).start();
        }
        int i = staticPlayer.doRequest(par, buf);
        if (i == -2) {
            return "txt";
        }
        if (i < 0) {
            buf.write("</body></html>".getBytes());
            return "html";
        }
        playerSong sng = staticPlayer.playlist.get(i);
        String s = new File(sng.file).getName();
        i = s.lastIndexOf(".");
        if (i < 0) {
            return sng.file + "\"" + s;
        }
        if (sng.title.length() < 0) {
            return sng.file + "\"" + s;
        }
        return sng.file + "\"" + sng.title + s.substring(i, s.length());
    }

    private void runProc(String cmd[]) {
        try {
            Runtime rtm = Runtime.getRuntime();
            Process prc = rtm.exec(cmd);
            prc.waitFor();
        } catch (Exception e) {
        }
    }

    private synchronized void stopProc(String s) {
        String[] cmd = new String[3];
        cmd[0] = "killall";
        cmd[1] = "-9";
        cmd[2] = s;
        runProc(cmd);
    }

    private synchronized void stopProc(long p) {
        String[] cmd = new String[3];
        cmd[0] = "kill";
        cmd[1] = "-9";
        cmd[2] = "" + p;
        runProc(cmd);
    }

    private synchronized void replaceCurrProc(String[] cmd) {
        if (currProc != null) {
            long p = currProc.pid();
            try {
                currProc.destroy();
            } catch (Exception e) {
            }
            stopProc(p);
        }
        currProc = null;
        if (cmd == null) {
            return;
        }
        try {
            Runtime rtm = Runtime.getRuntime();
            currProc = rtm.exec(cmd);
        } catch (Exception e) {
        }
    }

    private synchronized void stopFull() {
        stopProc("gmediarender");
        stopProc("mplayer");
        stopProc("ffmpeg");
        stopProc("ffplay");
        stopProc("cvlc");
        stopProc("vlc");
        stopProc("youtube-dl");
        stopProc("amixer");
    }

    private synchronized void setVolume(int vol) {
        currVlme = vol;
        int fvol = volMin + ((vol * (volMax - volMin)) / 100);
        String[] cmd = new String[4];
        cmd[0] = "amixer";
        cmd[1] = "sset";
        cmd[2] = mixer;
        cmd[3] = fvol + "%";
        runProc(cmd);
    }

    private synchronized void startPlayDlna() {
        currSong = 0;
        currTime = new Date().getTime();
        currLyrc = new playerLyric();
        currLyrc.add("dlna receiver");
        String[] cmd = new String[5];
        cmd[0] = "gmediarender";
        cmd[1] = "--gstout-audiosink=alsasink";
        cmd[2] = "--gstout-videosink=appsink";
        cmd[3] = "--friendly-name=" + urlF;
        cmd[4] = "--uuid=00001234-1234-1234-" + rndSeed.nextInt();
        replaceCurrProc(cmd);
    }

    private synchronized void startPlayRcvr() {
        currSong = 0;
        currTime = new Date().getTime();
        currLyrc = new playerLyric();
        currLyrc.add("multicast receiver");
        String[] cmd = new String[1];
        cmd[0] = path + ".rcvr";
        replaceCurrProc(cmd);
    }

    private synchronized void startPlayUrl(String url) {
        if (url.length() < 1) {
            return;
        }
        String[] cmd = new String[3];
        cmd[0] = "sh";
        cmd[1] = "-c";
        cmd[2] = "rm -f /tmp/player.*";
        runProc(cmd);
        String ply;
        if (headEnd) {
            ply = path + ".strm {} 0";
        } else {
            ply = "mplayer -ao alsa -vo none -srate " + srate + " {}";
        }
        cmd = new String[7];
        cmd[0] = "youtube-dl";
        cmd[1] = "-x";
        cmd[2] = "--output";
        cmd[3] = "/tmp/player.url";
        cmd[4] = "--exec";
        cmd[5] = ply;
        cmd[6] = "" + url;
        currSong = 0;
        currTime = new Date().getTime();
        currLyrc = new playerLyric();
        currLyrc.add("downloading " + url);
        replaceCurrProc(cmd);
    }

    private synchronized void startPlayNormal(int ntry, String ss) {
        if ((currSong >= 0) && (prevSong != currSong)) {
            prevSong = currSong;
        }
        for (;;) {
            if (!nextSong.remove(Integer.valueOf(ntry))) {
                break;
            }
        }
        for (;;) {
            if (nextSong.size() > 0) {
                break;
            }
            nextSong.add(rndSeed.nextInt(playlist.size()));
        }
        if (ntry >= playlist.size()) {
            return;
        }
        currSong = ntry;
        currLyrc = null;
        currTime = 0;
        replaceCurrProc(null);
        if (headEnd) {
            stopFull();
        }
        if (ntry < 0) {
            return;
        }
        String[] cmd;
        if (headEnd) {
            cmd = new String[3];
            cmd[0] = path + ".strm";
            cmd[1] = playlist.get(ntry).file;
            cmd[2] = ss;
        } else {
            cmd = new String[10];
            cmd[0] = "mplayer";
            cmd[1] = "-ao";
            cmd[2] = "alsa";
            cmd[3] = "-vo";
            cmd[4] = "none";
            cmd[5] = "-srate";
            cmd[6] = srate;
            cmd[7] = "-ss";
            cmd[8] = ss;
            cmd[9] = playlist.get(ntry).file;
        }
        replaceCurrProc(cmd);
        currTime = new Date().getTime() - (playerUtil.str2int(ss) * 1000);
        try {
            playerSong sng = playlist.get(currSong);
            currLyrc = playerUtil.readup(sng.lyrFile());
        } catch (Exception e) {
        }
    }

    /**
     * initialize
     */
    public void doInit() {
        playlists = playerUtil.readup("/etc/asound.conf");
        int volDef = 50;
        int autoPlay = 0;
        if (playlists != null) {
            for (int i = 0; i < playlists.size(); i++) {
                String a = playlists.get(i);
                int o = a.indexOf("=");
                if (o < 0) {
                    continue;
                }
                String b = a.substring(o + 1, a.length()).trim();
                a = a.substring(0, o).trim();
                if (a.equals("#player.class mixer")) {
                    mixer = b;
                    continue;
                }
                if (a.equals("#player.class headend")) {
                    headEnd = true;
                    continue;
                }
                if (a.equals("#player.class autoplay")) {
                    autoPlay = playerUtil.str2int(b);
                    continue;
                }
                if (a.equals("#player.class srate")) {
                    srate = b;
                    continue;
                }
                if (a.equals("#player.class volmin")) {
                    volMin = playerUtil.str2int(b);
                    continue;
                }
                if (a.equals("#player.class volmax")) {
                    volMax = playerUtil.str2int(b);
                    continue;
                }
                if (a.equals("#player.class voldef")) {
                    volDef = playerUtil.str2int(b);
                    continue;
                }
            }
        }
        playlists = playerUtil.readup(path + ".cfg");
        playlist = playerSong.txt2pls(null, playerUtil.readup(playlists.get(0)));
        prelock = playlist;
        stopFull();
        setVolume(volDef);
        startPlayNormal(-1, "0");
        switch (autoPlay) {
            case 0:
                break;
            case 1:
                startPlayNormal(0, "0");
                break;
            case 2:
                startPlayDlna();
                break;
            case 3:
                startPlayRcvr();
                break;
        }
        ready = true;
    }

    public void run() {
        for (;;) {
            try {
                synchronized (sleeper) {
                    sleeper.wait(1000);
                }
            } catch (Exception ex) {
            }
            try {
                if (currProc != null) {
                    flushStream(currProc.getErrorStream());
                    flushStream(currProc.getInputStream());
                    currProc.exitValue();
                }
            } catch (Exception e) {
                continue;
            }
            if (currSong < 0) {
                continue;
            }
            startPlayNormal(nextSong.get(0), "0");
        }
    }

    /**
     * put starting
     *
     * @param buf buffer to use
     * @param refresh refresh interval
     * @throws Exception on error
     */
    public void putStart(ByteArrayOutputStream buf, int refresh) throws Exception {
        String a = "<!DOCTYPE html><html lang=\"en\"><head><title>music player</title><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><link rel=\"stylesheet\" type=\"text/css\" href=\"index.css\" />";
        buf.write(a.getBytes());
        if (refresh > 0) {
            a = "<meta http-equiv=refresh content=\"" + refresh + ";url=" + urlR + "\">";
            buf.write(a.getBytes());
        }
        a = "</head><body>";
        buf.write(a.getBytes());
    }

    /**
     * put one link
     *
     * @param buf buffer to use
     * @param url url to link to
     * @param txt text to use
     * @throws Exception on error
     */
    public void putLink(ByteArrayOutputStream buf, String url, String txt) throws Exception {
        String a = "((<a href=\"" + url + "\">" + txt + "</a>))";
        buf.write(a.getBytes());
    }

    /**
     * put menu items
     *
     * @param buf buffer to use
     * @return currently playing song
     * @throws Exception on error
     */
    public playerSong putMenu(ByteArrayOutputStream buf) throws Exception {
        putLink(buf, urlR + "?cmd=list&song=", "playlist");
        putLink(buf, urlR + "?cmd=song&song=", "find");
        putLink(buf, urlR, "fresh");
        putLink(buf, urlR + "?cmd=play&song=-1", "stop");
        putLink(buf, urlR + "?cmd=play&song=" + prevSong, "prev");
        putLink(buf, urlR + "?cmd=play&song=" + nextSong.get(0), "next");
        putLink(buf, urlR + "?cmd=queue&song=", "queue");
        buf.write("<br/>".getBytes());
        if (currSong >= playlist.size()) {
            currSong = -1;
        }
        if (currSong < 0) {
            return null;
        }
        playerSong sng = playlist.get(currSong);
        String s = "file: " + sng.file + "<br/>";
        buf.write(s.getBytes());
        s = "song: " + sng.title + "<br/>";
        buf.write(s.getBytes());
        putLink(buf, urlR + "?cmd=play&song=" + currSong, "replay");
        putLink(buf, urlR + "?cmd=artistsong&song=" + currSong, "artist");
        putLink(buf, urlR + "?cmd=albumsong&song=" + currSong, "album");
        putLink(buf, urlR + "?cmd=download&song=" + currSong, "download");
        putLink(buf, urlR + "?cmd=vol", "volume");
        putLink(buf, urlR + "?cmd=seek", "seek");
        buf.write("<br/>".getBytes());
        return sng;
    }

    /**
     * put find bar
     *
     * @param buf buffer to use
     * @param flt filter text
     * @throws Exception on error
     */
    public void putFind(ByteArrayOutputStream buf, String flt) throws Exception {
        String s = "<form action=\"" + urlR + "\" method=get><input type=text name=song value=\"" + flt + "\">";
        buf.write(s.getBytes());
        buf.write("<input type=submit name=cmd value=\"song\">".getBytes());
        buf.write("<input type=submit name=cmd value=\"album\">".getBytes());
        buf.write("<input type=submit name=cmd value=\"lock\">".getBytes());
        buf.write("<input type=submit name=cmd value=\"albums\">".getBytes());
        buf.write("</form>".getBytes());
    }

    /**
     * find songs
     *
     * @param buf buffer to use
     * @param sng song to find
     * @param res results found
     * @throws Exception on error
     */
    public void doFind(ByteArrayOutputStream buf, String sng, List<String> res) throws Exception {
        final int max = 100;
        putStart(buf, -1);
        putMenu(buf);
        putFind(buf, sng);
        String a = res.size() + " results:<br/>";
        buf.write(a.getBytes());
        if (res.size() < max) {
            for (int i = 0; i < res.size(); i++) {
                buf.write(res.get(i).getBytes());
            }
        } else {
            for (int i = 0; i < max; i++) {
                buf.write(res.get(rndSeed.nextInt(res.size())).getBytes());
            }
            buf.write(("and " + (res.size() - max) + " more!<br/>").getBytes());
        }
        putFind(buf, sng);
        putMenu(buf);
    }

    /**
     * to found line
     *
     * @param num number of entry
     * @param ntry entry value
     * @return string to add
     */
    public String toFound1(int num, playerSong ntry) {
        return "((<a href=\"" + urlR + "?cmd=enqueue&song=" + num + "\">Q</a>))<a href=\"" + urlR + "?cmd=play&song=" + num + "\">" + ntry.title + "</a><br/>";
    }

    /**
     * to found line
     *
     * @param ntry entry value
     * @return string to add
     */
    public String toFound2(playerSong ntry) {
        String a = ntry.justPath();
        return "<a href=\"" + urlR + "?cmd=album&song=" + a + "\">" + a + "</a><br/>";
    }

    /**
     * flush input stream
     *
     * @param strm stream to flush
     * @throws Exception on error
     */
    public static void flushStream(InputStream strm) throws Exception {
        byte[] buf = new byte[strm.available()];
        if (buf.length < 1) {
            return;
        }
        strm.read(buf);
    }

    /**
     * do one request
     *
     * @param par parameters
     * @param buf buffer to use
     * @return -1 on html result
     * @throws Exception on error
     */
    public int doRequest(String[] par, ByteArrayOutputStream buf) throws Exception {
        if (!ready) {
            putStart(buf, 5);
            buf.write("player initializes!".getBytes());
            return -1;
        }
        String song = "-1";
        String cmd = "";
        for (int pn = 0; pn < par.length; pn++) {
            String a = par[pn];
            int i = a.indexOf("=");
            if (i < 0) {
                continue;
            }
            String b = a.substring(0, i);
            a = a.substring(i + 1, a.length());
            if (b.equals("cmd")) {
                cmd = a;
            }
            if (b.equals("song")) {
                song = a;
            }
        }
        if (cmd.equals("albumsong")) {
            int i = playerUtil.str2int(song);
            playerSong sng = playlist.get(i);
            song = new File(sng.file).getParent();
            cmd = "album";
        }
        if (cmd.equals("artistsong")) {
            int i = playerUtil.str2int(song);
            playerSong sng = playlist.get(i);
            song = new File(sng.file).getParent();
            i = song.indexOf(" - ");
            if (i >= 0) {
                song = song.substring(0, i + 2);
            }
            cmd = "album";
        }
        if (cmd.equals("song")) {
            cmd = ".*" + song.trim().toLowerCase().replaceAll(" ", ".*") + ".*";
            List<String> res = new ArrayList<String>();
            for (int i = 0; i < playlist.size(); i++) {
                playerSong ntry = playlist.get(i);
                if (!ntry.title.toLowerCase().matches(cmd)) {
                    continue;
                }
                res.add(toFound1(i, ntry));
            }
            doFind(buf, song, res);
            return -1;
        }
        if (cmd.equals("albums")) {
            cmd = ".*" + song.trim().toLowerCase().replaceAll(" ", ".*") + ".*";
            List<String> res = new ArrayList<String>();
            String old = "<null>";
            for (int i = 0; i < playlist.size(); i++) {
                playerSong ntry = playlist.get(i);
                if (!ntry.file.toLowerCase().matches(cmd)) {
                    continue;
                }
                if (!ntry.justPath().toLowerCase().matches(cmd)) {
                    continue;
                }
                String a = toFound2(ntry);
                if (a.equals(old)) {
                    continue;
                }
                res.add(a);
                old = a;
            }
            doFind(buf, song, res);
            return -1;
        }
        if (cmd.equals("album")) {
            cmd = ".*" + song.trim().toLowerCase().replaceAll(" ", ".*") + ".*";
            List<String> res = new ArrayList<String>();
            for (int i = 0; i < playlist.size(); i++) {
                playerSong ntry = playlist.get(i);
                if (!ntry.file.toLowerCase().matches(cmd)) {
                    continue;
                }
                res.add(toFound1(i, ntry));
            }
            doFind(buf, song, res);
            return -1;
        }
        if (cmd.equals("lock")) {
            putStart(buf, -1);
            putMenu(buf);
            cmd = ".*" + song.trim().toLowerCase().replaceAll(" ", ".*") + ".*";
            List<playerSong> res = new ArrayList<playerSong>();
            for (int i = 0; i < playlist.size(); i++) {
                playerSong ntry = playlist.get(i);
                if (!ntry.file.toLowerCase().matches(cmd)) {
                    continue;
                }
                res.add(ntry);
            }
            if (res.size() < 1) {
                buf.write("nothing selected!<br/>".getBytes());
                return -1;
            }
            playlist = res;
            buf.write("lockin successfully finished!<br/>".getBytes());
            return -1;
        }
        if (cmd.equals("reload")) {
            putStart(buf, 5);
            putMenu(buf);
            buf.write("as requested, rebooting for you".getBytes());
            staticPlayer = null;
            startPlayNormal(-1, "0");
            return -1;
        }
        if (cmd.equals("seek")) {
            putStart(buf, -1);
            putMenu(buf);
            int i = playerUtil.str2int(song);
            if (i > 0) {
                String a = "seeked to " + song + " seconds.<br/>";
                buf.write(a.getBytes());
                startPlayNormal(currSong, song);
            }
            int tim = (int) ((new Date().getTime() - currTime) / 1000);
            buf.write("<br/>seek:".getBytes());
            for (i = 0; i < 30; i++) {
                int o = (i + 1) * 10;
                String a = "" + o;
                if (i == (tim / 10)) {
                    a = "*" + a + "*";
                }
                putLink(buf, urlR + "?cmd=seek&song=" + o, a);
            }
            buf.write("<br/><br/>seek:".getBytes());
            for (i = -15; i < 15; i++) {
                int o = tim + i;
                if (o < 0) {
                    continue;
                }
                String a = "" + o;
                if (o == tim) {
                    a = "*" + a + "*";
                }
                putLink(buf, urlR + "?cmd=seek&song=" + o, a);
            }
            return -1;
        }
        if (cmd.equals("fav")) {
            putStart(buf, 5);
            playerSong sng = putMenu(buf);
            if (sng == null) {
                return -1;
            }
            String s = playlists.get(0);
            playerUtil.append(s, "File1=" + sng.file);
            playerUtil.append(s, "Title1=" + sng.title);
            buf.write("saved to favorites<br/>".getBytes());
            return -1;
        }
        if (cmd.equals("vol")) {
            putStart(buf, -1);
            putMenu(buf);
            int i = playerUtil.str2int(song);
            if (i >= 0) {
                setVolume(i);
                String a = "volume set to " + currVlme + " percent.<br/>";
                buf.write(a.getBytes());
            }
            buf.write("<br/>volume:".getBytes());
            for (i = 0; i < 11; i++) {
                int o = i * 10;
                String a = "" + o;
                if ((o / 10) == (currVlme / 10)) {
                    a = "*" + a + "*";
                }
                putLink(buf, urlR + "?cmd=vol&song=" + o, a);
            }
            buf.write("<br/><br/>volume:".getBytes());
            for (i = -15; i < 15; i++) {
                int o = currVlme + i;
                if (o < 0) {
                    continue;
                }
                if (o > 100) {
                    continue;
                }
                String a = "" + o;
                if (o == currVlme) {
                    a = "*" + a + "*";
                }
                putLink(buf, urlR + "?cmd=vol&song=" + o, a);
            }
            return -1;
        }
        if (cmd.equals("unlock")) {
            putStart(buf, 5);
            putMenu(buf);
            String a = "<br/>unlock successful.<br/>";
            buf.write(a.getBytes());
            playlist = prelock;
            return -1;
        }
        if (cmd.equals("resync")) {
            putStart(buf, 5);
            putMenu(buf);
            String a = "<br/>all active players synchronized.<br/>";
            buf.write(a.getBytes());
            rndSeed = new Random(playerUtil.str2int(song));
            nextSong.clear();
            nextSong.add(rndSeed.nextInt(playlist.size()));
            return -1;
        }
        if (cmd.equals("queue")) {
            putStart(buf, -1);
            putMenu(buf);
            buf.write("queued songs:<br/>".getBytes());
            for (int i = 0; i < nextSong.size(); i++) {
                int num = nextSong.get(i);
                playerSong ntry = playlist.get(num);
                String a = "((<a href=\"" + urlR + "?cmd=dequeue&song=" + num + "\">R</a>))" + ntry.title + "<br/>";
                buf.write(a.getBytes());
            }
            return -1;
        }
        if (cmd.equals("dequeue")) {
            int i = playerUtil.str2int(song);
            if (nextSong.size() > 1) {
                nextSong.remove(Integer.valueOf(i));
            }
            String a = "dequeued song #" + i + "<br/>";
            putStart(buf, 3);
            putMenu(buf);
            buf.write(a.getBytes());
            return -1;
        }
        if (cmd.equals("enqueue")) {
            int i = playerUtil.str2int(song);
            nextSong.add(i);
            String a = "queued song #" + i + "<br/>";
            putStart(buf, 3);
            putMenu(buf);
            buf.write(a.getBytes());
            return -1;
        }
        if (cmd.equals("pendrive")) {
            putStart(buf, -1);
            putMenu(buf);
            findSongs fnd = new findSongs();
            fnd.doFind("/media");
            fnd.doFind("/mnt");
            fnd.doSort();
            if (fnd.lst.size() < 1) {
                buf.write("nothing selected!<br/>".getBytes());
                return -1;
            }
            playlist = fnd.lst;
            prelock = playlist;
            String a = "media scanned, " + playlist.size() + " songs found.<br/><br/>";
            buf.write(a.getBytes());
            return -1;
        }
        if (cmd.equals("list")) {
            putStart(buf, -1);
            putMenu(buf);
            int i = playerUtil.str2int(song);
            if (i > 0) {
                List<playerSong> res = playerSong.txt2pls(null, playerUtil.readup(playlists.get(i - 1)));
                if (res.size() < 1) {
                    buf.write("nothing selected!<br/>".getBytes());
                    return -1;
                }
                playlist = res;
                prelock = playlist;
                String a = "playlist #" + song + " selected with " + playlist.size() + " songs.<br/><br/>";
                buf.write(a.getBytes());
            }
            for (i = 0; i < playlists.size(); i++) {
                String a = "<a href=\"" + urlR + "?cmd=list&song=" + (i + 1) + "\">" + playlists.get(i) + "</a><br/>";
                buf.write(a.getBytes());
            }
            String a = "<br/>headend=" + headEnd + ", mixer=" + mixer + ", rate=" + srate + ", songs=" + playlist.size() + ", volmin=" + volMin + ", volmax=" + volMax + ", lists=" + playlists.size() + "<br/><br/>";
            buf.write(a.getBytes());
            a = "<a href=\"" + urlR + "?cmd=fullstop\">!full stop!</a><br/>";
            buf.write(a.getBytes());
            a = "<a href=\"" + urlR + "?cmd=receive\">!multicast receiver!</a><br/>";
            buf.write(a.getBytes());
            a = "<a href=\"" + urlR + "?cmd=headend\">!multicast streamer!</a><br/>";
            buf.write(a.getBytes());
            a = "<a href=\"" + urlR + "?cmd=dlna\">!dlna!</a><br/>";
            buf.write(a.getBytes());
            a = "<a href=\"" + urlR + "?cmd=unlock\">!unlock!</a><br/>";
            buf.write(a.getBytes());
            a = "<a href=\"" + urlR + "?cmd=pendrive\">!pendrive!</a><br/>";
            buf.write(a.getBytes());
            a = "<a href=\"" + urlR + "?cmd=resync&song=" + rndSeed.nextInt() + "\">!resync!</a><br/>";
            buf.write(a.getBytes());
            a = "<br/><form action=\"" + urlR + "\" method=get>url:<input type=text name=song value=\"\"><input type=submit name=cmd value=\"url\"></form><br/>";
            buf.write(a.getBytes());
            return -1;
        }
        if (cmd.equals("url")) {
            putStart(buf, 120);
            putMenu(buf);
            String a = "<br/>downloading song, please wait.<br/>";
            buf.write(a.getBytes());
            startPlayUrl(song);
            return -1;
        }
        if (cmd.equals("fullstop")) {
            putStart(buf, 5);
            putMenu(buf);
            String a = "<br/>stopping everything.<br/>";
            buf.write(a.getBytes());
            startPlayNormal(-1, "0");
            stopFull();
            return -1;
        }
        if (cmd.equals("dlna")) {
            putStart(buf, 5);
            putMenu(buf);
            String a = "<br/>starting dlna server.<br/>";
            buf.write(a.getBytes());
            startPlayDlna();
            return -1;
        }
        if (cmd.equals("headend")) {
            headEnd = !headEnd;
            putStart(buf, 5);
            putMenu(buf);
            String a = "<br/>toggled multicast streamer.<br/>";
            buf.write(a.getBytes());
            return -1;
        }
        if (cmd.equals("receive")) {
            putStart(buf, 5);
            putMenu(buf);
            String a = "<br/>starting multicast receiver.<br/>";
            buf.write(a.getBytes());
            startPlayRcvr();
            return -1;
        }
        if (cmd.equals("download")) {
            return playerUtil.str2int(song);
        }
        if (cmd.equals("title")) {
            String a;
            if ((currSong >= playlist.size()) || (currSong < 0)) {
                a = "stopped";
            } else {
                a = playlist.get(currSong).title;
            }
            buf.reset();
            buf.write(a.getBytes());
            return -2;
        }
        if (cmd.equals("stop")) {
            startPlayNormal(-1, "0");
            putStart(buf, 1);
            putMenu(buf);
            buf.write("selected no song<br/>".getBytes());
            return -1;
        }
        if (cmd.equals("replay")) {
            startPlayNormal(currSong, "0");
            putStart(buf, 1);
            putMenu(buf);
            buf.write("selected current song<br/>".getBytes());
            return -1;
        }
        if (cmd.equals("next")) {
            startPlayNormal(nextSong.get(0), "0");
            putStart(buf, 1);
            putMenu(buf);
            buf.write("selected next song<br/>".getBytes());
            return -1;
        }
        if (cmd.equals("prev")) {
            startPlayNormal(prevSong, "0");
            putStart(buf, 1);
            putMenu(buf);
            buf.write("selected previous song<br/>".getBytes());
            return -1;
        }
        if (cmd.equals("play")) {
            startPlayNormal(playerUtil.str2int(song), "0");
            putStart(buf, 1);
            putMenu(buf);
            String a = "selected song #" + song + "<br/>";
            buf.write(a.getBytes());
            return -1;
        }
        if (currSong < 0) {
            putStart(buf, 5);
            putMenu(buf);
            buf.write("player currently stopped!".getBytes());
            return -1;
        }
        putStart(buf, 15);
        playerSong sng = putMenu(buf);
        if (currLyrc != null) {
            buf.write("lyrics:<br/><br/>".getBytes());
            for (int i = 0; i < currLyrc.size(); i++) {
                cmd = currLyrc.get(i) + "<br/>";
                buf.write(cmd.getBytes());
            }
        }
        if (sng != null) {
            String a = sng.title.replaceAll(" ", "+");
            a = "http://www.google.com/search?q=" + a + "&tbm=vid";
            a = "clip: <a href=\"" + a + "\">" + a + "</a><br/>";
            buf.write(a.getBytes());
        }
        putMenu(buf);
        return -1;
    }

}
