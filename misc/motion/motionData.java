
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.TimeZone;
import javax.imageio.ImageIO;

/**
 * one motion detection
 *
 * @author matecsaba
 */
public class motionData implements Runnable {

    /**
     * parent of me
     */
    protected final motion parent;

    /**
     * my number
     */
    protected int myNum;

    /**
     * name of camera
     */
    protected String myName;

    /**
     * url to read
     */
    protected String myUrl;

    /**
     * username to use
     */
    protected String userName;

    /**
     * password to use
     */
    protected String password;

    /**
     * inter image sleep
     */
    protected int sleep;

    /**
     * images to keep
     */
    protected int imgPre;

    /**
     * images to keep
     */
    protected int imgPost;

    /**
     * ignore diff
     */
    protected int ignore;

    /**
     * trigger diff
     */
    protected int trigger;

    /**
     * alarm mode
     */
    protected int alarm;

    /**
     * exceptions happened
     */
    protected int errors;

    /**
     * images got
     */
    protected int fetches;

    /**
     * events happened
     */
    protected int events;

    /**
     * last event time
     */
    protected long lastEvnt;

    /**
     * last event file
     */
    protected String lastPath;

    /**
     * last event speed
     */
    protected int lastBps;

    /**
     * images saved
     */
    protected int saved;

    /**
     * minimum diff
     */
    protected int difMin = Integer.MAX_VALUE;

    /**
     * maximum diff
     */
    protected int difMax = Integer.MIN_VALUE;

    /**
     * last diff
     */
    protected int difLst;

    /**
     * last diff
     */
    protected int difAvg;

    private final static Object sleeper = new Object();

    private byte[][] imgDat;

    private long[] imgTim;

    private int[] imgDif;

    private int[] imgCol;

    private BufferedImage imgLst;

    private int imgPos;

    /**
     * create instance
     *
     * @param lower parent
     */
    protected motionData(motion lower) {
        parent = lower;
    }

    /**
     * get last image
     *
     * @param buf where to write
     * @throws java.lang.Exception on error
     */
    protected void getImage(ByteArrayOutputStream buf) throws Exception {
        buf.write(imgDat[imgPos]);
    }

    /**
     * get last video
     *
     * @param buf where to write
     * @throws java.lang.Exception on error
     */
    protected void getVideo(ByteArrayOutputStream buf) throws Exception {
        buf.write(lastPath.getBytes());
        buf.write("\n\n".getBytes());
        buf.write("//multipart/x-mixed-replace;boundary=ThisRandomString".getBytes());
        buf.write(("\n" + lastBps + "\n").getBytes());
    }

    /**
     * get alert needed
     *
     * @return true if yes, false if no
     */
    protected boolean needAlert() {
        switch (alarm) {
            case 0:
                return false;
            case 1:
                return true;
            case 2:
                return parent.alarmed;
            default:
                return false;
        }
    }

    /**
     * get web line
     *
     * @param tim current time
     * @return string
     */
    protected String getMeas(long tim) {
        String a;
        if (lastPath == null) {
            a = "never";
        } else {
            a = "<a href=\"" + parent.url + "?cmd=vid&nam=" + myNum + "\">" + motionUtil.timePast(tim, lastEvnt) + "</a>";
        }
        return "<tr><td>" + myNum + "</td><td>" + myName + "</td><td>" + needAlert() + "</td><td>" + events + "</td><td>" + a + "</td><td>" + errors + "</td><td>" + fetches + "</td><td>" + saved + "</td><td><a href=\"" + parent.url + "?cmd=img&nam=" + myNum + "\">here</a></td><td>" + difMin + "</td><td>" + difLst + "</td><td>" + difMax + "</td><td>" + difAvg + "</td></tr>";
    }

    /**
     * clear statistics
     */
    protected void doClear() {
        difMin = Integer.MAX_VALUE;
        difMax = Integer.MIN_VALUE;
        difLst = -1;
        difAvg = -1;
    }

    public void run() {
        imgDat = new byte[imgPre][];
        imgTim = new long[imgPre];
        imgDif = new int[imgPre];
        imgCol = new int[imgPre];
        for (;;) {
            try {
                doRound();
            } catch (Exception e) {
                errors++;
            }
        }
    }

    private void sleep() throws Exception {
        synchronized (sleeper) {
            sleeper.wait(sleep);
        }
    }

    private BufferedImage fetchImage() throws Exception {
        URL testUrl = new URL(myUrl);
        URLConnection testConn = testUrl.openConnection();
        testConn.setConnectTimeout(5000);
        testConn.setReadTimeout(5000);
        String userpass = testUrl.getUserInfo();
        if (userpass != null) {
            String auth = "Basic " + new String(Base64.getEncoder().encode(userpass.getBytes()));
            testConn.setRequestProperty("Authorization", auth);
        }
        InputStream testStream = testConn.getInputStream();
        List<Byte> buf1 = new ArrayList<Byte>();
        for (;;) {
            byte[] buf0 = new byte[4096];
            int len = testStream.read(buf0);
            if (len < 0) {
                break;
            }
            for (int i = 0; i < len; i++) {
                buf1.add(buf0[i]);
            }
        }
        byte[] buf2 = new byte[buf1.size()];
        for (int i = 0; i < buf2.length; i++) {
            buf2[i] = buf1.get(i);
        }
        BufferedImage result = ImageIO.read(new ByteArrayInputStream(buf2));
        fetches++;
        imgPos = (imgPos + 1) % imgDat.length;
        imgDat[imgPos] = buf2;
        imgTim[imgPos] = motionUtil.getTime();
        imgLst = result;
        return result;
    }

    private int saveImage(int seq, OutputStream output) throws Exception {
        if (imgDat[seq] == null) {
            return -1;
        }
        byte[] crlf = new byte[2];
        crlf[0] = 13;
        crlf[1] = 10;
        output.write("--ThisRandomString".getBytes());
        output.write(crlf);
        output.write("Content-Type: image/jpeg".getBytes());
        output.write(crlf);
        output.write(("X-TimeStamp: " + imgTim[seq]).getBytes());
        output.write(crlf);
        output.write(("X-Differences: pixels=" + imgDif[seq] + " colors=" + imgCol[seq]).getBytes());
        output.write(crlf);
        output.write(("Content-Length: " + imgDat[seq].length).getBytes());
        output.write(crlf);
        output.write(crlf);
        output.write(imgDat[seq]);
        output.write(crlf);
        saved++;
        return imgDat[seq].length;
    }

    private static int getDiff(int p1, int p2, int rot) {
        p1 = (p1 >>> rot) & 0xff;
        p2 = (p2 >>> rot) & 0xff;
        p1 = p1 - p2;
        if (p1 < 0) {
            return -p1;
        } else {
            return p1;
        }
    }

    private int getDiff(BufferedImage i1, BufferedImage i2) {
        int res = 0;
        int tot = 0;
        int cnt = 0;
        for (int y = i1.getHeight() - 1; y >= 0; y--) {
            for (int x = i1.getWidth() - 1; x >= 0; x--) {
                int p1 = i1.getRGB(x, y);
                int p2 = i2.getRGB(x, y);
                int dif = getDiff(p1, p2, 0);
                dif += getDiff(p1, p2, 8);
                dif += getDiff(p1, p2, 16);
                tot += dif;
                cnt++;
                if (dif < ignore) {
                    continue;
                }
                res++;
            }
        }
        if (res < difMin) {
            difMin = res;
        }
        if (res > difMax) {
            difMax = res;
        }
        difAvg = tot / cnt;
        difLst = res;
        imgDif[imgPos] = difLst;
        imgCol[imgPos] = difAvg;
        return res;
    }

    private void doRound() throws Exception {
        sleep();
        BufferedImage old = imgLst;
        BufferedImage cur = fetchImage();
        if (old == null) {
            return;
        }
        int dif = getDiff(cur, old);
        if (dif < trigger) {
            return;
        }
        events++;
        lastEvnt = motionUtil.getTime();
        if (needAlert()) {
            motionSend snd = new motionSend(this);
            new Thread(snd).start();
        }
        Calendar cal = new GregorianCalendar(TimeZone.getTimeZone(parent.tzdata));
        cal.setTime(new Date());
        String date = cal.get(Calendar.YEAR) + motionUtil.padBeg("" + (cal.get(Calendar.MONTH) + 1), 2, "0") + motionUtil.padBeg("" + cal.get(Calendar.DAY_OF_MONTH), 2, "0");
        String time = motionUtil.padBeg("" + cal.get(Calendar.HOUR_OF_DAY), 2, "0") + motionUtil.padBeg("" + cal.get(Calendar.MINUTE), 2, "0") + motionUtil.padBeg("" + cal.get(Calendar.SECOND), 2, "0");
        String path = parent.target + date;
        new File(path).mkdir();
        path = path + "/" + myName + "-" + date + "-" + time + "-" + dif + ".mjpeg";
        OutputStream output = new FileOutputStream(new File(path));
        lastPath = path;
        for (int i = 0; i < imgDat.length; i++) {
            saveImage((imgPos + 1 + i) % imgDat.length, output);
        }
        int ago = 0;
        int sav = 0;
        for (;;) {
            ago++;
            if (ago > imgPost) {
                break;
            }
            sleep();
            dif = 0;
            try {
                old = imgLst;
                cur = fetchImage();
                dif = getDiff(cur, old);
                sav += saveImage(imgPos, output);
            } catch (Exception e) {
            }
            if (dif < trigger) {
                continue;
            }
            ago = 0;
        }
        output.flush();
        output.close();
        lastBps = sav / (int) ((motionUtil.getTime() - lastEvnt) / 1000);
    }

}

class motionSend implements Runnable {

    private motionData parent;

    public motionSend(motionData lower) {
        parent = lower;
    }

    public void run() {
        try {
            parent.parent.sendAlert(parent.myName);
        } catch (Exception e) {
            parent.errors++;
        }
    }

}
