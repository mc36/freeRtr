
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileReader;
import java.net.URL;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import javax.imageio.ImageIO;

/**
 * web temperature setter
 *
 * @author matecsaba
 */
public class temper implements Runnable {

    /**
     * this is needed for cli startup
     *
     * @param args command line parameters
     */
    public static void main(String[] args) {
        temper app = new temper();
        String a;
        try {
            ByteArrayOutputStream buf = new ByteArrayOutputStream();
            a = "" + app.getClass();
            a = temper.httpRequest("http://localhost/" + a, "./" + a, "cli", "clibrowser", "user", args, buf);
            a = "type=" + a + "\r\ndata:\r\n" + buf.toString();
        } catch (Exception e) {
            a = "exception " + e.getMessage();
        }
        System.out.println(a);
    }

    /**
     * where i'm located in ram
     */
    protected static temper staticTemper = null;

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
    public static String httpRequest(String url, String path, String peer,
            String agent, String user, String[] par, ByteArrayOutputStream buf)
            throws Exception {
        if (staticTemper == null) {
            staticTemper = new temper();
            staticTemper.path = path.substring(0, path.lastIndexOf("."));
            staticTemper.url = new URL(url).getPath();
            staticTemper.doInit();
            new Thread(staticTemper).start();
        }
        if (staticTemper.doRequest(par, buf, peer) == 2) {
            return "png";
        } else {
            return "html";
        }
    }

    /**
     * where i'm located on host
     */
    protected String path;

    /**
     * where i'm located on net
     */
    protected String url;

    /**
     * reading store
     */
    protected temperData measDat[] = new temperData[0];

    /**
     * reading used
     */
    protected int measUse = -1;

    /**
     * current value: 0=standby, 1=heating, 2=door
     */
    protected int currValue = 0x10000;

    /**
     * last needed temperature
     */
    protected float lastNeeded = 20;

    /**
     * time needed temperature
     */
    protected long timeNeeded = 0;

    /**
     * time heating temperature
     */
    protected long timeHeating = 0;

    /**
     * log file
     */
    protected String logFile = "temper.log";

    /**
     * door codes
     */
    protected List<String> doorCode = new ArrayList<String>();

    /**
     * door time
     */
    protected int doorTime = 300;

    /**
     * door pin
     */
    protected int doorPin = 0x2;

    /**
     * window min time
     */
    protected int windowMin = 15 * 60 * 1000;

    /**
     * window max time
     */
    protected int windowMax = 30 * 60 * 1000;

    /**
     * window tolerance
     */
    protected float windowTol = 1;

    /**
     * measure timeout
     */
    protected int measTime = 5 * 60 * 1000;

    /**
     * temperature minimum
     */
    protected float tempMin = 10;

    /**
     * temperature maximum
     */
    protected float tempMax = 30;

    /**
     * temperature tolerance
     */
    protected float tempTol = 1;

    /**
     * temperature pin
     */
    protected int tempPin = 0x1;

    /**
     * relay pin
     */
    protected int relayPin = 0x0;

    /**
     * last setter peer
     */
    protected String lastSetter = "nobody";

    private synchronized void setValue(int val) {
        val &= (tempPin | doorPin | relayPin);
        if (currValue == val) {
            return;
        }
        try {
            Runtime rtm = Runtime.getRuntime();
            String[] cmd = new String[2];
            cmd[0] = "./tempset.sh";
            cmd[1] = val + "";
            Process prc = rtm.exec(cmd);
            prc.waitFor();
        } catch (Exception e) {
            return;
        }
        if ((currValue & tempPin) != (val & tempPin)) {
            timeHeating = temperUtil.getTime();
            for (int i = 0; i < measDat.length; i++) {
                measDat[i].setWindow();
            }
        }
        currValue = val;
    }

    private void rangeCheck() {
        if (lastNeeded > tempMax) {
            lastNeeded = tempMax;
        }
        if (lastNeeded < tempMin) {
            lastNeeded = tempMin;
        }
    }

    private int doCalc() {
        for (int i = 0; i < measDat.length; i++) {
            measDat[i].getValue();
            measDat[i].doCalc(this);
        }
        measUse = -1;
        boolean win = false;
        for (int i = 0; i < measDat.length; i++) {
            if (!measDat[i].isWorking) {
                continue;
            }
            win |= measDat[i].isWindow;
            if (measUse >= 0) {
                continue;
            }
            measUse = i;
        }
        int i = currValue & (~tempPin);
        if (win) {
            return i;
        }
        if (measUse < 0) {
            return i;
        }
        return i | measDat[measUse].lastCalc;
    }

    private synchronized void writeLog(String who) {
        if (who == null) {
            who = lastSetter;
        }
        String a = "";
        for (int i = 0; i < measDat.length; i++) {
            a = a + ";" + measDat[i].getLog();
        }
        temperUtil.append(logFile, temperUtil.getTime() + ";" + who + ";" + currValue + ";" + lastNeeded + ";" + measUse + a);
    }

    /**
     * initialize
     */
    public void doInit() {
        readConfig();
        measDat[0].getValue();
        lastNeeded = measDat[0].lastMeasure;
        timeNeeded = temperUtil.getTime();
        timeHeating = timeNeeded;
        lastSetter = "boot";
        rangeCheck();
        writeLog("<boot>");
    }

    public void run() {
        for (;;) {
            rangeCheck();
            setValue(doCalc());
            writeLog(null);
            temperUtil.sleep(60 * 1000);
        }
    }

    private void readConfig() {
        logFile = path + ".log";
        List<String> c = temperUtil.readup(path + ".cfg");
        if (c == null) {
            return;
        }
        List<String> m = new ArrayList<String>();
        for (int i = 0; i < c.size(); i++) {
            String s = c.get(i);
            int o = s.indexOf("=");
            if (o < 0) {
                continue;
            }
            String a = s.substring(0, o).trim().toLowerCase();
            s = s.substring(o + 1, s.length()).trim();
            if (a.equals("door-code")) {
                doorCode.add(s);
                continue;
            }
            if (a.equals("door-time")) {
                doorTime = (int) temperUtil.str2num(s);
                continue;
            }
            if (a.equals("door-pin")) {
                doorPin = (int) temperUtil.str2num(s);
                continue;
            }
            if (a.equals("log-file")) {
                logFile = s;
                continue;
            }
            if (a.equals("temp-min")) {
                tempMin = temperUtil.str2num(s);
                continue;
            }
            if (a.equals("temp-max")) {
                tempMax = temperUtil.str2num(s);
                continue;
            }
            if (a.equals("temp-tol")) {
                tempTol = temperUtil.str2num(s);
                continue;
            }
            if (a.equals("temp-pin")) {
                tempPin = (int) temperUtil.str2num(s);
                continue;
            }
            if (a.equals("win-min")) {
                windowMin = (int) (temperUtil.str2num(s) * 60 * 1000);
                continue;
            }
            if (a.equals("win-max")) {
                windowMax = (int) (temperUtil.str2num(s) * 60 * 1000);
                continue;
            }
            if (a.equals("win-tol")) {
                windowTol = temperUtil.str2num(s);
                continue;
            }
            if (a.equals("timeout")) {
                measTime = (int) (temperUtil.str2num(s) * 60 * 1000);
                continue;
            }
            if (a.equals("relay-pin")) {
                relayPin = (int) temperUtil.str2num(s);
                continue;
            }
            if (a.equals("measure")) {
                m.add(s);
                continue;
            }
        }
        measDat = new temperData[m.size()];
        for (int i = 0; i < measDat.length; i++) {
            measDat[i] = new temperData(i + 1, m.get(i));
        }
    }

    private static void drawRightAlighed(Graphics2D g2d, int mx10, int y, String s) {
        FontMetrics fm = g2d.getFontMetrics();
        g2d.drawString(s, mx10 - fm.stringWidth(s), y);
    }

    private static void putStart(ByteArrayOutputStream buf, String tit, String res) throws Exception {
        buf.write("<!DOCTYPE html><html lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><link rel=\"stylesheet\" type=\"text/css\" href=\"index.css\" /><meta http-equiv=refresh content=\"3;url=/index.html\"><title>".getBytes());
        buf.write(tit.getBytes());
        buf.write("</title><body>".getBytes());
        buf.write(res.getBytes());
        buf.write("</body></html>".getBytes());
    }

    /**
     * do one request
     *
     * @param par parameters
     * @param buf buffer to use
     * @param peer address
     * @return 1 on html result
     * @throws Exception on error
     */
    public synchronized int doRequest(String[] par, ByteArrayOutputStream buf, String peer) throws Exception {
        String tmp = "";
        String cmd = "";
        for (int i = 0; i < par.length; i++) {
            String a = par[i];
            int o = a.indexOf("=");
            if (o < 1) {
                continue;
            }
            String b = a.substring(0, o);
            a = a.substring(o + 1, a.length());
            if (b.equals("temp")) {
                tmp = a;
            }
            if (b.equals("cmd")) {
                cmd = a;
            }
        }
        if (cmd.equals("heat")) {
            lastNeeded = temperUtil.str2num(tmp);
            timeNeeded = temperUtil.getTime();
            lastSetter = peer;
            rangeCheck();
        }
        if (cmd.equals("relayor")) {
            int i = ((int) temperUtil.str2num(tmp)) & relayPin;
            tmp = "" + (currValue | i);
            cmd = "relayset";
        }
        if (cmd.equals("relayand")) {
            int i = ((int) temperUtil.str2num(tmp)) & relayPin;
            tmp = "" + (currValue & i);
            cmd = "relayset";
        }
        if (cmd.equals("relayxor")) {
            int i = ((int) temperUtil.str2num(tmp)) & relayPin;
            tmp = "" + (currValue ^ i);
            cmd = "relayset";
        }
        if (cmd.equals("relayset")) {
            int i = ((int) temperUtil.str2num(tmp)) & relayPin;
            setValue((currValue & (~relayPin)) | i);
            writeLog(peer);
            putStart(buf, "relay", "relay set to " + i + " from range " + relayPin);
            return 1;
        }
        if (cmd.equals("door")) {
            boolean b = false;
            for (int i = 0; i < doorCode.size(); i++) {
                b |= tmp.equals(doorCode.get(i));
                if (b) {
                    break;
                }
            }
            String a = "bad code";
            if (b) {
                setValue(currValue | doorPin);
                writeLog(peer);
                temperUtil.sleep(doorTime);
                setValue(currValue & (~doorPin));
                writeLog(peer);
                a = "door opened";
            }
            putStart(buf, "door", a);
            return 1;
        }
        if (!cmd.equals("graph")) {
            rangeCheck();
            String a = "<!DOCTYPE html><html lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><link rel=\"stylesheet\" type=\"text/css\" href=\"index.css\" /><title>temper</title>";
            buf.write(a.getBytes());
            a = "<meta http-equiv=refresh content=\"30;url=" + url + "\"></head><body>";
            buf.write(a.getBytes());
            long tim = temperUtil.getTime();
            for (int i = 0; i < measDat.length; i++) {
                a = measDat[i].getMeas() + "<br/>";
                buf.write(a.getBytes());
            }
            a = "tolerance: " + tempTol + " celsius, window: " + windowTol + " celsius, " + temperUtil.timePast(windowMin, 0) + "-" + temperUtil.timePast(windowMax, 0) + "<br/>";
            buf.write(a.getBytes());
            a = "needed: " + lastNeeded + " celsius, " + temperUtil.timePast(tim, timeNeeded) + " ago by " + lastSetter + "<br/>";
            buf.write(a.getBytes());
            a = "heating: " + currValue + ", " + temperUtil.timePast(tim, timeHeating) + " ago, using #" + (measUse + 1) + " for " + temperUtil.timePast(measTime, 0) + "<br/>";
            buf.write(a.getBytes());
            a = "<form action=\"" + url + "\" method=get>wish: <input type=text name=temp value=\"" + lastNeeded + "\"> celsius (" + tempMin + "-" + tempMax + ")";
            buf.write(a.getBytes());
            buf.write("<input type=submit name=cmd value=\"heat\">".getBytes());
            buf.write("<input type=submit name=cmd value=\"graph\">".getBytes());
            buf.write("</form><br/>".getBytes());
            for (int i = -3; i <= 3; i++) {
                int o = i + (int) lastNeeded;
                a = "((<a href=\"" + url + "?temp=" + o + ".0&cmd=heat\">" + o + ".0</a>))";
                buf.write(a.getBytes());
                a = "((<a href=\"" + url + "?temp=" + o + ".5&cmd=heat\">" + o + ".5</a>))";
                buf.write(a.getBytes());
            }
            buf.write("</body></html>".getBytes());
            return 1;
        }
        File fi = new File(logFile);
        FileReader fr = new FileReader(fi);
        fr.skip(fi.length() - (long) (temperUtil.str2num(tmp) * 64000));
        BufferedReader f = new BufferedReader(fr);
        f.readLine();
        List<temperHist> history = new ArrayList<temperHist>();
        float tmpMin = 9999;
        float tmpMax = -tmpMin;
        while (f.ready()) {
            String s = f.readLine();
            temperHist l = new temperHist();
            l.parseLine(s);
            history.add(l);
            tmpMin = Float.min(tmpMin, l.need);
            tmpMax = Float.max(tmpMax, l.need);
            for (int i = 0; i < l.meas.length; i++) {
                float v = l.meas[i];
                tmpMin = Float.min(tmpMin, v);
                tmpMax = Float.max(tmpMax, v);
            }
        }
        f.close();
        tmpMax -= tmpMin;
        final int mx = 1800;
        final int my = 900;
        final int mx10 = mx - 10;
        final int mx20 = mx - 20;
        final int my1 = my - 1;
        final int my10 = my - 10;
        final int my20 = my - 20;
        final int my30 = my - 30;
        final int my40 = my - 40;
        final int my50 = my - 50;
        BufferedImage img = new BufferedImage(mx, my, BufferedImage.TYPE_INT_RGB);
        Graphics2D g2d = img.createGraphics();
        g2d.setBackground(Color.gray);
        g2d.setFont(new Font("Serif", Font.BOLD, 20));
        g2d.setPaint(Color.gray);
        g2d.fillRect(0, 0, img.getWidth(), img.getHeight());
        g2d.setPaint(Color.black);
        Color colors[] = {
            Color.blue,
            Color.green,
            Color.red,
            Color.cyan,
            Color.magenta,
            Color.orange,
            Color.pink,
            Color.white,
            Color.yellow,};
        for (int i = 0; i < history.size(); i++) {
            temperHist l = history.get(i);
            int x = ((i * mx20) / history.size()) + 10;
            g2d.setPaint(Color.black);
            g2d.drawRect(x, my10 - (int) (((l.need - tmpMin) * my20) / tmpMax), 1, 1);
            for (int o = 0; o < l.meas.length; o++) {
                g2d.setPaint(colors[o]);
                g2d.drawRect(x, my10 - (int) (((l.meas[o] - tmpMin) * my20) / tmpMax), 1, 1);
            }
        }
        for (int i = 0; i < measDat.length; i++) {
            g2d.setPaint(colors[i]);
            drawRightAlighed(g2d, mx10, my50 - (i * 20), measDat[i].myNam);
        }
        g2d.setPaint(Color.black);
        drawRightAlighed(g2d, mx10, my30, "needed");
        for (int i = 20; i < my20; i += 50) {
            String a = (tmpMin + ((i * tmpMax) / my20)) + "       ";
            g2d.drawString(a.substring(0, 6), 1, my10 - i);
        }
        String a;
        if ((history.get(history.size() - 1).time - history.get(0).time) < (86400 * 3000)) {
            a = "HH:MM";
        } else {
            a = "MMMdd";
        }
        for (int i = 0; i < mx20; i += 100) {
            temperHist l = history.get((i * history.size()) / mx20);
            DateFormat dat = new SimpleDateFormat(a, Locale.US);
            g2d.drawString(dat.format(new Date((long) l.time)), i + 10, my1);
        }
        ImageIO.write(img, "png", buf);
        return 2;
    }

}
