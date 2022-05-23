
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
            a = "" + app.getClass().getName() + ".";
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
     * inside reading used
     */
    protected int measIn = -1;

    /**
     * outside reading used
     */
    protected int measOut = -1;

    /**
     * current value sent
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
     * time zone to use
     */
    protected String tzdata = "Z";

    /**
     * log file
     */
    protected String logFile = "temper.log";

    /**
     * script file
     */
    protected String scrFile = "temper.sh";

    /**
     * window min time
     */
    protected int windowTim = 15 * 60 * 1000;

    /**
     * measure timeout
     */
    protected int measTime = 5 * 60 * 1000;

    /**
     * measure timeout
     */
    protected int measIotm = 5 * 1000;

    /**
     * measure interval
     */
    protected int collTime = 30 * 1000;

    /**
     * measure history
     */
    protected int collHist = 60;

    /**
     * temperature minimum
     */
    protected float tempMin = 10;

    /**
     * temperature maximum
     */
    protected float tempMax = 30;

    /**
     * heat tolerance
     */
    protected float heatTol = 0.3F;

    /**
     * cool tolerance
     */
    protected float coolTol = 0.5F;

    /**
     * cool tolerance
     */
    protected float ventTol = 0.1F;

    /**
     * window tolerance
     */
    protected float windowTol = 0.9F;

    /**
     * heat pin
     */
    protected int heatPin = 0x0;

    /**
     * cool pin
     */
    protected int coolPin = 0x0;

    /**
     * vent pin
     */
    protected int ventPin = 0x0;

    /**
     * relay pin
     */
    protected int relayPin = 0x0;

    /**
     * door pin
     */
    protected int doorPin = 0x0;

    /**
     * door time
     */
    protected int doorTime = 1000;

    /**
     * door codes
     */
    protected List<temperCode> doorCode = new ArrayList<temperCode>();

    /**
     * door log
     */
    protected List<temperLog> doorLog = new ArrayList<temperLog>();

    /**
     * door temporary codes allowed
     */
    protected boolean doorTemp = false;

    /**
     * door max
     */
    protected int doorMax = 5;

    /**
     * door count
     */
    protected int doorCnt = 0;

    /**
     * last setter peer
     */
    protected String lastSetter = "nobody";

    private synchronized void setValue(int val) {
        val &= (heatPin | coolPin | ventPin | doorPin | relayPin);
        if (currValue == val) {
            return;
        }
        try {
            Runtime rtm = Runtime.getRuntime();
            String[] cmd = new String[2];
            cmd[0] = scrFile;
            cmd[1] = val + "";
            Process prc = rtm.exec(cmd);
            prc.waitFor();
        } catch (Exception e) {
            return;
        }
        int mask = heatPin | coolPin | ventPin;
        if ((currValue & mask) != (val & mask)) {
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
            measDat[i].doCalc();
        }
        measIn = -1;
        measOut = -1;
        int prioIn = -1;
        int prioOut = -1;
        boolean win = false;
        for (int i = 0; i < measDat.length; i++) {
            if (!measDat[i].isWorking) {
                continue;
            }
            if (measDat[i].myPri < 0) {
                continue;
            }
            if (measDat[i].inside) {
                if (prioIn > measDat[i].myPri) {
                    continue;
                }
                measIn = i;
                prioIn = measDat[i].myPri;
            } else {
                if (prioOut > measDat[i].myPri) {
                    continue;
                }
                measOut = i;
                prioOut = measDat[i].myPri;
            }
            win |= measDat[i].isWindow;
        }
        int i = currValue & ~(heatPin | coolPin | ventPin);
        if (win) {
            return i;
        }
        if (measIn < 0) {
            return i;
        }
        temperData.results res = measDat[measIn].lastCalc;
        if ((ventPin != 0) && (measOut >= 0)) {
            boolean maybe = (currValue & ventPin) != 0;
            float diff = measDat[measIn].lastMeasure - measDat[measOut].lastMeasure;
            if (diff < 0) {
                diff = -diff;
            }
            maybe &= diff < ventTol;
            boolean good;
            switch (res) {
                case cool:
                    good = measDat[measIn].lastMeasure > (measDat[measOut].lastMeasure + coolTol);
                    break;
                case heat:
                    good = measDat[measIn].lastMeasure < (measDat[measOut].lastMeasure - heatTol);
                    break;
                default:
                    return i;
            }
            good &= (res != measDat[measOut].histRes) || (measDat[measOut].histRes == temperData.results.idle);
            if (good || maybe) {
                return i | ventPin;
            }
        }
        switch (res) {
            case cool:
                return i | coolPin;
            case heat:
                return i | heatPin;
            default:
                return i;
        }
    }

    private synchronized void writeLog(String who) {
        if (who == null) {
            who = lastSetter;
        }
        String a = "";
        for (int i = 0; i < measDat.length; i++) {
            a = a + ";" + measDat[i].getLog();
        }
        temperUtil.append(logFile, temperUtil.getTime() + ";" + who + ";" + currValue + ";" + lastNeeded + ";" + measIn + ";" + measOut + a);
    }

    /**
     * initialize
     */
    public void doInit() {
        readConfig();
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
            temperUtil.sleep(collTime);
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
            if (a.equals("script")) {
                scrFile = s;
                continue;
            }
            if (a.equals("needed")) {
                lastNeeded = temperUtil.str2num(s);
                continue;
            }
            if (a.equals("tzdata")) {
                tzdata = s;
                continue;
            }
            if (a.equals("door-count")) {
                doorMax = (int) temperUtil.str2num(s);
                continue;
            }
            if (a.equals("door-code")) {
                doorCode.add(new temperCode(doorCode.size() + 1, s, false));
                continue;
            }
            if (a.equals("door-tcode")) {
                doorCode.add(new temperCode(doorCode.size() + 1, s, true));
                continue;
            }
            if (a.equals("door-temp")) {
                doorTemp = s.equals("on");
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
            if (a.equals("heat-tol")) {
                heatTol = temperUtil.str2num(s);
                continue;
            }
            if (a.equals("heat-pin")) {
                heatPin = (int) temperUtil.str2num(s);
                continue;
            }
            if (a.equals("cool-tol")) {
                coolTol = temperUtil.str2num(s);
                continue;
            }
            if (a.equals("cool-pin")) {
                coolPin = (int) temperUtil.str2num(s);
                continue;
            }
            if (a.equals("vent-tol")) {
                ventTol = temperUtil.str2num(s);
                continue;
            }
            if (a.equals("vent-pin")) {
                ventPin = (int) temperUtil.str2num(s);
                continue;
            }
            if (a.equals("win-tim")) {
                windowTim = (int) (temperUtil.str2num(s) * 60 * 1000);
                continue;
            }
            if (a.equals("win-tol")) {
                windowTol = temperUtil.str2num(s);
                continue;
            }
            if (a.equals("collect")) {
                collTime = (int) (temperUtil.str2num(s) * 1000);
                continue;
            }
            if (a.equals("history")) {
                collHist = (int) (temperUtil.str2num(s) * 1000);
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
            measDat[i] = new temperData(this, i + 1, m.get(i));
        }
    }

    private static void drawRightAlighed(Graphics2D g2d, int mx10, int y, String s) {
        FontMetrics fm = g2d.getFontMetrics();
        g2d.drawString(s, mx10 - fm.stringWidth(s), y);
    }

    private static void putStart(ByteArrayOutputStream buf, String tit, String res) throws Exception {
        buf.write("<!DOCTYPE html><html lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" /><link rel=\"stylesheet\" type=\"text/css\" href=\"index.css\" /><meta http-equiv=refresh content=\"3;url=/index.html\"><title>".getBytes());
        buf.write(tit.getBytes());
        buf.write("</title><body>".getBytes());
        buf.write(res.getBytes());
        buf.write("</body></html>".getBytes());
    }

    private temperCode findCode(String s) {
        for (int i = 0; i < doorCode.size(); i++) {
            temperCode cur = doorCode.get(i);
            if (!s.equals(cur.code)) {
                continue;
            }
            return cur;
        }
        return null;
    }

    private BufferedImage drawHist(List<temperHist> hst, int dot) {
        float tmpMin = 9999;
        float tmpMax = -tmpMin;
        for (int o = 0; o < hst.size(); o++) {
            temperHist l = hst.get(o);
            tmpMin = Float.min(tmpMin, l.need);
            tmpMax = Float.max(tmpMax, l.need);
            for (int i = 0; i < l.meas.length; i++) {
                float v = l.meas[i];
                tmpMin = Float.min(tmpMin, v);
                tmpMax = Float.max(tmpMax, v);
            }
        }
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
            Color.green,
            Color.red,
            Color.cyan,
            Color.magenta,
            Color.orange,
            Color.pink,
            Color.yellow,
            Color.white,
            Color.blue,};
        for (int o = 0; o < hst.size(); o++) {
            temperHist l = hst.get(o);
            int x = ((o * mx20) / hst.size()) + 10;
            g2d.setPaint(Color.black);
            g2d.drawRect(x, my10 - (int) (((l.need - tmpMin) * my20) / tmpMax), dot, dot);
            if (l.curr != 0) {
                g2d.drawRect(x, 10, dot, dot);
            }
            for (int i = 0; i < l.meas.length; i++) {
                g2d.setPaint(colors[i]);
                g2d.drawRect(x, my10 - (int) (((l.meas[i] - tmpMin) * my20) / tmpMax), dot, dot);
            }
        }
        for (int i = 0; i < measDat.length; i++) {
            g2d.setPaint(colors[i]);
            drawRightAlighed(g2d, mx10, my50 - (i * 20), measDat[i].myNam);
        }
        g2d.setPaint(Color.black);
        drawRightAlighed(g2d, mx10, my30, "needed");
        for (int i = 20; i < my20; i += 50) {
            float v = (float) i * tmpMax;
            v = v / (float) my20;
            v += tmpMin;
            String a = v + "       ";
            g2d.drawString(a.substring(0, 6), 1, my10 - i);
        }
        String a;
        if ((hst.get(hst.size() - 1).time - hst.get(0).time) < (86400 * 3000)) {
            a = "HH:mm";
        } else {
            a = "MMMdd";
        }
        for (int i = 0; i < mx20; i += 100) {
            temperHist l = hst.get((i * hst.size()) / mx20);
            DateFormat dat = new SimpleDateFormat(a, Locale.US);
            g2d.drawString(dat.format(new Date((long) l.time)), i + 10, my1);
        }
        return img;
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
            if (relayPin == 0) {
                putStart(buf, "relay", "relay not set");
                return 1;
            }
            int i = ((int) temperUtil.str2num(tmp)) & relayPin;
            setValue((currValue & (~relayPin)) | i);
            writeLog(peer);
            putStart(buf, "relay", "relay set to " + i + " from range " + relayPin);
            return 1;
        }
        if (cmd.equals("guests")) {
            temperCode res = findCode(tmp);
            if (res == null) {
                putStart(buf, "door", "invalid code");
                return 1;
            }
            if (res.temp) {
                putStart(buf, "door", "disabled code");
                return 1;
            }
            doorTemp = true;
            putStart(buf, "door", "guests enabled");
            return 1;
        }
        if (cmd.equals("closed")) {
            temperCode res = findCode(tmp);
            if (res == null) {
                putStart(buf, "door", "invalid code");
                return 1;
            }
            if (res.temp) {
                putStart(buf, "door", "disabled code");
                return 1;
            }
            doorTemp = false;
            putStart(buf, "door", "guests disabled");
            return 1;
        }
        if (cmd.equals("door")) {
            temperCode res = findCode(tmp);
            if (res == null) {
                putStart(buf, "door", "invalid code");
                return 1;
            }
            if (res.temp && (!doorTemp)) {
                putStart(buf, "door", "disabled code");
                return 1;
            }
            setValue(currValue | doorPin);
            writeLog(peer);
            temperUtil.sleep(doorTime);
            setValue(currValue & (~doorPin));
            writeLog(peer);
            doorLog.add(new temperLog(this, peer, res.myNum));
            for (;;) {
                if (doorLog.size() <= doorMax) {
                    break;
                }
                doorLog.remove(0);
            }
            doorCnt++;
            putStart(buf, "door", "door opened");
            return 1;
        }
        if (cmd.equals("history")) {
            temperData cur = measDat[(int) temperUtil.str2num(tmp) - 1];
            ImageIO.write(drawHist(cur.histDat, 10), "png", buf);
            return 2;
        }
        if (!cmd.equals("graph")) {
            rangeCheck();
            String a = "<!DOCTYPE html><html lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" /><link rel=\"stylesheet\" type=\"text/css\" href=\"index.css\" /><title>temper</title>";
            buf.write(a.getBytes());
            a = "<meta http-equiv=refresh content=\"30;url=" + url + "\"></head><body>";
            buf.write(a.getBytes());
            long tim = temperUtil.getTime();
            a = "<table><thead><tr><td><b>num</b></td><td><b>name</b></td><td><b>value</b></td><td><b>last</b></td><td><b>err</b></td><td><b>read</b></td><td><b>work</b></td><td><b>win</b></td><td><b>res</b></td><td><b>hst</b></td><td><b>wtmp</b></td><td><b>wtim</b></td></tr></thead><tbody>";
            buf.write(a.getBytes());
            for (int i = 0; i < measDat.length; i++) {
                a = measDat[i].getMeas();
                buf.write(a.getBytes());
            }
            a = "</tbody></table><br/>";
            buf.write(a.getBytes());
            a = "tolerance: heat " + heatTol + ", cool " + coolTol + ", vent " + ventTol + ", window " + windowTol + ", time " + temperUtil.timePast(windowTim, 0) + "<br/>";
            buf.write(a.getBytes());
            a = "needed: " + lastNeeded + " celsius, since " + temperUtil.time2str(tzdata, timeNeeded) + ", " + temperUtil.timePast(tim, timeNeeded) + " ago by " + lastSetter + "<br/>";
            buf.write(a.getBytes());
            a = "doing: " + currValue + ", since " + temperUtil.time2str(tzdata, timeHeating) + ", " + temperUtil.timePast(tim, timeHeating) + " ago, using #" + (measIn + 1) + "/#" + (measOut + 1) + " for " + temperUtil.timePast(measTime, 0) + "<br/>";
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
            a = "<br/><br/>the door was opened " + doorCnt + " times:<br/>";
            buf.write(a.getBytes());
            a = "<table><thead><tr><td><b>num</b></td><td><b>when</b></td><td><b>ago</b></td><td><b>code</b></td><td><b>peer</b></td></tr></thead><tbody>";
            buf.write(a.getBytes());
            for (int i = doorLog.size() - 1; i >= 0; i--) {
                a = doorLog.get(i).getMeas(i, tim);
                buf.write(a.getBytes());
            }
            a = "</tbody></table><br/>";
            buf.write(a.getBytes());
            buf.write("</body></html>".getBytes());
            return 1;
        }
        File fi = new File(logFile);
        FileReader fr = new FileReader(fi);
        fr.skip(fi.length() - (long) (temperUtil.str2num(tmp) * 64000));
        BufferedReader f = new BufferedReader(fr);
        f.readLine();
        List<temperHist> history = new ArrayList<temperHist>();
        while (f.ready()) {
            String s = f.readLine();
            temperHist l = new temperHist();
            l.parseLine(s);
            history.add(l);
        }
        f.close();
        ImageIO.write(drawHist(history, 1), "png", buf);
        return 2;
    }

}
