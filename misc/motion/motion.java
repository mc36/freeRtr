
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.FileReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

/**
 * web motion detection
 *
 * @author matecsaba
 */
public class motion {

    /**
     * this is needed for cli startup
     *
     * @param args command line parameters
     */
    public static void main(String[] args) {
        motion app = new motion();
        String a;
        try {
            ByteArrayOutputStream buf = new ByteArrayOutputStream();
            a = "" + app.getClass().getName() + ".";
            a = motion.httpRequest("http://localhost/" + a, "./" + a, "cli", "clibrowser", "user", args, buf);
            a = "type=" + a + "\r\ndata:\r\n" + buf.toString();
        } catch (Exception e) {
            a = "exception " + e.getMessage();
        }
        System.out.println(a);
    }

    /**
     * where i'm located in ram
     */
    protected static motion staticMotion = null;

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
        if (staticMotion == null) {
            staticMotion = new motion();
            staticMotion.path = path.substring(0, path.lastIndexOf("."));
            staticMotion.url = new URL(url).getPath();
            staticMotion.doInit();
        }
        if (staticMotion.doRequest(par, buf) == 1) {
            return "jpeg";
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
     * where results located on host
     */
    protected String target = "./";

    /**
     * time zone to use
     */
    protected String tzdata = "Z";

    /**
     * command to use
     */
    protected String command = "echo motion detected";

    /**
     * alarmed
     */
    protected boolean alarmed = true;

    /**
     * cameras
     */
    protected List<motionData> cams = new ArrayList<motionData>();

    /**
     * initialize
     */
    public void doInit() {
        readConfig();
    }

    private void readConfig() {
        int sleep = 100;
        int pre = 10;
        int post = 20;
        int ignor = 50;
        int trigr = 1000;
        int alarm = 1;
        try {
            BufferedReader f = new BufferedReader(new FileReader(path + ".cfg"));
            for (;;) {
                String a = f.readLine();
                if (a == null) {
                    break;
                }
                String s;
                int i = a.indexOf(" ");
                if (i < 0) {
                    s = a;
                    a = "";
                } else {
                    s = a.substring(0, i);
                    a = a.substring(i + 1, a.length());
                }
                if (s.equals("target")) {
                    target = a;
                    continue;
                }
                if (s.equals("tzdata")) {
                    tzdata = a;
                    continue;
                }
                if (s.equals("command")) {
                    command = a;
                    continue;
                }
                if (s.equals("sleep")) {
                    sleep = motionUtil.str2num(a);
                    continue;
                }
                if (s.equals("before")) {
                    pre = motionUtil.str2num(a);
                    continue;
                }
                if (s.equals("after")) {
                    post = motionUtil.str2num(a);
                    continue;
                }
                if (s.equals("ignore")) {
                    ignor = motionUtil.str2num(a);
                    continue;
                }
                if (s.equals("trigger")) {
                    trigr = motionUtil.str2num(a);
                    continue;
                }
                if (s.equals("alarm")) {
                    alarm = motionUtil.str2num(a);
                    continue;
                }
                if (s.equals("alarmed")) {
                    alarmed = motionUtil.str2num(a) == 1;
                    continue;
                }
                if (s.equals("camera")) {
                    motionData ntry = new motionData(this);
                    i = a.indexOf(" ");
                    ntry.myName = a.substring(0, i);
                    ntry.myUrl = a.substring(i + 1, a.length());
                    ntry.sleep = sleep;
                    ntry.imgPre = pre;
                    ntry.imgPost = post;
                    ntry.ignore = ignor;
                    ntry.trigger = trigr;
                    ntry.alarm = alarm;
                    new Thread(ntry).start();
                    cams.add(ntry);
                    continue;
                }
            }
            f.close();
        } catch (Exception e) {
        }
    }

    /**
     * do one request
     *
     * @param nam name
     */
    protected void sendAlert(String nam) throws Exception {
        String[] cmd = new String[3];
        cmd[0] = "/bin/sh";
        cmd[1] = "-c";
        cmd[2] = command.replaceAll("%", nam);
        Runtime rtm = Runtime.getRuntime();
        Process prc = rtm.exec(cmd);
        prc.waitFor();
    }

    /**
     * do one request
     *
     * @param par parameters
     * @param buf buffer to use
     * @throws Exception if something went wrong
     */
    public int doRequest(String[] par, ByteArrayOutputStream buf) throws Exception {
        String cmd = "";
        String nam = "";
        for (int i = 0; i < par.length; i++) {
            String a = par[i];
            int o = a.indexOf("=");
            if (o < 1) {
                continue;
            }
            String b = a.substring(0, o);
            a = a.substring(o + 1, a.length());
            if (b.equals("cmd")) {
                cmd = a;
                continue;
            }
            if (b.equals("nam")) {
                nam = a;
                continue;
            }
        }
        if (cmd.equals("img")) {
            motionData ntry = cams.get(motionUtil.str2num(nam));
            ntry.getImage(buf);
            return 1;
        }
        if (cmd.equals("arm")) {
            alarmed = motionUtil.str2num(nam) == 1;
            buf.write("<!DOCTYPE html><html lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><link rel=\"stylesheet\" type=\"text/css\" href=\"index.css\" /><title>motion</title></head><body>".getBytes());
            buf.write(("armed=" + alarmed + "<br/>").getBytes());
            buf.write("</body></html>".getBytes());
            return 0;
        }
        buf.write("<!DOCTYPE html><html lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><link rel=\"stylesheet\" type=\"text/css\" href=\"index.css\" /><title>motion</title></head><body>".getBytes());
        buf.write("<table><thead><tr><td><b>name</b></td><td><b>armed</b></td><td><b>events</b></td><td><b>errors</b></td><td><b>fetches</b></td><td><b>saved</b></td><td><b>image</b></td><td><b>min</b></td><td><b>cur</b></td><td><b>max</b></td><td><b>avg</b></td></tr></thead><tbody>".getBytes());
        for (int i = 0; i < cams.size(); i++) {
            motionData ntry = cams.get(i);
            buf.write(("<tr><td>" + ntry.myName + "</td><td>" + ntry.needAlert() + "</td><td>" + ntry.events + "</td><td>" + ntry.errors + "</td><td>" + ntry.fetches + "</td><td>" + ntry.saved + "</td><td><a href=\"" + url + "?cmd=img&nam=" + i + "\">here</a></td><td>" + ntry.difMin + "</td><td>" + ntry.difLst + "</td><td>" + ntry.difMax + "</td><td>" + ntry.difAvg + "</td></tr>").getBytes());
        }
        buf.write("</tbody></table></body></html>".getBytes());
        return 0;
    }

}
