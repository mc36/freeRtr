
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.URI;
import java.net.URL;
import java.net.URLConnection;
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
            staticMotion.url = new URI(url).toURL().getPath();
            staticMotion.doInit();
        }
        switch (staticMotion.doRequest(par, buf, peer)) {
            case 1:
                return "jpeg";
            case 2:
                return "//file//";
            default:
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
     * server to use
     */
    protected String server = "mail.nop.hu";

    /**
     * address to use
     */
    protected String address = "somebody@somewhere.org";

    /**
     * address to use
     */
    protected String apiurl = "nothing.nowhere.org";

    /**
     * alarmed
     */
    protected boolean alarmed = true;

    /**
     * last setter peer
     */
    protected String lastSetter = "nobody";

    /**
     * time needed temperature
     */
    protected long timeNeeded = 0;

    /**
     * cameras
     */
    protected motionData[] cams;

    /**
     * initialize
     */
    public void doInit() {
        readConfig();
    }

    private void readConfig() {
        lastSetter = "boot";
        timeNeeded = motionUtil.getTime();
        List<motionData> lst = new ArrayList<motionData>();
        int sleep = 100;
        int pre = 10;
        int post = 20;
        int ignor = 50;
        int trigr = 1000;
        int alarM = 1;
        int alarH = 1;
        int arBx = 0;
        int arBy = 0;
        int arEx = Integer.MAX_VALUE;
        int arEy = Integer.MAX_VALUE;
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
                if (s.equals("server")) {
                    server = a;
                    continue;
                }
                if (s.equals("address")) {
                    address = a;
                    continue;
                }
                if (s.equals("apiurl")) {
                    apiurl = a;
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
                if (s.equals("alarm-mail")) {
                    alarM = motionUtil.str2num(a);
                    continue;
                }
                if (s.equals("alarm-http")) {
                    alarH = motionUtil.str2num(a);
                    continue;
                }
                if (s.equals("area-bx")) {
                    arBx = motionUtil.str2num(a);
                    continue;
                }
                if (s.equals("area-by")) {
                    arBy = motionUtil.str2num(a);
                    continue;
                }
                if (s.equals("area-ex")) {
                    arEx = motionUtil.str2num(a);
                    continue;
                }
                if (s.equals("area-ey")) {
                    arEy = motionUtil.str2num(a);
                    continue;
                }
                if (s.equals("alarmed")) {
                    alarmed = motionUtil.str2num(a) == 1;
                    continue;
                }
                if (s.equals("camera")) {
                    motionData ntry = new motionData(this);
                    i = a.indexOf(" ");
                    ntry.myNum = lst.size() + 1;
                    ntry.myName = a.substring(0, i);
                    ntry.myUrl = a.substring(i + 1, a.length());
                    ntry.sleep = sleep;
                    ntry.imgPre = pre;
                    ntry.imgPost = post;
                    ntry.ignore = ignor;
                    ntry.trigger = trigr;
                    ntry.alarmMail = alarM;
                    ntry.alarmHttp = alarH;
                    ntry.areaBx = arBx;
                    ntry.areaBy = arBy;
                    ntry.areaEx = arEx;
                    ntry.areaEy = arEy;
                    new Thread(ntry).start();
                    lst.add(ntry);
                    continue;
                }
            }
            f.close();
        } catch (Exception e) {
        }
        cams = new motionData[lst.size()];
        for (int i = 0; i < cams.length; i++) {
            cams[i] = lst.get(i);
        }
    }

    /**
     * do one request
     *
     * @param nam name
     * @param pat path
     * @throws java.lang.Exception
     */
    protected void mailAlert(String nam, String pat) throws Exception {
        Socket socket = new Socket(server, 25);
        PrintWriter writer = new PrintWriter(socket.getOutputStream());
        BufferedReader reader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
        String a = reader.readLine();
        writer.print("helo motion\r\n");
        writer.flush();
        a = reader.readLine();
        writer.print("mail from:<" + address + ">\r\n");
        writer.flush();
        a = reader.readLine();
        writer.print("rcpt to:<" + address + ">\r\n");
        writer.flush();
        a = reader.readLine();
        writer.print("data\r\n");
        writer.flush();
        a = reader.readLine();
        writer.print("From: <" + address + ">\r\n");
        writer.print("To: <" + address + ">\r\n");
        writer.print("Subject: check recording from " + nam + "\r\n");
        writer.print("\r\n");
        writer.print("motion detected in " + nam + ", check recording:\r\n");
        writer.print(pat + "\r\n");
        writer.print(".\r\n");
        writer.flush();
        a = reader.readLine();
        writer.print("quit\r\n");
        writer.flush();
        a = reader.readLine();
        socket.close();
    }

    /**
     * do one request
     *
     * @throws java.lang.Exception
     */
    protected void httpAlert() throws Exception {
        URL testUrl = new URI(apiurl).toURL();
        URLConnection testConn = testUrl.openConnection();
        testConn.setConnectTimeout(5000);
        testConn.setReadTimeout(5000);
        BufferedReader testReader = new BufferedReader(new InputStreamReader(testConn.getInputStream()));
        testReader.readLine();
        testReader.close();
    }

    /**
     * do one request
     *
     * @param par parameters
     * @param buf buffer to use
     * @param peer address
     * @return 0 on html result
     * @throws Exception if something went wrong
     */
    public int doRequest(String[] par, ByteArrayOutputStream buf, String peer) throws Exception {
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
            motionData ntry = cams[motionUtil.str2num(nam) - 1];
            ntry.getImage(buf);
            return 1;
        }
        if (cmd.equals("sel")) {
            motionData ntry = cams[motionUtil.str2num(nam) - 1];
            ntry.getAread(buf);
            return 1;
        }
        if (cmd.equals("vid")) {
            motionData ntry = cams[motionUtil.str2num(nam) - 1];
            ntry.getVideo(buf);
            return 2;
        }
        if (cmd.equals("liv")) {
            buf.write(("<!DOCTYPE html><html lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" /><link rel=\"stylesheet\" type=\"text/css\" href=\"index.css\" /><meta http-equiv=refresh content=\"1;url=" + url + "?cmd=liv&nam=" + nam + "\"><title>motion</title></head><body>").getBytes());
            buf.write(("<img src=\"" + url + "?cmd=img&nam=" + nam + "\" height=\"100%\" width=\"100%\">").getBytes());
            buf.write("</body></html>".getBytes());
            return 0;
        }
        if (cmd.equals("arm")) {
            lastSetter = peer;
            timeNeeded = motionUtil.getTime();
            alarmed = motionUtil.str2num(nam) == 1;
            buf.write("<!DOCTYPE html><html lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" /><link rel=\"stylesheet\" type=\"text/css\" href=\"index.css\" /><meta http-equiv=refresh content=\"3;url=/index.html\"><title>motion</title></head><body>".getBytes());
            buf.write(("armed=" + alarmed + "<br/>").getBytes());
            buf.write("</body></html>".getBytes());
            return 0;
        }
        if (cmd.equals("clear")) {
            for (int i = 0; i < cams.length; i++) {
                cams[i].doClear();
            }
            buf.write("<!DOCTYPE html><html lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" /><link rel=\"stylesheet\" type=\"text/css\" href=\"index.css\" /><meta http-equiv=refresh content=\"3;url=/index.html\"><title>motion</title></head><body>".getBytes());
            buf.write(("statistics cleared<br/>").getBytes());
            buf.write("</body></html>".getBytes());
            return 0;
        }
        buf.write(("<!DOCTYPE html><html lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" /><link rel=\"stylesheet\" type=\"text/css\" href=\"index.css\" /><meta http-equiv=refresh content=\"30;url=" + url + "\"><title>motion</title></head><body>").getBytes());
        buf.write("<table><thead><tr><td><b>num</b></td><td><b>name</b></td><td><b>arm</b></td><td><b>hit</b></td><td><b>ago</b></td><td><b>err</b></td><td><b>read</b></td><td><b>sav</b></td><td><b>live</b></td><td><b>min</b></td><td><b>cur</b></td><td><b>max</b></td><td><b>avg</b></td></tr></thead><tbody>".getBytes());
        long tim = motionUtil.getTime();
        for (int i = 0; i < cams.length; i++) {
            String a = cams[i].getMeas(tim);
            buf.write(a.getBytes());
        }
        buf.write(("</tbody></table><br/>armed: " + alarmed + ", " + motionUtil.timePast(tim, timeNeeded) + " ago by " + lastSetter + "<br/>((<a href=\"" + url + "?cmd=arm&nam=1\">arm</a>))((<a href=\"" + url + "?cmd=arm&nam=0\">unarm</a>))((<a href=\"" + url + "?cmd=clear\">clear</a>))<br/></body></html>").getBytes());
        return 0;
    }

}
