
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * web image gallery
 *
 * @author matecsaba
 */
public class gallery {

    /**
     * this is needed for cli startup
     *
     * @param args command line parameters
     */
    public static void main(String[] args) {
        gallery app = new gallery();
        String a;
        try {
            ByteArrayOutputStream buf = new ByteArrayOutputStream();
            a = "" + app.getClass().getName() + ".";
            a = gallery.httpRequest("http://localhost/" + a, "./" + a, "cli", "clibrowser", "user", args, buf);
            a = "type=" + a + "\r\ndata:\r\n" + buf.toString();
        } catch (Exception e) {
            a = "exception " + e.getMessage();
        }
        System.out.println(a);
    }

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
        gallery app = new gallery();
        app.path = path;
        app.url = new URI(url).toURL().getPath();
        app.doInit();
        return app.doRequest(par, buf);
    }

    /**
     * where i'm located on host
     */
    protected String path;

    /**
     * what displayed as header / folder
     */
    protected String index = "index.txt";

    /**
     * where i'm located on net
     */
    protected String url;

    /**
     * where images located on host
     */
    protected String album = "/data/www/";

    /**
     * columns
     */
    protected int cols = 3;

    /**
     * initialize
     */
    public void doInit() {
        readConfig();
    }

    /**
     * load buffer from text file
     *
     * @param fn name of file
     * @return list of lines, null if error happened
     */
    public static List<String> txt2lst(String fn) {
        List<String> ln = new ArrayList<String>();
        try {
            FileInputStream in = new FileInputStream(fn);
            BufferedReader rd = new BufferedReader(new InputStreamReader(in));
            while (rd.ready()) {
                ln.add(rd.readLine());
            }
            rd.close();
            in.close();
        } catch (Exception e) {
            return null;
        }
        return ln;
    }

    private void readConfig() {
        String a = path.substring(0, path.lastIndexOf(".")) + ".cfg";
        List<String> l = txt2lst(a);
        try {
            album = l.get(0);
            cols = str2int(l.get(1));
            path = path.substring(0, path.lastIndexOf("/") + 1);
            index = l.get(2);
        } catch (Exception e) {
        }
    }

    /**
     * string to integer
     *
     * @param s string
     * @return integer
     */
    public static int str2int(String s) {
        try {
            return Integer.parseInt(s);
        } catch (Exception e) {
            return 0;
        }
    }

    public String getExt(String fn) {
        int i = fn.lastIndexOf(".");
        if (i < 0) {
            return "";
        }
        return fn.substring(i, fn.length()).toLowerCase() + ".";
    }

    /**
     * extensions
     */
    public final static String imageExt = ".jpg.jpeg.png.gif.bmp.";

    /**
     * extensions
     */
    public final static String videoExt = ".avi.mp4.mov.";

    /**
     * extensions
     */
    public final static String textExt = ".pdf.txt.docx.odt.";

    /**
     * do one request
     *
     * @param par parameters
     * @param buf buffer to use
     * @return type
     * @throws Exception if something went wrong
     */
    public String doRequest(String[] par, ByteArrayOutputStream buf) throws Exception {
        String nam = "";
        String cmd = "";
        for (int i = 0; i < par.length; i++) {
            String a = par[i];
            int o = a.indexOf("=");
            if (o < 1) {
                continue;
            }
            String b = a.substring(0, o);
            a = a.substring(o + 1, a.length());
            if (b.equals("nam")) {
                nam = a;
                continue;
            }
            if (b.equals("cmd")) {
                cmd = a;
                continue;
            }
        }
        nam = nam.replaceAll("/./", "/");
        nam = nam.replaceAll("/../", "/");
        if (cmd.equals("view")) {
            buf.write(album.getBytes());
            buf.write(nam.getBytes());
            buf.write("\n\n".getBytes());
            int i = nam.lastIndexOf(".");
            buf.write(nam.substring(i, nam.length()).getBytes());
            return "//file//";
        }
        if (cmd.equals("small")) {
            buf.write(album.getBytes());
            buf.write(nam.getBytes());
            buf.write(".thumb".getBytes());
            buf.write("\n\n".getBytes());
            buf.write(".jpg".getBytes());
            if (new File(album + nam + ".thumb").exists()) {
                return "//file//";
            }
            String a = getExt(nam);
            String todo[] = null;
            if (imageExt.indexOf(a) >= 0) {
                todo = new String[2];
                todo[0] = path + "thumbImg.sh";
                todo[1] = album + nam;
            }
            if (videoExt.indexOf(a) >= 0) {
                todo = new String[2];
                todo[0] = path + "thumbVid.sh";
                todo[1] = album + nam;
            }
            if (textExt.indexOf(a) >= 0) {
                todo = new String[2];
                todo[0] = path + "thumbTxt.sh";
                todo[1] = album + nam;
            }
            if (todo != null) {
                Runtime rtm = Runtime.getRuntime();
                Process prc = rtm.exec(todo);
                prc.waitFor();
            }
            return "//file//";
        }
        File[] fls = new File[0];
        try {
            fls = new File(album + nam).listFiles();
        } catch (Exception e) {
        }
        List<String> fl = new ArrayList<String>();
        List<String> dr = new ArrayList<String>();
        for (int i = 0; i < fls.length; i++) {
            File f = fls[i];
            String n = f.getName();
            if (n.startsWith(".")) {
                continue;
            }
            if (f.isDirectory()) {
                dr.add(n);
            } else {
                fl.add(n);
            }
        }
        Collections.sort(dr);
        Collections.sort(fl);
        List<String> txt = txt2lst(album + nam + "/" + index);
        if (txt == null) {
            txt = new ArrayList<String>();
        }
        if (txt.size() < 1) {
            txt.add("gallery of " + nam);
            txt.add("");
            txt.add("<pre>");
            txt.add("listing of " + nam + ":");
            txt.add("</pre>");
        } else {
            for (int i = txt.size() - 1; i >= 0; i--) {
                String a = txt.get(i);
                int pb = a.indexOf("[") + 1;
                int pe = a.indexOf("]");
                if (pb < 0) {
                    continue;
                }
                if (pe < 0) {
                    continue;
                }
                if (pb >= pe) {
                    continue;
                }
                String b = a.substring(pb, pe).trim();
                a = a.substring(0, pb) + "<a href=\"http://" + b + "\">" + b + "</a>" + a.substring(pe, a.length());
                txt.remove(i);
                txt.add(i, a);
            }
            txt.add(1, "<pre>");
            txt.add("</pre>");
        }
        buf.write("<!DOCTYPE html><html lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" /><link rel=\"stylesheet\" type=\"text/css\" href=\"index.css\" /><title>".getBytes());
        buf.write(txt.get(0).getBytes());
        buf.write("</title></head><body>".getBytes());
        buf.write("<h1><b><u>".getBytes());
        buf.write(txt.get(0).getBytes());
        buf.write("</u></b></h1>".getBytes());
        for (int i = 1; i < txt.size(); i++) {
            buf.write(txt.get(i).getBytes());
            buf.write(13);
            buf.write(10);
        }
        buf.write("<br/><hr/><u>subfolders:</u><br/>".getBytes());
        for (int i = 0; i < dr.size(); i++) {
            String fn = dr.get(i);
            String a = url + "?nam=" + nam + "/" + fn;
            a = "<a href=\"" + a + "&cmd=browse\">" + fn + "</a><br/>";
            buf.write(a.getBytes());
        }
        buf.write("<br/><hr/><u>files:</u><br/>".getBytes());
        buf.write("<table><tr>".getBytes());
        int don = 0;
        for (int o = 0; o < fl.size(); o++) {
            if (don >= cols) {
                buf.write("</tr><tr>".getBytes());
                don = 0;
            }
            String fn = fl.get(o);
            String a = url + "?nam=" + nam + "/" + fn;
            if (fl.indexOf(fn + ".thumb") >= 0) {
                a = "<td><a href=\"" + a + "&cmd=view\"><img src=\"" + a + ".thumb&cmd=view\"><br/>" + fn + "</td>";
                buf.write(a.getBytes());
                don++;
                continue;
            }
            String b = getExt(fn);
            if (b.equals(".thumb.")) {
                continue;
            }
            a = "<td><a href=\"" + a + "&cmd=view\"><img src=\"" + a + "&cmd=small\"><br/>" + fn + "</td>";
            buf.write(a.getBytes());
            don++;
        }
        buf.write("</tr></table></body></html>".getBytes());
        return "html";
    }

}
