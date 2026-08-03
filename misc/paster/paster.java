
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.net.URI;
import java.util.Random;

/**
 * web pastebin
 *
 * @author matecsaba
 */
public class paster {

    /**
     * this is needed for cli startup
     *
     * @param args command line parameters
     */
    public static void main(String[] args) {
        paster app = new paster();
        String a;
        try {
            ByteArrayOutputStream buf = new ByteArrayOutputStream();
            a = "" + app.getClass().getName() + ".";
            a = paster.httpRequest("http://localhost/" + a, "./" + a, "cli", "clibrowser", "user", args, buf);
            a = "type=" + a + "\r\ndata:\r\n" + buf.toString();
        } catch (Exception e) {
            a = "exception " + e.getMessage();
        }
        System.out.println(a);
    }

    /**
     * where i'm located in ram
     */
    protected static paster staticpaster = null;

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
        if (staticpaster == null) {
            staticpaster = new paster();
            staticpaster.path = path.substring(0, path.lastIndexOf("."));
            staticpaster.proto = new URI(url).toURL().getProtocol() + "://";
            staticpaster.url = new URI(url).toURL().getHost();
            staticpaster.mynam = new URI(url).toURL().getFile();
            staticpaster.doInit();
        }
        staticpaster.doRequest(par, buf, user);
        return "html";
    }

    /**
     * where i'm located on host
     */
    protected String path;

    /**
     * where i'm located on net
     */
    protected String proto;

    /**
     * where i'm located on net
     */
    protected String url;

    /**
     * where i'm located on net
     */
    protected String mynam;

    /**
     * where paste located on host
     */
    protected String paste = "";

    /**
     * initialize
     */
    public void doInit() {
        readConfig();
    }

    private void readConfig() {
        try {
            BufferedReader f = new BufferedReader(new FileReader(path + ".cfg"));
            paste = f.readLine();
            f.close();
        } catch (Exception e) {
        }
    }

    /**
     * do one request
     *
     * @param par parameters
     * @param buf buffer to use
     * @param user username
     * @throws Exception if something went wrong
     */
    public void doRequest(String[] par, ByteArrayOutputStream buf, String user) throws Exception {
        String data = "";
        for (int i = 0; i < par.length; i++) {
            String a = par[i];
            int o = a.indexOf("=");
            if (o < 1) {
                continue;
            }
            String b = a.substring(0, o);
            a = a.substring(o + 1, a.length());
            if (b.equals("data")) {
                data = a;
            }
        }
        if (data.length() > 0) {
            buf.write("<!DOCTYPE html><html lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" /><link rel=\"stylesheet\" type=\"text/css\" href=\"index.css\" /><title>paster</title></head><body><pre>".getBytes());
            Random rnd = new Random();
            String fn = (rnd.nextInt() & 0x7fffffff) + "-" + rnd.nextInt() + "-" + rnd.nextInt() + ".txt";
            try {
                BufferedWriter wrtr = new BufferedWriter(new FileWriter(paste + fn));
                wrtr.write(data);
                wrtr.close();
            } catch (Exception e) {
            }
            String a = "your paste is:<br/>\n\n" + proto + url +"/"+ fn+"\n\n";
            buf.write(a.getBytes());
            buf.write("<pre></body></html>".getBytes());
            return;
        }
        buf.write("<!DOCTYPE html><html lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" /><link rel=\"stylesheet\" type=\"text/css\" href=\"index.css\" /><title>paster</title></head><body>".getBytes());
        String a = "<form action=\"" + proto + url + mynam + "\" method=\"post\" enctype=\"application/x-www-form-urlencoded\">";
        buf.write(a.getBytes());
        buf.write("<textarea name=\"data\" rows=\"25\" cols=\"80\"></textarea><br/><input type=\"submit\" value=\"paste\"/></form>".getBytes());
        buf.write("</body></html>".getBytes());
    }

}
