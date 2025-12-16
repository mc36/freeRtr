
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileReader;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * web mail reader
 *
 * @author matecsaba
 */
public class mailer {

    /**
     * this is needed for cli startup
     *
     * @param args command line parameters
     */
    public static void main(String[] args) {
        mailer app = new mailer();
        String a;
        try {
            ByteArrayOutputStream buf = new ByteArrayOutputStream();
            a = "" + app.getClass().getName() + ".";
            a = mailer.httpRequest("http://localhost/" + a, "./" + a, "cli", "clibrowser", "user", args, buf);
            a = "type=" + a + "\r\ndata:\r\n" + buf.toString();
        } catch (Exception e) {
            a = "exception " + e.getMessage();
        }
        System.out.println(a);
    }

    /**
     * where i'm located in ram
     */
    protected static mailer staticMailer = null;

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
        if (staticMailer == null) {
            staticMailer = new mailer();
            staticMailer.path = path.substring(0, path.lastIndexOf("."));
            staticMailer.url = new URI(url).toURL().getPath();
            staticMailer.doInit();
        }
        staticMailer.doRequest(par, buf, user);
        return "html";
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
     * where mail located on host
     */
    protected String mail = "";

    /**
     * initialize
     */
    public void doInit() {
        readConfig();
    }

    private void readConfig() {
        try {
            BufferedReader f = new BufferedReader(new FileReader(path + ".cfg"));
            mail = f.readLine();
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
        String id = "";
        for (int i = 0; i < par.length; i++) {
            String a = par[i];
            int o = a.indexOf("=");
            if (o < 1) {
                continue;
            }
            String b = a.substring(0, o);
            a = a.substring(o + 1, a.length());
            if (b.equals("id")) {
                id = a;
            }
        }
        if (id.length() > 0) {
            buf.write("<!DOCTYPE html><html lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" /><link rel=\"stylesheet\" type=\"text/css\" href=\"index.css\" /><title>mailer</title></head><body>".getBytes());
            try {
                BufferedReader f = new BufferedReader(new FileReader(mail + user + "/" + id));
                while (f.ready()) {
                    buf.write(f.readLine().getBytes());
                    buf.write("<br/>".getBytes());
                }
                f.close();
            } catch (Exception e) {
            }
            buf.write("</body></html>".getBytes());
            return;
        }
        File[] fls;
        try {
            fls = new File(mail + user + "/").listFiles();
        } catch (Exception e) {
            return;
        }
        List<File> fl = new ArrayList<File>();
        for (int i = 0; i < fls.length; i++) {
            fl.add(fls[i]);
        }
        Collections.sort(fl);
        buf.write("<!DOCTYPE html><html lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" /><link rel=\"stylesheet\" type=\"text/css\" href=\"index.css\" /><title>mailer</title></head><body><table>".getBytes());
        buf.write("<tr><td><b>from</b></td><td><b>subject</b></td><td><b>date</b></td></tr>".getBytes());
        for (int fn = fl.size() - 1; fn >= 0; fn--) {
            String a = fl.get(fn).getName();
            if (!a.endsWith(".msg")) {
                continue;
            }
            String u = url + "?id=" + a;
            String from = "unknown";
            String subj = "unspecified";
            String date = "unknown";
            try {
                BufferedReader f = new BufferedReader(new FileReader(mail + user + "/" + a));
                while (f.ready()) {
                    a = f.readLine();
                    if (a == null) {
                        break;
                    }
                    if (a.length() < 1) {
                        break;
                    }
                    int i = a.indexOf(":");
                    if (i < 0) {
                        continue;
                    }
                    String b = a.substring(i + 1, a.length()).trim();
                    a = a.substring(0, i).trim().toLowerCase();
                    if (a.equals("from")) {
                        from = b;
                        continue;
                    }
                    if (a.equals("subject")) {
                        subj = b;
                        continue;
                    }
                    if (a.equals("date")) {
                        date = b;
                        continue;
                    }
                }
                f.close();
            } catch (Exception e) {
                continue;
            }
            a = "<tr><td><a href=" + u + ">" + from + "</a></td><td><a href=" + u + ">" + subj + "</a></td><td><a href=" + u + ">" + date + "</a></td></tr>";
            buf.write(a.getBytes());
        }
        buf.write("</table></body></html>".getBytes());
    }

}
