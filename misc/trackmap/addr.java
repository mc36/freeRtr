
import java.io.ByteArrayOutputStream;

/**
 * sample http applet
 *
 * @author matecsaba
 */
public class addr {

    /**
     * this is needed for cli startup
     *
     * @param args command line parameters
     */
    public static void main(String[] args) {
        addr app = new addr();
        String a;
        try {
            ByteArrayOutputStream buf = new ByteArrayOutputStream();
            a = "" + app.getClass();
            a = app.httpRequest("http://localhost/" + a, "./" + a, "cli",
                    "clibrowser", "user", args, buf);
            a = "type=" + a + "\r\ndata:\r\n" + buf;
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
    public String httpRequest(String url, String path, String peer,
            String agent, String user, String[] par, ByteArrayOutputStream buf)
            throws Exception {
        String s = "<html><head><title>address</title></head><body bgcolor=black text=white>" + peer + "</body></html>";
        buf.write(s.getBytes());
        return "html";
    }

}
