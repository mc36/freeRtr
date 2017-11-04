package serv;

import java.io.ByteArrayOutputStream;

/**
 * sample http applet
 *
 * @author matecsaba
 */
public class servHttpApp {

    /**
     * this is needed for cli startup
     *
     * @param args command line parameters
     */
    public static void main(String[] args) {
        servHttpApp app = new servHttpApp();
        String a;
        try {
            ByteArrayOutputStream buf = new ByteArrayOutputStream();
            a = "" + app.getClass().getName();
            a = app.httpRequest("http://localhost/" + a, "./" + a, "cli",
                    "clibrowser", args, buf);
            a = "type=" + a + "\r\ndata:\r\n" + buf;
        } catch (Exception e) {
            a = "exception " + e.getMessage();
        }
        System.out.println(a);
    }

    /**
     * static variable
     */
    public static int stat;

    /**
     * global variable
     */
    public int glob;

    /**
     * do one request
     *
     * @param url url of app
     * @param path path of app
     * @param peer client address
     * @param agent user agent
     * @param par parameters
     * @param buf result buffer, if empty, pathname must present
     * @return [pathname"][file name.]extension
     * @throws Exception if something went wrong
     */
    public String httpRequest(String url, String path, String peer,
            String agent, String[] par, ByteArrayOutputStream buf)
            throws Exception {
        stat++;
        glob++;
        String pars = "";
        for (int i = 0; i < par.length; i++) {
            pars += "<br/>" + par[i];
        }
        String s = "stat=" + stat + "<br/>glob=" + glob + "<br/>url=" + url
                + "<br/>path=" + path + "<br/>peer=" + peer + "<br/>agent="
                + agent + pars + "<br/>";
        buf.write(s.getBytes());
        return "html";
    }

}
