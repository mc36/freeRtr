
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

/**
 * web position
 *
 * @author matecsaba
 */
public class position {

    /**
     * this is needed for cli startup
     *
     * @param args command line parameters
     */
    public static void main(String[] args) {
        position app = new position();
        String a;
        try {
            ByteArrayOutputStream buf = new ByteArrayOutputStream();
            a = "" + app.getClass().getName() + ".";
            a = position.httpRequest("http://localhost/" + a, "./" + a, "cli", "clibrowser", "user", args, buf);
            a = "type=" + a + "\r\ndata:\r\n" + buf.toString();
        } catch (Exception e) {
            a = "exception " + e.getMessage();
        }
        System.out.println(a);
    }

    /**
     * where i'm located in ram
     */
    protected static position staticposition = null;

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
        if (staticposition == null) {
            staticposition = new position();
            staticposition.path = path.substring(0, path.lastIndexOf("."));
            staticposition.url = new URI(url).toURL().getPath();
            staticposition.doInit();
        }
        staticposition.doRequest(par, buf, user);
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
     * measures
     */
    protected positionData meas[];

    /**
     * initialize
     */
    public void doInit() {
        readConfig();
    }

    private void readConfig() {
        try {
            List<positionData> m = new ArrayList<positionData>();
            BufferedReader f = new BufferedReader(new FileReader(path + ".cfg"));
            for (;;) {
                String a = f.readLine();
                if (a == null) {
                    break;
                }
                if (!a.startsWith("measure=")) {
                    continue;
                }
                a = a.substring(8, a.length());
                positionData v = new positionData(a);
                m.add(v);
            }
            f.close();
            meas = new positionData[m.size()];
            for (int i = 0; i < meas.length; i++) {
                meas[i] = m.get(i);
            }
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
        for (int i = 0; i < meas.length; i++) {
            meas[i].getNeighs();
        }
        List<positionAddr> res = new ArrayList<positionAddr>();
        for (int o = 0; o < meas.length; o++) {
            for (int p = 0; p < meas[o].data.size(); p++) {
                positionAddr ntry = meas[o].data.get(p);
                /**
                int i = Collections.binarySearch(res, ntry);
                if (i > 0) {
                    continue;
                }
                res.add(-i - 1, ntry);
                **/
                System.out.println(ntry + " " +ntry.chan+" "+ntry.sign+" "+ positionUtil.signal2distance(ntry.chan,ntry.sign));
            }
        }
        buf.write("<!DOCTYPE html><html lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" /><link rel=\"stylesheet\" type=\"text/css\" href=\"index.css\" /><title>paster</title></head><body>".getBytes());
        String a = "<form action=\"" + url + "\" method=\"post\" enctype=\"application/x-www-form-urlencoded\">";
        buf.write(a.getBytes());
        buf.write("<textarea name=\"data\" rows=\"25\" cols=\"80\"></textarea><br/><input type=\"submit\" value=\"paste\"/></form>".getBytes());
        buf.write("</body></html>".getBytes());
    }

}
