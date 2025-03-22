
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
                int i = Collections.binarySearch(res, ntry);
                if (i >= 0) {
                    continue;
                }
                res.add(-i - 1, ntry);
                positionAddr adr1 = ntry;
                positionAddr adr2 = null;
                positionAddr adr3 = null;
                positionData msr1 = meas[o];
                positionData msr2 = null;
                positionData msr3 = null;
                for (int q = o + 1; q < meas.length; q++) {
                    i = Collections.binarySearch(meas[q].data, ntry);
                    if (i < 0) {
                        continue;
                    }
                    positionAddr curr = meas[q].data.get(i);
                    if (curr.sign > adr1.sign) {
                        adr3 = adr2;
                        adr2 = adr1;
                        adr1 = curr;
                        msr3 = msr2;
                        msr2 = msr1;
                        msr1 = meas[q];
                        continue;
                    }
                    if ((adr2 == null) || (curr.sign > adr2.sign)) {
                        adr3 = adr2;
                        adr2 = curr;
                        msr3 = msr2;
                        msr2 = meas[q];
                        continue;
                    }
                    if (adr3 == null || (curr.sign > adr3.sign)) {
                        adr3 = curr;
                        msr3 = meas[q];
                        continue;
                    }
                }
                System.out.println(ntry+" "+adr1 + " " + adr2 + " " + adr3);
                if (adr3 == null) {
                    continue;
                }
                double[] val = new double[3];
                val[0] = positionUtil.signal2distance(adr1.chan, adr1.sign);
                val[1] = positionUtil.signal2distance(adr2.chan, adr2.sign);
                val[2] = positionUtil.signal2distance(adr3.chan, adr3.sign);
                val = positionUtil.trilateration(msr1.myX, msr1.myY, msr2.myX, msr2.myY, msr3.myX, msr3.myY, val[0], val[1], val[2]);
                ntry.curX = val[0];
                ntry.curY = val[1];
                System.out.println(ntry + " " + ntry.curX + " " + ntry.curY);
            }
        }
        buf.write("<!DOCTYPE html><html lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" /><link rel=\"stylesheet\" type=\"text/css\" href=\"index.css\" /><title>paster</title></head><body>".getBytes());
        String a = "<form action=\"" + url + "\" method=\"post\" enctype=\"application/x-www-form-urlencoded\">";
        buf.write(a.getBytes());
        buf.write("<textarea name=\"data\" rows=\"25\" cols=\"80\"></textarea><br/><input type=\"submit\" value=\"paste\"/></form>".getBytes());
        buf.write("</body></html>".getBytes());
    }

}
