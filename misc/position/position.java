
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileReader;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.imageio.ImageIO;

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
            a = app.httpRequest("http://localhost/" + a, "./" + a, "cli", "clibrowser", "user", args, buf);
            a = "type=" + a + "\r\ndata:\r\n" + buf.toString();
        } catch (Exception e) {
            a = "exception " + e.getMessage();
        }
        System.out.println(a);
    }

    private positionData[] meas;

    private List<positionAddr> neis;

    private BufferedImage img;

    private float scale;

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
        path = path.substring(0, path.lastIndexOf("."));
        url = new URI(url).toURL().getPath();
        img = ImageIO.read(new File(path + ".png"));
        readConfig(path);
        readNeighs();
        drawImage();
        ImageIO.write(img, "png", buf);
        return "png";
    }

    private void readConfig(String a) throws Exception {
        List<positionData> m = new ArrayList<positionData>();
        BufferedReader f = new BufferedReader(new FileReader(a + ".cfg"));
        for (;;) {
            a = f.readLine();
            if (a == null) {
                break;
            }
            if (a.startsWith("scale=")) {
                a = a.substring(6, a.length());
                scale = positionUtil.str2num(a);
                continue;
            }
            if (!a.startsWith("measure=")) {
                continue;
            }
            a = a.substring(8, a.length());
            positionData v = new positionData(a);
            m.add(v);
        }
        f.close();
        positionData[] r = new positionData[m.size()];
        for (int i = 0; i < r.length; i++) {
            r[i] = m.get(i);
        }
        meas = r;
    }

    private void readNeighs() throws Exception {
        for (int i = 0; i < meas.length; i++) {
            meas[i].getNeighs();
        }
        neis = new ArrayList<positionAddr>();
        for (int o = 0; o < meas.length; o++) {
            for (int p = 0; p < meas[o].data.size(); p++) {
                positionAddr ntry = meas[o].data.get(p);
                int i = Collections.binarySearch(neis, ntry);
                if (i >= 0) {
                    continue;
                }
                neis.add(-i - 1, ntry);
                ntry.curX = - 1;
                ntry.curY = - 1;
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
                if (adr3 == null) {
                    continue;
                }
                double[] val = new double[3];
                val[0] = positionUtil.signal2distance(adr1.chan, adr1.sign) * scale;
                val[1] = positionUtil.signal2distance(adr2.chan, adr2.sign) * scale;
                val[2] = positionUtil.signal2distance(adr3.chan, adr3.sign) * scale;
                val = positionUtil.trilateration(msr1.posX, msr1.posY, msr2.posX, msr2.posY, msr3.posX, msr3.posY, val[0], val[1], val[2]);
                ntry.curX = val[0];
                ntry.curY = val[1];
                ntry.good = true;
            }
        }
    }

    private void drawImage() {
        int mx = img.getWidth();
        int my = img.getHeight();
        for (int i = 0; i < meas.length; i++) {
            Graphics2D g2d = img.createGraphics();
            g2d.setBackground(Color.blue);
            g2d.setFont(new Font("Serif", Font.BOLD, 20));
            g2d.setPaint(Color.blue);
            g2d.drawString(meas[i].nam, (int) meas[i].posX, (int) meas[i].posY);
            g2d.dispose();
        }
        int py = my;
        for (int i = 0; i < neis.size(); i++) {
            positionAddr ntry = neis.get(i);
            Graphics2D g2d = img.createGraphics();
            FontMetrics fm = g2d.getFontMetrics();
            g2d.setBackground(Color.gray);
            g2d.setFont(new Font("Serif", Font.BOLD, 20));
            String s = ntry.getMac();
            if (!ntry.good) {
                g2d.setPaint(Color.red);
                py -= fm.getHeight();
                g2d.drawString(s, mx - fm.stringWidth(s), py);
                g2d.dispose();
                continue;
            }
            if (ntry.curX < 0) {
                ntry.curX = 0;
            }
            if (ntry.curY < 0) {
                ntry.curY = 0;
            }
            int o = mx - fm.stringWidth(s);
            if (ntry.curX > o) {
                ntry.curX = o;
            }
            o = my - fm.getHeight();
            if (ntry.curY > o) {
                ntry.curY = o;
            }
            g2d.setPaint(Color.green);
            g2d.drawString(s, (int) ntry.curX, (int) ntry.curY);
            g2d.dispose();
        }
    }

}
