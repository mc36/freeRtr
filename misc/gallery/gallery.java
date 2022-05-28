
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import javax.imageio.ImageIO;

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
        app.url = new URL(url).getPath();
        app.doInit();
        return app.doRequest(par, buf);
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
     * where images located on host
     */
    protected String album = "";

    /**
     * max x size
     */
    protected int maxX = 200;

    /**
     * max Y size
     */
    protected int maxY = 200;

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

    private void readConfig() {
        try {
            String a = path.substring(0, path.lastIndexOf(".")) + ".cfg";
            BufferedReader f = new BufferedReader(new FileReader(a));
            album = f.readLine();
            maxX = str2int(f.readLine());
            maxY = str2int(f.readLine());
            cols = str2int(f.readLine());
            f.close();
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

    /**
     * extensions
     */
    public static final String imageExt = ".jpg.jpeg.png.gif.bmp.";

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
        if (cmd.equals("view")) {
            buf.write(album.getBytes());
            buf.write(nam.getBytes());
            buf.write("\n\n".getBytes());
            int i = nam.lastIndexOf(".");
            buf.write(nam.substring(i, nam.length()).getBytes());
            return "//file//";
        }
        if (cmd.equals("small")) {
            try {
                BufferedImage img1 = ImageIO.read(new File(album + nam));
                int xs = img1.getWidth();
                int ys = img1.getHeight();
                float xr = (float) xs / (float) maxX;
                float yr = (float) ys / (float) maxY;
                if (xr > yr) {
                    xs = (int) (xs / xr);
                    ys = (int) (ys / xr);
                } else {
                    xs = (int) (xs / yr);
                    ys = (int) (ys / yr);
                }
                Image img2 = img1.getScaledInstance(xs, ys, BufferedImage.SCALE_FAST);
                BufferedImage img3 = new BufferedImage(xs, ys, BufferedImage.TYPE_INT_RGB);
                Graphics2D g2d = img3.createGraphics();
                g2d.drawImage(img2, 0, 0, null);
                g2d.dispose();
                ImageIO.write(img3, "jpg", buf);
            } catch (Exception e) {
            }
            return "jpeg";
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
        buf.write("<!DOCTYPE html><html lang=\"en\"><head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" /><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\" /><link rel=\"stylesheet\" type=\"text/css\" href=\"index.css\" /><title>gallery</title></head><body>".getBytes());
        buf.write("listing of <b>".getBytes());
        buf.write(nam.getBytes());
        buf.write("</b><br/>".getBytes());
        for (int i = 0; i < dr.size(); i++) {
            String fn = dr.get(i);
            String a = url + "?nam=" + nam + "/" + fn;
            a = "<a href=\"" + a + "&cmd=browse\">" + fn + "</a><br/>";
            buf.write(a.getBytes());
        }
        buf.write("<table><tr>".getBytes());
        int don = 0;
        for (int o = 0; o < fl.size(); o++) {
            String fn = fl.get(o);
            int i = fn.lastIndexOf(".");
            String b = fn.substring(i, fn.length()).toLowerCase() + ".";
            String a = url + "?nam=" + nam + "/" + fn;
            if (imageExt.indexOf(b) < 0) {
                a = "<td><a href=\"" + a + "&cmd=view\">" + fn + "</td>";
            } else {
                a = "<td><a href=\"" + a + "&cmd=view\"><img src=\"" + a + "&cmd=small\"></td>";
            }
            buf.write(a.getBytes());
            don++;
            if (don < cols) {
                continue;
            }
            don = 0;
            buf.write("</tr><tr>".getBytes());
        }
        buf.write("</tr></table></body></html>".getBytes());
        return "html";
    }

}
