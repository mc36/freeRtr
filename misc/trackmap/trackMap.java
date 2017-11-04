
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import javax.imageio.ImageIO;

/**
 * tracker mapper applet
 *
 * @author matecsaba
 */
public class trackMap {

/**
 * this is needed for cli startup
 *
 * @param args command line parameters
 */
public static void main(String[] args) {
  trackMap app = new trackMap();
  String a;
  try {
    ByteArrayOutputStream buf = new ByteArrayOutputStream();
    a = "" + app.getClass().getName();
    a = app.httpRequest("http://localhost/" + a, "./" + a, "cli", "clibrowser", args, buf);
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
 * @param par parameters
 * @param buf result buffer, if empty, pathname must present
 * @return [pathname"][file name.]extension
 * @throws Exception if something went wrong
 */
public String httpRequest(String url, String path, String peer, String agent, String[] par, ByteArrayOutputStream buf) throws Exception {
  try {
    int i = path.lastIndexOf("/");
    path = path.substring(0, i + 1);
    String s = par[0];
    i = s.lastIndexOf("=");
    s = s.substring(0, i);
    BufferedImage img = ImageIO.read(new File(path + s));
    for (int o = 1; o < par.length; o++) {
      s = par[o];
      i = s.lastIndexOf("=");
      s = s.substring(0, i);
      i = s.lastIndexOf("-");
      boolean up = s.substring(i + 1, s.length()).equals("up");
      s = s.substring(0, i);
      i = s.lastIndexOf("-");
      int y = Integer.parseInt(s.substring(i + 1, s.length()));
      s = s.substring(0, i);
      i = s.lastIndexOf("-");
      int x = Integer.parseInt(s.substring(i + 1, s.length()));
      s = s.substring(0, i);
      Graphics2D g2d = img.createGraphics();
      g2d.setBackground(Color.gray);
      g2d.setFont(new Font("Serif", Font.BOLD, 20));
      g2d.setPaint(Color.black);
      g2d.drawString(s, x - 1, y - 1);
      g2d.drawString(s, x - 1, y);
      g2d.drawString(s, x - 1, y + 1);
      g2d.drawString(s, x, y - 1);
      g2d.drawString(s, x, y + 1);
      g2d.drawString(s, x + 1, y - 1);
      g2d.drawString(s, x + 1, y);
      g2d.drawString(s, x + 1, y + 1);
      g2d.setPaint(up ? Color.green : Color.red);
      g2d.drawString(s, x, y);
      g2d.dispose();
    }
    ImageIO.write(img, "png", buf);
    return "png";
  } catch (Exception e) {
    buf.write("...exception happened...".getBytes());
    return "txt";
  }
}

}
