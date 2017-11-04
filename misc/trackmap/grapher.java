
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import javax.imageio.ImageIO;

/**
 * graph drawer
 * @author matecsaba
 */
public class grapher {

/**
 * this is needed for cli startup
 *
 * @param args command line parameters
 */
public static void main(String[] args) {
  grapher app = new grapher();
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
    str = par[0];
    str = str.substring(str.indexOf("=") + 1, str.length());
    BufferedImage img = doImage();
    ImageIO.write(img, "png", buf);
    return "png";
  } catch (Exception e) {
    buf.write(path.getBytes());
    buf.write("...exception happened...".getBytes());
    return "txt";
  }
}

private String str;

private int max;

private String word() {
  str = str.trim();
  int i = str.indexOf(" ");
  if (i < 0) {
    String a = str;
    str = "";
    return a;
  }
  String a = str.substring(0, i).trim();
  str = str.substring(i, str.length()).trim();
  return a;
}

private static int str2num(String s) {
  int i = 0;
  s = s.trim();
  try {
    i = Integer.parseInt(s, 10);
  } catch (Exception e) {
  }
  return i;
}

private int location(int i) {
  if (i<0) i=0;
  return  ((max - i) * 570) / max;
}

private BufferedImage doImage() {
  List<Integer> rx = new ArrayList<Integer>();
  List<Integer> tx = new ArrayList<Integer>();
  List<Integer> dr = new ArrayList<Integer>();
  for (;;) {
    String s = word();
    if (s.length() < 1) {
      break;
    }
    if (!s.endsWith("s")) {
      continue;
    }
    word();
    rx.add(str2num(word()));
    word();
    tx.add(str2num(word()));
    word();
    dr.add(str2num(word()));
  }
  max = 0;
  for (int i = 0; i < rx.size(); i++) {
    int o = rx.get(i);
    if (o > max) {
      max = o;
    }
    o = tx.get(i);
    if (o > max) {
      max = o;
    }
  }
  BufferedImage img = new BufferedImage(800, 600, BufferedImage.TYPE_INT_RGB);
  Graphics2D g2d = img.createGraphics();
  g2d.setBackground(Color.gray);
  g2d.setFont(new Font("Serif", Font.BOLD, 20));
  g2d.setPaint(Color.gray);
  g2d.fillRect(0, 0, img.getWidth(), img.getHeight());
  g2d.setPaint(Color.black);
  for (int i = 0; i < 10; i++) {
    int o = (max * i) / 10;
    String a = "" + o;
    if (o > 4096) {
      o /= 1024;
      a = o + "k";
      if (o > 4096) {
        o /= 1024;
        a = o + "m";
      }
    }
    g2d.drawString(a, 5, (10 - i) * 57);
    o = (rx.size() * i) / 10;
    g2d.drawString(o + "s", 80 + (i * 70), 595);
  }
  g2d.drawLine(80, 0, 80, 600);
  g2d.drawLine(0, 570, 800, 570);
  int or = location(rx.get(0));
  int ot = location(tx.get(0));
  int od = location(dr.get(0));
  int ox = 80;
  for (int i = 1; i < rx.size(); i++) {
    int cr = location(rx.get(i));
    int ct = location(tx.get(i));
    int cd = location(dr.get(i));
    int cx = 80 + ((i * 700) / rx.size());
    g2d.setPaint(Color.green);
    g2d.drawLine(ox, or, cx, cr);
    g2d.setPaint(Color.red);
    g2d.drawLine(ox, ot, cx, ct);
    g2d.setPaint(Color.black);
    g2d.drawLine(ox, od, cx, cd);
    or = cr;
    ot = ct;
    od = cd;
    ox = cx;
  }
  g2d.dispose();
  return img;
}

}
