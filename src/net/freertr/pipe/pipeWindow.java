package net.freertr.pipe;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import javax.imageio.ImageIO;
import javax.swing.JFrame;
import javax.swing.JPanel;
import net.freertr.user.userFonts;
import net.freertr.user.userScreen;
import net.freertr.util.bits;
import net.freertr.util.logger;

/**
 * one terminal window
 *
 * @author matecsaba
 */
public class pipeWindow extends JPanel {

    private static final long serialVersionUID = 10251979;

    /**
     * pipe image
     */
    transient protected pipeImage img;

    /**
     * buffered image
     */
    transient protected BufferedImage img3;

    /**
     * window handler
     */
    protected JFrame win;

    /**
     * convert image to ansi
     *
     * @param ps pipe to draw
     * @param fn file to convert
     * @return converted ansi
     */
    public static userScreen imageAnsi(pipeSide ps, String fn) {
        int[] chr = new int[3];
        chr[0] = 0x30;
        chr[1] = 0x40;
        chr[2] = 0x4f;
        return imageAnsi(ps, new File(fn), chr);
    }

    /**
     * quantize one color
     *
     * @param orig original color
     * @param pal palette to use
     * @return quantized color
     */
    public final static int trueColor2indexedColor(int orig, int[] pal) {
        int truncer = 0x00e0e0e0;
        orig &= truncer;
        int best = -1;
        int diff = 0xffffff;
        for (int i = 0; i < pal.length; i++) {
            int cur = pal[i] & truncer;
            if (cur == orig) {
                return i;
            }
            int dff = cur - orig;
            dff &= truncer;
            dff >>>= 5;
            int red = (dff >>> 16) & 3;
            int grn = (dff >>> 8) & 3;
            int blu = dff & 3;
            dff = (red << 16) + (grn << 8) + blu;
            if (diff < dff) {
                continue;
            }
            diff = dff;
            best = i;
        }
        return best;
    }

    /**
     * convert image to ansi
     *
     * @param ps pipe to draw
     * @param fil file to convert
     * @param chr characters to put
     * @return converted ansi
     */
    public static userScreen imageAnsi(pipeSide ps, File fil, int[] chr) {
        //////        fil = new File("/nfs2/own/web/fun/cats-black18.jpg");////////////////////////
        int chs = chr.length;
        String[] str = new String[chs];
        for (int i = 0; i < chs; i++) {
            str[i] = "" + chr[i];
        }
        userScreen scr = new userScreen(ps);
        scr.putCls();
        scr.putCur(0, 0);
        try {
            BufferedImage srcI = ImageIO.read(fil);
            ////            Graphics2D g = src.createGraphics();
            ////            g.drawImage(src, 0, 0, scr.sizX, scr.sizY, null);
            ////            g.dispose();
            List<String> txtD = imageText(srcI, scr.sizX, scr.sizY, str);
            int txtS = txtD.size();
            for (int cy = 0; cy < scr.sizY; cy++) {
                if (cy >= txtS) {
                    continue;
                }
                String a = txtD.get(cy);
                byte[] b = a.getBytes();
                int mx = b.length;
                for (int cx = 0; cx < scr.sizX; cx++) {
                    if (cx >= mx) {
                        continue;
                    }
                    if (b[cx] == 0x20) {
                        continue;
                    }
                    int i;
                    try {
                        i = srcI.getRGB(cx * 2, cy);
                    } catch (Exception e) {
                        i = 0;
                    }
                    int o = trueColor2indexedColor(i, userFonts.colorData);
                    scr.putInt(cx, cy, false, o, chr[bits.random(0, chs)]);
                }
            }
        } catch (Exception e) {
            logger.traceback(e, "error converting");
        }
        scr.refresh();
        return scr;
    }

    /**
     * convert image to string
     *
     * @param fil file
     * @param maxX max x value
     * @param maxY max y value
     * @param chrs chars to use
     * @return converted text
     */
    public static List<String> imageText(File fil, int maxX, int maxY, final String[] chrs) {
        BufferedImage img1 = null;
        try {
            img1 = ImageIO.read(fil);
        } catch (Exception e) {
            logger.traceback(e, "while converting");
        }
        if (img1 == null) {
            return new ArrayList<String>();
        }
        return imageText(img1, maxX, maxY, chrs);
    }

    /**
     * convert image to string
     *
     * @param img1 image to convert
     * @param maxX max x value
     * @param maxY max y value
     * @param chrs chars to use
     * @return converted text
     */
    public static List<String> imageText(BufferedImage img1, int maxX, int maxY, final String[] chrs) {
        List<String> txt = new ArrayList<String>();
        maxX = (img1.getWidth() / maxX) + 1;
        maxY = (img1.getHeight() / maxY) + 1;
        maxX /= 2;
        txt.add("res=" + maxX + "x" + maxY);
        int tmp = maxX < maxY ? maxY : maxX;
        BufferedImage img2 = new BufferedImage(img1.getWidth() / tmp, img1.getHeight() / tmp, BufferedImage.TYPE_USHORT_GRAY);
        Graphics2D g = img2.createGraphics();
        g.drawImage(img1, 0, 0, img2.getWidth(), img2.getHeight(), null);
        g.dispose();
        g.setComposite(AlphaComposite.Src);
        g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
        g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        int[][] img3 = new int[img2.getHeight()][img2.getWidth()];
        tmp = 0;
        for (int y = 0; y < img3.length; y++) {
            for (int x = 0; x < img3[0].length; x++) {
                int v = img2.getRGB(x, y);
                if (v < 0) {
                    v = -v;
                }
                img3[y][x] = v;
                if (tmp < v) {
                    tmp = v;
                }
            }
        }
        img2 = null;
        for (int y = 0; y < img3.length; y++) {
            String a = "";
            for (int x = 0; x < img3[0].length; x++) {
                int v = (img3[y][x] * (chrs.length - 1)) / tmp;
                a += chrs[v];
                a += chrs[v];
            }
            txt.add(a);
        }
        return txt;
    }

    /**
     * create image
     *
     * @param img image
     * @return created
     */
    public static BufferedImage createImage(pipeImage img) {
        return new BufferedImage(img.img2[0].length, img.img2.length, BufferedImage.TYPE_3BYTE_BGR);
    }

    /**
     * update image
     *
     * @param img3 target image
     * @param img source image
     */
    public static void updateImage(BufferedImage img3, pipeImage img) {
        for (int y = 0; y < img.img2.length; y++) {
            for (int x = 0; x < img.img2[0].length; x++) {
                img3.setRGB(x, y, img.img2[y][x]);
            }
        }
    }

    /**
     * convert image
     *
     * @param img image
     * @return converted
     */
    public static BufferedImage convertImage(pipeImage img) {
        BufferedImage img3 = createImage(img);
        updateImage(img3, img);
        return img3;
    }

    /**
     * get keyboard listener
     *
     * @param pipe pipe to use
     * @return keyboard listener
     */
    public static KeyListener getKeyLstnr(pipeSide pipe) {
        return new pipeWindowKey(pipe);
    }

    /**
     * create window
     *
     * @param x x size
     * @param y y size
     * @param fnt font to use
     * @param plt palette to use
     * @return pipeline to use, null if failed
     */
    public static pipeSide createOne(int x, int y, byte[][][] fnt, int[] plt) {
        pipeLine pip = new pipeLine(65536, false);
        pipeWindow win;
        try {
            win = new pipeWindow(pip.getSide(), x, y, fnt, plt);
        } catch (Exception e) {
            logger.traceback(e, "while converting");
            return null;
        }
        win.startWindow();
        pipeSide ps = pip.getSide();
        ps.lineTx = pipeSide.modTyp.modeCRLF;
        ps.lineRx = pipeSide.modTyp.modeCRorLF;
        ps.setTime(0);
        ps.setReady();
        return ps;
    }

    /**
     * get background color
     *
     * @return color
     */
    public static Color getBckgrd() {
        return Color.BLACK;
    }

    /**
     * paint once
     *
     * @param g graphics to paint
     */
    public void paint(Graphics g) {
        updateImage(img3, img);
        g.drawImage(img3, 0, 0, null);
    }

    /**
     * create window
     *
     * @param pip pipeline to use
     * @param x x size
     * @param y y size
     * @param fnt font to use
     * @param plt palette to use
     */
    public pipeWindow(pipeSide pip, int x, int y, byte[][][] fnt, int[] plt) {
        pip.lineTx = pipeSide.modTyp.modeCRLF;
        pip.lineRx = pipeSide.modTyp.modeCRorLF;
        pip.setTime(0);
        img = new pipeImage(pip, x, y, fnt, plt);
        img3 = createImage(img);
        win = new JFrame("console");
        win.setBackground(getBckgrd());
        win.addKeyListener(getKeyLstnr(pip));
        win.setFocusTraversalKeysEnabled(false);
        win.add(this);
        win.setSize(img.img2[0].length + 30, img.img2.length + 60);
        win.setVisible(true);
    }

    /**
     * start new window
     */
    public void startWindow() {
        new pipeWindowDoer(this);
    }

    /**
     * stop new window
     */
    public void stopWindow() {
        img.setClose();
    }

}

class pipeWindowDoer implements Runnable {

    private pipeWindow lower;

    public pipeWindowDoer(pipeWindow parent) {
        lower = parent;
        new Thread(this).start();
    }

    private void doer() {
        for (;;) {
            boolean b = lower.img.doRound(true);
            lower.img.doImage();
            lower.repaint();
            if (b) {
                break;
            }
        }
        lower.win.setVisible(false);
    }

    public void run() {
        try {
            doer();
        } catch (Exception e) {
            logger.traceback(e);
        }
    }

}

class pipeWindowKey implements KeyListener {

    private pipeSide pipe;

    public pipeWindowKey(pipeSide pip) {
        pipe = pip;
    }

    public void keyTyped(KeyEvent e) {
        pipe.strPut("" + e.getKeyChar());
    }

    public void keyPressed(KeyEvent e) {
        String a = null;
        switch (e.getKeyCode()) {
            case KeyEvent.VK_LEFT:
            case KeyEvent.VK_KP_LEFT:
                a = "\033[D";
                break;
            case KeyEvent.VK_RIGHT:
            case KeyEvent.VK_KP_RIGHT:
                a = "\033[C";
                break;
            case KeyEvent.VK_DOWN:
            case KeyEvent.VK_KP_DOWN:
                a = "\033[B";
                break;
            case KeyEvent.VK_UP:
            case KeyEvent.VK_KP_UP:
                a = "\033[A";
                break;
            /*
            case KeyEvent.VK_BACK_SPACE:
                a = "\010";
                break;
            case KeyEvent.VK_ENTER:
                a = "\015";
                break;
            case KeyEvent.VK_ESCAPE:
                a = "\033";
                break;
            case KeyEvent.VK_TAB:
                a = "\011";
                break;
             */
            case KeyEvent.VK_HOME:
                a = "\033[H";
                break;
            case KeyEvent.VK_END:
                a = "\033[F";
                break;
            case KeyEvent.VK_PAGE_UP:
                a = "\033[5~";
                break;
            case KeyEvent.VK_PAGE_DOWN:
                a = "\033[6~";
                break;
            case KeyEvent.VK_INSERT:
                a = "\033[2~";
                break;
            case KeyEvent.VK_DELETE:
                a = "\033[3~";
                break;
            case KeyEvent.VK_F1:
                a = "\033[11~";
                break;
            case KeyEvent.VK_F2:
                a = "\033[12~";
                break;
            case KeyEvent.VK_F3:
                a = "\033[13~";
                break;
            case KeyEvent.VK_F4:
                a = "\033[14~";
                break;
            case KeyEvent.VK_F5:
                a = "\033[15~";
                break;
            case KeyEvent.VK_F6:
                a = "\033[17~";
                break;
            case KeyEvent.VK_F7:
                a = "\033[18~";
                break;
            case KeyEvent.VK_F8:
                a = "\033[19~";
                break;
            case KeyEvent.VK_F9:
                a = "\033[20~";
                break;
            case KeyEvent.VK_F10:
                a = "\033[21~";
                break;
            case KeyEvent.VK_F11:
                a = "\033[23~";
                break;
            case KeyEvent.VK_F12:
                a = "\033[24~";
                break;
        }
        if (a != null) {
            pipe.strPut(a);
        }
    }

    public void keyReleased(KeyEvent e) {
    }

}
