package net.freertr.pipe;

import java.awt.AlphaComposite;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferByte;
import java.awt.image.IndexColorModel;
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

    private final static long serialVersionUID = 10251979;

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
     * convert image to ansi
     *
     * @param ps pipe to draw
     * @param fil file to convert
     * @param chr characters to put
     * @return converted ansi
     */
    public static userScreen imageAnsi(pipeSide ps, File fil, int[] chr) {
        userScreen scr = new userScreen(ps);
        scr.putCls();
        scr.putCur(0, 0);
        try {
            BufferedImage img1 = ImageIO.read(fil);
            BufferedImage img2 = scaleImage(img1, scr.sizX, scr.sizY, scr.ansP);
            int[][] img3 = colorImage(img2);
            for (int cy = 0; cy < scr.sizY; cy++) {
                if (cy >= img3.length) {
                    continue;
                }
                for (int cx = 0; cx < scr.sizX; cx++) {
                    if (cx >= img3[0].length) {
                        continue;
                    }
                    int i;
                    try {
                        i = img3[cy][cx];;
                    } catch (Exception e) {
                        i = 0;
                    }
                    scr.putInt(cx, cy, false, i, chr[bits.random(0, chr.length)]);
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
     * @return converted text
     */
    public static List<String> imageText(File fil, int maxX, int maxY) {
        BufferedImage img1 = null;
        char[] chr = new char[2];
        chr[0] = 0x20;
        chr[1] = 0x30;
        try {
            img1 = ImageIO.read(fil);
        } catch (Exception e) {
            logger.traceback(e, "while converting");
        }
        if (img1 == null) {
            return new ArrayList<String>();
        }
        return imageText(img1, maxX, maxY, chr);
    }

    private static BufferedImage scaleImage(BufferedImage img1, int maxX, int maxY, int[] col) {
        byte[] cls = new byte[col.length * 3];
        for (int i = 0; i < col.length; i++) {
            cls[i * 3 + 0] = (byte) (col[i] >>> 16);
            cls[i * 3 + 1] = (byte) (col[i] >>> 8);
            cls[i * 3 + 2] = (byte) col[i];
        }
        IndexColorModel icm = new IndexColorModel(8, col.length, cls, 0, false);
        maxX = (img1.getWidth() / maxX) + 1;
        maxY = (img1.getHeight() / maxY) + 1;
        int tmp = maxX < maxY ? maxY : maxX;
        BufferedImage img2 = new BufferedImage(img1.getWidth() / tmp, img1.getHeight() / tmp, BufferedImage.TYPE_BYTE_INDEXED, icm);
        Graphics2D g = img2.createGraphics();
        g.drawImage(img1, 0, 0, img2.getWidth(), img2.getHeight(), null);
        g.dispose();
        g.setComposite(AlphaComposite.Src);
        g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
        g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        return img2;
    }

    private static int[][] colorImage(BufferedImage img1) {
        byte[] img2 = ((DataBufferByte) img1.getRaster().getDataBuffer()).getData();
        int[][] img3 = new int[img1.getHeight()][img1.getWidth()];
        int p = 0;
        for (int y = 0; y < img3.length; y++) {
            for (int x = 0; x < img3[0].length; x++) {
                int v = img2[p];
                img3[y][x] = v;
                p++;
            }
        }
        return img3;
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
    public static List<String> imageText(BufferedImage img1, int maxX, int maxY, final char[] chrs) {
        List<String> txt = new ArrayList<String>();
        BufferedImage img2 = scaleImage(img1, maxX, maxY, userFonts.colorMono);
        int[][] img3 = colorImage(img2);
        for (int y = 0; y < img3.length; y++) {
            String a = "";
            for (int x = 0; x < img3[0].length; x++) {
                int v = img3[y][x];
                v = v * (chrs.length - 1);
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
