package org.freertr.pipe;

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
import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;
import javax.swing.JFrame;
import javax.swing.JPanel;
import org.freertr.cfg.cfgInit;
import org.freertr.util.bits;
import org.freertr.util.logger;

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
     * play animated image
     *
     * @param scr console to draw
     * @param fil file
     */
    public static void imageAnim(pipeScreen scr, File fil) {
        String a = fil.getName();
        int i = a.lastIndexOf(".");
        if (i < 0) {
            i = 0;
        }
        a = a.substring(i + 1, a.length());
        a = cfgInit.findMimeType(a);
        ImageReader ir = null;
        ImageInputStream is = null;
        int n = 0;
        try {
            ir = ImageIO.getImageReadersByMIMEType(a).next();
            is = ImageIO.createImageInputStream(fil);
            ir.setInput(is, false);
            n = ir.getNumImages(true);
        } catch (Exception e) {
            logger.traceback(e);
            return;
        }
        BufferedImage img1 = null;
        for (i = 0; i < n; i++) {
            BufferedImage img2 = null;
            try {
                img2 = ir.read(i);
            } catch (Exception e) {
                logger.traceback(e);
            }
            if (img2 == null) {
                continue;
            }
            if (img1 == null) {
                img1 = img2;
            } else {
                img1.getGraphics().drawImage(img2, 0, 0, null);
            }
            image2scr(img1, scr, pipeFonts.colorData, pipeFonts.ditherData);
            scr.refresh();
            bits.sleep(500);
            if (scr.keyPress()) {
                break;
            }
        }
    }

    /**
     * convert image to ansi
     *
     * @param scr console to draw
     * @param fil file to convert
     */
    public static void imageAnsi(pipeScreen scr, File fil) {
        BufferedImage img1 = null;
        try {
            img1 = ImageIO.read(fil);
        } catch (Exception e) {
            logger.traceback(e);
        }
        if (img1 == null) {
            return;
        }
        image2scr(img1, scr, pipeFonts.colorData, pipeFonts.ditherData);
        scr.refresh();
    }

    /**
     * convert image to string
     *
     * @param scr console to draw
     * @param fil file
     */
    public static void imageAscii(pipeScreen scr, File fil) {
        BufferedImage img1 = null;
        try {
            img1 = ImageIO.read(fil);
        } catch (Exception e) {
            logger.traceback(e);
        }
        if (img1 == null) {
            return;
        }
        image2scr(img1, scr, pipeFonts.colorMono, pipeFonts.ditherData);
    }

    /**
     * convert image to string
     *
     * @param scr console to draw
     * @param fil file
     */
    public static void imageTable(pipeScreen scr, File fil) {
        BufferedImage img1 = null;
        try {
            img1 = ImageIO.read(fil);
        } catch (Exception e) {
            logger.traceback(e);
        }
        if (img1 == null) {
            return;
        }
        int maxX = scr.sizX;
        int maxY = scr.sizY;
        maxX = (img1.getWidth() / maxX) + 1;
        maxY = (img1.getHeight() / maxY) + 1;
        int p = maxX < maxY ? maxY : maxX;
        if (p < 1) {
            p = 1;
        }
        maxX = img1.getWidth() / p;
        maxY = img1.getHeight() / p;
        byte[] img3 = image2idx(img1, maxX, maxY, pipeFonts.colorData, pipeFonts.ditherMono);
        pipeScreen.sendImageTable(scr.pipe, pipeFonts.colorIdxd, pipeFonts.ditherMono, img3, maxX, maxY);
    }

    /**
     * send image in dec-sixel format
     *
     * @param scr console to draw
     * @param fil file
     */
    public static void imageSixel(pipeScreen scr, File fil) {
        BufferedImage img1 = null;
        try {
            img1 = ImageIO.read(fil);
        } catch (Exception e) {
            logger.traceback(e);
        }
        if (img1 == null) {
            return;
        }
        int maxX = scr.sizX * 16;
        int maxY = scr.sizY * 16;
        maxX = (img1.getWidth() / maxX) + 1;
        maxY = (img1.getHeight() / maxY) + 1;
        int p = maxX < maxY ? maxY : maxX;
        if (p < 1) {
            p = 1;
        }
        maxX = img1.getWidth() / p;
        maxY = img1.getHeight() / p;
        byte[] img3 = image2idx(img1, maxX, maxY, pipeFonts.colorData, pipeFonts.ditherMono);
        pipeScreen.sendImageSixel(scr.pipe, pipeFonts.colorIdxd, pipeFonts.ditherMono, img3, maxX, maxY);
    }

    private static byte[] image2idx(BufferedImage img1, int maxX, int maxY, int[] col, char chr[]) {
        byte[] cls = new byte[col.length * chr.length * 3];
        int p = 0;
        for (int i = 0; i < col.length; i++) {
            int c = col[i];
            int r = (c >>> 16) & 0xff;
            int g = (c >>> 8) & 0xff;
            int b = c & 0xff;
            for (int o = 1; o <= chr.length; o++) {
                cls[p + 0] = (byte) ((r * o) / chr.length);
                cls[p + 1] = (byte) ((g * o) / chr.length);
                cls[p + 2] = (byte) ((b * o) / chr.length);
                p += 3;
            }
        }
        IndexColorModel icm = new IndexColorModel(8, cls.length / 3, cls, 0, false);
        BufferedImage img2 = new BufferedImage(maxX, maxY, BufferedImage.TYPE_BYTE_INDEXED, icm);
        Graphics2D g2d = img2.createGraphics();
        g2d.drawImage(img1, 0, 0, maxX, maxY, null);
        g2d.dispose();
        g2d.setComposite(AlphaComposite.Src);
        g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC);
        g2d.setRenderingHint(RenderingHints.KEY_ALPHA_INTERPOLATION, RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY);
        g2d.setRenderingHint(RenderingHints.KEY_COLOR_RENDERING, RenderingHints.VALUE_COLOR_RENDER_QUALITY);
        g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2d.setRenderingHint(RenderingHints.KEY_DITHERING, RenderingHints.VALUE_DITHER_ENABLE);
        return ((DataBufferByte) img2.getRaster().getDataBuffer()).getData();
    }

    private static void image2scr(BufferedImage img1, pipeScreen scr, int[] col, char chr[]) {
        int maxX = scr.sizX;
        int maxY = scr.sizY;
        maxX = ((2 * img1.getWidth()) / maxX) + 1;
        maxY = (img1.getHeight() / maxY) + 1;
        int p = maxX < maxY ? maxY : maxX;
        if (p < 1) {
            p = 1;
        }
        maxX = (2 * img1.getWidth()) / p;
        maxY = img1.getHeight() / p;
        byte[] img3 = image2idx(img1, maxX, maxY, col, chr);
        p = 0;
        scr.doClear();
        for (int y = 0; y < maxY; y++) {
            for (int x = 0; x < maxX; x++) {
                int v = img3[p];
                scr.putInt(x, y, pipeScreen.colBlack, v / chr.length, false, chr[v % chr.length]);
                p++;
            }
        }
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
            logger.traceback(e);
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
        win.setBackground(Color.BLACK);
        win.addKeyListener(new pipeWindowKey(pip));
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
        img.pipe.setClose();
    }

}

class pipeWindowDoer implements Runnable {

    private pipeWindow lower;

    public pipeWindowDoer(pipeWindow parent) {
        lower = parent;
        logger.startThread(this);
    }

    private void doer() {
        for (;;) {
            boolean b = lower.img.scr.doRound(true);
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
