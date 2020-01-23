package pipe;

import java.awt.Color;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.image.BufferedImage;
import user.userScreen;

/**
 * display pipeline to image
 *
 * @author matecsaba
 */
public class pipeImage {

    /**
     * palette
     */
    public final int[] pal;

    /**
     * current indexed image
     */
    public final int[][] img1;

    /**
     * current rgb image
     */
    public final int[][] img2;

    /**
     * current rgb image
     */
    public final BufferedImage img3;

    /**
     * current screen
     */
    public final pipeScreen scr;

    private final pipeSide pipe;

    private final byte[][][] font;

    private final int fntX;

    private final int fntY;

    /**
     * create new image
     *
     * @param pip pipeline to read
     * @param x x size
     * @param y y size
     * @param fnt font to use
     * @param plt palette to use
     */
    public pipeImage(pipeSide pip, int x, int y, short[] fnt, int[] plt) {
        pipe = pip;
        font = userScreen.fontConvert(fnt);
        pal = plt;
        fntY = font[0].length;
        fntX = font[0][0].length;
        scr = new pipeScreen(pipe, x, y);
        x *= fntX;
        y *= fntY;
        img1 = new int[y][x];
        img2 = new int[y][x];
        img3 = new BufferedImage(x, y, BufferedImage.TYPE_3BYTE_BGR);
        doImage();
    }

    private void putPixel(int x, int y, int c) {
        img1[y][x] = c;
        img2[y][x] = pal[c];
        img3.setRGB(x, y, pal[c]);
    }

    /**
     * write character to image
     *
     * @param px x coordinate
     * @param py y coordinate
     * @param chr character
     * @param clr clear color
     * @param set set color
     */
    public void fontChar(int px, int py, int chr, int clr, int set) {
        if (px < 0) {
            return;
        }
        if (py < 0) {
            return;
        }
        px *= fntX;
        py *= fntY;
        if (px >= img1[0].length) {
            return;
        }
        if (py >= img1.length) {
            return;
        }
        chr &= 0xff;
        clr &= 0xf;
        set &= 0xf;
        for (int y = 0; y < fntY; y++) {
            for (int x = 0; x < fntX; x++) {
                int a;
                if (font[chr][y][x] == 0) {
                    a = clr;
                } else {
                    a = set;
                }
                putPixel(px + x, py + y, a);
            }
        }
    }

    /**
     * write string to image
     *
     * @param px x coordinate
     * @param py y coordinate
     * @param str string to write
     * @param clr clear color
     * @param set set color
     */
    public void fontText(int px, int py, String str, int clr, int set) {
        byte[] buf = str.getBytes();
        for (int i = 0; i < buf.length; i++) {
            fontChar(px + i, py, buf[i], clr, set);
        }
    }

    /**
     * draw cursor
     *
     * @param x x coordinate
     * @param y y coordinate
     */
    public void fontCursor(int x, int y) {
        x *= fntX;
        y *= fntY;
        int c = scr.scr.col & 0xf;
        for (int i = 0; i < fntX; i++) {
            putPixel(x + i, y, c);
            putPixel(x + i, y + fntY - 1, c);
        }
        for (int i = 0; i < fntY; i++) {
            putPixel(x, y + i, c);
            putPixel(x + fntX - 1, y + i, c);
        }
    }

    /**
     * draw image
     */
    public void doImage() {
        for (int y = 0; y < scr.scr.sizY; y++) {
            for (int x = 0; x < scr.scr.sizX; x++) {
                fontChar(x, y, scr.scr.chrs[y][x], scr.scr.atrs[y][x] >>> 16, scr.scr.atrs[y][x]);
            }
        }
        fontCursor(scr.scr.curX, scr.scr.curY);
    }

    /**
     * do one round
     *
     * @param wait set true to wait for input, false to fail otherwise
     * @return true on error, false on success
     */
    public boolean doRound(boolean wait) {
        int i = pipe.ready2rx();
        if (i < 1) {
            if (wait) {
                i = 1;
            } else {
                return true;
            }
        }
        byte[] buf = new byte[i];
        if (pipe.moreGet(buf, 0, i) != i) {
            return true;
        }
        scr.doChar(buf);
        return false;
    }

    /**
     * stop work
     */
    public void setClose() {
        pipe.setClose();
    }

    /**
     * get background color
     *
     * @return color
     */
    public Color getBackground() {
        return Color.BLACK;
    }

    /**
     * send keycodes
     *
     * @param s string to send
     */
    public void doKey(String s) {
        pipe.strPut(s);
    }

    /**
     * get keyboard listener
     *
     * @return keyboard listener
     */
    public KeyListener getKeyLstnr() {
        return new pipeImageKey(pipe);
    }

}

class pipeImageKey implements KeyListener {

    private pipeSide pipe;

    public pipeImageKey(pipeSide pip) {
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
                a = "\008";
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
