package user;

import java.awt.AlphaComposite;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;
import pipe.pipeSide;

/**
 * screen handler
 *
 * @author matecsaba
 */
public class userScreen {

    /**
     * pipeline in use
     */
    public final pipeSide pipe;

    /**
     * screen size
     */
    public final int sizX;

    /**
     * screen size
     */
    public final int sizY;

    /**
     * cursor position
     */
    public int curX = 0;

    /**
     * cursor position
     */
    public int curY = 0;

    /**
     * cursor color
     */
    public int col = userScreen.colWhite;

    /**
     * cursor wraps
     */
    public boolean wrap = true;

    /**
     * character codes
     */
    public final int[][] chrs;

    /**
     * color codes (>>16 = background)
     */
    public final int[][] atrs;

    private int[][] remC; // character

    private int[][] remB; // background

    private int remX; // position

    private int remY; // position

    private int remP; // pen color

    /**
     * black
     */
    public static final byte colBlack = 0;

    /**
     * red
     */
    public static final byte colRed = 1;

    /**
     * green
     */
    public static final byte colGreen = 2;

    /**
     * yellow
     */
    public static final byte colYellow = 3;

    /**
     * blue
     */
    public static final byte colBlue = 4;

    /**
     * magenta
     */
    public static final byte colMagenta = 5;

    /**
     * cyan
     */
    public static final byte colCyan = 6;

    /**
     * white
     */
    public static final byte colWhite = 7;

    /**
     * bright/blink text
     */
    public static final byte colBright = 8;

    /**
     * bright black
     */
    public static final byte colBrBlack = colBright | colBlack;

    /**
     * bright red
     */
    public static final byte colBrRed = colBright | colRed;

    /**
     * bright green
     */
    public static final byte colBrGreen = colBright | colGreen;

    /**
     * bright yellow
     */
    public static final byte colBrYellow = colBright | colYellow;

    /**
     * bright blue
     */
    public static final byte colBrBlue = colBright | colBlue;

    /**
     * bright magenta
     */
    public static final byte colBrMagenta = colBright | colMagenta;

    /**
     * bright cyan
     */
    public static final byte colBrCyan = colBright | colCyan;

    /**
     * bright white
     */
    public static final byte colBrWhite = colBright | colWhite;

    /**
     * create one screen
     *
     * @param p connection to use
     * @param x screen x size
     * @param y screen y size
     */
    public userScreen(pipeSide p, int x, int y) {
        pipe = p;
        sizX = x;
        sizY = y;
        remC = new int[y][x];
        remB = new int[y][x];
        chrs = new int[y][x];
        atrs = new int[y][x];
        remP = -1;
        sendCol(colWhite);
        sendCur(0, 0);
        sendCls();
    }

    /**
     * send clear screen
     *
     * @param pip pipeline to use
     */
    public static void sendCls(pipeSide pip) {
        pip.strPut("\033[2J");
    }

    /**
     * send cursor position
     *
     * @param pip pipeline to use
     * @param x x coordinate
     * @param y y coordinate
     */
    public static void sendCur(pipeSide pip, int x, int y) {
        pip.strPut("\033[" + (y + 1) + ";" + (x + 1) + "H");
    }

    /**
     * send color change
     *
     * @param pip pipeline to use
     * @param col color to use
     */
    public static void sendCol(pipeSide pip, int col) {
        int bg = (col >>> 16) & 0xf;
        int fg = col & 0xf;
        String s = "\033[0";
        if ((bg & 8) != 0) {
            s += ";5";
        }
        s += ";4" + (bg & 7);
        if ((fg & 8) != 0) {
            s += ";1";
        }
        s += ";3" + (fg & 7);
        pip.strPut(s + "m");
    }

    private void sendCls() {
        sendCls(pipe);
        remP = 0x12345678;
        remX = remP;
        remY = remP;
        for (int y = 0; y < sizY; y++) {
            for (int x = 0; x < sizX; x++) {
                remC[y][x] = remP;
                remB[y][x] = remP;
            }
        }
    }

    private void sendCur(int x, int y) {
        if ((remX == x) && (remY == y)) {
            return;
        }
        sendCur(pipe, x, y);
        remX = x;
        remY = y;
    }

    private void sendCol(int col) {
        if (remP == col) {
            return;
        }
        remP = col;
        sendCol(pipe, col);
    }

    private void sendChr(int ch) {
        byte[] buf = new byte[1];
        buf[0] = (byte) ch;
        pipe.strPut(new String(buf));
        remC[remY][remX] = ch;
        remB[remY][remX] = remP;
        remX++;
    }

    /**
     * refresh terminal
     */
    public void refresh() {
        remC[sizY - 1][sizX - 1] = chrs[sizY - 1][sizX - 1];
        remB[sizY - 1][sizX - 1] = atrs[sizY - 1][sizX - 1];
        for (int y = 0; y < sizY; y++) {
            for (int x = 0; x < sizX; x++) {
                if ((remC[y][x] == chrs[y][x]) && (remB[y][x] == atrs[y][x])) {
                    continue;
                }
                sendCur(x, y);
                sendCol(atrs[y][x]);
                sendChr(chrs[y][x]);
            }
        }
        sendCur(curX, curY);
        sendCol(colWhite);
    }

    /**
     * check if keys available
     *
     * @return true if yes, false if no
     */
    public boolean keyPress() {
        return (pipe.ready2rx() > 0) || (pipe.isClosed() != 0);
    }

    /**
     * put clear screen
     */
    public void putCls() {
        curX = 0;
        curY = sizY - 1;
        for (int y = 0; y < sizY; y++) {
            for (int x = 0; x < sizX; x++) {
                chrs[y][x] = 32;
                atrs[y][x] = colWhite;
            }
        }
    }

    private boolean range(int x, int y) {
        if (x < 0) {
            return true;
        }
        if (y < 0) {
            return true;
        }
        if (x >= sizX) {
            return true;
        }
        if (y >= sizY) {
            return true;
        }
        return false;
    }

    /**
     * put one character
     *
     * @param x x coordinate
     * @param y y coordinate
     * @param bg background color
     * @param fg foreground color
     * @param cr set true to update cursor
     * @param ch character to write
     */
    public void putInt(int x, int y, int bg, int fg, boolean cr, int ch) {
        if (range(x, y)) {
            return;
        }
        bg &= 0xf;
        fg &= 0xf;
        chrs[y][x] = ch & 0xff;
        atrs[y][x] = (bg << 16) | fg;
        if (cr) {
            curX = x + 1;
            curY = y;
            col = (bg << 16) | fg;
        }
    }

    /**
     * put one color
     *
     * @param x x coordinate
     * @param y y coordinate
     * @param bg background color
     * @param fg foreground color
     */
    public void putCol(int x, int y, int bg, int fg) {
        if (range(x, y)) {
            return;
        }
        bg &= 0xf;
        fg &= 0xf;
        atrs[y][x] = (bg << 16) | fg;
    }

    /**
     * put one color
     *
     * @param x x coordinate
     * @param y y coordinate
     * @param bg background color
     * @param fg foreground color
     * @param siz number of colors
     */
    public void putCols(int x, int y, int bg, int fg, int siz) {
        for (int i = 0; i < siz; i++) {
            putCol(x + i, y, bg, fg);
        }
    }

    /**
     * put one character
     *
     * @param x x coordinate
     * @param y y coordinate
     * @param cr set true to update cursor
     * @param cl color to write
     * @param ch character to write
     */
    public void putInt(int x, int y, boolean cr, int cl, int ch) {
        putInt(x, y, cl >>> 16, cl, cr, ch);
    }

    /**
     * set cursor position
     *
     * @param x x coordinate
     * @param y y coordinate
     */
    public void putCur(int x, int y) {
        curX = x;
        curY = y;
    }

    /**
     * put one string
     *
     * @param x x coordinate
     * @param y y coordinate
     * @param bg background color
     * @param fg foreground color
     * @param cr set true to update cursor
     * @param s string to write
     */
    public void putStr(int x, int y, int bg, int fg, boolean cr, String s) {
        byte[] buf = s.getBytes();
        for (int i = 0; i < buf.length; i++) {
            putInt(x + i, y, bg, fg, cr, buf[i]);
        }
    }

    /**
     * put more lines
     *
     * @param x x coordinate
     * @param y y coordinate
     * @param bg background color
     * @param fg foreground color
     * @param cr set true to update cursor
     * @param l lines to write
     */
    public void putStrs(int x, int y, int bg, int fg, boolean cr, List<String> l) {
        for (int i = 0; i < l.size(); i++) {
            putStr(x, y + i, bg, fg, cr, l.get(i));
        }
    }

    /**
     * put one string
     *
     * @param x x coordinate
     * @param y y coordinate
     * @param bg background color
     * @param fg foreground color
     * @param cr set true to update cursor
     * @param s string to write
     */
    public void putMap(int x, int y, int bg, int fg, boolean cr, String s) {
        final int space = 32;
        byte[] buf = s.getBytes();
        for (int i = 0; i < buf.length; i++) {
            if (buf[i] == space) {
                putInt(x + i, y, bg, fg, cr, space);
            } else {
                putInt(x + i, y, fg, bg, cr, space);
            }
        }
    }

    /**
     * put more maps
     *
     * @param x x coordinate
     * @param y y coordinate
     * @param bg background color
     * @param fg foreground color
     * @param cr set true to update cursor
     * @param l lines to write
     */
    public void putMaps(int x, int y, int bg, int fg, boolean cr, List<String> l) {
        for (int i = 0; i < l.size(); i++) {
            putMap(x, y + i, bg, fg, cr, l.get(i));
        }
    }

    /**
     * fill line
     *
     * @param y line to fill
     * @param bx begin x
     * @param ex ending x
     * @param cl color
     * @param ch character
     */
    public void fillLine(int y, int bx, int ex, int cl, int ch) {
        for (int x = bx; x < ex; x++) {
            putInt(x, y, false, cl, ch);
        }
    }

    /**
     * fill lines
     *
     * @param by begin y
     * @param ey ending y
     * @param cl color
     * @param ch character
     */
    public void fillLines(int by, int ey, int cl, int ch) {
        for (int y = by; y < ey; y++) {
            fillLine(y, 0, sizX, cl, ch);
        }
    }

    /**
     * scroll lines up
     *
     * @param cl color
     */
    public void scrollUp(int cl) {
        for (int i = 1; i < sizY; i++) {
            chrs[i - 1] = chrs[i];
            atrs[i - 1] = atrs[i];
        }
        chrs[sizY - 1] = new int[sizX];
        atrs[sizY - 1] = new int[sizX];
        fillLine(sizY - 1, 0, sizX, cl, 32);
    }

    /**
     * scroll lines down
     *
     * @param cl color
     */
    public void scrollDn(int cl) {
        for (int i = sizY - 2; i >= 0; i++) {
            chrs[i + 1] = chrs[i];
            atrs[i + 1] = atrs[i];
        }
        chrs[0] = new int[sizX];
        atrs[0] = new int[sizX];
        fillLine(0, 0, sizX, cl, 32);
    }

    /**
     * range check cursor
     *
     * @param cl color
     */
    public void curRange(int cl) {
        if (curX < 0) {
            curX = 0;
        }
        if (curY < 0) {
            curY = 0;
        }
        if (curX >= sizX) {
            if (wrap) {
                curY++;
                curX = 0;
            } else {
                curX = sizX - 1;
            }
        }
        if (curY >= sizY) {
            curY = sizY - 1;
            scrollUp(cl);
        }
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
        maxX = (img1.getWidth() / maxX) + 1;
        maxY = (img1.getHeight() / maxY) + 1;
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
        List<String> txt = new ArrayList<String>();
        for (int y = 0; y < img3.length; y++) {
            String a = "";
            for (int x = 0; x < img3[0].length; x++) {
                int v = (img3[y][x] * (chrs.length - 1)) / tmp;
                a += chrs[v];
            }
            txt.add(a);
        }
        return txt;
    }

    /**
     * convert text to string
     *
     * @param str text to convert
     * @param clr clear character
     * @param set set character
     * @param fnt font to use
     * @return converted text
     */
    public static List<String> fontText(String str, String clr, String set, byte[][][] fnt) {
        final int maxY = fnt[0].length;
        final int maxX = fnt[0][0].length;
        final byte[] buf = str.getBytes();
        List<String> l = new ArrayList<String>();
        for (int y = 0; y < maxY; y++) {
            String s = "";
            for (int p = 0; p < buf.length; p++) {
                for (int x = 0; x < maxX; x++) {
                    String a;
                    if (fnt[buf[p]][y][x] == 0) {
                        a = clr;
                    } else {
                        a = set;
                    }
                    s += a;
                }
            }
            l.add(s);
        }
        return l;
    }

    /**
     * convert font
     *
     * @param src source
     * @return result
     */
    public static byte[][][] fontConvert(short[] src) {
        final int maxX = src[0];
        final int maxY = src[1];
        byte[][][] res = new byte[256][maxY][maxX];
        for (int c = 0; c < 256; c++) {
            for (int y = 0; y < maxY; y++) {
                int val = src[c * 16 + y + 2];
                for (int x = 0; x < maxX; x++) {
                    res[c][y][x] = (byte) ((val >>> (maxX - x - 1)) & 1);
                }
            }
        }
        return res;
    }

}
