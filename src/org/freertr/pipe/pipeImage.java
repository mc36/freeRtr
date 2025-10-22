package org.freertr.pipe;

import org.freertr.user.userRead;

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
     * current screen
     */
    public final pipeTerm scr;

    /**
     * current pipe
     */
    public final pipeSide pipe;

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
    public pipeImage(pipeSide pip, int x, int y, byte[][][] fnt, int[] plt) {
        pipe = pip;
        pipe.setReady();
        pipeTerm.setTermWdt(pipe, x);
        pipeTerm.setTermLen(pipe, y);
        font = fnt;
        pal = plt;
        fntY = font[0].length;
        fntX = font[0][0].length;
        scr = new pipeTerm(pipe, x, y);
        x *= fntX;
        y *= fntY;
        img1 = new int[y][x];
        img2 = new int[y][x];
        doImage();
    }

    private void putPixel(int x, int y, int c) {
        img1[y][x] = c;
        img2[y][x] = pal[c];
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

}
