package net.freertr.user;

import java.util.ArrayList;
import java.util.List;
import net.freertr.pipe.pipeSetting;
import net.freertr.pipe.pipeSide;
import net.freertr.util.bits;
import net.freertr.util.debugger;
import net.freertr.util.logger;

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
    public int col = colWhite;

    /**
     * cursor wraps
     */
    public boolean wrap = true;

    /**
     * character codes
     */
    public final int[][] chrs;

    /**
     * color codes (shl 16 = background)
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
     */
    public userScreen(pipeSide p) {
        int x = p.settingsGet(pipeSetting.width, 80);
        int y = p.settingsGet(pipeSetting.height, 25);
        if (x < 10) {
            x = 80;
        }
        if (y < 5) {
            y = 25;
        }
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
     * get one key
     *
     * @param pipe pipeline to use
     * @return key readed, -1 on error, -2 on unknown
     */
    public static int getKey(pipeSide pipe) {
        int i = readChr(pipe);
        switch (i) {
            case 127: // delete
                return 0x8003;
            case 8: // backspace
                return 0x8003;
            case 9: // tabulator
                return 0x8002;
            case 13: // enter
                return 0x8004;
            case 10: // ctrl+enter
                return 0x8004;
            case 27: // escape
                break;
            case 0:
            case 1:
            case 2:
            case 3:
            case 4:
            case 5:
            case 6:
            case 7:
            case 11:
            case 12:
            case 14:
            case 15:
            case 16:
            case 17:
            case 18:
            case 19:
            case 20:
            case 21:
            case 22:
            case 23:
            case 24:
            case 25:
            case 26:
            case 28:
            case 29:
            case 30:
            case 31:
                return 0x0260 | i;
            default: // any key
                return i;
        }
        i = readChr(pipe);
        switch (i) {
            case 8: // backspace
                return 0x8403;
            case 9: // tabulator
                return 0x8402;
            case 13: // enter
                return 0x8404;
            case 27: // escape
                return 0x8005;
            case 91: // [
                break;
            case 79: // O
                break;
            case 0:
            case 1:
            case 2:
            case 3:
            case 4:
            case 5:
            case 6:
            case 7:
            case 10:
            case 11:
            case 12:
            case 14:
            case 15:
            case 16:
            case 17:
            case 18:
            case 19:
            case 20:
            case 21:
            case 22:
            case 23:
            case 24:
            case 25:
            case 26:
            case 28:
            case 29:
            case 30:
            case 31:
                return 0x0660 | i;
            default: // any key
                return i | 0x0400;
        }
        String s = "" + (char) i;
        for (;;) {
            i = readChr(pipe);
            boolean need2stop = i < 0;
            switch (i) {
                case 0x30:
                case 0x31:
                case 0x32:
                case 0x33:
                case 0x34:
                case 0x35:
                case 0x36:
                case 0x37:
                case 0x38:
                case 0x39:
                    break;
                case 91: // [
                    break;
                case 59: // ;
                    break;
                default:
                    need2stop = true;
                    break;
            }
            if (need2stop) {
                break;
            }
            s += "" + (char) i;
        }
        int ctr = s.indexOf(";");
        if (ctr < 0) {
            ctr = 0;
        } else {
            String a = s.substring(ctr + 1, s.length());
            s = s.substring(0, ctr);
            ctr = bits.str2num(a);
            switch (ctr) {
                case 2: // shift
                    ctr = 0x100;
                    break;
                case 3: // alt
                    ctr = 0x400;
                    break;
                case 4: // alt+shift
                    ctr = 0x500;
                    break;
                case 5: // ctrl
                    ctr = 0x200;
                    break;
                case 6: // ctrl+shift
                    ctr = 0x300;
                    break;
                case 7: // ctrl+alt
                    ctr = 0x600;
                    break;
                case 8: // cltr+alt+shift
                    ctr = 0x700;
                    break;
                default:
                    ctr = 0;
                    break;
            }
        }
        if (s.startsWith("O")) {
            final int[] keys1 = {20, 21, 22, 23, 24};
            switch (i) {
                case 80:
                case 81:
                case 82:
                case 83:
                case 84:
                    return keys1[i - 80] | 0x8000 | ctr;
                default:
                    return -2;
            }
        }
        if (s.startsWith("[[")) {
            final int[] keys2 = {20, 21, 22, 23, 24};
            switch (i) {
                case 65:
                case 66:
                case 67:
                case 68:
                case 69:
                    return keys2[i - 65] | 0x8000 | ctr;
                default:
                    return -2;
            }
        }
        if (!s.startsWith("[")) {
            return -2;
        }
        final int[] keys3 = {12, 13, 15, 14, 0, 9, 0, 8};
        final int[] keys4 = {20, 21, 22, 23, 24};
        switch (i) {
            case 65:
            case 66:
            case 67:
            case 68:
            case 69:
            case 70:
            case 71:
            case 72:
                return keys3[i - 65] | 0x8000 | ctr;
            case 80:
            case 81:
            case 82:
            case 83:
            case 84:
                return keys4[i - 80] | 0x8000 | ctr;
            case 126:
                break;
            default:
                return -2;
        }
        i = bits.str2num(s.substring(1, s.length()));
        final int[] keys5 = {8, 6, 7, 9, 10, 11};
        final int[] keys6 = {20, 21, 22, 23, 24, 0, 25, 26, 27, 28, 29, 0, 30, 31};
        switch (i) {
            case 1:
            case 2:
            case 3:
            case 4:
            case 5:
            case 6:
                return keys5[i - 1] | 0x8000 | ctr;
            case 11:
            case 12:
            case 13:
            case 14:
            case 15:
            case 16:
            case 17:
            case 18:
            case 19:
            case 20:
            case 21:
            case 22:
            case 23:
            case 24:
                return keys6[i - 11] | 0x8000 | ctr;
            default:
                return -2;
        }
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
        pipe.blockingPut(buf, 0, buf.length);
        remC[remY][remX] = ch;
        remB[remY][remX] = remP;
        remX++;
    }

    private static int readChr(pipeSide pipe) {
        byte[] buf = new byte[1];
        if (pipe.blockingGet(buf, 0, buf.length) != buf.length) {
            return -1;
        }
        int i = buf[0] & 0xff;
        if (debugger.userScreenEvnt) {
            logger.debug("got " + i + " from client");
        }
        return i;
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
        for (int i = sizY - 2; i >= 0; i--) {
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
     * put one window
     *
     * @param bg background color
     * @param fg foreground color
     * @param bx beginning x
     * @param by beginning y
     * @param sx size x
     * @param sy size y
     */
    public void putWindow(int bg, int fg, int bx, int by, int sx, int sy) {
        for (int i = 0; i < sy; i++) {
            putStr(bx - 1, by + i, bg, fg, false, "|");
            putStr(bx + sx, by + i, bg, fg, false, "|");
            putCols(bx + sx + 1, by + i + 1, colBlack, colBrBlack, 2);
        }
        for (int o = 0; o < sx; o++) {
            for (int i = 0; i < sy; i++) {
                putStr(bx + o, by + i, bg, fg, false, " ");
            }
            putStr(bx + o, by, bg, fg, false, "-");
            putStr(bx + o, by + sy, bg, fg, false, "-");
        }
        putCols(bx + 1, by + sy + 1, colBlack, colBrBlack, sx + 2);
        putStr(bx - 1, by, bg, fg, false, "+");
        putStr(bx + sx, by, bg, fg, false, "+");
        putStr(bx - 1, by + sy, bg, fg, false, "+");
        putStr(bx + sx, by + sy, bg, fg, false, "+");
    }

    /**
     * ask user line
     *
     * @param bg background color
     * @param fg foreground color
     * @param sx screen x
     * @param sy screen y
     * @param siz size of line
     * @param ln original line
     * @return edited line
     */
    public String readLine(int bg, int fg, int sx, int sy, int siz, String ln) {
        if (ln == null) {
            ln = "";
        }
        int beg = 0;
        int cur = ln.length();
        for (;;) {
            if (cur < 0) {
                cur = 0;
            }
            if (cur > ln.length()) {
                cur = ln.length();
            }
            int i = cur - siz;
            if (beg < i) {
                beg = i;
            }
            if (beg > cur) {
                beg = cur;
            }
            putStr(sx, sy, bg, fg, false, bits.padEnd(ln.substring(beg, ln.length()), siz, " ").substring(0, siz));
            putCur(sx + cur - beg, sy);
            refresh();
            i = getKey(pipe);
            switch (i) {
                case -1: // end
                    return ln;
                case 0x8004: // enter
                    return ln;
                case 0x0261: // ctrl+a
                case 0x8008: // home
                    cur = 0;
                    break;
                case 0x0265: // ctrl+e
                case 0x8009: // end
                    cur = ln.length();
                    break;
                case 0x8003: // backspace
                    cur--;
                    if (cur < 0) {
                        break;
                    }
                    if (cur >= ln.length()) {
                        break;
                    }
                    ln = ln.substring(0, cur) + ln.substring(cur + 1, ln.length());
                    break;
                case 0x8007: // delete
                    if (cur >= ln.length()) {
                        break;
                    }
                    ln = ln.substring(0, cur) + ln.substring(cur + 1, ln.length());
                    break;
                case 0x800e: // left
                    cur--;
                    break;
                case 0x800f: // right
                    cur++;
                    break;
                default:
                    if (i < 0x20) {
                        break;
                    }
                    if (i > 0x7f) {
                        break;
                    }
                    ln = ln.substring(0, cur) + (char) i + ln.substring(cur, ln.length());
                    cur++;
                    break;
            }
        }
    }

    /**
     * help window
     *
     * @param bg background color
     * @param win window color
     * @param txt text color
     * @param bx screen x
     * @param by screen y
     * @param sx size x
     * @param sy size y
     * @param msg message
     */
    public void helpWin(int bg, int win, int txt, int bx, int by, int sx, int sy, List<String> msg) {
        if (bx < 0) {
            bx = 4;
        }
        if (by < 0) {
            by = 2;
        }
        if (sx < 0) {
            sx = sizX - 8;
        }
        if (sy < 0) {
            sy = sizY - 6;
        }
        int cur = 0;
        for (;;) {
            int i = msg.size() - sy;
            if (cur >= i) {
                cur = i;
            }
            if (cur < 0) {
                cur = 0;
            }
            putWindow(bg, win, bx, by, sx, sy);
            for (i = 0; i < sy - 1; i++) {
                String a;
                if ((cur + i) < msg.size()) {
                    a = msg.get(cur + i);
                } else {
                    a = "";
                }
                putStr(bx, by + i + 1, bg, txt, false, bits.padEnd(a, sx, " ").substring(0, sx));
            }
            putCur(bx, by + 1);
            refresh();
            i = getKey(pipe);
            switch (i) {
                case -1: // end
                    return;
                case 0x800c: // up
                    cur--;
                    break;
                case 0x800d: // down
                    cur++;
                    break;
                case 0x800a: // pgup
                    cur -= sy / 2;
                    break;
                case 0x800b: // pgdn
                    cur += sy / 2;
                    break;
                case 0x0271: // ctrl+q
                    return;
                case 0x0278: // ctrl+x
                    return;
                case 0x0263: // ctrl+c
                    return;
                case 0x8014: // f1
                    return;
                case 0x801d: // f10
                    return;
            }
        }
    }

    /**
     * ask user
     *
     * @param que question to ask
     * @param bg background color
     * @param win window color
     * @param tit title color
     * @param txt text color
     * @param sx screen x
     * @param sy screen y
     * @param siz size of window
     * @param ln original line
     * @return edited line
     */
    public String askUser(String que, int bg, int win, int tit, int txt, int sx, int sy, int siz, String ln) {
        if (sx < 0) {
            sx = 4;
        }
        if (sy < 0) {
            sy = (sizY / 2) - 2;
        }
        if (siz < 0) {
            siz = sizX - 8;
        }
        putWindow(bg, win, sx, sy, siz, 3);
        putStr(sx, sy + 1, bg, tit, false, que);
        return readLine(bg, txt, sx, sy + 2, siz, ln);
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
