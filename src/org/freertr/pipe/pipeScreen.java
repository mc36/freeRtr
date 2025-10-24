package org.freertr.pipe;

import java.util.ArrayList;
import java.util.List;
import org.freertr.enc.encBase64;
import org.freertr.util.bits;
import org.freertr.util.debugger;
import org.freertr.util.logger;

/**
 * screen handler
 *
 * @author matecsaba
 */
public class pipeScreen {

    /**
     * ansi modes
     */
    public enum ansiMode {
        /**
         * normal
         */
        none,
        /**
         * 8 colors
         */
        original,
        /**
         * 16 colors
         */
        normal,
        /**
         * 256 colors
         */
        indexed,
        /**
         * 16m colors
         */
        palette
    }

    /**
     * convert string to ansi mode
     *
     * @param a string
     * @return mode
     */
    public static ansiMode string2mode(String a) {
        if (a.equals("none")) {
            return ansiMode.none;
        }
        if (a.equals("original")) {
            return ansiMode.original;
        }
        if (a.equals("normal")) {
            return ansiMode.normal;
        }
        if (a.equals("indexed")) {
            return ansiMode.indexed;
        }
        if (a.equals("palette")) {
            return ansiMode.palette;
        }
        return ansiMode.normal;
    }

    /**
     * convert ansi mode to string
     *
     * @param m mode
     * @return string
     */
    public static String ansimod2str(ansiMode m) {
        switch (m) {
            case none:
                return "none";
            case original:
                return "original";
            case normal:
                return "normal";
            case indexed:
                return "indexed";
            case palette:
                return "palette";
            default:
                return "unknown";
        }
    }

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
     * ansi mode
     */
    public final ansiMode ansM;

    /**
     * ansi palette
     */
    public final int[] ansP;

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
    public final static int colBlack = 0;

    /**
     * red
     */
    public final static int colRed = 1;

    /**
     * green
     */
    public final static int colGreen = 2;

    /**
     * yellow
     */
    public final static int colYellow = 3;

    /**
     * blue
     */
    public final static int colBlue = 4;

    /**
     * magenta
     */
    public final static int colMagenta = 5;

    /**
     * cyan
     */
    public final static int colCyan = 6;

    /**
     * white
     */
    public final static int colWhite = 7;

    /**
     * bright/blink text
     */
    public final static int colBright = 8;

    /**
     * mask
     */
    public final static int colMask = 7;

    /**
     * bright black
     */
    public final static int colBrBlack = colBright | colBlack;

    /**
     * bright red
     */
    public final static int colBrRed = colBright | colRed;

    /**
     * bright green
     */
    public final static int colBrGreen = colBright | colGreen;

    /**
     * bright yellow
     */
    public final static int colBrYellow = colBright | colYellow;

    /**
     * bright blue
     */
    public final static int colBrBlue = colBright | colBlue;

    /**
     * bright magenta
     */
    public final static int colBrMagenta = colBright | colMagenta;

    /**
     * bright cyan
     */
    public final static int colBrCyan = colBright | colCyan;

    /**
     * bright white
     */
    public final static int colBrWhite = colBright | colWhite;

    /**
     * convert string to color code
     *
     * @param a color
     * @return code
     */
    public static int string2color(String a) {
        int i = 0;
        if (a.startsWith("bright-")) {
            a = a.substring(7, a.length());
            i = colBright;
        }
        if (a.equals("black")) {
            return i | colBlack;
        }
        if (a.equals("red")) {
            return i | colRed;
        }
        if (a.equals("green")) {
            return i | colGreen;
        }
        if (a.equals("yellow")) {
            return i | colYellow;
        }
        if (a.equals("blue")) {
            return i | colBlue;
        }
        if (a.equals("magenta")) {
            return i | colMagenta;
        }
        if (a.equals("cyan")) {
            return i | colCyan;
        }
        if (a.equals("white")) {
            return i | colWhite;
        }
        return -1;
    }

    /**
     * convert color code to string
     *
     * @param i code
     * @return color
     */
    public static String color2string(int i) {
        String a = "";
        if ((i & colBright) != 0) {
            a = "bright-";
        }
        switch (i & colMask) {
            case colBlack:
                return a + "black";
            case colRed:
                return a + "red";
            case colGreen:
                return a + "green";
            case colYellow:
                return a + "yellow";
            case colBlue:
                return a + "blue";
            case colMagenta:
                return a + "magenta";
            case colCyan:
                return a + "cyan";
            case colWhite:
                return a + "white";
        }
        return null;
    }

    /**
     * set background
     *
     * @param col original
     * @param bg new
     * @return updated
     */
    public static int setBackground(int col, int bg) {
        return (col & 0xffff) | (bg << 16);
    }

    /**
     * set foreground
     *
     * @param col original
     * @param fg new
     * @return updated
     */
    public static int setForeground(int col, int fg) {
        return (col & 0xffff0000) | (fg & 0xffff);
    }

    /**
     * create one screen
     *
     * @param p connection to use
     */
    public pipeScreen(pipeSide p) {
        int x = p.settingsGet(pipeSetting.width, 80);
        int y = p.settingsGet(pipeSetting.height, 25);
        ansM = p.settingsGet(pipeSetting.ansiMode, ansiMode.normal);
        switch (ansM) {
            case none:
                ansP = pipeFonts.colorMono;
                break;
            case original:
                ansP = pipeFonts.colorOrig;
                break;
            case normal:
                ansP = pipeFonts.colorData;
                break;
            case indexed:
                ansP = pipeFonts.colorIdxd;
                break;
            case palette:
                ansP = pipeFonts.colorIdxd;
                break;
            default:
                ansP = pipeFonts.colorData;
                break;
        }
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
        doReset();
        doClear();
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
            int old = ctr;
            ctr = string2control(s.substring(old + 1, s.length()));
            s = s.substring(0, old);
        }
        if (s.startsWith("O")) {
            ctr |= string2control(s.substring(1, s.length()));
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
            ctr |= string2control(s.substring(2, s.length()));
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

    private static int string2control(String str) {
        switch (bits.str2num(str)) {
            case 2: // shift
                return 0x100;
            case 3: // alt
                return 0x400;
            case 4: // alt+shift
                return 0x500;
            case 5: // ctrl
                return 0x200;
            case 6: // ctrl+shift
                return 0x300;
            case 7: // ctrl+alt
                return 0x600;
            case 8: // cltr+alt+shift
                return 0x700;
            default:
                return 0;
        }
    }

    /**
     * update terminal size
     *
     * @param pip pipe to use
     * @return true on error, false on success
     */
    public static boolean updtSiz(pipeSide pip) {
        int[] res = readSiz(pip);
        if (res == null) {
            return true;
        }
        pipeTerm.setTermWdt(pip, res[0]);
        pipeTerm.setTermLen(pip, res[1]);
        return false;
    }

    /**
     * detect terminal size
     *
     * @param pip pipe to use
     * @return null on error, array of x,y on success
     */
    public static int[] readSiz(pipeSide pip) {
        int[] sav = readRep(pip);
        if (sav == null) {
            return null;
        }
        sendCur(pip, 999, 999);
        int[] res = readRep(pip);
        sendCur(pip, sav[0] - 1, sav[1] - 1);
        return res;
    }

    private static int[] readRep(pipeSide pip) {
        pip.strPut("\033[6n");
        for (int r = 0; r < 10; r++) {
            pip.notif.misleep(100);
            byte[] buf = new byte[512];
            int i = pip.nonDestructiveGet(buf, 0, buf.length);
            if (i == pipeLine.tryLater) {
                continue;
            }
            if (i < 1) {
                return null;
            }
            String a = new String(buf, 0, i);
            int p = a.indexOf("\033[");
            if (p < 0) {
                continue;
            }
            a = a.substring(p + 2, a.length());
            i = a.indexOf("R");
            if (i < 0) {
                continue;
            }
            a = a.substring(0, i);
            i = a.indexOf(";");
            if (i < 0) {
                return null;
            }
            int res[] = new int[2];
            res[1] = bits.str2num(a.substring(0, i));
            res[0] = bits.str2num(a.substring(i + 1, a.length()));
            pip.nonBlockSkip(a.length() + p + 3);
            if (res[0] < 1) {
                return null;
            }
            if (res[1] < 1) {
                return null;
            }
            return res;
        }
        return null;
    }

    /**
     * send terminal title
     *
     * @param pip pipe to use
     * @param nam string to send
     */
    public static void sendTit(pipeSide pip, String nam) {
        pip.strPut("\033]0;" + nam + "\007");
    }

    /**
     * send clipboard data
     *
     * @param pip pipe to use
     * @param clp string to send
     */
    public static void sendClp(pipeSide pip, String clp) {
        pip.strPut("\033]52;pc;" + encBase64.encodeString(clp) + "\007");
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
     * send image in dec sixel format
     *
     * @param pip pipeline to use
     * @param col palette to use
     * @param chr dithering to use
     * @param img image to send
     * @param sx x size
     * @param sy y size
     */
    public static void sendImageSixel(pipeSide pip, int[] col, char[] chr, byte[] img, int sx, int sy) {
        pip.strPut("\033Pq");
        for (int i = 0; i < col.length; i++) {
            int c = col[i];
            int r = (c >>> 16) & 0xff;
            int g = (c >>> 8) & 0xff;
            int b = c & 0xff;
            pip.strPut("#" + i + ";2;" + ((r * 100) / 255) + ";" + ((g * 100) / 255) + ";" + ((b * 100) / 255));
        }
        int p = 0;
        for (int y = 0; y < sy; y++) {
            char b = (char) (1 << (y % 6));
            b += 63;
            int o = -1;
            String a = "";
            for (int x = 0; x < sx; x++) {
                int c = (img[p] & 0xff) / chr.length;
                p++;
                if (o != c) {
                    a += "#" + c;
                }
                o = c;
                a += b;
            }
            a += "$";
            if ((y % 6) == 5) {
                a += "-";
            }
            pip.strPut(a);
        }
        pip.strPut("$-\033\\");
    }

    /**
     * send terminal music
     *
     * @param pip pipeline to use
     * @param mus music to play
     */
    public static void sendMusicAnsi(pipeSide pip, String mus) {
        pip.strPut("\033[M" + mus + "\016");
    }

    /**
     * send terminal music
     *
     * @param pip pipeline to use
     * @param vol volume 0..7
     * @param ton tone 1..5
     * @param dur duration 0..255
     * @param mus music to play 1..25, 101..125, 41..137
     */
    public static void sendMusicDecps(pipeSide pip, int vol, int ton, int dur, List<Integer> mus) {
        pip.strPut("\033[[" + vol);
        if (ton > 0) {
            pip.strPut(":" + ton);
        }
        pip.strPut(";" + dur);
        for (int i = 0; i < mus.size(); i++) {
            int o = mus.get(i);
            if (o < 0) {
                continue;
            }
            pip.strPut(";" + o);
        }
        pip.strPut("],~");
    }

    /**
     * send terminal bell
     *
     * @param pip pipeline to use
     */
    public static void sendBeep(pipeSide pip) {
        pip.strPut("\007");
    }

    /**
     * send true colors
     *
     * @param pip pipeline to use
     * @param fg foreground rgb
     * @param bg background rgb
     */
    public static void sendTruCol(pipeSide pip, int fg, int bg) {
        pip.strPut("\033[38;2;" + ((fg >>> 16) & 0xff) + ";" + ((fg >>> 8) & 0xff) + ";" + (fg & 0xff) + "m");
        pip.strPut("\033[48;2;" + ((bg >>> 16) & 0xff) + ";" + ((bg >>> 8) & 0xff) + ";" + (bg & 0xff) + "m");
    }

    /**
     * send indexed colors
     *
     * @param pip pipeline to use
     * @param col color to use
     */
    public static void sendIdxCol(pipeSide pip, int col) {
        int bg = (col >>> 16) & 0xf;
        int fg = col & 0xf;
        pip.strPut("\033[38;5;" + (fg & 0xff) + "m");
        pip.strPut("\033[48;5;" + (bg & 0xff) + "m");
    }

    /**
     * send color change
     *
     * @param pip pipeline to use
     * @param col color to use
     */
    public static void sendOldCol(pipeSide pip, int col) {
        int bg = (col >>> 16) & 0xf;
        int fg = col & 0xf;
        String s = "\033[0";
        if ((bg & colBright) != 0) {
            s += ";5";
        }
        s += ";4" + (bg & colMask);
        if ((fg & colBright) != 0) {
            s += ";1";
        }
        s += ";3" + (fg & colMask);
        pip.strPut(s + "m");
    }

    /**
     * send color change
     *
     * @param pip pipeline to use
     * @param col color to use
     */
    public static void sendAnsCol(pipeSide pip, int col) {
        int bg = (col >>> 16) & 0xf;
        int fg = col & 0xf;
        String s = "\033[0";
        if ((bg & colBright) != 0) {
            bg = (bg & colMask) + 60;
        }
        if ((fg & colBright) != 0) {
            fg = (fg & colMask) + 60;
        }
        s += ";" + (bg + 40);
        s += ";" + (fg + 30);
        pip.strPut(s + "m");
    }

    private void doReset() {
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
        switch (ansM) {
            case original:
                sendOldCol(pipe, col);
                break;
            case normal:
                sendAnsCol(pipe, col);
                break;
            case indexed:
                sendIdxCol(pipe, col);
                break;
            case palette:
                sendTruCol(pipe, pipeFonts.colorIdxd[col & 0xf], pipeFonts.colorIdxd[(col >>> 16) & 0xf]);
                break;
        }
        remP = col;
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
     * get current screen
     *
     * @return ascii text
     */
    public List<String> getAscii() {
        List<String> res = new ArrayList<String>();
        for (int y = 0; y < sizY; y++) {
            String a = "";
            for (int x = 0; x < sizX; x++) {
                a += (char) chrs[y][x];
            }
            res.add(bits.trimE(a));
        }
        return res;
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
        sendCol(colWhite);
        sendCur(0, 0);
        sendCls(pipe);
        doReset();
        doClear();
    }

    /**
     * clear buffer
     */
    public void doClear() {
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
     * put other screen
     *
     * @param x x coordinate
     * @param y y coordinate
     * @param s screen to put
     * @param c true for cursor, false for contents
     */
    public void putScr(int x, int y, pipeScreen s, boolean c) {
        if (c) {
            curX = x + s.curX;
            curY = y + s.curY;
            return;
        }
        for (int o = 0; o < s.sizY; o++) {
            for (int i = 0; i < s.sizX; i++) {
                chrs[y + o][x + i] = s.chrs[o][i];
                atrs[y + o][x + i] = s.atrs[o][i];
            }
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
     * draw a line
     *
     * @param bx begin x
     * @param by begin y
     * @param ex end x
     * @param ey end y
     * @param bg background color
     * @param fg foreground color
     * @param ch character to write
     */
    public void drawLine(int bx, int by, int ex, int ey, int bg, int fg, int ch) {
        int step = Math.abs(bx - ex) + Math.abs(by - ey);
        ex -= bx;
        ey -= by;
        for (int i = 0; i < step; i++) {
            int x = (ex * i) / step;
            int y = (ey * i) / step;
            putInt(bx + x, by + y, bg, fg, false, ch);
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
            if (cl >= 0) {
                curY++;
                curX = 0;
            } else {
                curX = sizX - 1;
            }
        }
        if (curY >= sizY) {
            curY = sizY - 1;
            if (cl >= 0) {
                scrollUp(cl);
            }
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
            for (i = 0; i < (sy - 1); i++) {
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
