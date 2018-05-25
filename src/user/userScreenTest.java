package user;

import cfg.cfgAll;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.List;
import javax.imageio.ImageIO;
import util.bits;
import util.cmds;
import util.version;

/**
 * screen tester
 *
 * @author matecsaba
 */
public class userScreenTest {

    private userScreen scr;

    /**
     * screen tester
     *
     * @param conn connection to use
     */
    public userScreenTest(userScreen conn) {
        scr = conn;
    }

    /**
     * put one window
     *
     * @param pip screen to use
     * @param bg background color
     * @param fg foreground color
     * @param bx beginning x
     * @param by beginning y
     * @param sx size x
     * @param sy size y
     */
    public static void putWindow(userScreen pip, int bg, int fg, int bx, int by, int sx, int sy) {
        for (int i = 0; i < sy; i++) {
            pip.putStr(bx - 1, by + i, bg, fg, false, "|");
            pip.putStr(bx + sx, by + i, bg, fg, false, "|");
            pip.putCols(bx + sx + 1, by + i + 1, userScreen.colBlack, userScreen.colBrBlack, 2);
        }
        for (int o = 0; o < sx; o++) {
            for (int i = 0; i < sy; i++) {
                pip.putStr(bx + o, by + i, bg, fg, false, " ");
            }
            pip.putStr(bx + o, by, bg, fg, false, "-");
            pip.putStr(bx + o, by + sy, bg, fg, false, "-");
        }
        pip.putCols(bx + 1, by + sy + 1, userScreen.colBlack, userScreen.colBrBlack, sx + 2);
        pip.putStr(bx - 1, by, bg, fg, false, "+");
        pip.putStr(bx + sx, by, bg, fg, false, "+");
        pip.putStr(bx - 1, by + sy, bg, fg, false, "+");
        pip.putStr(bx + sx, by + sy, bg, fg, false, "+");
    }

    /**
     * ask user line
     *
     * @param pip screen to use
     * @param bg background color
     * @param fg foreground color
     * @param sx screen x
     * @param sy screen y
     * @param siz size of line
     * @param ln original line
     * @return edited line
     */
    public static String readLine(userScreen pip, int bg, int fg, int sx, int sy, int siz, String ln) {
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
            pip.putStr(sx, sy, bg, fg, false, bits.padEnd(ln.substring(beg, ln.length()), siz, " ").substring(0, siz));
            pip.putCur(sx + cur - beg, sy);
            pip.refresh();
            i = userVM.getKey(pip.pipe);
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
     * @param pip screen to use
     * @param bg background color
     * @param win window color
     * @param txt text color
     * @param bx screen x
     * @param by screen y
     * @param sx size x
     * @param sy size y
     * @param msg message
     */
    public static void helpWin(userScreen pip, int bg, int win, int txt, int bx, int by, int sx, int sy, List<String> msg) {
        if (bx < 0) {
            bx = 4;
        }
        if (by < 0) {
            by = 2;
        }
        if (sx < 0) {
            sx = pip.sizX - 8;
        }
        if (sy < 0) {
            sy = pip.sizY - 6;
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
            putWindow(pip, bg, win, bx, by, sx, sy);
            for (i = 0; i < sy - 1; i++) {
                String a;
                if ((cur + i) < msg.size()) {
                    a = msg.get(cur + i);
                } else {
                    a = "";
                }
                pip.putStr(bx, by + i + 1, bg, txt, false, bits.padEnd(a, sx, " ").substring(0, sx));
            }
            pip.putCur(bx, by + 1);
            pip.refresh();
            i = userVM.getKey(pip.pipe);
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
     * @param pip screen to use
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
    public static String askUser(userScreen pip, String que, int bg, int win, int tit, int txt, int sx, int sy, int siz, String ln) {
        if (sx < 0) {
            sx = 4;
        }
        if (sy < 0) {
            sy = (pip.sizY / 2) - 2;
        }
        if (siz < 0) {
            siz = pip.sizX - 8;
        }
        putWindow(pip, bg, win, sx, sy, siz, 3);
        pip.putStr(sx, sy + 1, bg, tit, false, que);
        return readLine(pip, bg, txt, sx, sy + 2, siz, ln);
    }

    /**
     * start screen
     */
    public void doStart() {
        scr.putCls();
    }

    /**
     * finish screen
     */
    public void doFinish() {
        scr.putCls();
        scr.refresh();
    }

    /**
     * palette test
     */
    public void doPalette() {
        scr.putCls();
        for (int i = 0; i < 16; i++) {
            int o = 15 - i;
            String a = bits.padEnd("  bg=" + o, 10, " ");
            String b = bits.padEnd("  fg=" + i, 10, " ");
            scr.putStr(10, i + 1, o, i, false, a + b);
            scr.putStr(40, i + 1, 0, i, false, b);
            scr.putStr(60, i + 1, o, 0, false, a);
        }
        scr.putCur(scr.sizX, scr.sizY);
        scr.refresh();
        for (;;) {
            if (scr.keyPress()) {
                break;
            }
            bits.sleep(1000);
        }
    }

    /**
     * ascii table
     */
    public void doAscTab() {
        scr.putCls();
        for (int o = 0; o < 16; o++) {
            scr.putStr(7, 2 + o, userScreen.colBlack, userScreen.colGreen, true, "" + o);
            scr.putStr(10 + (o * 3), 1, userScreen.colBlack, userScreen.colGreen, true, "" + o);
            for (int i = 0; i < 16; i++) {
                scr.putInt(10 + (i * 3), 2 + o, userScreen.colBlack, userScreen.colWhite, true, (o * 16) + i);
            }
        }
        scr.putCur(scr.sizX, scr.sizY);
        scr.refresh();
        for (;;) {
            if (scr.keyPress()) {
                break;
            }
            bits.sleep(1000);
        }
    }

    /**
     * flying text
     *
     * @param s text to use
     */
    public void doText(List<String> s) {
        int maxY = scr.sizY - s.size();
        if (maxY < 1) {
            return;
        }
        int maxX = 0;
        for (int i = 0; i < s.size(); i++) {
            int o = s.get(i).length();
            if (maxX < o) {
                maxX = o;
            }
        }
        maxX = scr.sizX - maxX;
        if (maxX < 1) {
            return;
        }
        for (;;) {
            if (scr.keyPress()) {
                break;
            }
            scr.putCls();
            scr.putMaps(bits.random(0, maxX), bits.random(0, maxY), 0, bits.random(1, 16), false, s);
            scr.refresh();
            bits.sleep(5000);
        }
    }

    /**
     * flying clock
     *
     * @param font font to use
     */
    public void doClock(byte[][][] font) {
        int maxX = scr.sizX - (font[0][0].length * 5);
        int maxY = scr.sizY - font[0].length;
        for (;;) {
            if (scr.keyPress()) {
                break;
            }
            String s = bits.time2str(cfgAll.timeZoneName, bits.getTime() + cfgAll.timeServerOffset, 2);
            s = s.substring(0, 5);
            scr.putCls();
            scr.putMaps(bits.random(0, maxX), bits.random(0, maxY), 0, bits.random(1, 16), false, userScreen.fontText(s, " ", userFonts1.fontFiller, font));
            scr.refresh();
            bits.sleep(5000);
        }
    }

    /**
     * moving snake
     */
    public void doSnake() {
        final int[] chars = {48, 48, 48, 48, 79, 79, 79, 79, 111, 111, 111, 111, 99, 99, 99, 99};
        int[] posX = new int[chars.length];
        int[] posY = new int[chars.length];
        int movX = -1;
        int movY = -1;
        int movC = 1;
        for (;;) {
            if (scr.keyPress()) {
                break;
            }
            for (int i = chars.length - 1; i > 0; i--) {
                posX[i] = posX[i - 1];
                posY[i] = posY[i - 1];
            }
            posX[0] += movX;
            posY[0] += movY;
            if ((posX[0] / 10) >= scr.sizX) {
                movX = -bits.random(1, 10);
                movC = bits.random(1, 16);
            }
            if ((posY[0] / 10) >= scr.sizY) {
                movY = -bits.random(1, 10);
                movC = bits.random(1, 16);
            }
            if (posX[0] <= 0) {
                movX = bits.random(1, 10);
                movC = bits.random(1, 16);
            }
            if (posY[0] <= 0) {
                movY = bits.random(1, 10);
                movC = bits.random(1, 16);
            }
            scr.putCls();
            for (int i = 0; i < chars.length; i++) {
                int x = posX[i] / 10;
                int y = posY[i] / 10;
                int c = chars[i];
                scr.putInt(x + 0, y, 0, movC, false, c);
                scr.putInt(x + 1, y, 0, movC, false, c);
            }
            scr.refresh();
            bits.sleep(500);
        }
    }

    /**
     * burning fire
     */
    public void doFire() {
        int[][] buf = new int[scr.sizY + 10][scr.sizX + 10];
        for (;;) {
            if (scr.keyPress()) {
                break;
            }
            int[][] old = buf;
            buf = new int[scr.sizY + 10][scr.sizX + 10];
            for (int y = 0; y < scr.sizY; y++) {
                for (int x = 0; x < scr.sizX; x++) {
                    int i = old[y + 4][x + 4] + old[y + 4][x + 5] + old[y + 4][x + 3] + old[y + 5][x + 4];
                    buf[y + 3][x + 4] = (i * 4) / 18;
                }
            }
            for (int i = 0; i < scr.sizX; i++) {
                buf[scr.sizY + 3][i + 4] = bits.random(0, 2) * 82;
            }
            scr.putCls();
            for (int y = 0; y < scr.sizY; y++) {
                for (int x = 0; x < scr.sizX; x++) {
                    int i = buf[y + 4][x + 4];
                    int o = userScreen.colBlack;
                    if (i > 3) {
                        o = userScreen.colBlue;
                    }
                    if (i > 11) {
                        o = userScreen.colRed;
                    }
                    if (i > 24) {
                        o = userScreen.colBrRed;
                    }
                    if (i > 45) {
                        o = userScreen.colBrYellow;
                    }
                    if (i > 70) {
                        o = userScreen.colBrWhite;
                    }
                    scr.putInt(x, y, userScreen.colBlack, o, false, 88);
                }
            }
            scr.refresh();
            bits.sleep(500);
        }
    }

    /**
     * life game if has 2 or 3 neighbors, then it stays alive, otherwise dies if
     * has 3 neighbors, then a new one borns
     */
    public void doLife() {
        int[][] buf = new int[scr.sizY + 10][scr.sizX + 10];
        for (int y = 0; y < scr.sizY; y++) {
            for (int x = 0; x < scr.sizX; x++) {
                int i = 0;
                if (bits.randomB() < 80) {
                    i = 1;
                }
                buf[y + 4][x + 4] = i;
            }
        }
        for (;;) {
            if (scr.keyPress()) {
                break;
            }
            int[][] old = buf;
            buf = new int[scr.sizY + 10][scr.sizX + 10];
            for (int y = 0; y < scr.sizY; y++) {
                for (int x = 0; x < scr.sizX; x++) {
                    int p = old[y + 3][x + 3] + old[y + 3][x + 4] + old[y + 3][x + 5] + old[y + 4][x + 3] + old[y + 4][x + 5] + old[y + 5][x + 3] + old[y + 5][x + 4] + old[y + 5][x + 5];
                    int o = old[y + 4][x + 4];
                    int i = 0;
                    if ((o == 1) && (p == 2)) {
                        i = 1;
                    }
                    if (p == 3) {
                        i = 1;
                    }
                    buf[y + 4][x + 4] = i;
                }
            }
            scr.putCls();
            for (int y = 0; y < scr.sizY; y++) {
                for (int x = 0; x < scr.sizX; x++) {
                    int i = buf[y + 4][x + 4];
                    if (i == 0) {
                        i = 32;
                    } else {
                        i = 88;
                    }
                    scr.putInt(x, y, userScreen.colBlack, userScreen.colWhite, false, i);
                }
            }
            scr.refresh();
            bits.sleep(500);
        }
    }

    private void doAntBall() {
        scr.putCls();
        for (;;) {
            if (scr.keyPress()) {
                return;
            }
            for (int o = 0; o < scr.sizY; o++) {
                for (int i = 0; i < scr.sizX; i++) {
                    int p;
                    if ((bits.randomB() & 1) == 0) {
                        p = userScreen.colBlack;
                    } else {
                        p = userScreen.colWhite;
                    }
                    scr.putStr(i, o, p, 0, false, " ");
                }
            }
            scr.refresh();
            bits.sleep(500);
        }
    }

    /**
     * do one command
     *
     * @param cmd parameters
     */
    public void doCommand(cmds cmd) {
        String a = cmd.word();
        if (a.equals("gomoku")) {
            userGomoku t = new userGomoku(scr);
            t.doStart();
            t.doGame();
            t.doFinish();
            return;
        }
        if (a.equals("tetris")) {
            userTetris t = new userTetris(scr);
            t.doStart();
            t.doGame();
            t.doFinish();
            return;
        }
        if (a.equals("minesweep")) {
            userMinesweep t = new userMinesweep(scr);
            t.doStart();
            t.doGame();
            t.doFinish();
            return;
        }
        if (a.equals("clear")) {
            return;
        }
        if (a.equals("color")) {
            doPalette();
            return;
        }
        if (a.equals("ascii")) {
            doAscTab();
            return;
        }
        if (a.equals("text")) {
            a = cmd.getRemaining();
            if (a.length() < 1) {
                a = version.namVer;
            }
            doText(bits.str2lst(a));
            return;
        }
        if (a.equals("logo")) {
            a = cmd.getRemaining();
            List<String> txt;
            if (a.length() < 1) {
                txt = version.shLogo(0x08);
            } else {
                txt = userScreen.fontText(a, " ", userFonts1.fontFiller, userFonts1.fontDefault());
            }
            doText(txt);
            return;
        }
        if (a.equals("image")) {
            BufferedImage img1;
            try {
                img1 = ImageIO.read(new File(cmd.getRemaining()));
            } catch (Exception e) {
                return;
            }
            doText(userScreen.imageText(img1, scr.sizX, scr.sizY, userFonts1.imageData));
            return;
        }
        if (a.equals("clock")) {
            doClock(userFonts1.fontDefault());
            return;
        }
        if (a.equals("snake")) {
            doSnake();
            return;
        }
        if (a.equals("fire")) {
            doFire();
            return;
        }
        if (a.equals("life")) {
            doLife();
            return;
        }
        if (a.equals("antball")) {
            doAntBall();
            return;
        }
        cmd.badCmd();
        return;
    }

}
