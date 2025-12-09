package org.freertr.user;

import org.freertr.pipe.pipeFonts;
import org.freertr.pipe.pipeScreen;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.freertr.auth.authResult;
import org.freertr.cfg.cfgAll;
import org.freertr.cfg.cfgInit;
import org.freertr.pipe.pipeSetting;
import org.freertr.serv.servQuote;
import org.freertr.util.bits;
import org.freertr.util.cmds;
import org.freertr.util.logger;

/**
 * screen games
 *
 * @author matecsaba
 */
public class userGame {

    private final pipeScreen console;

    private final userRead reader;

    /**
     * screen tester
     *
     * @param conn connection to use
     * @param rdr reader to use
     */
    public userGame(pipeScreen conn, userRead rdr) {
        console = conn;
        if (rdr == null) {
            rdr = new userRead(conn.pipe, null);
        }
        reader = rdr;
    }

    /**
     * start screen
     */
    public void doStart() {
        console.putCls();
    }

    /**
     * finish screen
     */
    public void doFinish() {
        console.putCls();
        console.refresh();
    }

    /**
     * send a broadcast message
     *
     * @param cmd command parser
     * @return messages sent
     */
    public String doSend(cmds cmd) {
        List<String> txt = new ArrayList<String>();
        String a = cmd.getRemaining().trim();
        if (a.length() > 0) {
            txt.add(a);
        } else {
            reader.keyFlush();
            doStart();
            userEditor e = new userEditor(console, txt, "message", false);
            boolean r = e.doEdit();
            doFinish();
            reader.keyFlush();
            if (r) {
                return "send cancelled";
            }
        }
        a = cmd.pipe.settingsGet(pipeSetting.authed, new authResult()).user + " from " + cmd.pipe.settingsGet(pipeSetting.origin, "?");
        return "sent to " + userLine.sendBcastMsg(a, txt) + " terminals";
    }

    /**
     * palette test
     */
    public void doPalette() {
        console.doClear();
        for (int i = 0; i < 16; i++) {
            int o = 15 - i;
            String a = bits.padEnd("  bg=" + o, 10, " ");
            String b = bits.padEnd("  fg=" + i, 10, " ");
            console.putStr(10, i + 1, o, i, false, a + b);
            console.putStr(40, i + 1, 0, i, false, b);
            console.putStr(60, i + 1, o, 0, false, a);
        }
        console.putCur(0, 0);
        console.refresh();
        pipeScreen.getKey(console.pipe);
    }

    /**
     * ascii table
     */
    public void doAscTab() {
        console.doClear();
        for (int o = 0; o < 16; o++) {
            console.putStr(7, 2 + o, pipeScreen.colBlack, pipeScreen.colGreen, true, "" + o);
            console.putStr(10 + (o * 3), 1, pipeScreen.colBlack, pipeScreen.colGreen, true, "" + o);
            for (int i = 0; i < 16; i++) {
                console.putInt(10 + (i * 3), 2 + o, pipeScreen.colBlack, pipeScreen.colWhite, true, (o * 16) + i);
            }
        }
        console.putCur(0, 0);
        console.refresh();
        pipeScreen.getKey(console.pipe);
    }

    /**
     * keyboard codes
     */
    public void doKeys() {
        console.doClear();
        console.putCur(0, 0);
        console.refresh();
        for (;;) {
            int i = pipeScreen.getKey(console.pipe);
            console.putStr(console.sizX / 2, console.sizY / 2, pipeScreen.colBlack, pipeScreen.colWhite, false, bits.toHexW(i) + " " + (i & 0xff) + "       ");
            console.putCur(0, 0);
            console.refresh();
            switch (i) {
                case -1: // end
                    return;
                case 0x0271: // ctrl+q
                    return;
                case 0x0278: // ctrl+x
                    return;
                case 0x801d: // f10
                    return;
                default:
                    break;
            }
        }
    }

    /**
     * flying text
     *
     * @param s text to use
     */
    public void doText(String s) {
        int maxX = console.sizX - s.length();
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            console.doClear();
            console.putStr(bits.random(0, maxX), bits.random(0, console.sizY), pipeScreen.colBlack, bits.random(1, 15), false, s);
            console.refresh();
            bits.sleep(5000);
        }
    }

    /**
     * flying logo
     *
     * @param s text to use
     */
    public void doLogo(List<String> s) {
        int maxY = console.sizY - s.size();
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
        maxX = console.sizX - maxX;
        if (maxX < 1) {
            return;
        }
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            console.doClear();
            console.putMaps(bits.random(0, maxX), bits.random(0, maxY), pipeScreen.colBlack, bits.random(1, 15), false, s);
            console.refresh();
            bits.sleep(5000);
        }
    }

    /**
     * draw clock
     *
     * @param a time
     * @param bg background color
     * @param fg foreground color
     */
    public void drawClock(String a, int bg, int fg) {
        int hlfX = console.sizX / 2;
        int hlfY = console.sizY / 2;
        int step = console.sizX + console.sizY;
        step *= 4;
        for (int i = -step; i < step; i++) {
            double v = i * Math.PI / step;
            int x = (int) (hlfX * Math.cos(v));
            int y = (int) (hlfY * Math.sin(v));
            console.putInt(hlfX + x, hlfY + y, bg, fg, false, '*');
        }
        drawClock(bg, fg, hlfX, hlfY, 0.6, bits.str2num(a.substring(0, 2)) / 12.0, '@');
        drawClock(bg, fg, hlfX, hlfY, 0.8, bits.str2num(a.substring(3, 5)) / 60.0, '#');
        if (a.length() < 8) {
            return;
        }
        drawClock(bg, fg, hlfX, hlfY, 1.0, bits.str2num(a.substring(6, 8)) / 60.0, '%');
    }

    private void drawClock(int bg, int fg, int bx, int by, double scl, double val, int ch) {
        val *= Math.PI * 2.0;
        val += Math.PI * 1.5;
        double px = bx * Math.cos(val) * scl;
        double py = by * Math.sin(val) * scl;
        console.drawLine(bx, by, bx + (int) px, by + (int) py, bg, fg, ch);
    }

    /**
     * flying clock
     */
    public void doClock() {
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            console.doClear();
            drawClock(bits.time2str(cfgAll.timeZoneName, bits.getTime(), 2), pipeScreen.colBlack, bits.random(1, 15));
            console.refresh();
            bits.sleep(5000);
        }
    }

    /**
     * flying clock
     *
     * @param font font to use
     */
    public void doClock(byte[][][] font) {
        int maxX = console.sizX - (font[0][0].length * 5);
        int maxY = console.sizY - font[0].length;
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            String s = bits.time2str(cfgAll.timeZoneName, bits.getTime() + cfgAll.timeServerOffset, 2);
            s = s.substring(0, 5);
            console.doClear();
            console.putMaps(bits.random(0, maxX), bits.random(0, maxY), pipeScreen.colBlack, bits.random(1, 15), false, pipeScreen.fontText(s, " ", pipeFonts.fontFiller, font));
            console.refresh();
            bits.sleep(5000);
        }
    }

    private byte[] getMatrixStr() {
        byte[] res = new byte[bits.random(4, console.curY * 3)];
        for (int i = 0; i < res.length; i++) {
            res[i] = (byte) bits.random(32, 127);
        }
        return res;
    }

    private boolean doMatrix(int x, int pos, byte[] str) {
        int len = str.length;
        for (int o = 0; o < console.sizY; o++) {
            int i = o + pos;
            if (i < 0) {
                continue;
            }
            if (i >= len) {
                continue;
            }
            int c = pipeScreen.colGreen;
            if (i >= (len - 2)) {
                c = pipeScreen.colBrGreen;
            }
            if (i >= (len - 1)) {
                c = pipeScreen.colBrWhite;
            }
            console.putInt(x, o, pipeScreen.colBlack, c, false, str[i]);
        }
        return pos > -len;
    }

    /**
     * falling columns
     */
    public void doMatrix() {
        int[] poss = new int[console.sizX / 2];
        byte[][] strs = new byte[poss.length][];
        for (int i = 0; i < strs.length; i++) {
            strs[i] = getMatrixStr();
            int len = strs[i].length;
            poss[i] = bits.random(-len, +len);
        }
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            console.doClear();
            for (int i = 0; i < poss.length; i++) {
                poss[i]--;
                if (doMatrix(i * 2, poss[i], strs[i])) {
                    continue;
                }
                strs[i] = getMatrixStr();
                int len = strs[i].length;
                poss[i] = bits.random(-len, +len);
            }
            console.refresh();
            bits.sleep(500);
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
            if (console.keyPress()) {
                break;
            }
            for (int i = chars.length - 1; i > 0; i--) {
                posX[i] = posX[i - 1];
                posY[i] = posY[i - 1];
            }
            posX[0] += movX;
            posY[0] += movY;
            if ((posX[0] / 10) >= console.sizX) {
                movX = -bits.random(1, 10);
                movC = bits.random(1, 16);
            }
            if ((posY[0] / 10) >= console.sizY) {
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
            console.doClear();
            for (int i = 0; i < chars.length; i++) {
                int x = posX[i] / 10;
                int y = posY[i] / 10;
                int c = chars[i];
                console.putInt(x + 0, y, 0, movC, false, c);
                console.putInt(x + 1, y, 0, movC, false, c);
            }
            console.refresh();
            bits.sleep(500);
        }
    }

    /**
     * moving fractal
     */
    public void doFractal() {
        double xmin = -2;
        double xmax = +2;
        double ymin = -2;
        double ymax = +2;
        int iter = 255;
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            double dx = (xmax - xmin) / console.sizX;
            double dy = (ymax - ymin) / console.sizY;
            for (int j = 0; j < console.sizY; j++) {
                double y = ymax - j * dy;
                for (int i = 0; i < console.sizX; i++) {
                    double u = 0.0;
                    double v = 0.0;
                    double u2 = u * u;
                    double v2 = v * v;
                    double x = xmin + i * dx;
                    int k;
                    for (k = 1; k < iter; k++) {
                        if ((u2 + v2) > 4.0) {
                            break;
                        }
                        v = 2 * u * v + y;
                        u = u2 - v2 + x;
                        u2 = u * u;
                        v2 = v * v;
                    }
                    if (k < iter) {
                        console.putInt(i, j, false, pipeScreen.colWhite, ' ');
                    } else {
                        console.putInt(i, j, false, pipeScreen.colWhite, '*');
                    }
                }
            }
            switch (bits.random(0, 6)) {
                case 0:
                    xmin *= 1.2;
                    xmax *= 1.2;
                    ymin *= 1.2;
                    ymax *= 1.2;
                    break;
                case 1:
                    xmin *= 0.8;
                    xmax *= 0.8;
                    ymin *= 0.8;
                    ymax *= 0.8;
                    break;
                case 2:
                    double mov = (xmax - xmin) * 0.1;
                    xmin += mov;
                    xmax += mov;
                    break;
                case 3:
                    mov = (xmax - xmin) * 0.1;
                    xmin -= mov;
                    xmax -= mov;
                    break;
                case 4:
                    mov = (ymax - ymin) * 0.1;
                    ymin += mov;
                    ymax += mov;
                    break;
                case 5:
                    mov = (ymax - ymin) * 0.1;
                    ymin -= mov;
                    ymax -= mov;
                    break;
            }
            console.refresh();
            bits.sleep(500);
        }
    }

    /**
     * rotating donut
     */
    public void doDonut() {
        userGameZbuf gfx = new userGameZbuf(console);
        int[] chrs = new int[]{'@', '#', '$', '%', '&', '*', '!'};
        int[] cols = new int[]{pipeScreen.colBrWhite, pipeScreen.colBrMagenta, pipeScreen.colBrCyan,
            pipeScreen.colBrGreen, pipeScreen.colBrRed, pipeScreen.colBrYellow, pipeScreen.colBrBlue};
        double r1 = 2;
        double r2 = 1;
        double tau = 2.0 * Math.PI;
        double mul = gfx.max / 2;
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            gfx.clear();
            gfx.rotate();
            for (double u = 0; u < tau; u += 0.07) {
                for (double v = 0; v < tau; v += 0.02) {
                    double x = (r1 + r2 * Math.cos(v)) * Math.cos(u) * mul;
                    double y = (r1 + r2 * Math.cos(v)) * Math.sin(u) * mul;
                    double z = r2 * Math.sin(v) * mul;
                    int p = (int) (cols.length * u / tau);
                    gfx.pixelR(x, y, z, pipeScreen.colBlack, cols[p], chrs[p]);
                }
            }
            gfx.refresh();
            bits.sleep(500);
        }
    }

    /**
     * rotating cube
     */
    public void doCube() {
        userGameZbuf gfx = new userGameZbuf(console);
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            gfx.clear();
            gfx.rotate();
            for (int x = -gfx.max; x < gfx.max; x += 1) {
                for (int y = -gfx.max; y < gfx.max; y += 1) {
                    gfx.pixelR(x, y, -gfx.max, pipeScreen.colBlack, pipeScreen.colBrBlue, '@');
                    gfx.pixelR(gfx.max, y, x, pipeScreen.colBlack, pipeScreen.colBrCyan, '#');
                    gfx.pixelR(-gfx.max, y, -x, pipeScreen.colBlack, pipeScreen.colBrGreen, '$');
                    gfx.pixelR(-x, y, gfx.max, pipeScreen.colBlack, pipeScreen.colBrMagenta, '%');
                    gfx.pixelR(x, -gfx.max, -y, pipeScreen.colBlack, pipeScreen.colBrRed, '&');
                    gfx.pixelR(x, gfx.max, y, pipeScreen.colBlack, pipeScreen.colBrYellow, '*');
                }
            }
            gfx.refresh();
            bits.sleep(500);
        }
    }

    /**
     * moving plasma
     */
    public void doPlasma() {
        int[] plasma1 = new int[console.sizY * console.sizX * 4];
        int[] plasma2 = new int[console.sizY * console.sizX * 4];
        int i = 0;
        for (int y = 0; y < console.sizY * 2; y++) {
            for (int x = 0; x < console.sizX * 2; x++) {
                plasma1[i] = (int) (64 + 63 * (Math.sin(Math.sqrt((console.sizY - y) * (console.sizY - y) + (console.sizX - x) * (console.sizX - x)) / (console.sizX / 20))));
                plasma2[i] = (int) (64 + 63 * Math.sin(x / (74 + 15 * Math.cos(y / 140))) * Math.cos(y / (61 + 11 * Math.sin(x / 114))));
                i++;
            }
        }
        int fc = bits.randomD();
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            int src1 = console.sizX / 2;
            int src2 = console.sizY / 2;
            int x1 = (int) Math.floor(src1 + ((src1 - 1) * Math.cos(fc / 97)));
            int x2 = (int) Math.floor(src1 + ((src1 - 1) * Math.sin(fc / -114)));
            int x3 = (int) Math.floor(src1 + ((src1 - 1) * Math.sin(fc / -137)));
            int y1 = (int) Math.floor(src2 + ((src2 - 1) * Math.sin(fc / 123)));
            int y2 = (int) Math.floor(src2 + ((src2 - 1) * Math.cos(fc / -75)));
            int y3 = (int) Math.floor(src2 + ((src2 - 1) * Math.cos(fc / -108)));
            src1 = y1 * console.sizX * 2 + x1;
            src2 = y2 * console.sizX * 2 + x2;
            int src3 = y3 * console.sizX * 2 + x3;
            for (int y = 0; y < console.sizY; y++) {
                for (int x = 0; x < console.sizX; x++) {
                    i = (plasma1[src1++] + plasma2[src2++] + plasma2[src3++]) & 0xFF;
                    console.putInt(x, y, pipeScreen.colBlack, i >>> 4, false, 88);
                }
                src1 += console.sizX;
                src2 += console.sizX;
                src3 += console.sizX;
            }
            fc += 2;
            console.refresh();
            bits.sleep(500);
        }
    }

    /**
     * burning fire
     */
    public void doFire() {
        int[][] buf = new int[console.sizY + 10][console.sizX + 10];
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            int[][] old = buf;
            buf = new int[console.sizY + 10][console.sizX + 10];
            for (int y = 0; y < console.sizY; y++) {
                for (int x = 0; x < console.sizX; x++) {
                    int i = old[y + 4][x + 4] + old[y + 4][x + 5] + old[y + 4][x + 3] + old[y + 5][x + 4];
                    buf[y + 3][x + 4] = (i * 4) / 18;
                }
            }
            for (int i = 0; i < console.sizX; i++) {
                buf[console.sizY + 3][i + 4] = bits.random(0, 2) * 82;
            }
            console.doClear();
            for (int y = 0; y < console.sizY; y++) {
                for (int x = 0; x < console.sizX; x++) {
                    int i = buf[y + 4][x + 4];
                    int o = pipeScreen.colBlack;
                    if (i > 3) {
                        o = pipeScreen.colBlue;
                    }
                    if (i > 11) {
                        o = pipeScreen.colRed;
                    }
                    if (i > 24) {
                        o = pipeScreen.colBrRed;
                    }
                    if (i > 45) {
                        o = pipeScreen.colBrYellow;
                    }
                    if (i > 70) {
                        o = pipeScreen.colBrWhite;
                    }
                    console.putInt(x, y, pipeScreen.colBlack, o, false, 88);
                }
            }
            console.refresh();
            bits.sleep(500);
        }
    }

    /**
     * life game if has 2 or 3 neighbors, then it stays alive, otherwise dies if
     * has 3 neighbors, then a new one borns
     */
    public void doLife() {
        int[][] buf = new int[console.sizY + 10][console.sizX + 10];
        for (int y = 0; y < console.sizY; y++) {
            for (int x = 0; x < console.sizX; x++) {
                int i = 0;
                if (bits.randomB() < 80) {
                    i = 1;
                }
                buf[y + 4][x + 4] = i;
            }
        }
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            int[][] old = buf;
            buf = new int[console.sizY + 10][console.sizX + 10];
            for (int y = 0; y < console.sizY; y++) {
                for (int x = 0; x < console.sizX; x++) {
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
            console.doClear();
            for (int y = 0; y < console.sizY; y++) {
                for (int x = 0; x < console.sizX; x++) {
                    int i = buf[y + 4][x + 4];
                    if (i == 0) {
                        i = 32;
                    } else {
                        i = 88;
                    }
                    console.putInt(x, y, pipeScreen.colBlack, pipeScreen.colWhite, false, i);
                }
            }
            console.refresh();
            bits.sleep(500);
        }
    }

    private void doAntBall() {
        console.doClear();
        for (;;) {
            if (console.keyPress()) {
                return;
            }
            for (int o = 0; o < console.sizY; o++) {
                for (int i = 0; i < console.sizX; i++) {
                    int p;
                    if ((bits.randomB() & 1) == 0) {
                        p = pipeScreen.colBlack;
                    } else {
                        p = pipeScreen.colWhite;
                    }
                    console.putStr(i, o, p, 0, false, " ");
                }
            }
            console.refresh();
            bits.sleep(500);
        }
    }

    /**
     * random quotes from zen master
     *
     * @param cmd command line to use
     */
    public void doZenmaster(cmds cmd) {
        servQuote ntry = new servQuote();
        ntry.srvName = cmd.word();
        ntry = cfgAll.dmnQuote.find(ntry, false);
        if (ntry == null) {
            cmd.error("no such server");
            return;
        }
        for (;;) {
            cmd.pipe.strPut("you: ");
            String a = cmd.pipe.lineGet(0x12);
            if (a.length() < 1) {
                if (cmd.pipe.isClosed() != 0) {
                    break;
                }
                continue;
            }
            cmd.pipe.linePut("");
            String b = a.trim().toLowerCase();
            if (b.equals("quit") || b.equals("exit")) {
                break;
            }
            cmd.pipe.linePut("zen: " + ntry.getOneLine());
        }
    }

    /**
     * breakout
     */
    public void doBreakout() {
        int cols[] = new int[]{pipeScreen.colBrBlack, pipeScreen.colBrRed, pipeScreen.colBrYellow, pipeScreen.colBrBlue, pipeScreen.colBrMagenta, pipeScreen.colBrGreen};
        boolean[][] del = new boolean[6][console.sizX / 4];
        int px = console.sizX / 2;
        int pm = 3;
        int bx = px + 2;
        int by = console.sizY - 2;
        int dx = bits.random(-2, 2);
        int dy = -1;
        for (;;) {
            console.doClear();
            for (int y = 0; y < del.length; y++) {
                for (int x = 0; x < del[0].length; x++) {
                    if (del[y][x]) {
                        continue;
                    }
                    console.putStr(x * 4, y, pipeScreen.colBlack, cols[y], false, "###");
                }
            }
            console.putStr(px, console.sizY - 1, pipeScreen.colBlack, pipeScreen.colBrWhite, false, "^^^^^^^");
            console.putInt(bx, by, pipeScreen.colBlack, pipeScreen.colBrCyan, false, '@');
            console.refresh();
            for (; console.keyPress();) {
                int i = pipeScreen.getKey(console.pipe);
                switch (i) {
                    case -1: // end
                        return;
                    case 0x800c: // up
                        pm = 0;
                        break;
                    case 0x800d: // down
                        pm = 0;
                        break;
                    case 0x800e: // left
                        pm = -3;
                        break;
                    case 0x800f: // right
                        pm = 3;
                        break;
                    case 0x0271: // ctrl+q
                        return;
                    case 0x0278: // ctrl+x
                        return;
                    case 0x8005: // escape
                        return;
                }
            }
            bits.sleep(500);
            bx += dx;
            by += dy;
            px += pm;
            if (px < 0) {
                px = 0;
                pm = 3;
            }
            if (px >= (console.sizX - 7)) {
                px = console.sizX - 7;
                pm = -3;
            }
            if (bx < 0) {
                bx = dx;
                dx = bits.random(1, 2);
            }
            if (bx >= console.sizX) {
                bx = console.sizX - dx;
                dx = -bits.random(1, 2);
            }
            boolean bnc = by < 0;
            if (!bnc && (by < del.length)) {
                int i = bx / 4;
                if (i >= del[0].length) {
                    i = del[0].length - 1;
                }
                bnc = !del[by][i];
                del[by][i] = true;
            }
            if (bnc) {
                by -= dy;
                dy = 1;
                dx = bits.random(-2, 2);
            }
            if (by < console.sizY) {
                continue;
            }
            if (bx < px) {
                break;
            }
            if (bx > (px + 7)) {
                break;
            }
            by = console.sizY - 1;
            dy = -1;
            dx = bits.random(-2, 2);
        }
    }

    /**
     * nibbles
     */
    public void doNibbles() {
        List<Integer> px = new ArrayList<Integer>();
        List<Integer> py = new ArrayList<Integer>();
        int dx = -1;
        int dy = 0;
        int cx = console.sizX / 4;
        int cy = console.sizY / 2;
        int fx = bits.random(0, console.sizX / 2);
        int fy = bits.random(0, console.sizY);
        for (int i = 1; i < 5; i++) {
            px.add(cx + i);
            py.add(cy);
        }
        for (;;) {
            console.doClear();
            for (int i = 0; i < px.size(); i++) {
                console.putStr(px.get(i) * 2, py.get(i), pipeScreen.colBlack, pipeScreen.colBrGreen, false, "##");
            }
            console.putStr(fx * 2, fy, pipeScreen.colBlack, pipeScreen.colBrMagenta, false, "$$");
            console.putStr(cx * 2, cy, pipeScreen.colBlack, pipeScreen.colBrCyan, true, "@@");
            console.refresh();
            for (; console.keyPress();) {
                int i = pipeScreen.getKey(console.pipe);
                switch (i) {
                    case -1: // end
                        return;
                    case 0x800c: // up
                        dx = 0;
                        dy = -1;
                        break;
                    case 0x800d: // down
                        dx = 0;
                        dy = +1;
                        break;
                    case 0x800e: // left
                        dy = 0;
                        dx = -1;
                        break;
                    case 0x800f: // right
                        dy = 0;
                        dx = +1;
                        break;
                    case 0x0271: // ctrl+q
                        return;
                    case 0x0278: // ctrl+x
                        return;
                    case 0x8005: // escape
                        return;
                }
            }
            bits.sleep(500);
            px.add(0, cx);
            py.add(0, cy);
            cx += dx;
            cy += dy;
            if (cx < 0) {
                break;
            }
            if (cy < 0) {
                break;
            }
            if (cx >= (console.sizX / 2)) {
                break;
            }
            if (cy >= console.sizY) {
                break;
            }
            boolean bit = false;
            for (int i = 1; i < px.size(); i++) {
                if (cx != px.get(i)) {
                    continue;
                }
                if (cy != py.get(i)) {
                    continue;
                }
                bit = true;
                break;
            }
            if (bit) {
                break;
            }
            if ((cx == fx) && (cy == fy)) {
                fx = bits.random(0, console.sizX / 2);
                fy = bits.random(0, console.sizY);
                continue;
            }
            px.remove(px.size() - 1);
            py.remove(py.size() - 1);
        }
    }

    /**
     * tower of hanoi
     */
    public void doHanoi() {
        List<List<Integer>> towerC = new ArrayList<List<Integer>>();
        int towerX[] = new int[3];
        int curX = 0;
        int selX = -1;
        console.doClear();
        int towerS = towerX.length + 2;
        for (int i = 0; i < towerX.length; i++) {
            towerX[i] = (i + 1) * (console.sizX / towerS);
            towerC.add(new ArrayList<Integer>());
        }
        List<Integer> tw = towerC.get(0);
        for (int i = 10; i > 0; i--) {
            tw.add(Integer.valueOf(i));
        }
        for (;;) {
            console.doClear();
            for (int i = 0; i < towerX.length; i++) {
                tw = towerC.get(i);
                for (int o = 0; o < tw.size(); o++) {
                    int p = tw.get(o);
                    int r = console.sizY - o - 1;
                    for (int q = 0; q <= p; q++) {
                        console.putInt(towerX[i] - q, r, false, pipeScreen.colBrGreen, 'X');
                        console.putInt(towerX[i] + q, r, false, pipeScreen.colBrGreen, 'X');
                    }
                }
            }
            for (int i = 0; i < towerX.length; i++) {
                int p = pipeScreen.colWhite;
                if (selX == i) {
                    p = pipeScreen.colBrCyan;
                }
                if (curX == i) {
                    p = pipeScreen.colBrYellow;
                }
                for (int o = 0; o < console.sizY; o++) {
                    console.putInt(towerX[i], o, false, p, '|');
                }
            }
            console.putCur(towerX[curX], 0);
            console.refresh();
            int i = pipeScreen.getKey(console.pipe);
            switch (i) {
                case -1: // end
                    return;
                case 0x800e: // left
                    curX--;
                    break;
                case 0x800f: // right
                    curX++;
                    break;
                case 0x8004: // enter
                    if (selX < 0) {
                        selX = curX;
                        break;
                    }
                    if (selX == curX) {
                        selX = -1;
                        break;
                    }
                    List<Integer> src = towerC.get(selX);
                    if (src.size() < 1) {
                        selX = -1;
                        break;
                    }
                    List<Integer> trg = towerC.get(curX);
                    int val = src.get(src.size() - 1);
                    boolean ok = trg.size() < 1;
                    if (!ok) {
                        ok = trg.get(trg.size() - 1) > val;
                    }
                    if (!ok) {
                        selX = -1;
                        break;
                    }
                    trg.add(val);
                    src.remove(src.size() - 1);
                    selX = -1;
                    break;
                case 0x0271: // ctrl+q
                    return;
                case 0x0278: // ctrl+x
                    return;
            }
            if (curX < 0) {
                curX = 0;
            }
            if (curX >= towerX.length) {
                curX = towerX.length - 1;
            }
        }
    }

    /**
     * maze game
     */
    public void doMaze() {
        byte[][] maze = new byte[console.sizY - 2][console.sizX - 2];
        for (int y = 0; y < maze.length; y++) {
            for (int x = 0; x < maze[0].length; x++) {
                int i = 0;
                if (bits.randomB() < 80) {
                    maze[y][x] = 1;
                }
            }
        }
        for (int rnd = 0; rnd < 16; rnd++) {
            byte[][] old = maze;
            maze = new byte[maze.length][maze[0].length];
            for (int y = 0; y < (maze.length - 2); y++) {
                for (int x = 0; x < (maze[0].length - 2); x++) {
                    int p = old[y + 0][x + 0] + old[y + 0][x + 1] + old[y + 0][x + 2] + old[y + 1][x + 0] + old[y + 1][x + 2] + old[y + 2][x + 0] + old[y + 2][x + 1] + old[y + 2][x + 2];
                    int o = old[y + 1][x + 1];
                    byte i = 0;
                    if ((o == 1) && (p <= 4)) {
                        i = 1;
                    }
                    if (p == 3) {
                        i = 1;
                    }
                    maze[y + 1][x + 1] = i;
                }
            }
        }
        for (int y = 0; y < maze.length; y++) {
            maze[y][0] = 1;
            maze[y][maze[0].length - 1] = 1;
        }
        for (int x = 0; x < maze[0].length; x++) {
            maze[0][x] = 1;
            maze[maze.length - 1][x] = 1;
        }
        int curx = 0;
        int cury = 0;
        int dir = 0;
        for (int rnd = 0; rnd < 128; rnd++) {
            curx = bits.random(1, maze[0].length - 1);
            cury = bits.random(1, maze.length - 1);
            if (maze[cury][curx] == 0) {
                break;
            }
        }
        for (;;) {
            int lftx;
            int lfty;
            int rgtx;
            int rgty;
            int advx;
            int advy;
            switch (dir) {
                case 0: // north
                    lftx = -1;
                    lfty = 0;
                    rgtx = 1;
                    rgty = 0;
                    advx = 0;
                    advy = -1;
                    break;
                case 1: // west
                    lftx = 0;
                    lfty = 1;
                    rgtx = 0;
                    rgty = -1;
                    advx = -1;
                    advy = 0;
                    break;
                case 2: // south
                    lftx = 1;
                    lfty = 0;
                    rgtx = -1;
                    rgty = 0;
                    advx = 0;
                    advy = 1;
                    break;
                case 3: // east
                    lftx = 0;
                    lfty = -1;
                    rgtx = 0;
                    rgty = 1;
                    advx = 1;
                    advy = 0;
                    break;
                default:
                    return;
            }
            console.doClear();
            int dszx = console.sizX / 10;
            int dszy = console.sizY / 10;
            int posx = curx;
            int posy = cury;
            for (int step = 0; step < 6; step++) {
                int sszx = step * dszx;
                int sszy = step * dszy;
                int pszx = (step + 1) * dszx;
                int pszy = (step + 1) * dszy;
                if (maze[posy + rgty][posx + rgtx] != 0) {
                    console.drawLine(console.sizX - sszx, sszy, console.sizX - pszx, pszy, pipeScreen.colBlack, pipeScreen.colBrWhite, '*');
                    console.drawLine(console.sizX - sszx, console.sizY - sszy, console.sizX - pszx, console.sizY - pszy, pipeScreen.colBlack, pipeScreen.colBrWhite, '*');
                } else {
                    console.drawLine(console.sizX - sszx, sszy, console.sizX - sszx, console.sizY - sszy, pipeScreen.colBlack, pipeScreen.colBrWhite, '*');
                    if (maze[posy + advy][posx + advx] == 0) {
                        console.drawLine(console.sizX - pszx, pszy, console.sizX - pszx, console.sizY - pszy, pipeScreen.colBlack, pipeScreen.colBrWhite, '*');
                    }
                    console.drawLine(console.sizX - pszx, pszy, console.sizX - sszx, pszy, pipeScreen.colBlack, pipeScreen.colBrWhite, '*');
                    console.drawLine(console.sizX - pszx, console.sizY - pszy, console.sizX - sszx, console.sizY - pszy, pipeScreen.colBlack, pipeScreen.colBrWhite, '*');
                }
                if (maze[posy + lfty][posx + lftx] != 0) {
                    console.drawLine(sszx, sszy, pszx, pszy, pipeScreen.colBlack, pipeScreen.colBrWhite, '*');
                    console.drawLine(sszx, console.sizY - sszy, pszx, console.sizY - pszy, pipeScreen.colBlack, pipeScreen.colBrWhite, '*');
                } else {
                    console.drawLine(sszx, sszy, sszx, console.sizY - sszy, pipeScreen.colBlack, pipeScreen.colBrWhite, '*');
                    if (maze[posy + advy][posx + advx] == 0) {
                        console.drawLine(pszx, pszy, pszx, console.sizY - pszy, pipeScreen.colBlack, pipeScreen.colBrWhite, '*');
                    }
                    console.drawLine(pszx, pszy, sszx, pszy, pipeScreen.colBlack, pipeScreen.colBrWhite, '*');
                    console.drawLine(pszx, console.sizY - pszy, sszx, console.sizY - pszy, pipeScreen.colBlack, pipeScreen.colBrWhite, '*');
                }
                posx += advx;
                posy += advy;
                if (maze[posy][posx] == 0) {
                    continue;
                }
                posx = console.sizX - pszx;
                posy = console.sizY - pszy;
                console.drawLine(posx, posy, pszx, posy, pipeScreen.colBlack, pipeScreen.colBrWhite, '*');
                console.drawLine(pszx, posy, pszx, pszy, pipeScreen.colBlack, pipeScreen.colBrWhite, '*');
                console.drawLine(pszx, pszy, posx, pszy, pipeScreen.colBlack, pipeScreen.colBrWhite, '*');
                console.drawLine(posx, pszy, posx, posy, pipeScreen.colBlack, pipeScreen.colBrWhite, '*');
                break;
            }
            console.refresh();
            boolean map = false;
            int i = pipeScreen.getKey(console.pipe);
            switch (i) {
                case -1: // end
                    return;
                case 0x800c: // up
                    if (maze[cury + advy][curx + advx] != 0) {
                        break;
                    }
                    curx += advx;
                    cury += advy;
                    break;
                case 0x800d: // down
                    if (maze[cury - advy][curx - advx] != 0) {
                        break;
                    }
                    curx -= advx;
                    cury -= advy;
                    break;
                case 0x800e: // left
                    dir = (dir + 1) & 3;
                    break;
                case 0x800f: // right
                    dir = (dir - 1) & 3;
                    break;
                case 0x8002: // tabulator
                    map = true;
                    break;
                case 0x8004: // enter
                    map = true;
                    break;
                case 0x0271: // ctrl+q
                    return;
                case 0x0278: // ctrl+x
                    return;
                case 0x8005: // escape
                    return;
            }
            if (!map) {
                continue;
            }
            console.doClear();
            for (int y = 0; y < console.sizY; y++) {
                if (y >= maze.length) {
                    break;
                }
                for (int x = 0; x < console.sizX; x++) {
                    if (x >= maze[0].length) {
                        break;
                    }
                    if (maze[y][x] == 0) {
                        continue;
                    }
                    console.putStr(x, y, pipeScreen.colBlack, pipeScreen.colWhite, false, "#");
                }
            }
            console.putStr(curx, cury, pipeScreen.colBlack, pipeScreen.colBrGreen, false, "@");
            console.refresh();
            pipeScreen.getKey(console.pipe);
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
            userGameGomoku t = new userGameGomoku(console);
            t.doStart();
            t.doGame();
            t.doFinish();
            return;
        }
        if (a.equals("tetris")) {
            userGameTetris t = new userGameTetris(console);
            t.doStart();
            t.doGame();
            t.doFinish();
            return;
        }
        if (a.equals("chess")) {
            userGameChess t = new userGameChess(console);
            t.doStart();
            t.doGame();
            t.doFinish();
            return;
        }
        if (a.equals("minesweep")) {
            userGameMines t = new userGameMines(console);
            t.doStart();
            t.doGame();
            t.doFinish();
            return;
        }
        if (a.equals("racer")) {
            userGameRacer t = new userGameRacer(console);
            t.doStart();
            t.doGame();
            t.doFinish();
            return;
        }
        if (a.equals("hanoi")) {
            doHanoi();
            return;
        }
        if (a.equals("nibbles")) {
            doNibbles();
            return;
        }
        if (a.equals("breakout")) {
            doBreakout();
            return;
        }
        if (a.equals("clear")) {
            pipeScreen.sendTit(console.pipe, cfgAll.hostName);
            return;
        }
        if (a.equals("chat")) {
            userChat c = new userChat(cmd.pipe, reader);
            c.doChat();
            return;
        }
        if (a.equals("send")) {
            a = doSend(cmd);
            cmd.error(a);
            return;
        }
        if (a.equals("ansi")) {
            userFlash.ansiArt(cmd.getRemaining(), console);
            pipeScreen.getKey(console.pipe);
            return;
        }
        if (a.equals("movie")) {
            userFlash.ansiAnim(cmd.getRemaining(), console);
            pipeScreen.getKey(console.pipe);
            return;
        }
        if (a.equals("sixel")) {
            userFlash.ansiPix(cmd.getRemaining(), console);
            pipeScreen.getKey(console.pipe);
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
        if (a.equals("vmkeys")) {
            doKeys();
            return;
        }
        if (a.equals("title")) {
            a = cmd.getRemaining();
            if (a.length() < 1) {
                a = cfgAll.hostName;
            }
            pipeScreen.sendTit(console.pipe, a);
            return;
        }
        if (a.equals("text")) {
            a = cmd.getRemaining();
            if (a.length() < 1) {
                a = cfgInit.versionName;
            }
            doText(a);
            return;
        }
        if (a.equals("logo")) {
            a = cmd.getRemaining();
            List<String> txt;
            if (a.length() < 1) {
                txt = cfgInit.getShLogo(0x08);
            } else {
                txt = pipeScreen.fontText(a, " ", pipeFonts.fontFiller, pipeFonts.font8x16());
            }
            doLogo(txt);
            return;
        }
        if (a.equals("time")) {
            doClock(pipeFonts.font8x16());
            return;
        }
        if (a.equals("clock")) {
            doClock();
            return;
        }
        if (a.equals("snake")) {
            doSnake();
            return;
        }
        if (a.equals("matrix")) {
            doMatrix();
            return;
        }
        if (a.equals("fire")) {
            doFire();
            return;
        }
        if (a.equals("plasma")) {
            doPlasma();
            return;
        }
        if (a.equals("zenmaster")) {
            doZenmaster(cmd);
            return;
        }
        if (a.equals("maze")) {
            doMaze();
            return;
        }
        if (a.equals("cube")) {
            doCube();
            return;
        }
        if (a.equals("donut")) {
            doDonut();
            return;
        }
        if (a.equals("fractal")) {
            doFractal();
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
        List<String> lst = cfgInit.secretsFind(a);
        if (lst == null) {
            cmd.badCmd();
            return;
        }
        int[] god = new int[]{pipeScreen.colBrCyan, pipeScreen.colBrWhite, pipeScreen.colBrYellow,
            pipeScreen.colBrGreen, pipeScreen.colBrBlue, pipeScreen.colBrRed};
        console.doClear();
        for (int o = 0; o < lst.size(); o++) {
            String s = lst.get(o);
            byte[] b = s.getBytes();
            for (int i = 0; i < b.length; i++) {
                int ch = b[i];
                int cl = userFormat.zeroesColor(ch, pipeScreen.colBrGreen, god);
                console.putInt(i, o, false, cl, ch);
            }
        }
        console.refresh();
        pipeScreen.getKey(console.pipe);
    }

}

class userGameZbuf {

    public final pipeScreen scr;

    public final double[][] dep;

    public final int max;

    public double a = 0;

    public double b = 0;

    public double c = 0;

    /**
     * create instance
     *
     * @param s screen to use
     */
    public userGameZbuf(pipeScreen s) {
        scr = s;
        dep = new double[scr.sizY][scr.sizX];
        if (scr.sizX > scr.sizY) {
            max = scr.sizY;
        } else {
            max = scr.sizX;
        }
        clear();
    }

    public void clear() {
        for (int i = 0; i < scr.sizY; i++) {
            for (int o = 0; o < scr.sizX; o++) {
                dep[i][o] = Double.MIN_VALUE;
            }
        }
        scr.doClear();
    }

    public void putCls() {
        clear();
        scr.doClear();
    }

    public void rotate() {
        a += bits.random(10, 20) / 100.0;
        b += bits.random(10, 20) / 100.0;
        c += bits.random(20, 40) / 100.0;
    }

    public void refresh() {
        scr.refresh();
    }

    public void pixelR(double cx, double cy, double cz, int bg, int fg, int ch) {
        double x = cy * Math.sin(a) * Math.sin(b) * Math.cos(c)
                - cz * Math.cos(a) * Math.sin(b) * Math.cos(c)
                + cy * Math.cos(a) * Math.sin(c)
                + cz * Math.sin(a) * Math.sin(c)
                + cx * Math.cos(b) * Math.cos(c);
        double y = cy * Math.cos(a) * Math.cos(c)
                + cz * Math.sin(a) * Math.cos(c)
                - cy * Math.sin(a) * Math.sin(b) * Math.sin(c)
                + cz * Math.cos(a) * Math.sin(b) * Math.sin(c)
                - cx * Math.cos(b) * Math.sin(c);
        double z = cz * Math.cos(a) * Math.cos(b)
                - cy * Math.sin(a) * Math.cos(b)
                + cx * Math.sin(b) + max * 3;
        if (z == 0.0) {
            z = 0.000001;
        }
        double ooz = max / z / 1.6;
        int px = (int) (scr.sizX / 2 + ooz * x * 2);
        int py = (int) (scr.sizY / 2 + ooz * y);
        pixelZ(px, py, ooz, bg, fg, ch);
    }

    public void pixelZ(int x, int y, double z, int bg, int fg, int ch) {
        if (x < 0) {
            return;
        }
        if (y < 0) {
            return;
        }
        if (x >= scr.sizX) {
            return;
        }
        if (y >= scr.sizY) {
            return;
        }
        if (z <= dep[y][x]) {
            return;
        }
        dep[y][x] = z;
        scr.putInt(x, y, bg, fg, false, ch);
    }

}

class userGameGomoku {

    private int[][] store;

    private int curX;

    private int curY;

    private pipeScreen scr;

    private final static int sizeX = 30;

    private final static int sizeY = 20;

    /**
     * create game
     *
     * @param screen screen to use
     */
    public userGameGomoku(pipeScreen screen) {
        scr = screen;
    }

    /**
     * start screen
     */
    public void doStart() {
        scr.doClear();
        store = new int[sizeX][sizeY];
        curX = sizeX / 2;
        curY = sizeY / 2;
    }

    /**
     * print table
     */
    public void doPrint() {
        scr.doClear();
        for (int i = 0; i < sizeY; i++) {
            for (int o = 0; o < sizeX; o++) {
                String s;
                int p;
                switch (store[o][i]) {
                    case 1:
                        s = "X";
                        p = pipeScreen.colGreen;
                        break;
                    case 2:
                        s = "O";
                        p = pipeScreen.colRed;
                        break;
                    default:
                        s = ".";
                        p = pipeScreen.colWhite;
                        break;
                }
                scr.putStr(o * 2, i, pipeScreen.colBlack, p, false, s);
            }
        }
        scr.putCur(curX * 2, curY);
        scr.refresh();
    }

    /**
     * play game
     */
    public void doGame() {
        for (;;) {
            doPrint();
            int i = pipeScreen.getKey(scr.pipe);
            switch (i) {
                case -1: // end
                    return;
                case 0x800c: // up
                    curY--;
                    break;
                case 0x800d: // down
                    curY++;
                    break;
                case 0x800e: // left
                    curX--;
                    break;
                case 0x800f: // right
                    curX++;
                    break;
                case 0x8004: // enter
                    if (store[curX][curY] != 0) {
                        break;
                    }
                    store[curX][curY] = 1;
                    int[] ps = evalMove(2);
                    curX = ps[0];
                    curY = ps[1];
                    store[curX][curY] = 2;
                    break;
                case 0x0271: // ctrl+q
                    return;
                case 0x0278: // ctrl+x
                    return;
            }
            if (curX < 0) {
                curX = 0;
            }
            if (curY < 0) {
                curY = 0;
            }
            if (curX >= sizeX) {
                curX = sizeX - 1;
            }
            if (curY >= sizeY) {
                curY = sizeY - 1;
            }
        }
    }

    /**
     * finish screen
     */
    public void doFinish() {
        scr.doClear();
        scr.refresh();
    }

    private int get(int x, int y) {
        try {
            return store[x][y];
        } catch (Exception e) {
            return 99;
        }
    }

    private int evalDir(int x, int y, int dx, int dy, int ai) {
        int free = 0;
        int mine = 0;
        boolean own = true;
        for (int i = 1; i < 6; i++) {
            int o = get(x + (i * dx), y + (i * dy));
            if (own && (o == ai)) {
                mine++;
                continue;
            }
            if (o == 0) {
                own = false;
                free++;
                continue;
            }
            break;
        }
        own = true;
        for (int i = 1; i < 6; i++) {
            int o = get(x - (i * dx), y - (i * dy));
            if (own && (o == ai)) {
                mine++;
                continue;
            }
            if (o == 0) {
                own = false;
                free++;
                continue;
            }
            break;
        }
        if (mine + free < 5) {
            return 0;
        }
        if (mine > 5) {
            mine = 5;
        }
        return mine;
    }

    private int evalScore(int x, int y, int ai) {
        final int[] scoreOwn = {0, 2, 4, 6, 100, 1000};
        final int[] scoreOtr = {0, 1, 3, 7, 51, 500};
        int pl = 3 - ai;
        if (store[x][y] != 0) {
            return 0;
        }
        return +scoreOwn[evalDir(x, y, 0, 1, ai)]
                + scoreOwn[evalDir(x, y, 1, 0, ai)]
                + scoreOwn[evalDir(x, y, 1, 1, ai)]
                + scoreOwn[evalDir(x, y, 1, -1, ai)]
                + scoreOtr[evalDir(x, y, 0, 1, pl)]
                + scoreOtr[evalDir(x, y, 1, 0, pl)]
                + scoreOtr[evalDir(x, y, 1, 1, pl)]
                + scoreOtr[evalDir(x, y, 1, -1, pl)];
    }

    private int[] evalMove(int ai) {
        int score = 0;
        int[] pos = new int[2];
        for (int x = 0; x < sizeX; x++) {
            for (int y = 0; y < sizeY; y++) {
                int temp = evalScore(x, y, ai);
                if (temp < score) {
                    continue;
                }
                if (temp == score) {
                    if (bits.randomB() < 128) {
                        continue;
                    }
                }
                score = temp;
                pos[0] = x;
                pos[1] = y;
            }
        }
        return pos;
    }

}

class userGameMines {

    private boolean[][] bomb;

    private boolean[][] mark;

    private boolean[][] show;

    private int curX;

    private int curY;

    private pipeScreen scr;

    private final static int sizeX = 60;

    private final static int sizeY = 20;

    /**
     * create game
     *
     * @param screen screen to use
     */
    public userGameMines(pipeScreen screen) {
        scr = screen;
    }

    /**
     * start screen
     */
    public void doStart() {
        scr.doClear();
        bomb = new boolean[sizeY][sizeX];
        mark = new boolean[sizeY][sizeX];
        show = new boolean[sizeY][sizeX];
        for (int i = 0; i < (sizeX * sizeY / 10); i++) {
            bomb[bits.random(0, sizeY)][bits.random(0, sizeX)] = true;
        }
        curX = sizeX / 2;
        curY = sizeY / 2;
    }

    /**
     * print table
     */
    public void doPrint() {
        scr.doClear();
        for (int i = 0; i < sizeX; i++) {
            for (int o = 0; o < sizeY; o++) {
                String s;
                int p;
                if (show[o][i]) {
                    s = "" + bombs(o, i);
                    p = pipeScreen.colGreen;
                } else {
                    s = ".";
                    p = pipeScreen.colWhite;
                }
                if (mark[o][i]) {
                    s = "*";
                    p = pipeScreen.colRed;
                }
                scr.putStr(i, o, pipeScreen.colBlack, p, false, s);
            }
        }
        scr.putCur(curX, curY);
        scr.refresh();
    }

    /**
     * play game
     */
    public void doGame() {
        for (;;) {
            doPrint();
            int i = pipeScreen.getKey(scr.pipe);
            switch (i) {
                case -1: // end
                    return;
                case 0x800c: // up
                    curY--;
                    break;
                case 0x800d: // down
                    curY++;
                    break;
                case 0x800e: // left
                    curX--;
                    break;
                case 0x800f: // right
                    curX++;
                    break;
                case 0x8004: // enter
                    if (bomb[curY][curX]) {
                        return;
                    }
                    shows(curY, curX);
                    break;
                case 0x0020: // space
                    mark[curY][curX] = !mark[curY][curX];
                    break;
                case 0x0271: // ctrl+q
                    return;
                case 0x0278: // ctrl+x
                    return;
            }
            if (curX < 0) {
                curX = 0;
            }
            if (curY < 0) {
                curY = 0;
            }
            if (curX >= sizeX) {
                curX = sizeX - 1;
            }
            if (curY >= sizeY) {
                curY = sizeY - 1;
            }
        }
    }

    /**
     * finish screen
     */
    public void doFinish() {
        scr.doClear();
        scr.refresh();
    }

    private boolean get(int y, int x) {
        try {
            return bomb[y][x];
        } catch (Exception e) {
            return false;
        }
    }

    private int bombs(int y, int x) {
        if (get(y, x)) {
            return -1;
        }
        int res = 0;
        if (get(y - 1, x - 1)) {
            res++;
        }
        if (get(y, x - 1)) {
            res++;
        }
        if (get(y + 1, x - 1)) {
            res++;
        }
        if (get(y - 1, x)) {
            res++;
        }
        if (get(y + 1, x)) {
            res++;
        }
        if (get(y - 1, x + 1)) {
            res++;
        }
        if (get(y, x + 1)) {
            res++;
        }
        if (get(y + 1, x + 1)) {
            res++;
        }
        return res;
    }

    private void shows(int y, int x) {
        try {
            if (show[y][x]) {
                return;
            }
        } catch (Exception e) {
            return;
        }
        show[y][x] = true;
        if (bombs(y, x) != 0) {
            return;
        }
        shows(y - 1, x - 1);
        shows(y, x - 1);
        shows(y + 1, x - 1);
        shows(y - 1, x);
        shows(y + 1, x);
        shows(y - 1, x + 1);
        shows(y, x + 1);
        shows(y + 1, x + 1);
    }

}

class userGameTetris implements Runnable {

    private pipeScreen scr;

    private List<userGameTetrisThing> things;

    private int[][] tab;

    private int curX;

    private int curY;

    private int lines;

    private userGameTetrisThing thg;

    private userGameTetrisThing nxt;

    private boolean need2run;

    private final static int sizeX = 10;

    private final static int sizeY = 20;

    /**
     * create game
     *
     * @param screen screen to use
     */
    public userGameTetris(pipeScreen screen) {
        scr = screen;
        things = new ArrayList<userGameTetrisThing>();
        String[] lst = new String[3];
        lst[0] = " X";
        lst[1] = "XX";
        lst[2] = "X ";
        things.add(userGameTetrisThing.fromString(lst, pipeScreen.colBrBlue));
        lst[0] = "X ";
        lst[1] = "XX";
        lst[2] = " X";
        things.add(userGameTetrisThing.fromString(lst, pipeScreen.colBrGreen));
        lst[0] = "X ";
        lst[1] = "XX";
        lst[2] = "X ";
        things.add(userGameTetrisThing.fromString(lst, pipeScreen.colBrCyan));
        lst[0] = "X ";
        lst[1] = "X ";
        lst[2] = "XX";
        things.add(userGameTetrisThing.fromString(lst, pipeScreen.colBrYellow));
        lst[0] = " X";
        lst[1] = " X";
        lst[2] = "XX";
        things.add(userGameTetrisThing.fromString(lst, pipeScreen.colBrWhite));
        lst = new String[4];
        lst[0] = "X";
        lst[1] = "X";
        lst[2] = "X";
        lst[3] = "X";
        things.add(userGameTetrisThing.fromString(lst, pipeScreen.colBrMagenta));
        lst = new String[2];
        lst[0] = "XX";
        lst[1] = "XX";
        things.add(userGameTetrisThing.fromString(lst, pipeScreen.colBrRed));
    }

    /**
     * start screen
     */
    public void doStart() {
        need2run = true;
        scr.doClear();
        tab = new int[sizeY][];
        thg = things.get(bits.random(0, things.size())).copyBytes();
        nxt = things.get(bits.random(0, things.size())).copyBytes();
        curY = 0;
        curX = sizeX / 2;
        for (int o = 0; o < sizeY; o++) {
            tab[o] = new int[sizeX];
            for (int i = 0; i < sizeX; i++) {
                tab[o][i] = pipeScreen.colBlack;
            }
        }
        lines = 0;
        new Thread(this).start();
    }

    /**
     * print table
     */
    public synchronized void doPrint() {
        scr.doClear();
        for (int o = 0; o < sizeY; o++) {
            scr.putStr(19, o + 1, pipeScreen.colBlack, pipeScreen.colWhite, false, "|");
            scr.putStr(20 + (sizeX * 2), o + 1, pipeScreen.colBlack, pipeScreen.colWhite, false, "|");
            for (int i = 0; i < sizeX; i++) {
                if (tab[o][i] == pipeScreen.colBlack) {
                    continue;
                }
                scr.putStr((i * 2) + 20, o + 1, tab[o][i], tab[o][i], false, "XX");
            }
        }
        nxt.doPrint(2, 2, scr);
        scr.putStr(2, 10, pipeScreen.colBlack, pipeScreen.colWhite, false, "lines=" + lines);
        thg.doPrint((curX * 2) + 20, curY + 1, scr);
        scr.putCur(0, 0);
        scr.refresh();
    }

    /**
     * finish screen
     */
    public void doFinish() {
        need2run = false;
        scr.doClear();
        scr.refresh();
    }

    /**
     * play game
     */
    public void doGame() {
        for (;;) {
            doPrint();
            int i = pipeScreen.getKey(scr.pipe);
            if (!need2run) {
                return;
            }
            switch (i) {
                case -1: // end
                    return;
                case 0x800c: // up
                    userGameTetrisThing o = thg.copyBytes();
                    thg = thg.doRotate();
                    if (thg.isSpace(tab, curX, curY)) {
                        break;
                    }
                    thg = o;
                    break;
                case 0x800d: // down
                    curY++;
                    if (thg.isSpace(tab, curX, curY)) {
                        break;
                    }
                    curY--;
                    break;
                case 0x800e: // left
                    curX--;
                    if (thg.isSpace(tab, curX, curY)) {
                        break;
                    }
                    curX++;
                    break;
                case 0x800f: // right
                    curX++;
                    if (thg.isSpace(tab, curX, curY)) {
                        break;
                    }
                    curX--;
                    break;
                case 0x0271: // ctrl+q
                    return;
                case 0x0278: // ctrl+x
                    return;
            }
        }
    }

    private void doLines() {
        for (int o = sizeY - 1; o >= 0; o--) {
            int p = 0;
            for (int i = 0; i < sizeX; i++) {
                if (tab[o][i] == pipeScreen.colBlack) {
                    continue;
                }
                p++;
            }
            if (p < sizeX) {
                continue;
            }
            for (int i = o; i > 0; i--) {
                tab[i] = tab[i - 1];
            }
            tab[0] = new int[sizeX];
            for (int i = 0; i < sizeX; i++) {
                tab[0][i] = pipeScreen.colBlack;
            }
            lines++;
            o++;
        }
    }

    public void run() {
        try {
            for (;;) {
                doPrint();
                bits.sleep(1000);
                if (!need2run) {
                    return;
                }
                curY++;
                if (thg.isSpace(tab, curX, curY)) {
                    continue;
                }
                curY--;
                thg.putThing(tab, curX, curY);
                doLines();
                thg = nxt;
                nxt = things.get(bits.random(0, things.size())).copyBytes();
                curY = 0;
                curX = sizeX / 2;
                if (!thg.isSpace(tab, curX, curY)) {
                    break;
                }
            }
        } catch (Exception e) {
            logger.traceback(e);
        }
        need2run = false;
    }

}

class userGameTetrisThing {

    private byte[][] tab;

    private int sizeX;

    private int sizeY;

    public userGameTetrisThing(int sx, int sy) {
        sizeX = sx;
        sizeY = sy;
        tab = new byte[sizeY][];
        for (int o = 0; o < sizeY; o++) {
            tab[o] = new byte[sizeX];
            for (int i = 0; i < sizeX; i++) {
                tab[o][i] = pipeScreen.colBlack;
            }
        }
    }

    public static userGameTetrisThing fromString(String[] lst, int col) {
        userGameTetrisThing t = new userGameTetrisThing(lst[0].length(), lst.length);
        for (int o = 0; o < t.sizeY; o++) {
            String a = lst[o];
            for (int i = 0; i < t.sizeX; i++) {
                t.tab[o][i] = a.substring(i, i + 1).equals("X") ? (byte) col : pipeScreen.colBlack;
            }
        }
        return t;
    }

    public userGameTetrisThing copyBytes() {
        userGameTetrisThing t = new userGameTetrisThing(sizeX, sizeY);
        for (int o = 0; o < sizeY; o++) {
            bits.byteCopy(tab[o], 0, t.tab[o], 0, sizeX);
        }
        return t;
    }

    public userGameTetrisThing doRotate() {
        userGameTetrisThing t = new userGameTetrisThing(sizeY, sizeX);
        for (int o = 0; o < sizeY; o++) {
            for (int i = 0; i < sizeX; i++) {
                t.tab[i][o] = tab[o][sizeX - i - 1];
            }
        }
        return t;
    }

    public void doPrint(int x, int y, pipeScreen scr) {
        for (int o = 0; o < sizeY; o++) {
            for (int i = 0; i < sizeX; i++) {
                if (tab[o][i] == pipeScreen.colBlack) {
                    continue;
                }
                scr.putStr((i * 2) + x, o + y, tab[o][i], tab[o][i], false, "XX");
            }
        }
    }

    public boolean isSpace(int[][] t, int x, int y) {
        if (x < 0) {
            return false;
        }
        if (x > (t[0].length - sizeX)) {
            return false;
        }
        if ((y + sizeY) > t.length) {
            return false;
        }
        for (int o = 0; o < sizeY; o++) {
            for (int i = 0; i < sizeX; i++) {
                if (tab[o][i] == pipeScreen.colBlack) {
                    continue;
                }
                if (t[o + y][i + x] != pipeScreen.colBlack) {
                    return false;
                }
            }
        }
        return true;
    }

    public void putThing(int[][] t, int x, int y) {
        for (int o = 0; o < sizeY; o++) {
            for (int i = 0; i < sizeX; i++) {
                if (tab[o][i] == pipeScreen.colBlack) {
                    continue;
                }
                t[o + y][i + x] = tab[o][i];
            }
        }
    }

}

class userGameChess {

    private pipeScreen scr;

    private byte[] tab = new byte[]{
        // 0x10=black, 0x20=white, 1=pawn, 2=knight, 3=bishop, 4=rook, 5=queen, 6=king
        0x14, 0x12, 0x13, 0x15, 0x16, 0x13, 0x12, 0x14,
        0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21, 0x21,
        0x24, 0x22, 0x23, 0x25, 0x26, 0x23, 0x22, 0x24
    };

    private List<Integer> mov; // moves, bytes are fx fy tx ty

    private final static String[][] thingScr = {
        {// empty
            "      ",
            "      ",
            "      ",},
        {// pawn
            "  XX  ",
            "  XX  ",
            " /XX\\ ",},
        {// knight
            " /XX  ",
            "  XX  ",
            " /XX\\ ",},
        {// bishop
            " /XX\\ ",
            "  XX  ",
            " /XX\\ ",},
        {// rook
            " |XX| ",
            "  XX  ",
            " /XX\\ ",},
        {// queen
            " \\**/ ",
            "  XX  ",
            " /XX\\ ",},
        {// king
            " \\++/ ",
            "  XX  ",
            " /XX\\ ",},
        {// empty
            "      ",
            "      ",
            "      ",}};

    private final static int[][] thingPos = {
        { //empty
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,},
        {//pawn
            0, 0, 0, 0, 0, 0, 0, 0,
            50, 50, 50, 50, 50, 50, 50, 50,
            10, 10, 20, 30, 30, 20, 10, 10,
            5, 5, 10, 25, 25, 10, 5, 5,
            0, 0, 0, 20, 20, 0, 0, 0,
            5, -5, -10, 0, 0, -10, -5, 5,
            5, 10, 10, -20, -20, 10, 10, 5,
            0, 0, 0, 0, 0, 0, 0, 0
        }, {//knight
            -50, -40, -30, -30, -30, -30, -40, -50,
            -40, -20, 0, 0, 0, 0, -20, -40,
            -30, 0, 10, 15, 15, 10, 0, -30,
            -30, 5, 15, 20, 20, 15, 5, -30,
            -30, 0, 15, 20, 20, 15, 0, -30,
            -30, 5, 10, 15, 15, 10, 5, -30,
            -40, -20, 0, 5, 5, 0, -20, -40,
            -50, -40, -30, -30, -30, -30, -40, -50
        }, {//bishop
            -20, -10, -10, -10, -10, -10, -10, -20,
            -10, 0, 0, 0, 0, 0, 0, -10,
            -10, 0, 5, 10, 10, 5, 0, -10,
            -10, 5, 5, 10, 10, 5, 5, -10,
            -10, 0, 10, 10, 10, 10, 0, -10,
            -10, 10, 10, 10, 10, 10, 10, -10,
            -10, 5, 0, 0, 0, 0, 5, -10,
            -20, -10, -10, -10, -10, -10, -10, -20
        }, {//rook
            0, 0, 0, 0, 0, 0, 0, 0,
            5, 10, 10, 10, 10, 10, 10, 5,
            -5, 0, 0, 0, 0, 0, 0, -5,
            -5, 0, 0, 0, 0, 0, 0, -5,
            -5, 0, 0, 0, 0, 0, 0, -5,
            -5, 0, 0, 0, 0, 0, 0, -5,
            -5, 0, 0, 0, 0, 0, 0, -5,
            0, 0, 0, 5, 5, 0, 0, 0
        }, {//queen
            -20, -10, -10, -5, -5, -10, -10, -20,
            -10, 0, 0, 0, 0, 0, 0, -10,
            -10, 0, 5, 5, 5, 5, 0, -10,
            -5, 0, 5, 5, 5, 5, 0, -5,
            0, 0, 5, 5, 5, 5, 0, -5,
            -10, 5, 5, 5, 5, 5, 0, -10,
            -10, 0, 5, 0, 0, 0, 0, -10,
            -20, -10, -10, -5, -5, -10, -10, -20
        }, {//king
            -50, -40, -30, -20, -20, -30, -40, -50,
            -30, -20, -10, 0, 0, -10, -20, -30,
            -30, -10, 20, 30, 30, 20, -10, -30,
            -30, -10, 30, 40, 40, 30, -10, -30,
            -30, -10, 30, 40, 40, 30, -10, -30,
            -30, -10, 20, 30, 30, 20, -10, -30,
            -30, -30, 0, 0, 0, 0, -30, -30,
            -50, -30, -30, -30, -30, -30, -30, -50
        }, { //empty
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,}};

    private final static int[] thingVal = {0, 100, 320, 330, 500, 900, 2000, 0};

    /**
     * create game
     *
     * @param screen screen to use
     */
    public userGameChess(pipeScreen screen) {
        scr = screen;
    }

    private void doPrint(int x, int y, int b) {
        int v = tab[(y * 8) + x];
        int f;
        switch (v >>> 4) {
            case 0: // empty
                f = pipeScreen.colBlack;
                break;
            case 1: // black
                f = pipeScreen.colBrRed;
                break;
            case 2: // white
                f = pipeScreen.colBrGreen;
                break;
            default:
                return;
        }
        String[] cur = thingScr[v & 7];
        x = (scr.sizX / 2) - 24 + (x * 6);
        y = (scr.sizY / 2) - 12 + (y * 3);
        for (int i = 0; i < 3; i++) {
            scr.putStr(x, y + i, b, f, false, cur[i]);
        }
    }

    private int doEval(int col) {
        int p = 0;
        for (int i = 0; i < tab.length; i++) {
            if ((tab[i] & 0xf0) != col) {
                continue;
            }
            int o = tab[i] & 7;
            p += thingPos[o][i];
            p += thingVal[o];
        }
        return p;
    }

    private int addMove(int fx, int fy, int tx, int ty, boolean hit) {
        if (tx < 0) {
            return -1;
        }
        if (ty < 0) {
            return -1;
        }
        if (tx >= 8) {
            return -1;
        }
        if (ty >= 8) {
            return -1;
        }
        int v = tab[(ty * 8) + tx];
        if ((v & 0xf0) == (tab[(fy * 8) + fx] & 0xf0)) {
            return v;
        }
        if (hit && (v == 0)) {
            return v;
        }
        mov.add((fy << 24) | (fx << 16) | (ty << 8) | tx);
        return v;
    }

    private void doMove(int x, int y, int c) {
        int v = tab[(y * 8) + x];
        if ((v & 0xf0) != c) {
            return;
        }
        int p;
        switch (v & 7) {
            case 1: // pawn
                if (c == 0x10) {
                    p = +1;
                } else {
                    p = -1;
                }
                addMove(x, y, x, y + p, false);
                addMove(x, y, x - 1, y + p, true);
                addMove(x, y, x + 1, y + p, true);
                break;
            case 2: // knight
                addMove(x, y, x - 2, y - 1, false);
                addMove(x, y, x + 2, y - 1, false);
                addMove(x, y, x - 2, y + 1, false);
                addMove(x, y, x + 2, y + 1, false);
                addMove(x, y, x - 1, y - 2, false);
                addMove(x, y, x + 1, y - 2, false);
                addMove(x, y, x - 1, y + 2, false);
                addMove(x, y, x + 1, y + 2, false);
                break;
            case 3: // bishop
                for (int i = 1; i < 8; i++) {
                    if (addMove(x, y, x + i, y + i, false) > 0) {
                        break;
                    }
                }
                for (int i = 1; i < 8; i++) {
                    if (addMove(x, y, x - i, y + i, false) > 0) {
                        break;
                    }
                }
                for (int i = 1; i < 8; i++) {
                    if (addMove(x, y, x + i, y - i, false) > 0) {
                        break;
                    }
                }
                for (int i = 1; i < 8; i++) {
                    if (addMove(x, y, x - i, y - i, false) > 0) {
                        break;
                    }
                }
                break;
            case 4: // rook
                for (int i = 1; i < 8; i++) {
                    if (addMove(x, y, x + i, y, false) > 0) {
                        break;
                    }
                }
                for (int i = 1; i < 8; i++) {
                    if (addMove(x, y, x - i, y, false) > 0) {
                        break;
                    }
                }
                for (int i = 1; i < 8; i++) {
                    if (addMove(x, y, x, y + i, false) > 0) {
                        break;
                    }
                }
                for (int i = 1; i < 8; i++) {
                    if (addMove(x, y, x, y - i, false) > 0) {
                        break;
                    }
                }
                break;
            case 5: // queen
                for (int i = 1; i < 8; i++) {
                    if (addMove(x, y, x + i, y + i, false) > 0) {
                        break;
                    }
                }
                for (int i = 1; i < 8; i++) {
                    if (addMove(x, y, x - i, y + i, false) > 0) {
                        break;
                    }
                }
                for (int i = 1; i < 8; i++) {
                    if (addMove(x, y, x + i, y - i, false) > 0) {
                        break;
                    }
                }
                for (int i = 1; i < 8; i++) {
                    if (addMove(x, y, x - i, y - i, false) > 0) {
                        break;
                    }
                }
                for (int i = 1; i < 8; i++) {
                    if (addMove(x, y, x + i, y, false) > 0) {
                        break;
                    }
                }
                for (int i = 1; i < 8; i++) {
                    if (addMove(x, y, x - i, y, false) > 0) {
                        break;
                    }
                }
                for (int i = 1; i < 8; i++) {
                    if (addMove(x, y, x, y + i, false) > 0) {
                        break;
                    }
                }
                for (int i = 1; i < 8; i++) {
                    if (addMove(x, y, x, y - i, false) > 0) {
                        break;
                    }
                }
                break;
            case 6: // king
                addMove(x, y, x - 1, y - 1, false);
                addMove(x, y, x, y - 1, false);
                addMove(x, y, x + 1, y - 1, false);
                addMove(x, y, x - 1, y, false);
                addMove(x, y, x + 1, y, false);
                addMove(x, y, x - 1, y + 1, false);
                addMove(x, y, x, y + 1, false);
                addMove(x, y, x + 1, y + 1, false);
                break;
        }
    }

    private void doMove(int c) {
        mov = new ArrayList<Integer>();
        for (int y = 0; y < 8; y++) {
            for (int x = 0; x < 8; x++) {
                doMove(x, y, c);
            }
        }
    }

    private void doMov(int m) {
        int fy = (m >> 24) & 7;
        int fx = (m >> 16) & 7;
        int ty = (m >> 8) & 7;
        int tx = m & 7;
        tab[(ty * 8) + tx] = tab[(fy * 8) + fx];
        tab[(fy * 8) + fx] = 0;
    }

    private void doPrint() {
        for (int y = 0; y < 8; y++) {
            for (int x = 0; x < 8; x++) {
                doPrint(x, y, (((x + y) & 1) == 0) ? pipeScreen.colBrBlack : pipeScreen.colBlack);
            }
        }
    }

    /**
     * start screen
     */
    public void doStart() {
        scr.doClear();
    }

    /**
     * play game
     */
    public void doGame() {
        int curX = 4;
        int curY = 6;
        int selX = -1;
        int selY = -1;
        for (;;) {
            doPrint();
            doPrint(curX, curY, pipeScreen.colBlue);
            if (selX >= 0) {
                doPrint(selX, selY, pipeScreen.colGreen);
            }
            scr.refresh();
            int i = pipeScreen.getKey(scr.pipe);
            boolean moved = false;
            switch (i) {
                case -1: // end
                    return;
                case 0x8008: // home
                    curX--;
                    curY--;
                    break;
                case 0x8009: // end
                    curX--;
                    curY++;
                    break;
                case 0x800a: // pgup
                    curX++;
                    curY--;
                    break;
                case 0x800b: // pgdn
                    curX++;
                    curY++;
                    break;
                case 0x800c: // up
                    curY--;
                    break;
                case 0x800d: // down
                    curY++;
                    break;
                case 0x800e: // left
                    curX--;
                    break;
                case 0x800f: // right
                    curX++;
                    break;
                case 0x8004: // enter
                    if (selX >= 0) {
                        moved = true;
                        break;
                    }
                    selX = curX;
                    selY = curY;
                    break;
                case 0x8005: // escape
                    selX = -1;
                    selY = -1;
                    break;
                case 0x0271: // ctrl+q
                    return;
                case 0x0278: // ctrl+x
                    return;
            }
            if (curX < 0) {
                curX = 0;
            }
            if (curY < 0) {
                curY = 0;
            }
            if (curX > 7) {
                curX = 7;
            }
            if (curY > 7) {
                curY = 7;
            }
            if (!moved) {
                continue;
            }
            doMove(0x20);
            i = (selY << 24) | (selX << 16) | (curY << 8) | curX;
            if (mov.indexOf(i) < 0) {
                selX = -1;
                selY = -1;
                continue;
            }
            doMov(i);
            selX = -1;
            selY = -1;
            doMove(0x10);
            byte[] old = tab;
            tab = new byte[old.length];
            int q = Integer.MIN_VALUE;
            int r = -1;
            List<Integer> mov1 = mov;
            for (i = 0; i < mov1.size(); i++) {
                bits.byteCopy(old, 0, tab, 0, tab.length);
                doMov(mov1.get(i));
                doMove(0x20);
                List<Integer> mov2 = mov;
                for (int o = 0; o < mov2.size(); o++) {
                    bits.byteCopy(old, 0, tab, 0, tab.length);
                    doMov(mov1.get(i));
                    doMov(mov2.get(o));
                    doMove(0x10);
                    for (int p = 0; p < mov.size(); p++) {
                        bits.byteCopy(old, 0, tab, 0, tab.length);
                        doMov(mov1.get(i));
                        doMov(mov2.get(o));
                        doMov(mov.get(p));
                        int t = doEval(0x10) - doEval(0x20);
                        if (t < q) {
                            continue;
                        }
                        q = t;
                        r = i;
                        break;
                    }
                }
            }
            tab = old;
            if (r < 0) {
                break;
            }
            doMov(mov1.get(r));
        }
    }

    /**
     * finish screen
     */
    public void doFinish() {
        scr.doClear();
        scr.refresh();
    }

}

class userGameRacerOpp implements Comparable<userGameRacerOpp> {

    public int lne;

    public int spd;

    public int dst;

    public int clr;

    public void random() {
        spd = bits.random(90, 130);
        lne = bits.random(-1, 2);
        dst = bits.random(0, 2) * 100;
        clr = pipeScreen.colBright + bits.random(1, 8);
    }

    public int compareTo(userGameRacerOpp o) {
        if (dst < o.dst) {
            return -1;
        }
        if (dst > o.dst) {
            return +1;
        }
        return 0;
    }

    public void draw(pipeScreen scr, int dir, int bx, int by, int max) {
        if (max < 1) {
            return;
        }
        dir += lne;
        String[] chr;
        if (dir == 0) {
            chr = new String[]{
                "     _________",
                "   /`'-------'`\\",
                "  / /        \\ \\",
                " / /----------\\ \\",
                "/_-'-----------`-_\\",
                "\\[::]_________[::]/",
                " `--'         `--'"
            };
        } else if (dir < 0) {
            chr = new String[]{
                "        _,---____",
                "      ,/-`-----__^^`",
                "     /'         /  ()",
                "   /___       ,/  /",
                "___-__ ---___/  /'",
                "(_ --_^^----' _/",
                "  ^'  ^^^---.__)"
            };
        } else {
            chr = new String[]{
                "    ____---._",
                " '^^__-----'-.\\",
                "()   \\        `\\",
                "  \\  \\.       ___\\",
                "   `\\  \\___--- __-_\\",
                "     \\__`____^^_-- _)",
                "     (__.---^^^  `^"
            };
        }
        int tot = 0;
        for (int y = chr.length - 1; y >= 0; y--) {
            int x = chr[y].length();
            if (x < tot) {
                continue;
            }
            tot = x;
        }
        for (int y = chr.length - 1; y >= 0; y--) {
            byte[] b = chr[y].getBytes();
            int x = 0;
            for (; x < b.length; x++) {
                if (b[x] != 32) {
                    break;
                }
            }
            int py = by + (y * max) / tot;
            for (; x < b.length; x++) {
                int ch = b[x];
                if (ch == 32) {
                    continue;
                }
                int px = bx + (x * max) / tot;
                scr.putInt(px, py, pipeScreen.colBlack, clr, false, ch);
                if (max <= tot) {
                    continue;
                }
                scr.putInt(1 + px, py, pipeScreen.colBlack, clr, false, ch);
                scr.putInt(px, py + 1, pipeScreen.colBlack, clr, false, ch);
                scr.putInt(px + 1, py + 1, pipeScreen.colBlack, clr, false, ch);
            }
        }
    }

}

class userGameRacer {

    private pipeScreen scr;

    private List<userGameRacerOpp> ops;

    private int opm;

    private int opd;

    private String grs;

    private int sped;

    private int lane;

    /**
     * create game
     *
     * @param screen screen to use
     */
    public userGameRacer(pipeScreen screen) {
        scr = screen;
    }

    /**
     * start screen
     */
    public void doStart() {
        scr.doClear();
        ops = new ArrayList<userGameRacerOpp>();
        sped = 100;
        lane = 0;
        opm = bits.random(3, 6);
        opd = 0;
        grs = "";
        char[] gras = new char[]{' ', ' ', '_', '.', '-', '=', '^'};
        int len = 0;
        for (int i = 0; i < scr.sizX; i++) {
            len += bits.random(-1, 2);
            if (len < 0) {
                len = 0;
            }
            if (len >= gras.length) {
                len = gras.length - 1;
            }
            grs += gras[len];
        }
    }

    /**
     * finish screen
     */
    public void doFinish() {
        scr.doClear();
        scr.refresh();
    }

    public int[] coords(int lane, int dist) {
        dist = (int) (Math.sqrt(dist) * 10);
        int[] res = new int[2];
        res[0] = (scr.sizX / 2) + (lane * scr.sizX * (100 - dist)) / 400;
        res[1] = scr.sizY - ((dist * scr.sizY) / 100);
        return res;
    }

    /**
     * play game
     */
    public void doGame() {
        int[] lanep = new int[]{-5, -3, -1, 1, 3, 5};
        int[] lanes = new int[]{'/', '/', '/', '\\', '\\', '\\'};
        for (;;) {
            boolean crs = false;
            for (int i = ops.size() - 1; i >= 0; i--) {
                userGameRacerOpp op = ops.get(i);
                op.dst -= (sped - op.spd) / 2;
                if (op.dst > 100) {
                    ops.remove(op);
                    continue;
                }
                if (op.dst > 0) {
                    continue;
                }
                crs |= lane == op.lne;
                ops.remove(op);
            }
            for (int i = ops.size(); i < opm; i++) {
                userGameRacerOpp op = new userGameRacerOpp();
                op.random();
                for (;;) {
                    if (op.dst > 10) {
                        break;
                    }
                    if (lane != op.lne) {
                        break;
                    }
                    op.random();
                }
                opd++;
                ops.add(op);
            }
            Collections.sort(ops);
            scr.doClear();
            for (int i = 1; i < 5; i++) {
                int[] a = coords(lanep[i - lane], 1);
                int[] b = coords(lanep[i - lane], 100);
                scr.drawLine(a[0], a[1], b[0], b[1], pipeScreen.colBlack, pipeScreen.colWhite, lanes[i - lane]);
            }
            for (int i = 0; i < ops.size(); i++) {
                userGameRacerOpp op = ops.get(i);
                int[] a = coords(lanep[op.lne + 2 - lane], op.dst);
                int[] b = coords(lanep[op.lne + 3 - lane], op.dst);
                op.draw(scr, -lane, a[0] + 1, a[1], b[0] - a[0] - 2);
            }
            scr.putStr(0, 0, pipeScreen.colBlack, pipeScreen.colGreen, false, grs);
            scr.putStr(0, 0, 0, 7, false, sped + " km/h, score=" + opd);
            scr.refresh();
            if (crs) {
                return;
            }
            for (; scr.keyPress();) {
                int i = pipeScreen.getKey(scr.pipe);
                switch (i) {
                    case -1: // end
                        return;
                    case 0x800c: // up
                        sped += 5;
                        break;
                    case 0x800d: // down
                        sped -= 5;
                        break;
                    case 0x800e: // left
                        lane--;
                        break;
                    case 0x800f: // right
                        lane++;
                        break;
                    case 0x0271: // ctrl+q
                        return;
                    case 0x0278: // ctrl+x
                        return;
                    case 0x8005: // escape
                        return;
                }
            }
            if (sped < 0) {
                sped = 0;
            }
            if (sped > 250) {
                sped = 250;
            }
            if (lane < -1) {
                lane = -1;
            }
            if (lane > +1) {
                lane = +1;
            }
            bits.sleep(500);
        }
    }

}
