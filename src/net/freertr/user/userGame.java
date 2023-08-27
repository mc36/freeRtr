package net.freertr.user;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import net.freertr.cfg.cfgAll;
import net.freertr.pipe.pipeWindow;
import net.freertr.serv.servQuote;
import net.freertr.util.bits;
import net.freertr.util.cmds;
import net.freertr.util.version;

/**
 * screen games
 *
 * @author matecsaba
 */
public class userGame {

    private final userScreen console;

    /**
     * screen tester
     *
     * @param conn connection to use
     */
    public userGame(userScreen conn) {
        console = conn;
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

    private void colorDrawer(int[] god, List<String> sec) {
        int gods = god.length;
        console.putCls();
        for (int o = 0; o < sec.size(); o++) {
            String s = sec.get(o);
            byte[] b = s.getBytes();
            for (int i = 0; i < b.length; i++) {
                int ch = b[i];
                int cl = userScreen.colBrGreen;
                char chr = (char) ch;
                switch (chr) {
                    case 'o':
                    case '0':
                    case '@':
                    case 'O':
                    case '3':
                        cl = god[bits.random(0, gods)];
                        break;
                    default:
                        break;
                }
                console.putInt(i, o, false, cl, ch);
            }
        }
        console.refresh();
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            bits.sleep(1000);
        }
    }

    /**
     * palette test
     */
    public void doPalette() {
        console.putCls();
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
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            bits.sleep(1000);
        }
    }

    /**
     * ascii table
     */
    public void doAscTab() {
        console.putCls();
        for (int o = 0; o < 16; o++) {
            console.putStr(7, 2 + o, userScreen.colBlack, userScreen.colGreen, true, "" + o);
            console.putStr(10 + (o * 3), 1, userScreen.colBlack, userScreen.colGreen, true, "" + o);
            for (int i = 0; i < 16; i++) {
                console.putInt(10 + (i * 3), 2 + o, userScreen.colBlack, userScreen.colWhite, true, (o * 16) + i);
            }
        }
        console.putCur(0, 0);
        console.refresh();
        for (;;) {
            if (console.keyPress()) {
                break;
            }
            bits.sleep(1000);
        }
    }

    /**
     * keyboard codes
     */
    public void doKeys() {
        console.putCls();
        console.putCur(0, 0);
        console.refresh();
        for (;;) {
            int i = userScreen.getKey(console.pipe);
            console.putStr(console.sizX / 2, console.sizY / 2, userScreen.colBlack, userScreen.colWhite, false, bits.toHexW(i) + " " + (i & 0xff) + "       ");
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
    public void doText(List<String> s) {
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
            console.putCls();
            console.putMaps(bits.random(0, maxX), bits.random(0, maxY), userScreen.colBlack, bits.random(1, 15), false, s);
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
            console.putCls();
            console.putMaps(bits.random(0, maxX), bits.random(0, maxY), userScreen.colBlack, bits.random(1, 15), false, userScreen.fontText(s, " ", userFonts.fontFiller, font));
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
            int c = userScreen.colGreen;
            if (i >= (len - 2)) {
                c = userScreen.colBrGreen;
            }
            if (i >= (len - 1)) {
                c = userScreen.colBrWhite;
            }
            console.putInt(x, o, userScreen.colBlack, c, false, str[i]);
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
            console.putCls();
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
            console.putCls();
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
            console.putCls();
            for (int y = 0; y < console.sizY; y++) {
                for (int x = 0; x < console.sizX; x++) {
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
                    console.putInt(x, y, userScreen.colBlack, o, false, 88);
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
            console.putCls();
            for (int y = 0; y < console.sizY; y++) {
                for (int x = 0; x < console.sizX; x++) {
                    int i = buf[y + 4][x + 4];
                    if (i == 0) {
                        i = 32;
                    } else {
                        i = 88;
                    }
                    console.putInt(x, y, userScreen.colBlack, userScreen.colWhite, false, i);
                }
            }
            console.refresh();
            bits.sleep(500);
        }
    }

    private void doAntBall() {
        console.putCls();
        for (;;) {
            if (console.keyPress()) {
                return;
            }
            for (int o = 0; o < console.sizY; o++) {
                for (int i = 0; i < console.sizX; i++) {
                    int p;
                    if ((bits.randomB() & 1) == 0) {
                        p = userScreen.colBlack;
                    } else {
                        p = userScreen.colWhite;
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
        if (a.equals("zenmaster")) {
            doZenmaster(cmd);
            return;
        }
        if (a.equals("tetris")) {
            userGameTetris t = new userGameTetris(console);
            t.doStart();
            t.doGame();
            t.doFinish();
            return;
        }
        if (a.equals("minesweep")) {
            userGameMinesweep t = new userGameMinesweep(console);
            t.doStart();
            t.doGame();
            t.doFinish();
            return;
        }
        if (a.equals("clear")) {
            userScreen.sendTit(console.pipe, cfgAll.hostName);
            return;
        }
        int i = version.findSecret(a);
        if (i >= 0) {
            List<String> sec = version.shSecret(i);
            int[] god = new int[6];
            god[0] = userScreen.colBrCyan;
            god[1] = userScreen.colBrWhite;
            god[2] = userScreen.colBrYellow;
            god[3] = userScreen.colBrGreen;
            god[4] = userScreen.colBrBlue;
            god[5] = userScreen.colBrRed;
            colorDrawer(god, sec);
            return;
        }
        if (a.equals("ansi")) {
            userFlash.ansiArt(cmd.getRemaining(), console);
            console.refresh();
            for (;;) {
                if (console.keyPress()) {
                    break;
                }
                bits.sleep(1000);
            }
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
        if (a.equals("keys")) {
            doKeys();
            return;
        }
        if (a.equals("title")) {
            a = cmd.getRemaining();
            if (a.length() < 1) {
                a = cfgAll.hostName;
            }
            userScreen.sendTit(console.pipe, a);
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
                txt = userScreen.fontText(a, " ", userFonts.fontFiller, userFonts.font8x16());
            }
            doText(txt);
            return;
        }
        if (a.equals("clock")) {
            doClock(userFonts.font8x16());
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
        if (a.equals("life")) {
            doLife();
            return;
        }
        if (a.equals("antball")) {
            doAntBall();
            return;
        }
        if (a.equals("image")) {
            if (cfgAll.limited) {
                cmd.error("not in a vdc");
                return;
            }
            doText(pipeWindow.imageText(new File(cmd.getRemaining()), console.sizX, console.sizY, userFonts.imageData));
            return;
        }
        cmd.badCmd();
    }

}

class userGameMinesweep {

    private boolean[][] bomb;

    private boolean[][] mark;

    private boolean[][] show;

    private int curX;

    private int curY;

    private userScreen scr;

    private final static int sizeX = 60;

    private final static int sizeY = 20;

    /**
     * create game
     *
     * @param screen screen to use
     */
    public userGameMinesweep(userScreen screen) {
        scr = screen;
    }

    /**
     * start screen
     */
    public void doStart() {
        scr.putCls();
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
        scr.putCls();
        for (int i = 0; i < sizeX; i++) {
            for (int o = 0; o < sizeY; o++) {
                String s;
                int p;
                if (show[o][i]) {
                    s = "" + bombs(o, i);
                    p = userScreen.colGreen;
                } else {
                    s = ".";
                    p = userScreen.colWhite;
                }
                if (mark[o][i]) {
                    s = "*";
                    p = userScreen.colRed;
                }
                scr.putStr(i, o, userScreen.colBlack, p, false, s);
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
            int i = userScreen.getKey(scr.pipe);
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
        scr.putCls();
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

class userGameGomoku {

    private int[][] store;

    private int curX;

    private int curY;

    private userScreen scr;

    private final static int sizeX = 30;

    private final static int sizeY = 20;

    /**
     * create game
     *
     * @param screen screen to use
     */
    public userGameGomoku(userScreen screen) {
        scr = screen;
    }

    /**
     * start screen
     */
    public void doStart() {
        scr.putCls();
        store = new int[sizeX][sizeY];
        curX = sizeX / 2;
        curY = sizeY / 2;
    }

    /**
     * print table
     */
    public void doPrint() {
        scr.putCls();
        for (int i = 0; i < sizeY; i++) {
            for (int o = 0; o < sizeX; o++) {
                String s;
                int p;
                switch (store[o][i]) {
                    case 1:
                        s = "X";
                        p = userScreen.colGreen;
                        break;
                    case 2:
                        s = "O";
                        p = userScreen.colRed;
                        break;
                    default:
                        s = ".";
                        p = userScreen.colWhite;
                        break;
                }
                scr.putStr(o * 2, i, userScreen.colBlack, p, false, s);
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
            int i = userScreen.getKey(scr.pipe);
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
        scr.putCls();
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

class userGameTetris implements Runnable {

    private userScreen scr;

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
    public userGameTetris(userScreen screen) {
        scr = screen;
        things = new ArrayList<userGameTetrisThing>();
        String[] lst = new String[3];
        lst[0] = " X";
        lst[1] = "XX";
        lst[2] = "X ";
        things.add(userGameTetrisThing.fromString(lst, userScreen.colBrBlue));
        lst[0] = "X ";
        lst[1] = "XX";
        lst[2] = " X";
        things.add(userGameTetrisThing.fromString(lst, userScreen.colBrGreen));
        lst[0] = "X ";
        lst[1] = "XX";
        lst[2] = "X ";
        things.add(userGameTetrisThing.fromString(lst, userScreen.colBrCyan));
        lst[0] = "X ";
        lst[1] = "X ";
        lst[2] = "XX";
        things.add(userGameTetrisThing.fromString(lst, userScreen.colBrYellow));
        lst[0] = " X";
        lst[1] = " X";
        lst[2] = "XX";
        things.add(userGameTetrisThing.fromString(lst, userScreen.colBrWhite));
        lst = new String[4];
        lst[0] = "X";
        lst[1] = "X";
        lst[2] = "X";
        lst[3] = "X";
        things.add(userGameTetrisThing.fromString(lst, userScreen.colBrMagenta));
        lst = new String[2];
        lst[0] = "XX";
        lst[1] = "XX";
        things.add(userGameTetrisThing.fromString(lst, userScreen.colBrRed));
    }

    /**
     * start screen
     */
    public void doStart() {
        need2run = true;
        scr.putCls();
        tab = new int[sizeY][];
        thg = things.get(bits.random(0, things.size())).copyBytes();
        nxt = things.get(bits.random(0, things.size())).copyBytes();
        curY = 0;
        curX = sizeX / 2;
        for (int o = 0; o < sizeY; o++) {
            tab[o] = new int[sizeX];
            for (int i = 0; i < sizeX; i++) {
                tab[o][i] = userScreen.colBlack;
            }
        }
        lines = 0;
        new Thread(this).start();
    }

    /**
     * print table
     */
    public synchronized void doPrint() {
        scr.putCls();
        for (int o = 0; o < sizeY; o++) {
            scr.putStr(19, o + 1, userScreen.colBlack, userScreen.colWhite, false, "|");
            scr.putStr(20 + (sizeX * 2), o + 1, userScreen.colBlack, userScreen.colWhite, false, "|");
            for (int i = 0; i < sizeX; i++) {
                if (tab[o][i] == userScreen.colBlack) {
                    continue;
                }
                scr.putStr((i * 2) + 20, o + 1, tab[o][i], tab[o][i], false, "XX");
            }
        }
        nxt.doPrint(2, 2, scr);
        scr.putStr(2, 10, userScreen.colBlack, userScreen.colWhite, false, "lines=" + lines);
        thg.doPrint((curX * 2) + 20, curY + 1, scr);
        scr.putCur(0, 0);
        scr.refresh();
    }

    /**
     * finish screen
     */
    public void doFinish() {
        need2run = false;
        scr.putCls();
        scr.refresh();
    }

    /**
     * play game
     */
    public void doGame() {
        for (;;) {
            doPrint();
            int i = userScreen.getKey(scr.pipe);
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
                if (tab[o][i] == userScreen.colBlack) {
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
                tab[0][i] = userScreen.colBlack;
            }
            lines++;
            o++;
        }
    }

    public void run() {
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
                need2run = false;
                return;
            }
        }
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
                tab[o][i] = userScreen.colBlack;
            }
        }
    }

    public static userGameTetrisThing fromString(String[] lst, byte col) {
        userGameTetrisThing t = new userGameTetrisThing(lst[0].length(), lst.length);
        for (int o = 0; o < t.sizeY; o++) {
            String a = lst[o];
            for (int i = 0; i < t.sizeX; i++) {
                t.tab[o][i] = a.substring(i, i + 1).equals("X") ? col : userScreen.colBlack;
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

    public void doPrint(int x, int y, userScreen scr) {
        for (int o = 0; o < sizeY; o++) {
            for (int i = 0; i < sizeX; i++) {
                if (tab[o][i] == userScreen.colBlack) {
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
                if (tab[o][i] == userScreen.colBlack) {
                    continue;
                }
                if (t[o + y][i + x] != userScreen.colBlack) {
                    return false;
                }
            }
        }
        return true;
    }

    public void putThing(int[][] t, int x, int y) {
        for (int o = 0; o < sizeY; o++) {
            for (int i = 0; i < sizeX; i++) {
                if (tab[o][i] == userScreen.colBlack) {
                    continue;
                }
                t[o + y][i + x] = tab[o][i];
            }
        }
    }

}
