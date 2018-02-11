package user;

import java.util.ArrayList;
import java.util.List;
import util.bits;

/**
 * tetris
 *
 * @author matecsaba
 */
public class userTetris implements Runnable {

    private userScreen scr;

    private List<userTetrisThing> things;

    private int tab[][];

    private int curX;

    private int curY;

    private int lines;

    private userTetrisThing thg;

    private userTetrisThing nxt;

    private boolean need2run;

    private final static int sizeX = 10;

    private final static int sizeY = 20;

    /**
     * create game
     *
     * @param screen screen to use
     */
    public userTetris(userScreen screen) {
        scr = screen;
        things = new ArrayList<userTetrisThing>();
        String[] lst = new String[3];
        lst[0] = " X";
        lst[1] = "XX";
        lst[2] = "X ";
        things.add(userTetrisThing.fromString(lst, userScreen.colBrBlue));
        lst[0] = "X ";
        lst[1] = "XX";
        lst[2] = " X";
        things.add(userTetrisThing.fromString(lst, userScreen.colBrGreen));
        lst[0] = "X ";
        lst[1] = "XX";
        lst[2] = "X ";
        things.add(userTetrisThing.fromString(lst, userScreen.colBrCyan));
        lst[0] = "X ";
        lst[1] = "X ";
        lst[2] = "XX";
        things.add(userTetrisThing.fromString(lst, userScreen.colBrYellow));
        lst[0] = " X";
        lst[1] = " X";
        lst[2] = "XX";
        things.add(userTetrisThing.fromString(lst, userScreen.colBrWhite));
        lst = new String[4];
        lst[0] = "X";
        lst[1] = "X";
        lst[2] = "X";
        lst[3] = "X";
        things.add(userTetrisThing.fromString(lst, userScreen.colBrMagenta));
        lst = new String[2];
        lst[0] = "XX";
        lst[1] = "XX";
        things.add(userTetrisThing.fromString(lst, userScreen.colBrRed));
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
            int i = userVM.getKey(scr.pipe);
            if (!need2run) {
                return;
            }
            switch (i) {
                case -1: // end
                    return;
                case 0x800c: // up
                    userTetrisThing o = thg.copyBytes();
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

class userTetrisThing {

    private byte tab[][];

    private int sizeX;

    private int sizeY;

    public userTetrisThing(int sx, int sy) {
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

    public static userTetrisThing fromString(String[] lst, byte col) {
        userTetrisThing t = new userTetrisThing(lst[0].length(), lst.length);
        for (int o = 0; o < t.sizeY; o++) {
            String a = lst[o];
            for (int i = 0; i < t.sizeX; i++) {
                t.tab[o][i] = a.substring(i, i + 1).equals("X") ? col : userScreen.colBlack;
            }
        }
        return t;
    }

    public userTetrisThing copyBytes() {
        userTetrisThing t = new userTetrisThing(sizeX, sizeY);
        for (int o = 0; o < sizeY; o++) {
            bits.byteCopy(tab[o], 0, t.tab[o], 0, sizeX);
        }
        return t;
    }

    public userTetrisThing doRotate() {
        userTetrisThing t = new userTetrisThing(sizeY, sizeX);
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

    public boolean isSpace(int t[][], int x, int y) {
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

    public void putThing(int t[][], int x, int y) {
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
