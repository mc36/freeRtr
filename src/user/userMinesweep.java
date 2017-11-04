package user;

import util.bits;

/**
 * minesweeper
 *
 * @author matecsaba
 */
public class userMinesweep {

    private boolean bomb[][];

    private boolean mark[][];

    private boolean show[][];

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
    public userMinesweep(userScreen screen) {
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
            int i = userVM.getKey(scr.pipe);
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
