package user;

import util.bits;

/**
 * gomoku
 *
 * @author matecsaba
 */
public class userGomoku {

    private int store[][];

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
    public userGomoku(userScreen screen) {
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
                    if (store[curX][curY] != 0) {
                        break;
                    }
                    store[curX][curY] = 1;
                    int ps[] = evalMove(2);
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
