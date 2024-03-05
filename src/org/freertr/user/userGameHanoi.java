package org.freertr.user;

import java.util.ArrayList;
import java.util.List;

/**
 * tower of hanoi
 *
 * @author matecsaba
 */
public class userGameHanoi {

    private userScreen scr;

    private List<List<Integer>> towerC;

    private int towerX[];

    private int curX;

    private int selX;

    /**
     * create game
     *
     * @param screen screen to use
     */
    public userGameHanoi(userScreen screen) {
        scr = screen;
    }

    /**
     * start screen
     */
    public void doStart() {
        scr.putCls();
        towerC = new ArrayList<List<Integer>>();
        towerX = new int[3];
        int towerS = towerX.length + 2;
        for (int i = 0; i < towerX.length; i++) {
            towerX[i] = (i + 1) * (scr.sizX / towerS);
            towerC.add(new ArrayList<Integer>());
        }
        List<Integer> tw = towerC.get(0);
        for (int i = 10; i > 0; i--) {
            tw.add(Integer.valueOf(i));
        }
        curX = 0;
        selX = -1;
    }

    /**
     * print table
     */
    public void doPrint() {
        scr.putCls();
        for (int i = 0; i < towerX.length; i++) {
            List<Integer> tw = towerC.get(i);
            for (int o = 0; o < tw.size(); o++) {
                int p = tw.get(o);
                int r = scr.sizY - o - 1;
                for (int q = 0; q <= p; q++) {
                    scr.putInt(towerX[i] - q, r, false, userScreen.colBrGreen, 'X');
                    scr.putInt(towerX[i] + q, r, false, userScreen.colBrGreen, 'X');
                }
            }
        }
        for (int i = 0; i < towerX.length; i++) {
            int p = userScreen.colWhite;
            if (selX == i) {
                p = userScreen.colBrCyan;
            }
            if (curX == i) {
                p = userScreen.colBrYellow;
            }
            for (int o = 0; o < scr.sizY; o++) {
                scr.putInt(towerX[i], o, false, p, '|');
            }
        }
        scr.putCur(towerX[curX], 0);
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
     * finish screen
     */
    public void doFinish() {
        scr.putCls();
        scr.refresh();
    }

}
