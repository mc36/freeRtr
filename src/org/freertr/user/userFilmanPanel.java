package org.freertr.user;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import org.freertr.util.bits;

/**
 * file manager panel
 *
 * @author matecsaba
 */
public class userFilmanPanel {

    /**
     * the screen
     */
    protected final userScreen con;

    /**
     * beginning of screen
     */
    protected final int scrX;

    /**
     * beginning of screen
     */
    protected final int scrY;

    /**
     * size of screen
     */
    protected final int sizX;

    /**
     * size of screen
     */
    protected final int sizY;

    /**
     * beginning of screen
     */
    protected int begL;

    /**
     * list of files
     */
    protected final List<String> fil = new ArrayList<String>();

    /**
     * selection of files
     */
    protected final List<Boolean> sel = new ArrayList<Boolean>();

    /**
     * current path
     */
    protected String path;

    /**
     * current position
     */
    protected int curL;

    /**
     * create instance
     *
     * @param scr screen to draw to
     * @param bx beginning on screen
     * @param by beginning on screen
     * @param sx size on screen
     * @param sy size on screen
     */
    protected userFilmanPanel(userScreen scr, int bx, int by, int sx, int sy) {
        con = scr;
        scrX = bx;
        scrY = by;
        sizX = sx;
        sizY = sy;
    }

    /**
     * read up directory
     */
    protected void readUp() {
        fil.clear();
        sel.clear();
        if (path.length() > 1) {
            fil.add(path + "../");
            sel.add(false);
        }
        File[] res = userFlash.dirList(path);
        if (res == null) {
            return;
        }
        for (int i = 0; i < res.length; i++) {
            String a = path + res[i].getName();
            if (res[i].isDirectory()) {
                a += "/";
            }
            fil.add(a);
            sel.add(false);
        }
        doRange();
    }

    /**
     * count selected files
     *
     * @param sl selection
     * @return number of files
     */
    protected int cntSel(boolean sl) {
        int res = 0;
        for (int i = 0; i < sel.size(); i++) {
            if (sel.get(i) == sl) {
                res++;
            }
        }
        return res;
    }

    /**
     * count selected files
     *
     * @param sl selection
     * @return list of files
     */
    protected List<String> getSel(boolean sl) {
        List<String> res = new ArrayList<String>();
        for (int i = 0; i < sel.size(); i++) {
            if (sel.get(i) != sl) {
                continue;
            }
            res.add("" + fil.get(i));
        }
        return res;
    }

    /**
     * check if in range
     */
    protected void doRange() {
        if (curL < 0) {
            curL = 0;
        }
        if (curL >= fil.size()) {
            curL = fil.size() - 1;
        }
        if (begL > curL) {
            begL = curL - 5;
        }
        if (begL <= (curL - sizY)) {
            begL = curL - sizY + 5;
        }
        if (begL > (fil.size() - sizY)) {
            begL = fil.size() - sizY;
        }
        if (begL < 0) {
            begL = 0;
        }
    }

    /**
     * get current file
     *
     * @return file, null if nothing
     */
    protected String getFn() {
        if (curL >= fil.size()) {
            return null;
        }
        return fil.get(curL);
    }

    /**
     * process enter key
     */
    protected void doEnter() {
        String a = getFn();
        begL = 0;
        curL = 0;
        if (!a.endsWith("/")) {
            return;
        }
        if (a.endsWith("/./")) {
            return;
        }
        if (!a.endsWith("/../")) {
            path = a;
            readUp();
            doRange();
            return;
        }
        a = path.substring(0, path.length() - 1);
        int i = a.lastIndexOf("/");
        if (i < 0) {
            return;
        }
        path = a.substring(0, i + 1);
        a += "/";
        readUp();
        for (i = 0; i < fil.size(); i++) {
            if (a.equals(fil.get(i))) {
                curL = i;
            }
        }
        doRange();
    }

    /**
     * put cursor
     */
    protected void doCurs() {
        int ln = scrY + curL - begL + 1;
        con.putCols(scrX, ln, userScreen.colWhite, userScreen.colBlack, sizX);
    }

    /**
     * draw the panel
     */
    protected void doDraw() {
        String a = bits.padEnd(path, sizX, " ").substring(0, sizX);
        con.putStr(scrX, scrY, userScreen.colGreen, userScreen.colBrYellow, false, a);
        for (int i = 0; i < sizY; i++) {
            int o = userScreen.colWhite;
            if ((begL + i) >= fil.size()) {
                a = "";
            } else {
                if (sel.get(begL + i)) {
                    o = userScreen.colBrYellow;
                }
                a = fil.get(begL + i);
            }
            if (a.startsWith(path)) {
                a = a.substring(path.length(), a.length());
            }
            a = bits.padEnd(a, sizX, " ").substring(0, sizX);
            con.putStr(scrX, scrY + i + 1, userScreen.colBlack, o, false, a);
        }
        a = bits.padEnd(cntSel(true) + "/" + (curL + 1) + "/" + fil.size(), sizX, " ");
        con.putStr(scrX, scrY + sizY + 1, userScreen.colBlue, userScreen.colBrCyan, false, a);
        con.putStr(con.sizX - 8, con.sizY - 1, userScreen.colBlue, userScreen.colWhite, false, "f1=help");
    }

}
