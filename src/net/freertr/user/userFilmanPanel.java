package net.freertr.user;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import net.freertr.util.bits;

/**
 * file manager panel
 *
 * @author matecsaba
 */
public class userFilmanPanel {

    private final userScreen con;

    private final int scrX;

    private final int scrY;

    private final int sizX;

    private final int sizY;

    private int begL;

    /**
     * list of files
     */
    protected final List<String> fil = new ArrayList<String>();

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
        if (path.length() > 1) {
            fil.add(path + "../");
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
        }
        doRange();
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
            if ((begL + i) >= fil.size()) {
                a = "";
            } else {
                a = fil.get(begL + i);
            }
            if (a.startsWith(path)) {
                a = a.substring(path.length(), a.length());
            }
            a = bits.padEnd(a, sizX, " ").substring(0, sizX);
            con.putStr(scrX, scrY + i + 1, userScreen.colBlack, userScreen.colWhite, false, a);
        }
        a = bits.padEnd((curL + 1) + "/" + fil.size(), sizX, " ");
        con.putStr(scrX, scrY + sizY + 1, userScreen.colBlue, userScreen.colBrCyan, false, a);
        con.putStr(con.sizX - 8, con.sizY - 1, userScreen.colBlue, userScreen.colWhite, false, "f1=help");
    }

}
